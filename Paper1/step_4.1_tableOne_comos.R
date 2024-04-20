#######################################################################################################
#                                           *
#                                       PhD PROJECT
#                                    ELENI DOMZARIDOU
#                                UNIVERSITY OF MANCHESTER
#                                       July 2021
#                                   CPRD GOLD / Aurum
#                                           *
# 
# Aims:
# a) The code populates table one for comorbidities.
# b) It works for both Aurum/GOLD, since user provides the name of the database
# 
#######################################################################################################

library(tableone)
library(dplyr)

# --------------------------------------------------------------------------------------
# continue working using the dataframe: united_como_master from the previous step
# --------------------------------------------------------------------------------------

# Provide database name
database <- "Aurum" # <----- *** USER INPUT ***

# load the master file
load(paste0("~/rds/Eleni_", database,"_JAN_2021/final_master.RData"))

# keep the patid, index_date from the master and merged them with the separated files of comorbid conditions
# to examine comorbidities at baseline (como_base = YES if my_first_diagnosis_date < index_date):
if(database == "GOLD"){
  master_OST_mod <- my_patients %>% 
    filter(prereg >= 1) %>%
    select(c("patid_set20", "therapy", "switcher")) %>% 
    distinct()
}else{
  master_OST_mod <- my_patients %>% 
    filter(prereg >= 1) %>%
    select(c("patid", "therapy", "switcher")) %>% 
    distinct()
}
colnames(master_OST_mod)
str(master_OST_mod)
head(master_OST_mod)
master_OST_mod %>% group_by(patid) %>% summarise(N = n())
sum(is.na(master_OST_mod$patid))
master_OST_mod %>% group_by(therapy, switcher) %>% summarise(N = n())
ls(master_OST_mod)

# merge with (PC/HES) comorbidities file
ls(united_como_master2)
todata <- left_join(master_OST_mod, united_como_master2, by = "patid")
ls(todata)
todata %>% group_by(patid) %>% summarise(N = n())
summary(todata)

# if base_como.hes/fu_como.hes = NAs --> this patid does not appear in HES 
# If index_date = NA --> this patid does not have any of the comos of interest but we do not exclude
# them, otherwise the % in TableOne will refer to comorbid people and what we want is the % to refer
# to the overall cohort of patients (comorbid or not)
todata <- todata %>% distinct()

todata %>% count(patid)

ls(todata)

dput(names(todata))
myvars <- c("ACS", "Alcohol", "Anxiety", 
            "Asthma", "Bipolar", "CKD", "COPD", "Current_ex_Smoker", 
            "CVD_all", "Depression", "Diabetes", "Endocarditis", "Gastric_ulcer", 
            "HBV", "HCV", "Hepatitis_NOS", "HIV", "Homelessness", "Hypertension", 
            "IHD", "Liver_NH", "Overdose", "Pericarditis", "PersonalityDisorders", 
            "Prison", "Pulmonary_embolism", "Schizophrenia", "Self_harm", 
            "SSTI", "Stroke") # Arrhythmia, "Current_Ex_Smoker", 

# Unify results from HES.APC and Primary care records by merging records whereby
# e.g. the patients appears without CVD in PC but s/he has a CVD code in HES.
# We startify here be therapy and medication switch, otherwise thesevars will disappear
# from tableOne.
todata <- todata %>% group_by(patid, therapy, switcher) %>% summarise_at(myvars, max) %>% ungroup()

# convert columns into factor vars
todata[,myvars] <- lapply(todata[,myvars], factor)

# Assign strata
factorVars <- c("therapy", "switcher")

# produce TableOne
TOoutput <- CreateTableOne(vars = myvars, strata = factorVars, data = todata)
TOoutput
capture.output(print(TOoutput, formatOptions = list(big.mark = ",")), 
               file =paste0("./", database, "_tableone_comos.txt"))

# ------------------SAVE THE MASTER COMORBIDITIES FILE TO CREATE THE MASTER COHORT LATER------------

united_como_master2$X <- NULL

# Unify results from HES.APC and Primary care records by merging records whereby
# e.g. the patients appears without CVD in PC but s/he has a CVD code in HES.
# Each row must represent one patient with his/her medical history on var of interest.
mytemp_e <-united_como_master2 %>% group_by(patid) %>% summarise_at(myvars, max) %>% ungroup()
mytemp_e %>% count(patid)
summary(mytemp_e)

write.csv(mytemp_e, paste0("Comorbidities_master_", database, ".HES.csv"))

# --------------------------------------------------------------------------------------------------

# <|>

# --------------------------------------------------------------------------------------------------
# LAST CHECK: Examine if the Table One contains the correct number of comos that you have identified
# in GP and HES records
# --------------------------------------------------------------------------------------------------

united_como_master.hes <- full_join(united_como_master, temp, by = c("patid", "como_base"))

# count the number of patids per comorbidity that you have identified in HES
xx <- united_como_master.hes %>% filter(!is.na(source)) %>%
  group_by(como_base) %>% 
  summarise(N = n_distinct(patid))
View(xx)

# count the number of patids per comorbidity that you have identified in BOTH HES AND GP records
xx2 <- united_como_master.hes %>% 
  group_by(como_base) %>% 
  filter(como_id == 1 & base_como.hes == 1) %>%
  summarise(N = n_distinct(patid))
View(xx2)

# To check is the results are the same with table 1 apply the formula:
#  ToOutput_results_per row = comos_PC_GOLD$Comos + (xx$N - xx2$N)
#
# Compare the results with those from files "comos_PC_GOLD/Aurum", "b.GOLD/Aurum.HES.csv"
#  and " GOLD/Aurum_tableone_comos.txt"
#
# --------------------------------------------------------------------------------------------------

