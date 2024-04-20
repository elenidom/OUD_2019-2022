#######################################################################################################
#                                           *
#                                       PhD PROJECT
#                                    ELENI DOMZARIDOU
#                                UNIVERSITY OF MANCHESTER
#                                   March-September 2021
#                                        CPRD Aurum
#                                           *
# 
# Aims:
# a) To append clinical and product codes in order to generate the Table one that describes
#    medication and comorbidities at baseline.
#    This code prepares the medical and prescription data in order to be linked to the master
#    files before modelling, in a way that enables a time-dependent analysis.
# b) Here, we append clinical codes from GP records and those that have been identified via
#    HES.APC into one file 'united_como_master2'
# c) The dataframe 'united_como_master2' is the feed for the next file 'step_4.1_tableOne_comos.R'
#    that generates the Table one.
# 
#######################################################################################################

library(dplyr)
library(lubridate)

# -----------------------------------------------------------------------------------------------------
#                           Product codes dictionary + merged_therapy
# -----------------------------------------------------------------------------------------------------

setwd("/mnt/bmh01-rds/Ashcroft_Domzaridou_PSTRC/mbmhted6/Eleni_Aurum_JAN_2021")

# merge the separated therapy files (they are already merged with the master cohort)
merged_therapy_1 <- read.csv("/mnt/bmh01-rds/Ashcroft_Domzaridou_PSTRC/mbmhted6/Eleni_Aurum_JAN_2021/source_data/merged_drugIssue_part1.csv") 
merged_therapy_2 <- read.csv("/mnt/bmh01-rds/Ashcroft_Domzaridou_PSTRC/mbmhted6/Eleni_Aurum_JAN_2021/source_data/merged_drugIssue_part2.csv") 
merged_therapy <- dplyr::union(merged_therapy_1, merged_therapy_2) %>% distinct()

head(merged_therapy)
str(merged_therapy)

merged_therapy$patid <- as.character(merged_therapy$patid)
merged_therapy$X <- NULL
merged_therapy$staffid <- as.character(merged_therapy$staffid)
merged_therapy$pracid <- as.character(merged_therapy$pracid)
merged_therapy$prodcodeid <- as.character(merged_therapy$prodcodeid)

# merge the therapy file with the product codes dictonary in order to complete the names of the products
product <- read.table(file = "./source_data/Lookups_2021_01_Aurum/202101_EMISProductDictionary.txt",
                      fill = TRUE,
                      quote = "", 
                      header = TRUE, 
                      sep = "\t")
head(product)
str(product)
summary(product)
colnames(product)
names(product) <- c("prodcodeid", "dmdid", "term", "productname", "formulation",
                    "route", "drugsubstancename", "strength", "bnf", "release")
product$prodcodeid <- as.character(product$prodcodeid)
product$dmdid <- as.character(product$dmdid)
product$release <- NULL


# merge therapy and product codes dictionary:
merged_therapy <- left_join(merged_therapy, product, by = "prodcodeid")

# check
head(merged_therapy)
summary(merged_therapy)
merged_therapy %>% summarise(N = n_distinct(patid))

# Load the working space that contains your Aurum master file
load("./final_master.RData")

ls(my_patients)

# merge therapy + master file (because we need additional variables from master file)
my_patients <- my_patients %>% 
  filter(prereg >= 1) %>% # here, we use only the current registration date since Aurum does not contain uts data
  select(c("patid", "index_date", "prodcodeid", "therapy", "switcher", "mycrd")) %>% 
  distinct() 

my_patients %>% count(patid)

my_patients$prodcodeid <- as.character(my_patients$prodcodeid)

my_patients$therapy <- as.factor(my_patients$therapy)

str(my_patients)

# Merge product codes (all records) with additional columns from the master file. If you merged 
# like: left_join(my_patients, merged_therapy, by ...) then you will get only records with
# patients exposed to OST only, excluding other drugs, this is why we run the merge using the
# arguments the other way round.
merged_therapy <- merged_therapy %>% distinct()
merged_therapy <- left_join(merged_therapy, my_patients, by = c("patid", "prodcodeid"))


# Calculate prescriptions frequency
a <- data.frame(with(merged_therapy, table(productname)))
a <- arrange(a, desc(Freq))


# The merged_therapy dataframe contains patids that are not present in my master cohort (e.g. they may
# have prereg < 1 month etc). Therefore, we need a variable to clean the data. Such patids
# do not have index_date, since they are not present in the my_patients dataframe whereby all the
# patiens have index_date. We will filter the data to exclude patids without index_date, but first we need to
# fill-in missing values for drug substance, therapy and switcher vars

require(tidyverse)

merged_therapy <- merged_therapy %>%
  group_by(patid) %>%
  fill(switcher, index_date, mycrd, .direction = "down") %>%
  fill(switcher, index_date, mycrd, .direction = "up") %>%
  dplyr::ungroup()

merged_therapy %>% count(patid)

merged_therapy <- merged_therapy %>% filter(!is.na(index_date))

# check if the numbers you get per therapy arm are same:
my_patients %>% group_by(therapy, switcher) %>% summarise(N = n_distinct(patid))

my_patients %>% summarise(N = n_distinct(patid))

summary(merged_therapy)

# clean env
rm("my_patients_vars")

# --------------------------------------------------------------------------------------------------
#           APPEND PRODUCT CODE LISTS (e.g.: benzodiazepines, z-drugs, gabapentinoids)
# --------------------------------------------------------------------------------------------------

# ----------------------------------------------------------------------------------------
# NEW FUNCTION: append_drug_lists (returns the dataframe that you provided as an argument)
# ----------------------------------------------------------------------------------------
# Examine whether the patient received the meds of interest in the year before the index date

append_drug_lists <- function(merged_therapy, drug_name){ # name of the list with meds of interest
  
  mydrug <- read.csv(paste0("./source_data/comorbid/", drug_name, "_Aurum.csv"))
  mydrug$prodcodeid <- as.character(mydrug$prodcodeid)
  mydrug$X <- NULL
  
  # append product codes with master file
  merged_therapy <- left_join(merged_therapy, mydrug, by = "prodcodeid") # merge mydrug codes with master file that contains all the prodcodes for people who receive OST
  
  merged_therapy <- merged_therapy %>%
    mutate(
      "{drug_name}" := ifelse(is.na(.data[[drug_name]]), 0, .data[[drug_name]]) # substitute NAs with zero or with the name of the drug (e.g. BNZ)
    )
  
  return(merged_therapy)
}

# ----------------
# Function call!
# ----------------
t <- append_drug_lists(merged_therapy, "GABA") # Gabapentinoids including pregabalin and gabapentin
t <- append_drug_lists(t, "BNZ")               # Benzodiazepines including 
t <- append_drug_lists(t, "Z_drugs")           # Z-drugs including Zopiclone, Zolpidem, Zaleplon
t <- append_drug_lists(t, "ANTIPSYCHO")        # Antipsychotic drugs
t <- append_drug_lists(t, "MOODSTAB")          # Mood stabilisers including anticonvulsants, Li+, and off-label drugs excluding GABApentinoids
t <- append_drug_lists(t, "ANTIDEP")           # Antidepressants including MAOIs, SSRIs, TCAs and others
t <- append_drug_lists(t, "OPIOIDS_PAIN")      # Opioids for pain excluding OAT formulations

# Cleaning
# ----------
# Unite the separated columns that contain information on the drug name (e.g. Clonazepam instead of BNZ)
# into one column named "mydrugsubstance_all" and remove the rest of the related columns that you used
# to create the new one (see tidyr::unite(..., remove = TRUE)).
ls(t)
cols_unite <- c("mydrugsubstance.x", "mydrugsubstance.x.x", "mydrugsubstance",
                "mydrugsubstance.x.x.x", "mydrugsubstance.y",    
                "mydrugsubstance.y.y", "mydrugsubstance.y.y.y")

t <- t %>% tidyr::unite("mydrugsubstance_all", cols_unite, na.rm = TRUE, remove = TRUE)
t %>% group_by(mydrugsubstance_all) %>% summarise(N = n_distinct(patid))

# Do the same for drug classes
cols_unite_class <- c("my_class.x", "my_class.x.x", "my_class.x.x.x", "my_class",
                      "my_class.y", "my_class.y.y", "my_class.y.y.y")

t <- t %>% tidyr::unite("myclass_all", all_of(cols_unite_class), na.rm = TRUE, remove = TRUE)
t %>% group_by(myclass_all) %>% summarise(N = n_distinct(patid))

# -----------------------------------------------------------------------------
#    myclass_all                     Comments
# -----------------------------------------------------------------------------
# 1 ""                   = Any other
# 2 "Anticonvulsant"     
# 3 "Benzodiazepine"    
# 4 "FirstGeneration"    = Antipsychotics
# 5 "Lithium"             
# 6 "NBBRA"              = Non-benzodiazepine (usually Z-drugs)
# 7 "OffLabel"            = Mood stabilisers
# 8 "SecondGeneration"   = Antipsychotics
# 9 "NRI"                = Norepinephrine reuptake inhibitors
# 10 "OffLabel"            
# 11 "Other"               
# 12 "SARI"              = Serotonin antagonist and reuptake inhibitors
# 13 "SecondGeneration"  = * note there is the same code above for antopsychotics
# 14 "SNRI"              = Serotonin-norepinephrine reuptake inhibitors
# 15 "SSRI"              
# 16 "TCA"               = Tricyclic antidepressants 
# 17 "TCA_FirstGeneration"     
# 18 "TeCA"              = Tetracyclic antidepressants
# 19 "Opioid analgesic"  = any opioid apart from OAT
# -----------------------------------------------------------------------------

# Fill-in the gaps in variable 'mydrugsubstance_all' and 'myclass_all'
OST_vctr <- c("Methadone", "Buprenorphine", "Buprenorphine_naloxone")

t <- t %>%
  mutate(
    mydrugsubstance_all = if_else(mydrugsubstance_all == "", "Other", mydrugsubstance_all),
    myclass_all = if_else(myclass_all == "", "Other", myclass_all),
    myclass_all = if_else(myclass_all == "Other" & mydrugsubstance_all %in% OST_vctr, "OST", myclass_all)
  )

temp <- t %>% group_by(mydrugsubstance_all, myclass_all) %>% summarise(Class = n_distinct(patid))

# If patients received a specific class of medication (e.g. BNZ, GABA etc) then, substitute the 
# content of the relevant column with the difference (in days) from the index date and prescription date.
# Also, remember that the CPRD Therapy files may contain data that are outside of the patients' registration
# preiod. This is why we restrict the analysis in prescriptions issued after the crd or uts date.


# delete OST prescriptions or those that refer to other types of drugs that are not of interest now
t <- t %>% filter(!(myclass_all %in% c("OST", "Other")))

dput(names(t))

t <- t %>% select(c("patid", "issueid", "pracid", "probobsid", "drugrecid", "issuedate", 
                    "enterdate", "staffid", "prodcodeid", "dosageid", "quantity", 
                    "quantunitid", "duration", "estnhscost", "myeventdate", "dmdid", 
                    "term", "formulation", "route", "mydrugsubstance_all", 
                    "strength", "bnf", "index_date", "switcher", "mycrd", "myclass_all",
                    "GABA", "BNZ", "Z_drugs", "ANTIPSYCHO", "MOODSTAB", "OPIOIDS_PAIN", 
                    "ANTIDEP")) # erase columns

summary(t)

# -------------------------------------------------------
# UPDATE VARIABLES
# -------------------------------------------------------

mycols <- c( "GABA", "BNZ", "Z_drugs", "ANTIPSYCHO", "MOODSTAB", "ANTIDEP", "OPIOIDS_PAIN")

t$myeventdate <- ymd(t$myeventdate)

str(t)

t <- t %>%
  mutate( # the prescription date must be within the patients' registration period, excluding the ID
    across(mycols, ~if_else(myeventdate > mycrd & 
                              (myeventdate > index_date |
                                 # or, the patient received e.g. BNZ (x == 1) in the year before the ID
                               myeventdate >= index_date %m-% years(1)) & .x > 0, 
                            time_length(myeventdate - index_date, unit = "days"), 0)) 
  )# if conditions are not satisfied, insert zero

summary(t)

# remove duplicated records
t <- t %>% distinct()

# Save thedataset that contains prescription data on the year before the ID and during follow-up, so you
# can link it to the master file for modelling:
#write.csv(t, "Meds_tvc_master_Aurum.csv")

#write.csv(t, "Meds_tvc_master_Aurum_Addiction_journal.csv")

# -----------------------------------------------------------------------------------------------------

# <|>

# -----------------------------------------------------------------------------------------------------
#                                   Medical terms CPRD Aurum
# -----------------------------------------------------------------------------------------------------

library(dplyr)
library(lubridate)

setwd("/mnt/bmh01-rds/Ashcroft_Domzaridou_PSTRC/mbmhted6/Eleni_Aurum_JAN_2021")

# read the two separated merged_medical codes file that includes medical codes for all patients in the OST cohort
# and then unite them into one file
merged_clinical_1st_part <- read.csv("./source_data/merged_observation_part1.csv")
merged_clinical_2nd_part <- read.csv("./source_data/merged_observation_part2.csv")
merged_clinical <- dplyr::union(merged_clinical_1st_part, merged_clinical_2nd_part)
head(merged_clinical)
merged_clinical$X <- NULL
str(merged_clinical)
merged_clinical$medcodeid <- as.character(merged_clinical$medcodeid)
merged_clinical$myeventdate <- ymd(merged_clinical$myeventdate)
merged_clinical %>% count(patid)
sapply(merged_clinical, summary)


# --------------------------------------------------------------------------
# >> NEW FUNCTION: append_medical_como_lists (returns dataframe)
# --------------------------------------------------------------------------
# Usage: merges the merged_clinical with the (files) medcode lists (e.g. liver disease)
# It is expected that patients who have not been identified with certain types of diseases
# to be absent at the final call of this function. This will be obvious when we merge the clinical
# codes with the master file.

append_medical_como_lists <- function(merged, dtname){
  x <- read.csv(paste0("./source_data/comorbid/medcodes_", dtname, ".csv")) # read disease code list
  x$medcodeid <- as.character(x$medcode_aurum)
  #x$term <- x$term_gold
  #x <- x %>% select(c(dtname, "medcodeid", "term"))
  x <- x %>% select(c(dtname, "medcodeid"))
  merged <- left_join(merged, x, by = "medcodeid")
  
  merged <- merged %>% # the .data[[var]] wrapper is applied when the var argument is a character vector
    filter(!is.na(.data[[dtname]]) & !is.na(myeventdate)) %>% # remove NAs 
    mutate(comorbidity = dtname) %>%   # assign disease name
    group_by(patid) %>%
    mutate(myfirstdiagnosisdate = min((myeventdate))) %>% 
    filter(!is.na(myfirstdiagnosisdate)) %>%
    ungroup()
  
  merged <- merged %>%
    select(c("patid", "myfirstdiagnosisdate", "comorbidity")) %>% 
    arrange(patid, myfirstdiagnosisdate) %>% # keep the earliest date of diagnosis first
    distinct() %>%
    ungroup()
  
  return(merged)
}

# -------------------
# :: Function call ::
# -------------------

# ---- USER input vars that are part of the files name that include medcodes.
# e.g.: c("CHF", "MI", "angina", "stroke")
disease_vector <- c("Stroke", "Self_harm", "Bipolar", "Schizophrenia", "PersonalityDisorders", "IHD", "ACS",
                    "Depression", "Anxiety", "Asthma", "HIV", "Liver_NH", "Diabetes", "CVD_all", "Hypertension",
                    "HCV", "HBV", "Overdose", "Prison", "Homelessness", "Alcohol", 
                    "Pericarditis", "Endocarditis", "COPD", "CKD", "Pulmonary_embolism", "Gastric_ulcer")
                  # , Current_Ex_Smoker, "Arrhythmia") 


# For these diseases, call the above function and assign the result (which is a dataframe) to separate 
# dataframes and named them with the corresponding disease name
for(i in 1:length(disease_vector)){ 
  assign(disease_vector[i], append_medical_como_lists(merged_clinical, disease_vector[i]))
}

# load the master file
load("~/rds/Eleni_Aurum_JAN_2021/final_master.RData")

censoring_days <- 365 # <------- USER INPUT - enter the days you want to censor after the completion of the last issued prescription


## \+/ --new code (16.08.2021)
# keep the patid, index_date from the master and merged them with the separated files of comorbid conditions
# to examine comorbidities at baseline (como_base = YES if my_first_diagnosis_date < index_date):
my_patids <- my_patients %>% 
  filter(prereg >= 1) %>%
  select(c("patid", "index_date", "mycrd", "presc_exit_days", "exit_date", "mydod", "expected")) %>% 
  mutate(
    exit_date_cens = if_else(presc_exit_days > censoring_days, 
                             last(expected) %m+% days(censoring_days), 
                             exit_date)) %>%
  group_by(patid) %>%
  filter(expected == last(expected)) %>%
  distinct() %>%
  ungroup()
## \+/ --new code (16.08.2021)


my_patids %>% count(patid)
class(my_patids$patid)
colnames(my_patids) 

# create a list that includes all the comorbidity datasets you have
como_list <- list(Stroke, Self_harm, Bipolar, Schizophrenia, PersonalityDisorders, IHD,
                  Depression, Anxiety, Asthma, HIV, Liver_NH, Diabetes, CVD_all, Hypertension,
                  CKD, HCV, HBV, Overdose, Prison, Homelessness, Alcohol, ACS,
                  Pericarditis, Endocarditis, Gastric_ulcer, Pulmonary_embolism, COPD) 

united_como <- dplyr::union(como_list[[1]], como_list[[2]]) # append the first two comorbidity datasets

for(i in 3:length(como_list)){ # append all the comorbidity datasets together
  united_como <- dplyr::union(united_como, como_list[[i]])
}

head(united_como)
str(united_como)
united_como$patid <- as.character(united_como$patid)
summary(united_como)


# merge the patids from the master file with each comorbidity of interestin PC in order to
# examine comorbidities at baseline (using the index_date)
united_como_master <- left_join(my_patids, united_como, by = "patid") 
united_como_master %>% count(patid)
summary(united_como_master)
head(united_como_master)

# ------------------------------------------------------
# NEW VARIABLES: como_base (factor); como_id (binary)
# ------------------------------------------------------
# >> Como_base: 
# Generate a factor variable that will include every comorbibity name per patient
# This is an easy way to produce TableOne by stratifying on this variable:
#
# values: <comorbidity_name> or 'NA'
#
# ...............
#
# >> Como_id: 
# If comorbidity was present at baseline, then assign 1, otherwise 0
# we need two variables to spread the data, this is why the como_id index was created
# 
# values: 1 (TRUE); 0 (FALSE)

head(united_como_master)

summary(united_como_master)

united_como_master <- united_como_master %>% 
  mutate(
    # comorbidities at baseline/follow-up
    disease_final = if_else(!is.na(myfirstdiagnosisdate) & (myfirstdiagnosisdate > mycrd),
                            comorbidity, "Unknown/No disease"),
    como.PC_days = time_length(myfirstdiagnosisdate - index_date, unit = "days") # if possitive, then como refers to follow-up
  )

head(united_como_master)

united_como_master <- united_como_master %>% 
  select(-comorbidity, -index_date, -myfirstdiagnosisdate, -mycrd) %>%
  distinct()

summary(united_como_master)

head(united_como_master)

write.csv(united_como_master, "comos_PC_Aurum2.csv")


# ------------------------------------------------------------------------
# Merge with comorbidities that we have identified in HES.APC data
# ------------------------------------------------------------------------

# HES comorbidity codes per patid
HES_comos <- read.csv("/mnt/bmh01-rds/Ashcroft_Domzaridou_PSTRC/mbmhted6/20_000077/Base_FU_Comos_in_HES_2.csv")
sapply(HES_comos, class)
HES_comos$patid <- as.character(HES_comos$patid)
HES_comos$X <- NULL
HES_comos$fu_como.hes <- NULL
HES_comos$base_como.hes <- NULL
tail(HES_comos)
HES_comos$source <- "HES.APC"

# merge HES with the patids list
temp <- left_join(my_patids, HES_comos, by = "patid")
summary(temp)
head(temp)
temp %>% group_by(disease_final) %>% summarise(N = n_distinct(patid))
# note that results refer to both G/A with NAs refering to patids that appear in PC and not in HES

temp <- temp %>% 
  filter(!grepl("Other/no disease", disease_final)) %>% # keep only those records with comos present
  select(c(patid, disease_final, source, index_date, mycrd, como.hes_days))

# merge HES with Aurum comorbidity codes
united_como_master.hes <- full_join(united_como_master, temp, by = c("patid", "disease_final"))
head(united_como_master.hes)
summary(united_como_master.hes)

# remove records without any comorbidity recorded
united_como_master.hes <- united_como_master.hes %>% filter(disease_final != "Unknown/No disease")
summary(united_como_master.hes)

# -----------------------------------------------------
# NEW VARIABLE: como_present (binary: 0; 1)
# -----------------------------------------------------
# It shows if a patient has a como (either in HES/PC) 

united_como_master.hes <- united_como_master.hes %>% 
  select(-source) %>%
  group_by(patid, disease_final) %>%
  mutate(
    como.PC_days = min(como.PC_days, na.rm = TRUE), # this is because a disease might have multiple diagnostic dates
    como.hes_days = min(como.hes_days, na.rm = TRUE)
  ) %>% 
  distinct() %>% 
  ungroup()

head(united_como_master.hes)
summary(united_como_master.hes)


# create a final day variable that stands for the min days since ID that the comorbidity has been recorded 
# either to HES or PC. We will keep the min diagnosis date between PC and HES, and to patids that appear  
# having no comorbidity, we will assign the min PC date or NA
united_como_master.hes <- united_como_master.hes %>%
  group_by(patid, disease_final) %>%
  mutate(
    como.days = min(como.hes_days, como.PC_days) # comparison between Inf and number results in number
  ) %>% ungroup()

# clean the final dataset
united_como_master.hes <- united_como_master.hes %>%
  select(-como.PC_days, -como.hes_days) %>% # patids without ID/mycrd have HES as a source for comos
  distinct()

###################...... THIS IS THE DATAFRAME THAT YOU WILL USE FOR MODELLING  ......######################
#
# save the file and move to step that you combine the cohorts in a master ffile
#
# ...........................................................................................................

write.csv(united_como_master.hes, "Comorbidities_master_Aurum.HES.v2.csv")

# ...........................................................................................................


# -----------------------------------------------------------------------------------
# The following lines create a dataframe in which every row represents one patient
# who might have had multiple comorbidities like:
#
# Patid Angina  Stroke  Schizo <....>
# 1001  1       1       0       ....
# -----------------------------------------------------------------------------------

# This is just to supress tidyr error from the following spread() command
united_como_master.hes$X <- 1:dim(united_como_master.hes)[1] 

# Create a new master file that has a column for each comorbidity and a row for every patid
united_como_master2 <- tidyr::spread(united_como_master.hes, como_base, como_present)
colnames(united_como_master2)
head(united_como_master2)
summary(united_como_master2)

# substitute all NA values with zero
united_como_master2 <- tidyr::replace_na(united_como_master2, 
                                        list(Anxiety = 0, Asthma = 0, Bipolar = 0, CVD_all = 0,
                                             Depression = 0, HIV = 0, Liver_NH = 0, PersonalityDisorders = 0,
                                             Schizophrenia = 0, Stroke = 0, Diabetes = 0, IHD = 0,
                                             Self_harm = 0, CKD = 0, HBV = 0, HCV = 0, Hypertension = 0,
                                             Prison = 0, Homelessness = 0, Overdose = 0, Alcohol = 0,
                                             Pericarditis = 0, Endocarditis = 0, SSTI = 0, Hepatitis_NOS = 0,
                                             Gastric_ulcer = 0, Pulmonary_embolism = 0, COPD = 0, SSTI = 0,
                                             ACS = 0, IHD = 0))

# convert all the comorbidity-relevant vars into factor vars
dput(names(united_como_master2))

summary(united_como_master2)

cols <- c("ACS", "Alcohol", "Anxiety", "Arrhythmia", "Asthma", 
          "Bipolar", "CKD", "COPD", "Current_ex_Smoker", "CVD_all", "Depression", 
          "Diabetes", "Endocarditis", "Gastric_ulcer", "HBV", "HCV", "Hepatitis_NOS", 
          "HIV", "Homelessness", "Hypertension", "IHD", "Liver_NH", "Overdose", 
          "Pericarditis", "PersonalityDisorders", "Prison", "Pulmonary_embolism", 
          "Schizophrenia", "Self_harm", "SSTI", "Stroke")
