
#######################################################################################################
#                                           *
#                                      PhD PROJECT
#                                    ELENI DOMZARIDOU
#                                UNIVERSITY OF MANCHESTER
#                                      August 2022
#                                    CPRD GOLD/Aurum
#                                           *
# 
# Aims:
# -----
# a) The code populates table one.
# b) It includes stratification by medication type.
# c) Comorbidities at baseline and during fu.
# d) Other vars like OST_episode counts.
# 
#######################################################################################################

library(tableone)
library(dplyr)

# --------------------------------------------------------------------------------------
# Basic Table One >> Continue working using the dataframe: my_patients.single 
# --------------------------------------------------------------------------------------

load("/mnt/bmh01-rds/Ashcroft_Domzaridou_PSTRC/mbmhted6/Eleni_Combined_DBs/IRs_Analysis/generatePS_0.RData")

k <- my_patients.single

k %>% summarise(Np = n_distinct(patid))

# assign labels in region var
k <- k %>% mutate(
  region.txt = case_when(
    region ==	1 ~ "North East",
    region ==	2 ~ "North West",
    region ==	3 ~ "Yorkshire & The Humber", 
    region ==	4 ~ "East Midlands",
    region ==	5 ~ "West Midlands",
    region ==	6 ~ "East of England",
    region ==	7 ~ "South West",
    region ==	8 ~ "South Central",
    region ==	9 ~ "London",
    region ==	10 ~ "South East Coast",
    region ==	11 ~ "Northern Ireland",
    TRUE ~ "Unknown"
  )
)

# assign labels to IMD var
k <- k %>% mutate(
  imd2015_5.txt = case_when(
    imd2015_5 == 0 ~ "Uknonwn",
    imd2015_5 == 1 ~ "1 (least deprived)",
    imd2015_5 == 2 ~ "2",
    imd2015_5 == 3 ~ "3",
    imd2015_5 == 4 ~ "4",
    imd2015_5 == 5 ~ "5 (most deprived)"
  )
)

# convert all the comos and meds codes into factor vars
mycols <- c("region.txt", "gender", "myage_cat", 
                      "imd2015_5.txt", "ethnos_final", 
                      "GABA", "BNZ", "Z_drugs", "ANTIPSYCHO", "MOODSTAB", 
                      "ANTIDEP", "Alcohol", "Anxiety", "Bipolar", "Depression",  
                      "PersonalityDisorders", 
                      "Schizophrenia", "Self_harm")

k[,mycols] <- lapply(k[,mycols], factor) 

summary(k)

myvars <- c("region.txt", "gender", "myage_cat", 
                      "imd2015_5.txt", "ethnos_final", 
                      "GABA", "BNZ", "Z_drugs", "ANTIPSYCHO", "MOODSTAB", 
                      "ANTIDEP", "Alcohol", "Anxiety", "Bipolar", "Depression",  
                      "PersonalityDisorders", 
                      "Schizophrenia", "Self_harm")


k %>% summarise(Np = n_distinct(patid))

# Assign strata
factorVars <- "therapy"

# produce TableOne
TOoutput <- CreateTableOne(vars = myvars, strata = factorVars, data = k)
TOoutput
capture.output(print(TOoutput, nonnormal = 'myage', formatOptions = list(big.mark = ",")), # nonormal ~ IQR
               file = paste0("./TableOne_Master.txt"))

# ----------------------------------------------------------------------------------------------------------
#    Stratification by medication subtype at exit date (e.g. SSRIs; MAOIs etc)
# ----------------------------------------------------------------------------------------------------------

# Read CPRD files that contain info about medication

# Aurum
meds_aurum <- read.csv("/mnt/bmh01-rds/Ashcroft_Domzaridou_PSTRC/mbmhted6/Eleni_Aurum_JAN_2021/Meds_tvc_master_Aurum.csv")

meds_aurum <- meds_aurum %>% 
  dplyr::select(c(patid, myeventdate, mydrugsubstance_all, myclass_all, strength, quantity,
           ANTIDEP, Z_drugs, ANTIPSYCHO, MOODSTAB, GABA, BNZ)) %>%
  rename("qty" = "quantity")

# GOLD
meds_gold <- read.csv("/mnt/bmh01-rds/Ashcroft_Domzaridou_PSTRC/mbmhted6/Eleni_GOLD_JAN_2021/Meds_tvc_master_GOLD.csv")

meds_gold <- meds_gold %>% dplyr::select(c(patid, myeventdate, mydrugsubstance_all, myclass_all, 
                                           strength, qty,
                                    ANTIDEP, Z_drugs, ANTIPSYCHO, MOODSTAB, GABA, BNZ))

# Unite the above files
meds_all <- dplyr::union(meds_aurum, meds_gold)
meds_all$patid <- as.character(meds_all$patid)
head(meds_all)

# append therapy info to stratify according to OAT modality
therapy.dat <- my_patients.single %>% dplyr::select(c(patid, therapy)) %>% distinct()
 
# merge therapy + meds data
meds_all <- dplyr::left_join(therapy.dat, meds_all, by = "patid")
summary(meds_all)

# remove patids who never received any of those drugs during fu or they received them at baseline
meds_all <- meds_all %>% filter(!is.na(MOODSTAB)) 
meds_all <- meds_all %>% mutate(
  anydrug = ifelse((ANTIDEP + ANTIPSYCHO + MOODSTAB + Z_drugs + GABA + BNZ) <= 0, 0, 1)
)

head(meds_all)
meds_all <- meds_all %>% filter(anydrug > 0)

summary(meds_all)


# ----------------------------------------------------------------------------
# keep only prescriptions issued within the obs period of the patients
# ----------------------------------------------------------------------------

obs.per <- my_patients %>% dplyr::select(c(patid, index_date, exit_date)) %>% distinct()

# append with master data
meds_all <- left_join(meds_all, obs.per, by = "patid")
meds_all %>% count(patid)

meds_all <- meds_all %>% filter(myeventdate <= exit_date & myeventdate > index_date)
meds_all %>% count(patid)

# NEW VARIABLE: mydrugclass
meds_all <- meds_all %>%
  mutate(
    mydrugclass = case_when(
      ANTIDEP > 0 ~ "Antidepressants",
      ANTIPSYCHO > 0 ~ "Antipsychotics",
      MOODSTAB > 0 ~ "Mood stabilisers",
      Z_drugs > 0 ~ "Z-drugs",
      GABA > 0 ~ "Gabapentinoids",
      BNZ > 0 ~ "Benzodiazepines"
    )
  )

# ----------------------------------------------------------------------------
# Summaries
# ----------------------------------------------------------------------------

summary.meds.00 <- meds_all %>% group_by(mydrugclass) %>% 
  summarise(N = n_distinct(patid), P = round(N/20898*100, 1))

write.csv(summary.meds.00, "summary.meds.00.csv")

# ----------------------------------------------------------------------------

summary.meds.0 <- meds_all %>% group_by(mydrugclass, therapy) %>% 
  summarise(N = n_distinct(patid)) %>%
  tidyr::spread(therapy, N) %>%
  rename("Methadone" = "1", "Buprenorphine" = "2")

write.csv(summary.meds.0, "summary.meds.0.csv")

# ----------------------------------------------------------------------------

summary.meds.1 <- meds_all %>% group_by(myclass_all) %>% 
  summarise(N = n_distinct(patid), P = round(N/20898*100, 1))

write.csv(summary.meds.1, "summary.meds.1.csv")

# ----------------------------------------------------------------------------

summary.meds.2 <- meds_all %>% group_by(myclass_all, therapy) %>% 
  summarise(N = n_distinct(patid)) %>%
  tidyr::spread(therapy, N) %>%
  rename("Methadone" = "1", "Buprenorphine" = "2")

write.csv(summary.meds.2, "summary.meds.2.csv")

# ----------------------------------------------------------------------------

summary.meds.3 <- meds_all %>% group_by(therapy, myclass_all, mydrugsubstance_all) %>% 
  summarise(N = n_distinct(patid)) %>%
  tidyr::spread(therapy, N) %>%
  rename("Methadone" = "1", "Buprenorphine" = "2")

write.csv(summary.meds.3, "summary.meds.3.csv")

# ----------------------------------------------------------------------------

summary.pres <- meds_all %>% group_by(therapy, myclass_all, mydrugsubstance_all) %>% 
  summarise(N = n()) %>%
  tidyr::spread(therapy, N) %>%
  rename("Methadone" = "1", "Buprenorphine" = "2")

write.csv(summary.pres, "summary.pres.csv")

# ----------------------------------------------------------------------------------------------------------
#    RUN in case you have to report on extra variables that relate to follow-up and treatment episodes
# ----------------------------------------------------------------------------------------------------------

load("/mnt/bmh01-rds/Ashcroft_Domzaridou_PSTRC/mbmhted6/Eleni_Combined_DBs/ERs_Analysis/NFO_ERs.RData")

library(tableone)

# select vars of interest
dput(names(newdf2))

k2 <- test %>% select(c("patid", "OST_episode", "daysin", "daysout", "OST_daysin_epi", "OST_daysout_epi"))

# append with therapy data
k3 <- my_patients_cens %>% select(c(patid, therapy)) %>% distinct() 

# merge
k2 <- left_join(k2, k3, by = "patid")

k2 <- k2 %>%
  mutate(
    OST_daysin_epi.cat = case_when(
      OST_daysin_epi <= 30 ~ "up to 1 month",
      OST_daysin_epi  > 30 & OST_daysin_epi <= 90 ~ "1-3 months",
      OST_daysin_epi  > 90 & OST_daysin_epi <= 180 ~ "3-6 months",
      OST_daysin_epi > 180 & OST_daysin_epi <= 365 ~ "6-12 months",
      TRUE ~ "> 12 months"
    )
  )

k2$OST_daysin_epi.cat <- as.factor(k2$OST_daysin_epi.cat)

myvars <- c("OST_episode", "daysin", "daysout", "OST_daysin_epi", "OST_daysout_epi", "OST_daysin_epi.cat")

# Assign strata
factorVars <- "therapy"

# produce TableOne
TOoutput <- CreateTableOne(vars = myvars, strata = factorVars, data = k2)
TOoutput
capture.output(print(TOoutput, nonnormal = myvars, formatOptions = list(big.mark = ",")), 
               file = paste0("./TableOne_Master2.txt"))
