#######################################################################################################
#                                           *
#                                       PhD PROJECT
#                                    ELENI DOMZARIDOU
#                                UNIVERSITY OF MANCHESTER
#                                       July 2021
#                                       CPRD Aurum
#                                           *
#
# This code utilises ethnicity medcodes from Dr Alison Wright.  The code also implements Alison's
# algorithm to identify ethinicity codes in HES data.
#
# Aims:
# a) To append ethinicity medical codes from Aurum records 
# b) To examine HES records for codes that are missing from PC records
# c) and for patids with multiple ethnicity codes
# d)
#
#######################################################################################################

library(dplyr)
library(lubridate)

setwd("/mnt/bmh01-rds/Ashcroft_Domzaridou_PSTRC/mbmhted6/Eleni_Aurum_JAN_2021")

# -------------------------------------------------------------------------------------------------------
#                                EXAMINE PRIMARY CARE ETHNICITY DATA
# -------------------------------------------------------------------------------------------------------

# read the two separated observation files that include medical codes for all patients in the OST cohort
# and then unite them into one file
merged_clinical_1st_part <- read.csv("./source_data/merged_observation_part1.csv")
merged_clinical_2nd_part <- read.csv("./source_data/merged_observation_part2.csv")
merged_clinical <- dplyr::union(merged_clinical_1st_part, merged_clinical_2nd_part)
head(merged_clinical)
merged_clinical$X <- NULL
str(merged_clinical)
merged_clinical$patid <- as.character(merged_clinical$patid)
merged_clinical$medcodeid <- as.character(merged_clinical$medcodeid)
merged_clinical$myeventdate <- ymd(merged_clinical$myeventdate)
merged_clinical %>% count(patid)
sapply(merged_clinical, summary)

# load Aurum master cohort
load("~/rds/Eleni_Aurum_JAN_2021/final_master.RData")

# keep the patid, index_date from the master and merged them with the separated files of comorbid conditions
# to examine comorbidities at baseline (como_base = YES if my_first_diagnosis_date < index_date):
my_patids <- my_patients %>% filter(prereg >= 1) %>% select(c("patid", "index_date")) %>% distinct()
my_patids %>% count(patid)

class(my_patids$patid)

colnames(my_patids)

rm(my_patients, my_patients_vars)

# Merge only the patids from master cohort with those patids that are present to the clinical codes file, because
# the clinical codes is an extract from all patids in CPRD Aurum, which some of them are not present in 
# my master cohort:
my_patients.clinical <- left_join(my_patids, merged_clinical, by = "patid")
summary(my_patients.clinical)

# rename the var to enable merging with the ethnicity codes
my_patients.clinical <- my_patients.clinical %>% rename("medcode_aurum" = "medcodeid")
my_patients.clinical$medcode_aurum <- as.character(my_patients.clinical$medcode_aurum)

ls(my_patients.clinical)

# Keep only vars that you need in order to have a patid and possibly multiple ethnicity codes. So it 
# might be sth like:
# patid ethnicity
# 100   NA
# 100   White
# 100   White/British
# because patients' ethnicity was updated during different consultations or there is a different
# medcode for the same ethnicity label. 
my_patients.clinical <- my_patients.clinical %>% select(c(patid, index_date, medcode_aurum)) %>% distinct()
head(my_patients.clinical)

# ------------------------------------------------------
# Load ethinicity medcodes (source: Dr Alison Wright)
# ------------------------------------------------------
#ethnicity <- read.delim("~/rds/Eleni_GOLD_JAN_2021/source_data/Ethnicity_GOLD_Aurum_Alison.txt", sep = "\t")
ethnicity <- read.csv("./source_data/Final_ethnos_Aurum_AW.csv") # reviewed by Alison Wright (Sept 2021)
ls(ethnicity)
ethnicity$medcode_aurum <- as.character(ethnicity$medcodeid)
head(ethnicity)

# Clean the codes according to Alison's .do file commands; She excluded codes with '?' or 'X' from
# the variable 'CPRD_race' that indicates the ethnicity class:
ethnicity <- ethnicity %>% filter(class != "?" & class != "X")
ethnicity <- ethnicity %>% filter(!is.na(medcode_aurum))
ethnicity <- ethnicity %>% filter(is.na(exclude))
ethnicity <- ethnicity %>% rename("CPRD_race" = "class")
table(ethnicity$CPRD_race)
summary(ethnicity)

# Keep only vars that you need to avoid duplicates:
ethnicity <- ethnicity %>% select(c(medcode_aurum, CPRD_race))

# Extent this list to include new medcodeids from Matt:
# a) Read Matt's additional codes to those from Alison Wright
# ethnicity_Matt_newcodes <- read.delim("~/rds/Eleni_Aurum_JAN_2021/source_data/Matts_add_ethnos.txt", sep = ",")
# colnames(ethnicity_Matt_newcodes)
# names(ethnicity_Matt_newcodes)[3] <- "medcode_aurum"
# ethnicity_Matt_newcodes$medcode_aurum <- as.character(ethnicity_Matt_newcodes$medcode_aurum)
# ethnicity_Matt_newcodes$term <- NULL

# b) Merge Matt's codes with Alison's and check if those appear in CPRD Aurum
# ethnicity <- dplyr::union(ethnicity, ethnicity_Matt_newcodes)
# table(ethnicity$CPRD_race)

# -------------------------------------------------------------------------------
# Merge ethnicity codes from Alison with the medcodes file from CPRD (here Aurum)
# -------------------------------------------------------------------------------

my_patients.ethnos <- left_join(my_patients.clinical, ethnicity, by = "medcode_aurum")
my_patients.ethnos <- my_patients.ethnos %>% select(-medcode_aurum) %>% distinct()
summary(my_patients.ethnos)

# Count patids with recorded ethnicity code in Aurum; NAs correspond to medcodes that are
# not ethnicity related or missing ethnicity:
All_patients <- as.numeric(my_patients.ethnos %>% summarise(N = n_distinct(patid))) # All patids in the cohort
my_patients.ethnos %>% group_by(CPRD_race) %>% summarise(N = n_distinct(patid),
                                                         P = paste0(round(N/All_patients*100,2), "%"))
 
#   CPRD_race               N
# -----------------------------
# 1 Asian/British Asian   415 2.56% 
# 2 Black/Black British   326 2.01% 
# 3 Mixed                 290 1.79% 
# 4 Other                 121 0.75% 
# 5 White                9539 58.74%
# 6 NA                  16240 100%  

# There are NAs in the var CPRD_race.
# This may happen for two reasons:
# a) either the patient misses ethnicity codes; or
# b) s/he has two or more medcodes in clinical records and some of them match the ethnicity dictionary
#    but some do not and they appear as NAs. In this case, we fill-in the NAs with the code that exists
#    in CPRD Aurum and matches the dictionary ethnicity code.


# -----------------------------------------------------------
# NEW VARIABLE: my_race.cprd (factor)
# -----------------------------------------------------------
# Create the same levels of ethnicity in PC and HES order to enable merging;
# The labels are based on cprd_race variable and Alison's classification

my_patients.ethnos$CPRD_race <- as.character(my_patients.ethnos$CPRD_race)

my_patients.ethnos <- my_patients.ethnos %>%
  mutate(
    my_race.cprd = case_when(
      CPRD_race %in% "Asian/British Asian" ~ "Asian",
      CPRD_race %in% "Black/Black British" ~ "Black",
      is.na(CPRD_race) ~ "Unknown",
      TRUE ~ CPRD_race
    )
  )

my_patients.ethnos %>% group_by(CPRD_race) %>% summarise(N = n_distinct(patid))
my_patients.ethnos %>% group_by(my_race.cprd) %>% summarise(N = n_distinct(patid))

# -----------------------------------------------------------
# NEW VARIABLE: CPRD_race.num (numeric)
# -----------------------------------------------------------

my_patients.ethnos <- my_patients.ethnos %>%
  mutate(
    CPRD_race.num = case_when(
      my_race.cprd %in% "White" ~ 1,
      my_race.cprd %in% "Mixed" ~ 2,
      my_race.cprd %in% "Asian" ~ 3,
      my_race.cprd %in% "Black" ~ 4,
      my_race.cprd %in% "Other" ~ 5,
      my_race.cprd %in% "Unknown" ~ 6
    )
  )

my_patients.ethnos %>% group_by(my_race.cprd, CPRD_race.num) %>% summarise(N = n_distinct(patid))

# ----------------------------------------------------------------------
# NEW VARIABLE: known (refers to record; binary)
# ----------------------------------------------------------------------
# Known (flag)
# 1: if CPRD_race is available
# 0: if CPRD_race is missing

my_patients.ethnos <- my_patients.ethnos %>%
  mutate(
    known = if_else(CPRD_race.num == 6, 0, 1)
  ) %>% ungroup()

my_patients.ethnos %>% group_by(known) %>% summarise(N = n_distinct(patid))

# --------------------------------------------------------------------
# NEW VARIABLE: my_ethnos_class (numeric) - continues some line below
# --------------------------------------------------------------------
# 1: patids with just one ethinicity record which is complete
# 2: patids with more than one ethinicities (at least one recorded, rest could be missing)
# 3: patids with missing record (opposite of case 1)

# my_ethnos_class - STEP A
# .................................
# First, define ethnos category for patients with all records missing
my_patients.ethnos <- my_patients.ethnos %>%
  group_by(patid) %>%
  mutate(
    my_ethnos_class = case_when(
      min(CPRD_race.num) == max(CPRD_race.num) & CPRD_race.num == 6 ~ 3, # missing
      TRUE ~ 0
    )) %>% ungroup()

my_patients.ethnos %>% group_by(my_ethnos_class) %>% summarise(N = n_distinct(patid))

# -------------------------------------------
# NEW DATAFRAME: my_patients.ethnos.miss
# -------------------------------------------
# Patids with completely missing ethnicity in CPRD Aurum; 

my_patients.ethnos.miss <- my_patients.ethnos %>% filter(my_ethnos_class == 3)

my_patients.ethnos.miss %>% summarise(N = n_distinct(patid))

# -------------------------------------------

# Exclude the patients with all missing records in CPRD from the initial my_patients.ethnos dataset:
my_patients.ethnos <- my_patients.ethnos %>% filter(my_ethnos_class != 3)

# exclude records with missing data (these are records that include at least one recorded code in CPRD)
my_patients.ethnos <- my_patients.ethnos %>% filter(known == 1)

# ----------------------------------------
# NEW VARIABLE: dupli (binary)
# ----------------------------------------
# Indicates duplicated records
my_patients.ethnos <- my_patients.ethnos %>%
  group_by(patid, CPRD_race.num) %>%
  mutate(
    dupli = if_else(n() > 1, 1, 0)
  ) %>% ungroup()

# ----------------------------------------------------------------------
# NEW VARIABLE: n_records (numeric)
# ----------------------------------------------------------------------
# n_records - check for duplicated patid
# 1: unique record of this patid
# 2: patient has multiple records

my_patients.ethnos <- my_patients.ethnos %>%
  group_by(patid) %>%
  mutate(
    n_records = max(row_number())
  ) %>% ungroup() 

summary(my_patients.ethnos$n_records)

my_patients.ethnos %>% group_by(n_records, my_race.cprd) %>% summarise(N = n_distinct(patid))

# .................................
# my_ethnos_class - STEP B
# .................................
# Redefine ethnos class

my_patients.ethnos <- my_patients.ethnos %>%
  group_by(patid) %>%
  mutate(
    my_ethnos_class = case_when(
      n_records == 1 ~ 1, # single
      TRUE ~ 2            # multi
    )                     # see lines above for 3rd category which is 'missing'
  ) %>% ungroup()

my_patients.ethnos %>% group_by(my_ethnos_class) %>% summarise(N = n_distinct(patid))

# -------------------------------------------
# NEW DATAFRAME: my_patients.ethnos.single
# -------------------------------------------
# Patids with single ethnicity in CPRD Aurum

my_patients.ethnos.single <- my_patients.ethnos %>% filter(my_ethnos_class == 1)
my_patients.ethnos.single <- as.data.frame(my_patients.ethnos.single)
my_patients.ethnos.single %>% summarise(N = n_distinct(patid))
my_patients.ethnos.single %>% group_by(my_race.cprd) %>% summarise(N = n_distinct(patid))

# -------------------------------------------
# NEW DATAFRAME: my_patients.ethnos.multi
# -------------------------------------------
# Since the number of records is > that the number of patids, some patients have multiple ethnicity codes.
# We can identify them:

my_patients.ethnos.multi <- my_patients.ethnos %>% filter(my_ethnos_class == 2)
my_patients.ethnos.multi %>% count(patid)
my_patients.ethnos.multi %>% group_by(my_race.cprd) %>% summarise(N = n_distinct(patid))

# <|>

# -------------------------------------------------------------------------------------------------------
#                                    EXAMINE HES ETHNICITY DATA
# -------------------------------------------------------------------------------------------------------

# Load HES RData working space that contains HES files with patids and ethnicity codes for both Aurum/Aurum
load("/mnt/bmh01-rds/Ashcroft_Domzaridou_PSTRC/mbmhted6/20_000077/HES_combined.RData")

# Keep the vars that you need
ls(myhes_comb)

my_patids.hes_ethnos <- myhes_comb %>% 
  select(c(patid, gen_ethnicity, source)) %>% 
  filter(source == "A") %>% # keep only patids from Aurum
  distinct()

my_patids.hes_ethnos %>% summarise(N = n_distinct(patid)) # N of patids in HES
my_patids.hes_ethnos %>% group_by(gen_ethnicity) %>% summarise(N = n_distinct(patid))

# -----------------------------------------------------------
# NEW VARIABLE: my_ethnicity.hes (factor)
# -----------------------------------------------------------
# Create the same levels of ethnicity in PC and HES order to enable merging

my_patids.hes_ethnos <- my_patids.hes_ethnos %>%
  mutate(
    my_ethnicity.hes = case_when(
      gen_ethnicity %in% c("Oth_Asian", "Pakistani", "Bangladesi", "Indian") ~ "Asian",
      gen_ethnicity %in% c("Bl_Carib", "Bl_Afric", "Bl_Other") ~ "Black",
      gen_ethnicity %in% c("Other", "Chinese") ~ "Other",
      TRUE ~ gen_ethnicity
    )
  )

my_patids.hes_ethnos %>% group_by(gen_ethnicity, my_ethnicity.hes) %>% summarise(N = n_distinct(patid))
my_patids.hes_ethnos %>% group_by(my_ethnicity.hes) %>% summarise(N = n_distinct(patid))

# <|>

# ------------------------------------------------------------------------------
# STEP 1: Examine patients with single ethnicity in CPRD 
# ------------------------------------------------------------------------------

a <- left_join(my_patients.ethnos.single, my_patids.hes_ethnos, by = "patid")

# -----------------------------------
# NEW VARIABLE: ethnos_final (factor)
# -----------------------------------
# Levels:
# 1: recored in CPRD but not in HES (HES data is either unknown or missing because the patid appears only in PC)
# 2: recorded in both CPRD and HES with the same CPRD-HES ethnicity class
# 3: recorded in both CPRD and HES with different CPRD-HES ethnicity class

a <- a %>%
  mutate(
    ethnos_final = case_when(
      my_race.cprd != "Unknown" & (my_ethnicity.hes == "Unknown" | is.na(my_ethnicity.hes)) ~ 1,
      my_race.cprd == my_ethnicity.hes ~ 2,
      my_race.cprd != my_ethnicity.hes ~ 3
    )
  )

a$ethnos_final <- as.factor(a$ethnos_final)
a %>% group_by(ethnos_final) %>% summarise(N = n_distinct(patid))

# rename labels:
a <- a %>%
  mutate(
    ethnos_final = case_when(
      my_race.cprd != "Unknown" & (my_ethnicity.hes == "Unknown" | is.na(my_ethnicity.hes)) ~ my_race.cprd,
      my_race.cprd == my_ethnicity.hes ~ my_race.cprd,
      my_race.cprd != my_ethnicity.hes ~ "Unknown"
    )
  )

a <- a %>% select(c(patid, ethnos_final)) %>% distinct()

# ------------------------------------------------------------------------------
# STEP 2: Examine patients with missing ethnicity in CPRD 
# ------------------------------------------------------------------------------

b <- left_join(my_patients.ethnos.miss, my_patids.hes_ethnos, by = "patid")

# -----------------------------------
# NEW VARIABLE: ethnos_final (factor)
# -----------------------------------
# Unknown       : not recored in HES
# <any ethnos>  : recorded in HES 

b <- b %>%
  mutate(
    ethnos_final = case_when(
      my_ethnicity.hes == "Unknown" | is.na(my_ethnicity.hes) ~ "Unknown",
      TRUE ~ my_ethnicity.hes
    )
  )

b$ethnos_final <- as.factor(b$ethnos_final)
b %>% group_by(ethnos_final) %>% summarise(N = n_distinct(patid))
b <- b %>% select(c(patid, ethnos_final)) %>% distinct()

# ------------------------------------------------------------------------------
# STEP 3: Examine patients with multiple ethnicity in CPRD 
# ------------------------------------------------------------------------------

c <- left_join(my_patients.ethnos.multi, my_patids.hes_ethnos, by = "patid")

c <- c %>% select(c(patid, my_race.cprd, CPRD_race.num, my_ethnicity.hes))

c %>% group_by(my_race.cprd, my_ethnicity.hes) %>% summarise(N = n_distinct(patid))

# check for duplicates
cc <- c %>% select(c(patid, my_race.cprd))
cc <- cc %>% group_by(patid, my_race.cprd) %>% mutate(dupli = n() > 1)
table(cc$dupli) # there are no duplicated records
rm(cc)

# Since there are no duplicated records in aurum, we expect that all the patients have codes from
# different categories

# -----------------------------------------------
# NEW VARIABLE: any_cat 
# -----------------------------------------------
# 0: multiple ethnicities belong to same category
# 1: multiple ethnicities from different categories

c %>% group_by(CPRD_race.num) %>% summarise(N = n_distinct(patid))

c <- c %>% 
  group_by(patid) %>%
  mutate(
    any_cat = case_when(
      min(CPRD_race.num) == max(CPRD_race.num) ~ 0,
      TRUE ~ 1
    )
  ) %>% ungroup() 

c %>% group_by(any_cat) %>% summarise(N = n_distinct(patid))

c1 <- c %>% filter(any_cat == 0) # multiple ethnicities belong to same category
c2 <- c %>% filter(any_cat == 1) # multiple ethnicities belong to different categories

# -----------------------------------
# NEW VARIABLE: ethnos_final (factor)
# -----------------------------------
# Levels:
# 1-unknown: recored in CPRD but not in HES (HES data is either unknown or missing because the patid appears only in PC)
# 2-CPRD   : recorded in both CPRD and HES with al least one match
# 3-Unknown: recorded in both CPRD and HES with no matches

c2 <- c2 %>%
  mutate(
    ethnos_final  = case_when(
      my_race.cprd != "Unknown" & (my_ethnicity.hes == "Unknown" | is.na(my_ethnicity.hes)) ~ 1, # HES missing
      my_race.cprd == my_ethnicity.hes ~ 2, # match
      (my_ethnicity.hes != "Unknown" | !is.na(my_ethnicity.hes)) & my_race.cprd != my_ethnicity.hes ~ 3
    )
  ) %>%
  group_by(patid) %>%
  mutate(my_match = min(ethnos_final)) %>% # indicates that at least one record had same ethnicity in PC & HES
  ungroup()

c2 %>% group_by(my_match) %>% summarise(N = n_distinct(patid))

# redefine labels
c2 <- c2 %>%
  mutate(
    ethnos_final = if_else(my_match %in% c(1,3), "Unknown", my_ethnicity.hes)
  )

c2 %>% group_by(ethnos_final) %>% summarise(N = n_distinct(patid))

c2 <- c2 %>% select(c(patid, ethnos_final)) %>% distinct()

rm(c, c1)

# ----------------------------------------------------------------------------------------------------------
#      STEP 4: Merge all the a,b,c datasets into one that will have only patid and final ethnicity code
# ----------------------------------------------------------------------------------------------------------

ethnos_final.df <- union(a, b)
ethnos_final.df <- union(ethnos_final.df, c2)
ethnos_final.df %>% group_by(ethnos_final) %>% summarise(N = n_distinct(patid),
                                                       P = paste0(round(N/All_patients*100,2), "%"))
summary(ethnos_final.df)

write.csv(ethnos_final.df, "Ethnicity_Aurum.HES.csv")

rm(list = ls())
