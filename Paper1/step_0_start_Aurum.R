#######################################################################################################
#                                           *
#                                       PhD PROJECT
#                                    ELENI DOMZARIDOU
#                                UNIVERSITY OF MANCHESTER
#                                       February 2021
#                                        CPRD Aurum
#                                           *
# 
# Aims: To read and append acceptable patients, with linkage eligibility, (migrators), practices,
#        part of therapy files, and product codes
#
#######################################################################################################

library(dplyr)

# ---------------------------------------------------------------------------------------------------
#                                        READING FILES                                              #
# ---------------------------------------------------------------------------------------------------

acceptable_pats <- read.table(file = "./source_data/202101_CPRDAurum_AcceptablePats.txt",
                              header = T, sep = "\t")
head(acceptable_pats)
sapply(acceptable_pats, class)
acceptable_pats$patid <- as.character(acceptable_pats$patid) # converts patid into a character string
acceptable_pats$pracid <- as.character(acceptable_pats$pracid)
# acceptable_pats %>% summarise(N = n_distinct(patid))
acceptable_pats %>% count(gender) # check


# Drug Issue initial files
# FILE 1
drug_issue1 = read.table(file= "./source_data/Eleni_Aurum_Define_Inc1_Drug_Issue_001.txt", header = T, sep ="\t")
head(drug_issue1)
a <- as.data.frame(sapply(drug_issue1, class))
drug_issue1$patid <- as.character(drug_issue1$patid)
drug_issue1$prodcodeid <- as.character(drug_issue1$prodcodeid)
#drug_issue1 %>% summarise(N=n_distinct(patid))


# FILE 2
drug_issue2 = read.table(file ="./source_data/Eleni_Aurum_Define_Inc1_Drug_Issue_002.txt", header = T, sep ="\t")
head(drug_issue2)
as.data.frame(sapply(drug_issue2, class))
drug_issue2$patid <- as.character(drug_issue2$patid)
drug_issue2$prodcodeid <- as.character(drug_issue2$prodcodeid)
#drug_issue2 %>% summarise(N=n_distinct(patid))


# PRACTICES
all_practices_JAN2021 = read.table(file = "./source_data/202101_CPRDAurum_Practices.txt", header = T, sep = "\t")
ls(all_practices_JAN2021)
head(all_practices_JAN2021)
sapply(all_practices_JAN2021, class)
all_practices_JAN2021$pracid <- as.character(all_practices_JAN2021$pracid) # convert into string
all_practices_JAN2021 %>% summarise(P=n_distinct(pracid))


# Methadone and buprenorphine (all available codes from Jan 2021 Dictionary)
candidates_JAN2021 = read.table(file = "./source_data/candidates_meth_bup_Aurum.txt", header = T, sep = "\t")
ls(candidates_JAN2021)
head(candidates_JAN2021[1:3])
sapply(candidates_JAN2021, class)
candidates_JAN2021[,1:3] <- sapply(candidates_JAN2021[,1:3], as.character) # convert into character strings

#                      -----------------------
# Linkage Eligibility - ATTENTION!!! - SET 18
#                      -----------------------
linkage_elig_Aurum = read.table(file = "./source_data/linkage_eligibility_Aurum_set18.txt", header = T, sep = "\t")
linkage_elig_Aurum$patid <- as.character(linkage_elig_Aurum$patid)
linkage_elig_Aurum$pracid <- as.character(linkage_elig_Aurum$pracid)
ls(linkage_elig_Aurum)
tail(linkage_elig_Aurum)

#                                                            -----------------------
# Migrators from GOLD to Aurum - read file and convert dates - ATTENTION!!! - SET 18
#                                                            -----------------------
#migrators = read.table(file = "./source_data/VisionToEmisMigrators.txt", header = T, sep = "\t")
migrators = read.table(file = "./source_data/VisionToEmisMigrators_set18.txt", header = T, sep = "\t")
ls(migrators)
head(migrators)
sapply(migrators, class)

migrators_emis_pracid <- migrators %>% select(emis_pracid)
names(migrators_emis_pracid) <- "pracid"
migrators_emis_pracid$pracid <- as.character(migrators_emis_pracid$pracid)


# ---------------------------------------------------------------------------------------------------
#                                   MERGING & CLEANING FILES                                        #
# ---------------------------------------------------------------------------------------------------

# Merge with linkage eligibility file:
accept_link <- left_join(acceptable_pats, linkage_elig_Aurum, by = c("patid", "pracid"))

# ATTENTION!!! - FILTERING!!!
# Keep only patients available for linkage to ONS, HES and IMD
accept_link <- accept_link %>% filter(lsoa_e == 1 & death_e == 1 & hes_e == 1)
accept_link %>% summarize(Np = n_distinct(patid))

# Migrators - remove from Aurum those patients who have migrated from Vision to Emis; they
# have remained in GOLD to give it more power:
accept_link <- anti_join(accept_link, migrators_emis_pracid, by = "pracid")
accept_link %>% summarise(Nr = n(), Np = n_distinct(patid))
   

# Merge the file that includes patids and product codes with the denominator (or acceptable pats) to
# acquire all the needed vars (e.g. gender; death date from CPRD; yob etc).
# >> STAGE 1 - use the drug_issue1 file:
my_patients1 <- left_join(accept_link, drug_issue1, by = "patid")
my_patients1 %>% summarise(N = n_distinct(patid))
my_patients1 %>% count(gender)# Na's indicate that there are patients who were not acceptable for research
ls(my_patients1)

# >> STAGE 2 - use the drug_issue2 file:
my_patients2 <- left_join(accept_link, drug_issue2, by = "patid")
my_patients2 %>% summarise(N = n_distinct(patid))
ls(my_patients2)

# only in Aurum: append the two patients files into one data frame
my_patients <- dplyr::bind_rows(my_patients1, my_patients2)
my_patients %>% summarise(N = n_distinct(patid))

# exclude those who did not receive methadone/buprenorphine prescriptions
my_patients <- my_patients %>% filter(!is.na(prodcodeid))
my_patients %>% summarise(N = n_distinct(patid))

# exclude if gender = NA or different than M/F
my_patients <- my_patients %>% filter(!is.na(gender))
my_patients %>% summarise(Nr = n(), Np = n_distinct(patid))
my_patients <- my_patients %>% filter(gender %in% c("M", "F"))
my_patients %>% count(gender)

# Merge the above result with the candidates list so you know what product every patient 
# have been prescribed:
my_patients <- left_join(my_patients, candidates_JAN2021, by = "prodcodeid")
my_patients %>% summarise(N = n_distinct(patid))
ls(my_patients)
summary(my_patients$patid)
colnames(my_patients)
colnames(my_patients)[2] <- "pracid" # rename and remove pracid.x/y
my_patients$pracid <- as.character(my_patients$pracid)
my_patients$pracid.y <- NULL



# Merge with the all_practices file
# all_practices_JAN2021 <- all_practices_JAN2021 %>% select(c("pracid", "region"))
# my_patients <- left_join(my_patients, all_practices_JAN2021, by = "pracid")


# clean the environment
dput(ls())
rm("acceptable_pats", "all_practices_JAN2021", "drug_issue1", "drug_issue2", "my_patients1",
     "my_patients2", "accept_link", "linkage_elig_Aurum", "migrators", "migrators_emis_pracid")


# Keep the list of unique patids in a txt file to submit it for the linkage request form to CPRD:
cprd_link_req_aurum_list_of_patids <- my_patients %>% select("patid", "lsoa_e", "hes_e", "death_e") %>% distinct() 
write.table(cprd_link_req_aurum_list_of_patids, "linkage_req_aurum_patids_set18.txt", sep = "\t")

temp <- read.table("linkage_req_aurum_patids_set18.txt", header = T, sep = "\t") # check
head(temp)

# <|>

# -------------------------------------------------------------------------------------------------------
#                                           Merge with ONS data
# -------------------------------------------------------------------------------------------------------

myONS <- read.csv("/mnt/bmh01-rds/Ashcroft_Domzaridou_PSTRC/mbmhted6/20_000077/Aurum_linked/Extract_Aurum_HES_csv/death_ONS_patient.csv") 

# ----------------------------------------
# PRE-PROCESSING
# ----------------------------------------

colnames(myONS)

# exclude empty columns
myONS[18:35] <- NULL
myONS$X <- NULL
myONS$dod_partial <- NULL

# convert dates
myONS$mydod <- dmy(myONS$dod)
myONS$mydor <- dmy(myONS$dor)
summary(myONS$mydod)
myONS <- myONS %>% filter(!is.na(mydod))
summary(myONS$mydor)

str(myONS)

# convert patid into a character vector
myONS$patid <- as.character(myONS$patid)

# keep only distinct rows from the ONS data
myONS <- myONS %>% distinct() # TRUE

# -------------------------------------
# Merge: ONS + CPRD Aurum
# -------------------------------------

# merge the ONS data with patids from the 'my_patients' file
my_patients <- left_join(my_patients, myONS, by = "patid")
my_patients$X <- NULL
colnames(my_patients)
colnames(my_patients)[2] <- "pracid" # rename pracid.x -> pracid
my_patients$pracid.y <- NULL


# -------------------------------------------------------------------------------------------------------
#                                           Merge with IMD data
# -------------------------------------------------------------------------------------------------------

myIMD <- read.csv("/mnt/bmh01-rds/Ashcroft_Domzaridou_PSTRC/mbmhted6/20_000077/Aurum_linked/Extract_Aurum_HES_csv/patient_imd2015.csv") 

head(myIMD)

# drop variables that we do not need
myIMD$X <- NULL
myIMD$pracid <- NULL
myIMD$patid <- as.character(myIMD$patid) # change class to enable merging

# merge with my_patients
my_patients <- left_join(my_patients, myIMD, by = "patid")

