#######################################################################################################
#                                           *
#                                       PhD PROJECT
#                                    ELENI DOMZARIDOU
#                                UNIVERSITY OF MANCHESTER
#                                       April 2021
#                                       CPRD Aurum
#######################################################################################################

library(dplyr)
library(lubridate)

# -----------------------------------
part <- 2 # <------------ USER INPUT
# -----------------------------------

setwd(paste0("~/rds/Eleni_Aurum_JAN_2021/source_data/Extract_Aurum_Part", part))

if(part == 1){
  drugIssue_fileslist <- list.files(pattern = 'Eleni_Aurum_Extract_DrugIssue_.*\\.txt') # keep all the file names that are Therapy files in the variable "therapy_fileslist"
}else{
  drugIssue_fileslist <- list.files(pattern = 'Eleni_Aurum_2b_Extract_DrugIssue_.*\\.txt') # keep all the file names that are Therapy files in the variable "therapy_fileslist"
}

# Run only if you have not saved drug issue files as.cvs 
for(i in 1:length(drugIssue_fileslist)){ # Convert each txt file into csv and keep it within a new folder as follows:
  DrugIssue = read.table(file = drugIssue_fileslist[i], header = T, sep = "\t")  # Read the file
  file_name <- paste0(gsub('.{3}$', '', drugIssue_fileslist[i]),"csv")  # Substitute the 3 last digits (.txt extention) with .csv
  file_path <- paste0("../Extract_Aurum_csv/", file_name)   # Create a path to save the file
  write.csv(DrugIssue, file_path)   # Save it
}

temp_str1 <- c(1:9)
if(part == 1){
  temp_str2 <- c(10:99)
  temp_str3 <- c(100:110)
  temp_str1.1 <- paste0("00", as.character(temp_str1))
  temp_str2.1 <- paste0("0", as.character(temp_str2))
  temp_str3.1 <- as.character(temp_str3)
  temp_str <- c(temp_str1.1, temp_str2.1, temp_str3.1)
}else{
  temp_str2 <- c(10:86)
  temp_str1.1 <- paste0("00", as.character(temp_str1))
  temp_str2.1 <- paste0("0", as.character(temp_str2))
  temp_str <- c(temp_str1.1, temp_str2.1) 
}
temp_str
rm(temp_str1, temp_str1.1, temp_str2, temp_str2.1, temp_str3, temp_str3.1)


# read the patids from the my_patients. These are patids from Matt's initial extraction
# therefore patids with M/B recipients:
load("../../final_master.RData")
head(my_patients)
patids_m_b_all <- my_patients %>% select(patid) %>% distinct()
patids_m_b_all$patid <- as.character(patids_m_b_all$patid)
rm(my_patients, my_patients_vars)


# merge every therapy file with patids from the master file
read_csv <- 1
for(i in 1:length(drugIssue_fileslist)){    
  if(part == 1){
    read_csv <- read.csv(paste0("../Extract_Aurum_csv/PART1/Eleni_Aurum_Extract_DrugIssue_", temp_str[i], ".csv"))
  }else{
    read_csv <- read.csv(paste0("../Extract_Aurum_csv/PART2/Eleni_Aurum_2b_Extract_DrugIssue_", temp_str[i], ".csv"))
  }
  read_csv$patid <- as.character(read_csv$patid)
  merged_drugissue <- left_join(patids_m_b_all, read_csv, by = "patid")
  if(part == 1){
    assign(paste0("aurum_ext_drug_", temp_str[i], "_merged"), merged_drugissue)
  }else{
    assign(paste0("aurum_2b_ext_drug_", temp_str[i], "_merged"), merged_drugissue)
  }
}

# unite all the merged therapy-master files
if(part == 1){
  u <- dplyr::union(aurum_ext_drug_001_merged, aurum_ext_drug_002_merged) # files 1+2
  u <- u[!duplicated(u),] # deduplicate records that appear more than once
  
  for(i in 3:length(drugIssue_fileslist)){ # rest of files
    u <- dplyr::union(u, eval(parse(text = paste0("aurum_ext_drug_", temp_str[i], "_merged"))))
    u <- u[!duplicated(u),]
  }
}else {
  u <- dplyr::union(aurum_2b_ext_drug_001_merged, aurum_2b_ext_drug_002_merged) # files 1+2
  u <- u[!duplicated(u),] # deduplicate records that appear more than once
  
  for(i in 3:length(drugIssue_fileslist)){ # rest of files
    u <- dplyr::union(u, eval(parse(text = paste0("aurum_2b_ext_drug_", temp_str[i], "_merged"))))
    u <- u[!duplicated(u),]
  }
}

# remove missing product codes
u <- u %>% filter(!is.na(prodcodeid))

# examine duplicated records
dplyr::distinct(u)

# count distinct patids
u %>% summarise(n_distinct(patid))

# clear, tidy and tranform into dataframe
u <- as.data.frame(u)
u$patid <- as.character(u$patid)
u$staffid <- as.character(u$staffid)
u$prodcodeid <- as.character(u$prodcodeid)
u$myeventdate <- dmy(u$issuedate)
u$X <- NULL
head(u)
# summary(u)

# save the file as csv
if(part == 1){
  write.csv(u, file = "../merged_drugIssue_part1.csv")
}else {
  write.csv(u, file = "../merged_drugIssue_part2.csv")
}
