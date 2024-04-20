#######################################################################################################
#                                           *
#                                       PhD PROJECT
#                                    ELENI DOMZARIDOU
#                                UNIVERSITY OF MANCHESTER
#                                       March 2021
#                                        CPRD Aurum
#                                           *
# 
# Aims:
# a) 
# b) 
# c) 
# 
#######################################################################################################

library(dplyr)
library(lubridate)

part <- 1 # USER INPUT THAT INDICATES WHICH PART OF AURUM EXTRACT YOU LOOK AT (parts 1 and 2 have all the primary care records 
# but are split due to big size)

setwd(paste0("~/rds/Eleni_Aurum_JAN_2021/source_data/Extract_Aurum_Part", part))

if(part == 1){
  observation_fileslist <- list.files(pattern = 'Eleni_Aurum_Extract_Observation_.*\\.txt') # keep all the file names that are Therapy files in the variable "therapy_fileslist"
}else{
  observation_fileslist <- list.files(pattern = 'Eleni_Aurum_2b_Extract_Observation_.*\\.txt') # keep all the file names that are Therapy files in the variable "therapy_fileslist"
}

# Run only if you have not saved observation files as.cvs 
for(i in 1:length(observation_fileslist)){ # Convert each txt file into csv and keep it within a new folder as follows:
  observation_file = read.table(file = observation_fileslist[i], header = T, sep = "\t")  # Read the file
  file_name <- paste0(gsub('.{3}$', '', observation_fileslist[i]),"csv")  # Substitute the 3 last digits (.txt extention) with .csv
  file_path <- paste0("../Extract_Aurum_csv/PART",part,"/", file_name)   # Create a path to save the file
  write.csv(observation_file, file_path)   # Save it
}

temp_str1 <- c(1:9)

if(part == 1){
  temp_str2 <- c(10:88)
}else{
  temp_str2 <- c(10:68)  
}

temp_str1.1 <- paste0("00", as.character(temp_str1))
temp_str2.1 <- paste0("0", as.character(temp_str2))
temp_str <- c(temp_str1.1, temp_str2.1)
temp_str
rm(temp_str1, temp_str1.1, temp_str2, temp_str2.1)


# read the patids from the my_patients - from Matt's initial extraction (M/B recipients):
load("../../final_master.RData")
patids_m_b_all <- my_patients %>% select(patid) %>% distinct()
patids_m_b_all$patid <- as.character(patids_m_b_all$patid)
rm(my_patients, candidates_JAN2021, All_pats, excl_drugs, my_patients_vars)

rm(observation_file)

# merge every observation file with patids from the master file
read_csv <- 1
for(i in 1:length(observation_fileslist)){    
  if(part == 2){
    read_csv <- read.csv(paste0("../Extract_Aurum_csv/PART2/Eleni_Aurum_2b_Extract_Observation_", temp_str[i], ".csv"))
  }else{
    read_csv <- read.csv(paste0("../Extract_Aurum_csv/PART1/Eleni_Aurum_Extract_Observation_", temp_str[i], ".csv"))
  }
  read_csv$patid <- as.character(read_csv$patid)
  merged_observation <- left_join(patids_m_b_all, read_csv, by = "patid")
  assign(paste0("Aurum_ext_clin_", temp_str[i], "_merged"), merged_observation)
}

# unite all the merged observation-master files
u <- dplyr::union(Aurum_ext_clin_001_merged, Aurum_ext_clin_002_merged) # files 1+2
u <- u[!duplicated(u),] # deduplicate records that appear more than once

for(i in 3:length(observation_fileslist)){ # rest of files
  u <- dplyr::union(u, eval(parse(text = paste0("Aurum_ext_clin_", temp_str[i], "_merged"))))
  u <- u[!duplicated(u),]
}


# examine duplicated records
dplyr::distinct(u)

# count how many medcodeis are missing
sum(!is.na(u$medcodeid))

# filter records with medcodeid == NA
u <- u %>% filter(!is.na(medcodeid))

# count distinct patids
u %>% summarise(n_distinct(patid))

# clear, tidy and tranform into dataframe
u <- as.data.frame(u)
str(u)
u$patid <- as.character(u$patid)
u$staffid <- as.character(u$staffid)
u$medcodeid <- as.character(u$medcodeid)
u$myeventdate <- dmy(u$enterdate)
u$X <- NULL
head(u)
summary(u)


# save the file as csv
write.csv(u, file = paste0("../merged_observation_part",part,".csv"))







