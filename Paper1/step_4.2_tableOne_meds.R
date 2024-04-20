
#######################################################################################################
#                                           *
#                                       PhD PROJECT
#                                    ELENI DOMZARIDOU
#                                UNIVERSITY OF MANCHESTER
#                                       August 2021
#                                    CPRD GOLD/Aurum
#                                           *
# 
# Aims:
# a) The code populates table one with common medications at baseline
# b) It works for both Aurum/GOLD, since user provides the name of the database
# 
#######################################################################################################

library(tableone)
library(dplyr)

# --------------------------------------------------------------------------------------
# continue working using the dataframe: mymeds.base from the previous step
# --------------------------------------------------------------------------------------

# Provide database name
database <- "Aurum" # <----- *** USER INPUT ***

mymeds.base <- read.csv(paste0("Meds.base_of_interest_for_master_merging.", database, ".csv"))
str(mymeds.base)
mymeds.base$X <- NULL
mymeds.base$patid <- as.character(mymeds.base$patid)


# load the master file
load(paste0("~/rds/Eleni_", database,"_JAN_2021/final_master.RData"))

# keep the patid, index_date from the master and merged them with the separated files of medicines
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
names(master_OST_mod) <- c("patid", "therapy", "switcher")
str(master_OST_mod)
head(master_OST_mod)
master_OST_mod %>% group_by(patid) %>% summarise(N = n())
master_OST_mod %>% group_by(therapy, switcher) %>% summarise(N = n())

# merge with meds file
ls(mymeds.base)
head(mymeds.base)
todata <- left_join(master_OST_mod, mymeds.base, by = "patid")
todata %>% group_by(patid) %>% summarise(N = n())
summary(todata)

# If med = NA --> this patid did not received any of interest at baseline but we do not exclude
# them, otherwise the % in TableOne will refer only to people received meds and what we want is the % to refer
# to the overall cohort of patients (with meds or not)

todata <- todata %>% distinct()

dput(names(todata))
myvars <- c("GABA.base", "BNZ.base", "Z_drugs.base", "ANTIPSYCHO.base", "MOODSTAB.base", "ANTIDEP.base")

todata <- todata %>% group_by(patid, therapy, switcher) %>% summarise_at(myvars, max) %>% ungroup()

# convert columns into factor vars
todata[,myvars] <- lapply(todata[,myvars], factor)

# Assign strata
factorVars <- c("therapy", "switcher")

# generate TableOne
TOoutput <- CreateTableOne(vars = myvars, strata = factorVars, data = todata)
TOoutput
capture.output(print(TOoutput, formatOptions = list(big.mark = ",")), 
               file = paste0("./", database, "_tableone_meds.txt"))


