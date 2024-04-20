
ever.doi <- master.ddi.gabaever # USER INPUT
DOI <- "GABA" # drug of interest (doi)

a <- ever.doi %>% dplyr::select(patid) %>% distinct() # no drugs during FU

# acquire new IPWs after reruning the PS:
# The env generatePS_0.RData is the result of code from file step_3.0_create_master_file_for_modelling_phase.R
load("/mnt/bmh01-rds/Ashcroft_Domzaridou_PSTRC/mbmhted6/Eleni_Combined_DBs/IRs_Analysis/generatePS_0.RData")

b <- my_patients.single %>% dplyr::select(c(patid, DOI)) %>% distinct() # all patids

a$fu <- 22

ele <- left_join(b, a, by = "patid")
head(ele)

ele <- ele %>% mutate(fu = ifelse(is.na(fu), 11, fu))

table(ele$GABA, ele$fu) # USER INPUT


###### Calculate all the patients who were never exposed to any of the drugs of interest

meds.au <- read.csv("/mnt/bmh01-rds/Ashcroft_Domzaridou_PSTRC/mbmhted6/Eleni_Aurum_JAN_2021/Meds_tvc_master_Aurum.csv")
meds.au <- meds.au %>% dplyr::select(c(patid, ANTIDEP, ANTIPSYCHO, GABA, Z_drugs, BNZ)) %>% distinct()

meds.go <- read.csv("/mnt/bmh01-rds/Ashcroft_Domzaridou_PSTRC/mbmhted6/Eleni_GOLD_JAN_2021/Meds_tvc_master_GOLD.csv")
meds.go <- meds.go %>% dplyr::select(c(patid, ANTIDEP, ANTIPSYCHO, GABA, Z_drugs, BNZ)) %>% distinct()

meds.all <- union(meds.au, meds.go)
head(meds.all)
summary(meds.all)

# patients who were never exposed to any of the DOI:
meds.all <- meds.all %>% mutate(anydrug = ANTIDEP + ANTIPSYCHO + GABA + Z_drugs + BNZ)
meds.all %>% filter(anydrug == 0) %>% distinct()


