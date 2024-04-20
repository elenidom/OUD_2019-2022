
# Here we investigate whether primary care overdose codes are systematically used to
# declare drug overdose. However, note that the codes might not be specific, involving
# poisoning due to self-harm or other common drugs (e.g. paracetamol)

# Merge the clinical codes with the master cohort
class(merged_clinical$patid)
merged_clinical$patid <- as.character(merged_clinical$patid)

# load the master file
load("~/rds/Eleni_Aurum_JAN_2021/final_master.RData")

# keep the patid, index_date from the master and merged them with the separated files of comorbid conditions
# to examine comorbidities at baseline (como_base = YES if my_first_diagnosis_date < index_date):
my_patids <- my_patients %>% filter(prereg >= 1) %>% select(c("patid", "index_date")) %>% distinct()
my_patids %>% count(patid)
class(my_patids$patid)

names(my_patids) <- c("patid", "index_date")

bb <- left_join(my_patids, merged_clinical, by = "patid") 
bb$medcodeid <- as.character(bb$medcodeid)
bb <- bb %>% rename("medcode_aurum" = "medcodeid")

# merge the above file with overdose Read codes
over <- read.csv("./source_data/comorbid/medcodes_Overdose.csv")
over$medcode_aurum <- as.character(over$medcode_aurum)
bb <- left_join(bb, over, by = "medcode_aurum")
ls(bb)
bb <- bb %>% filter(Overdose == 1)
over_summary <- bb %>% select(c(term, medcode_aurum, patid)) %>% group_by(term, medcode_aurum) %>% summarise(N = n_distinct(patid))

write.table(over_summary, "Overdose_codes.Aurum_summary.txt", sep = ",")


home <- read.csv("./source_data/comorbid/medcodes_Homelessness.csv")
home$medcode <- as.character(home$medcode_aurum)
bb <- left_join(my_patids, merged_clinical, by = "patid") 
bb$medcode <- as.character(bb$medcode)
bb <- left_join(bb, home, by = "medcode")
ls(bb)
bb <- bb %>% filter(homelessness == 1)
home_summary <- bb %>% group_by(term) %>% summarise(N = n_distinct(patid))

write.csv(home_summary, "homelessness_codes.aurum_summary.csv")

# merge the above file with prison Read codes
prison <- read.csv("./source_data/comorbid/medcodes_Prison.csv")
prison$medcode <- as.character(prison$medcode_aurum)
merged_clinical$patid <- as.character(merged_clinical$patid)
bb <- left_join(my_patids, merged_clinical, by = "patid") 
bb$medcode <- as.character(bb$medcode)
bb <- left_join(bb, prison, by = "medcode")
ls(bb)
bb <- bb %>% filter(prison == 1)
prison_summary <- bb %>% group_by(term) %>% summarise(N = n_distinct(patid))

write.csv(prison_summary, "prison_codes.aurum_summary.csv")

