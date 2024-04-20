
# Clean opioids list from Teng-Chou

# Change to GOLD

setwd("/mnt/bmh01-rds/Ashcroft_Domzaridou_PSTRC/mbmhted6/Eleni_Aurum_JAN_2021/source_data/comorbid")

opioids_aurum <- read.table("Opioids_pain_Aurum.txt", sep = "\t", header = TRUE)
opioids_aurum$prodcodeid <- as.character(opioids_aurum$prodcodeid)

head(opioids_aurum)

library(dplyr)

opioids_aurum <- opioids_aurum %>% select(c(prodcodeid, drugsubstancename, productname))
opioids_aurum$my_class <- "Opioid analgesic"
opioids_aurum$OPIOIDS_PAIN <- 1
opioids_aurum <- opioids_aurum %>% rename("mydrugsubstancename" = "drugsubstancename")
write.csv(opioids_aurum, "/mnt/bmh01-rds/Ashcroft_Domzaridou_PSTRC/mbmhted6/Eleni_Aurum_JAN_2021/source_data/comorbid/OPIOIDS_PAIN_Aurum.csv")
