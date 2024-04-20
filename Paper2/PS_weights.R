

# ----------------------------------------------------------------------------------------------------------------------
# >> Calculate adjusted IRRs & weighted IRRs using Poisson regression models
# ----------------------------------------------------------------------------------------------------------------------
setwd("/mnt/bmh01-rds/Ashcroft_Domzaridou_PSTRC/mbmhted6/Eleni_Combined_DBs/ERs_Analysis")

testmulti <- read.csv("mymasterERs.csv")

ls(testmulti)

library(dplyr)

# acquire new IPWs after reruning the PS:
# The env generatePS_0.RData is the result of code from file step_3.0_create_master_file_for_modelling_phase.R
setwd("/mnt/bmh01-rds/Ashcroft_Domzaridou_PSTRC/mbmhted6/Eleni_Combined_DBs/IRs_Analysis")
load("generatePS_0.RData")

# the following env has the master file for modelling
load("/mnt/bmh01-rds/Ashcroft_Domzaridou_PSTRC/mbmhted6/Eleni_Combined_DBs/generatePS.RData")

mydata <- as.data.frame(my_patients.single)

# substitute NAs in region with 11 (Unknown)
mydata$region <- as.numeric(mydata$region)

# Replace NAs in region
mydata <- mydata %>% mutate(
  region = if_else(is.na(region), 11, region)
)

# PS model
ps.model <- glm(as.factor(therapy) ~ as.factor(gender) + 
                  as.factor(Alcohol) + as.factor(myage_cat) +
                  as.factor(imd2015_5) + as.factor(ethnos_final) + as.factor(region) +
                  as.factor(Schizophrenia) + as.factor(PersonalityDisorders) + 
                  as.factor(Bipolar) + as.factor(Overdose) + as.factor(Self_harm) +
                  as.factor(BNZ) + as.factor(GABA) + as.factor(ANTIPSYCHO) + 
                  as.factor(ANTIDEP) + as.factor(HIV) + 
                  as.factor(HBV) + as.factor(HCV), 
                data = mydata, family = "binomial")

# calculate IPWs
ps.obs <- ifelse(mydata$therapy == 1, 1 - predict(ps.model, type = "response"),
                 predict(ps.model, type = "response"))

mydata$myipws <- 1 / ps.obs

summary(mydata$myipws)
sd(mydata$myipws)

### Stabilised weights:
pd.m <-  predict(ps.model, type = "response")

numer.fit <- glm(factor(therapy) ~ 1, family = binomial(), data = mydata)
summary(numer.fit)

pn.m <- predict(numer.fit, type = "response")

mydata$mysipws <- ifelse(mydata$therapy == 1, ((1-pn.m)/(1-pd.m)), (pn.m/pd.m))
summary(mydata$mysipws)

###

myweights <- mydata %>% dplyr::select(c(patid, myipws, mysipws)) %>% distinct()

testmulti.slice <- testmulti %>% dplyr::select(c(patid, gender, age.tvc, epoc, 
                                                 exposure_interval, therapy, 
                                                 status, tstart, tstop, NFO,
                                                 physicalHealthDis, mentalHealthDis))

testmulti.slice$patid <- as.character(testmulti.slice$patid)

# append IPWs (new) to ERs master file
testmulti.slice <- left_join(testmulti.slice, myweights, by = "patid")
head(testmulti.slice)

testmulti.slice <- testmulti.slice %>% 
  group_by(patid) %>% 
  dplyr::select(c(patid, age.tvc, gender, epoc, physicalHealthDis, 
                  mentalHealthDis, myipws, mysipws)) %>%
  slice(1) %>%
  rename("age" = "age.tvc") %>% # because we keep only the age at first row which is the age at ID
  ungroup()

summary(testmulti.slice)

# HISTOGRAMS - distribution of weights
hist(testmulti.slice$myipws, 
     breaks = 100, 
     main = "Distribution of normal weights", 
     xlim = c(0, 10), 
     xlab = "weights")

hist(testmulti.slice$mysipws, 
     breaks = 100, 
     main = "Distribution of stabilised weights", 
     xlim = c(0, 3), 
     xlab = "weights")


# NEXT STEP -----------------------------------------------------------------------
#
#  --> Merge with drug-of-interest data (go back to files: "new.*.nfo.format.R")
#
# ---------------------------------------------------------------------------------

