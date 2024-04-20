
library(dplyr)
library(lubridate)

load("temp_ws_sccs.RData")

# rename to allow linkage
mymeds.antipsycho <- mymeds.pc %>% rename("patid" = "patid_set20") %>% 
  dplyr::select(c(patid, ANTIPSYCHO)) %>% 
  distinct() %>%
  filter(ANTIPSYCHO > 0) # exclude antipsychos that were prescribed before baseline

# load data that are in format:
#  ost_episode|tstart|tstop
# ----------------------------
#  1              0     288
#  2            288     512
#  3            512    1076

rm(master.ddi, mydata)

# load dataset that shows in/out of OST info and NFO without time-split
load("/mnt/bmh01-rds/Ashcroft_Domzaridou_PSTRC/mbmhted6/Eleni_Combined_DBs/ERs_Analysis/NFO_ERs.RData")

mydata <- newdf2

head(mydata)

# EXCLUDE TEMPORARILY THE THERAPY VAR BECAUSE IT GENERATES DUPLICATES
mydata <- mydata %>% 
  dplyr::select(c("patid", "index_date", "exit_date",
                  "OST_episode", "status", "tstart", "tstop", 
                  "NFO", "gender", "epoc")) 

mydata %>% count(patid)

# merge ost episode data with medication of interest
master.ddi <- left_join(mydata, mymeds.antipsycho, by = "patid")

master.ddi <- master.ddi %>% rename("antipsycho.sta" = "ANTIPSYCHO") 

summary(master.ddi)

head(master.ddi)

# keep antipsycho in the appropriate OST interval, exclude presciptions that exceed the fu are have NAs,
# and also exclude prescriptions according to which patients were exposed to antipsychos after the end of fu
master.ddi <- master.ddi %>%
  mutate(
    antipsycho.sta = ifelse(antipsycho.sta >= tstart & antipsycho.sta <= tstop & !is.na(antipsycho.sta), antipsycho.sta, 0)
  )

# arrange data
master.ddi <- master.ddi %>% arrange(patid, tstart, antipsycho.sta) %>% distinct()

# create an id that identifies uniquely the patid within a certain time-frame (tstop)
master.ddi <- master.ddi %>% mutate(id = row_number()) 
master.ddi <- master.ddi %>% group_by(patid, tstop) %>% mutate(id = max(id)) %>% ungroup()

# keep a list of ids with patids to append them later after formatting the data with SCCS::formatdata()
mypatids <- master.ddi %>% dplyr::select(c(patid, id)) %>% distinct()

# exclude patids who were never exposued to ANTIPSYCHO - we will append them back later
master.ddi.antipsychoever <- master.ddi %>% group_by(patid) %>% 
  mutate(ANTIPSYCHO.ever = ifelse(max(antipsycho.sta) == 0, 1, 0)) %>%
  filter(ANTIPSYCHO.ever == 1) %>%
  dplyr::select(-ANTIPSYCHO.ever) %>%
  ungroup()

master.ddi.antipsychoever %>% count(patid) # patients with no exposure to ANTIPSYCHOs

master.ddi.antipsycho <- master.ddi %>% group_by(patid) %>% filter(max(antipsycho.sta) > 0) %>% ungroup()

master.ddi.antipsycho %>% count(patid) # patients with exposure to ANTIPSYCHOs

master.ddi.antipsycho <- master.ddi.antipsycho %>% dplyr::select(c(id, tstart, tstop, status, antipsycho.sta))

# format the data
library(SCCS)

# run only once
# formated.data <- SCCS::formatdata(indiv = id, astart = tstart, aend = tstop,
#                                   aevent = NA,
#                                   adrug = antipsycho.sta, aedrug = antipsycho.sta + 28,
#                                   data = master.ddi.antipsycho)

# write.csv(formated.data, "formated.antipsycho.csv")

formated.data <- read.csv("formated.antipsycho.csv")
formated.data <- as.data.frame(formated.data)
formated.data <- formated.data %>% rename("id" = "indiv")
formated.data$id <- as.integer(formated.data$id)
head(formated.data)

formated.data <- formated.data %>% dplyr::select(-event, -eventday, -aevent, -age)

# create a new df by appending the formated data with the patid info
head(mypatids)
master.ddi.antipsycho2 <- left_join(formated.data, mypatids, by = "id")
summary(master.ddi.antipsycho2)


# checks
head(master.ddi.antipsycho2)
sum(master.ddi.antipsycho2$lower < master.ddi.antipsycho2$astart)
sum(master.ddi.antipsycho2$upper > master.ddi.antipsycho2$aend)
master.ddi.antipsycho2 <- master.ddi.antipsycho2 %>% rename("tstart" = "astart", "tstop" = "aend")
master.ddi.antipsycho2 %>% group_by(patid) %>% summarise(N = n_distinct(patid))


# append status and episode (rank) data
status_epi_data <- master.ddi %>% dplyr::select(c(id, status, OST_episode, tstart, tstop)) %>% distinct()
status_epi_data <- left_join(status_epi_data, mypatids, by = "id") # append patid
status_epi_data$id <- NULL
head(status_epi_data)

master.ddi.antipsycho3 <- left_join(master.ddi.antipsycho2, status_epi_data, by = c("patid", "tstart", "tstop"))
summary(master.ddi.antipsycho3)


# select NFO data
NFO_info <- master.ddi %>% 
  dplyr::select(c(patid, tstart, tstop, NFO)) %>% 
  filter(NFO == 1) %>% # keep only dates with NFO
  distinct()

head(NFO_info)

# append NFO with the formated data that contains ANTIPSYCHO info per time span
master.ddi.antipsycho3 <- left_join(master.ddi.antipsycho3, NFO_info, by = c("patid", "tstart", "tstop"))

# check the number of NFOs (with the one among patients who did not receive ANTIPSYCHO, it is 12,973)
# a <- master.ddi.antipsycho3 %>% 
#   filter(NFO == 1) %>%
#   dplyr::select(c(patid, tstop)) %>%
#   distinct()

# keep NFO in the correct time interval (lower-upper)
master.ddi.antipsycho3 <- master.ddi.antipsycho3 %>% 
  group_by(patid) %>%
  mutate(
    NFO_event = ifelse(NFO == 1 & tstop == upper, 1, 0),
    NFO_event = ifelse(is.na(NFO_event), 0, NFO_event) # replace NAs with zero
  ) %>% ungroup()

sum(master.ddi.antipsycho3$NFO_event) # check

master.ddi.antipsycho3 <- master.ddi.antipsycho3 %>% 
  dplyr::select(c(-indivL, -interval, -id, -NFO, -tstart, -tstop)) %>%
  rename("tstart" = "lower", "tstop" = "upper")

summary(master.ddi.antipsycho3)
head(master.ddi.antipsycho3)

# This is a fix because after formating the data using the SCCS::formatdata(), some patids
# had continuous tstart/stop values and some missed the unity - e.g.:
# 
# patid tstart  tstop
# --------------------
# 1001  0       14
# 1001  15      29
#
# Instead of:
#
# patid tstart  tstop
# --------------------
# 1001  0       14
# 1001  14      29
#
master.ddi.antipsycho3 <- master.ddi.antipsycho3 %>% group_by(patid) %>%
  mutate( # chech if the previous tstop differs from the current tstart (they must be the same)
    tstart2 = ifelse(tstart != lag(tstop, default = first(tstop)), tstart-1, tstart),
    tstart2 = ifelse(tstart2 < 0, 0, tstart2)
  ) %>% ungroup()

master.ddi.antipsycho3 <- master.ddi.antipsycho3 %>% mutate(tstart = tstart2) %>% dplyr::select(-tstart2)

# merge with non-antipsycho data
head(master.ddi.antipsychoever)

master.ddi.antipsychoever <- master.ddi.antipsychoever %>% 
  dplyr::select(c(-index_date, -exit_date, -id, -gender, -epoc)) %>%
  rename("NFO_event" = "NFO")

master.ddi.antipsychoever$antipsycho.sta <- as.factor(master.ddi.antipsychoever$antipsycho.sta)
master.ddi.antipsycho3$X <- NULL
master.ddi.antipsycho3$antipsycho.sta <- as.factor(master.ddi.antipsycho3$antipsycho.sta)
master.ddi.antipsycho_all <- dplyr::union(master.ddi.antipsychoever, master.ddi.antipsycho3)
master.ddi.antipsycho_all <- master.ddi.antipsycho_all %>% rename("ANTIPSYCHO" = "antipsycho.sta")

# ---------------------------------------------------------------------
# Merge with WEIGHTS AND OTHER VARS TO ADJUST THE MODELS
# >=>=> RUN THE CODE IN 'PS_weights.R' file
# ---------------------------------------------------------------------

source("./PS_weights.R") # located in the SCCS folder

setwd("/mnt/bmh01-rds/Ashcroft_Domzaridou_PSTRC/mbmhted6/Eleni_Combined_DBs/SCCS")

master.ddi.antipsycho_all <- dplyr::left_join(master.ddi.antipsycho_all, testmulti.slice, by = "patid")

head(master.ddi.antipsycho_all)

summary(master.ddi.antipsycho_all)

# ---------------------------------------------------------------------
# NEW VAR: time-varying age that changes at an OST episode
# ---------------------------------------------------------------------

master.ddi.antipsycho_all <- master.ddi.antipsycho_all %>%
  group_by(patid, OST_episode) %>%
  mutate(
    age.tv = max(age + tstop/365.25)
  ) %>% ungroup()

summary(master.ddi.antipsycho_all$age.tv)
head(master.ddi.antipsycho_all)

# ---------------------------------------------------------------------
# NEW VAR: age at index date
# ---------------------------------------------------------------------

master.ddi.antipsycho_all <- master.ddi.antipsycho_all %>%
  group_by(patid) %>%
  mutate(
    age.id = min(age.tv)
  ) %>% ungroup()

summary(master.ddi.antipsycho_all$age.id)

# ---------------------------------------------------------------------

# Append treatment modality data
ls(my_patients)

modality <- my_patients %>% 
  filter(prereg >= 1) %>%
  group_by(patid) %>%
  slice(1) %>%
  ungroup() %>%
  dplyr::select(c(patid, therapy)) %>% 
  distinct() 

# merge with formated data
master.ddi.antipsycho_all <- left_join(master.ddi.antipsycho_all, modality, by = "patid")

head(master.ddi.antipsycho_all)

# ---------------------------------------------------------------------
# NEW VARIABLE: over_history (binary; within the patients OST episodes)
# ---------------------------------------------------------------------

master.ddi.antipsycho_all <- master.ddi.antipsycho_all %>%
  group_by(patid, OST_episode) %>%
  mutate(
    over_history = ifelse(lag(NFO_event, defaualt = first(NFO_event)) == 1, 1, 0),
    over_history = ifelse(is.na(over_history), 0, over_history)
  ) %>% ungroup()

# ---------------------------------------------------------------------

# <|>

# ---------------------------------------------------------------------
# NEW VARIABLE: epi_n (numeric; number of OST episodes per patid)
# ---------------------------------------------------------------------

master.ddi.antipsycho_all <- master.ddi.antipsycho_all %>%
  group_by(patid) %>%
  mutate(
    epi_n = max(OST_episode)
  ) %>% ungroup() %>% ungroup()

# ---------------------------------------------------------------------

# remove records whereby tstart and tstop are the same
master.ddi.antipsycho_all <- master.ddi.antipsycho_all %>% filter(tstart != tstop)


# ----------------------------------------------------------------------------------------
# All the data (antipsycho on/off and NFO yes/no) *** CRUDE RATES / RELATIVE ESTIMATES ***
# ----------------------------------------------------------------------------------------

# summary of OAT episode duration when on ANTIPSYCHOs
a <- master.ddi.antipsycho_all %>% 
  group_by(patid) %>% 
  filter(ANTIPSYCHO == 1) %>%
  mutate(ANTIPSYCHO.fu = sum(tstop - tstart)) %>%
  ungroup() 

a1 <- a %>% group_by(patid) %>% slice(1) %>% ungroup()

hist(a1$ANTIPSYCHO.fu, breaks = 100, 
     main = "Observation years among patients who received antipsychotics",
     xlab = "time (years)",
     xlim = c(0, 8000))

summary(a1$ANTIPSYCHO.fu)

#######################################################################
#                                                                     #
# If you need to restrict to 1 year fu after the completion of the    #
# last OST episode,                                                   #
# GO TO FILE "SENSITIVITY/1year.R" and run that code and return here. #                                                  #
#                                                                     #
#######################################################################


# -------------------------------------------------------------------
# [!] Treatment discontinuation gap: 
# -------------------------------------------------------------------
#  If prescriptions of the drugs of interest were
#   interrapted (namely, if there was a treatment gap between
#   two continuous treatment episodes lasted less than 7 days),
#   then assume that the patient was receiving treatment over 
#   that time. This is a matter of sensitivity analysis.
# -------------------------------------------------------------------

master.ddi.antipsycho_all <- master.ddi.antipsycho_all %>% 
  group_by(patid) %>%
  mutate(ANTIPSYCHO = ifelse(ANTIPSYCHO == 0 & tstop - tstart <= 14 &  # DEFAULT: 14 / SENSITIVITY: 28
                         lag(ANTIPSYCHO == 1, default = first(ANTIPSYCHO) == 1) & 
                         lead(ANTIPSYCHO == 1, default = last(ANTIPSYCHO) == 1), 
                       1, ANTIPSYCHO)
  ) %>% ungroup()

# -------------------------------------------------------------------
# RUN FOR SENSITIVITY ANALYSIS ONLY: 
# If you need to exclude patids with history of overdose go to file
# 'No_overdose_history.R'
# -------------------------------------------------------------------



# ------------------------------------------------------------------------
# on/off ANTIPSYCHOs any time OST (Crude estimates and regression models)
# ------------------------------------------------------------------------

master.ddi.antipsycho_all %>%
  summarise(PY = sum(tstop - tstart)/365.25,
            Events = sum(NFO_event),
            RR = Events/PY * 100)

master.ddi.antipsycho_all %>%
  group_by(ANTIPSYCHO) %>%
  summarise(PY = sum(tstop - tstart)/365.25,
            Events = sum(NFO_event),
            RR = Events/PY * 100)

# prepare the data to run Poisson/NB unadjusted regression models
poi_data <- master.ddi.antipsycho_all %>% 
  dplyr::select(c("patid", "tstart", "tstop", "ANTIPSYCHO", "gender", "age.id", "epoc",
                  "epi_n", "therapy", "mysipws", "NFO_event")) %>% 
  distinct()

poi_data <- poi_data %>% mutate(diff = tstop - tstart)

# on/off ANTIPSYCHOs
poi_data <- poi_data %>%
  group_by(patid, ANTIPSYCHO) %>%
  mutate(
    fu = sum(diff), # fu on/off ANTIPSYCHO
    NFO_count = sum(NFO_event)
  ) %>% ungroup()

# keep only some vars to generate models
poi_data <- poi_data %>% 
  dplyr::select(c(patid, ANTIPSYCHO, gender, age.id, epoc, epi_n, 
                  therapy, mysipws, fu, NFO_count)) %>%
  distinct()

head(poi_data)

# ---------
# MODELS
# ---------

library(MASS)

poi_model.0 <- glm(NFO_count ~ factor(ANTIPSYCHO) + offset(log(fu)),
                   data = poi_data, subset = fu > 0, family = poisson)

# examine model's fit
with(poi_model.0, cbind(res.deviance = deviance, df = df.residual, 
                        p = pchisq(deviance, df.residual, lower.tail = FALSE)))

pchisq(deviance(poi_model.0), df.residual(poi_model.0), lower.tail = FALSE)

summary(poi_model.0)

est <- cbind(Estimate = coef(poi_model.0), confint(poi_model.0))
exp(est)


# NB
nb_model.0 <- glm.nb(NFO_count ~ factor(ANTIPSYCHO) + offset(log(fu)), data = poi_data, subset = fu > 0)

summary(nb_model.0)

# CIs
est <- cbind(Estimate = coef(nb_model.0), confint(nb_model.0))
exp(est)

# adjusted models
ls(poi_data)

poi_model.1 <- glm(NFO_count ~ factor(ANTIPSYCHO) + offset(log(fu)) + factor(epoc) + age.id +
                     factor(therapy) + factor(gender) + epi_n,
                   data = poi_data, subset = fu > 0, family = poisson)

summary(poi_model.1)

# CIs
est <- cbind(Estimate = coef(poi_model.1), confint(poi_model.1))
exp(est)

# deviance goodness-of-fit test (if p > 0.05, namely not statistically significant, it implies a good fit):
with(poi_model.1, cbind(res.deviance = deviance, df = df.residual, 
                        p = pchisq(deviance, df.residual, lower.tail = FALSE)))

# alternatively for p-value:
pchisq(deviance(poi_model.1),
       df.residual(poi_model.1),
       lower.tail = FALSE)

# NB
nb_model.1 <- glm.nb(NFO_count ~ factor(ANTIPSYCHO) + offset(log(fu)) + factor(epoc) + age.id +# epi_n +
                       factor(therapy) + factor(gender), 
                     data = poi_data, subset = fu > 0)

summary(nb_model.1)

est <- cbind(Estimate = coef(nb_model.1), confint(nb_model.1))
exp(est)

# a and theta parameters
a = 1/nb_model.1$theta # the dispersion parameter (a)
1/a # theta

# >> weights

# poisson
poi_model.1.w <- glm(NFO_count ~ factor(ANTIPSYCHO) + offset(log(fu)) + factor(epoc),# + epi_n,
                     data = poi_data, subset = fu > 0,
                     weights = mysipws, family = poisson)

# CIs
summary(poi_model.1.w)
est <- cbind(Estimate = coef(poi_model.1.w), confint(poi_model.1.w))
exp(est)

# NB
nb_model.1.w <- glm.nb(NFO_count ~ factor(ANTIPSYCHO) + offset(log(fu)) + factor(epoc),# + epi_n,
                       data = poi_data, subset = fu > 0,
                       weights = mysipws)

# CIs
summary(nb_model.1.w)
est <- cbind(Estimate = coef(nb_model.1.w), confint(nb_model.1.w))
exp(est)

# -------------------------- end of any time OST on/off ANTIPSYCHOs results -----------------------

# <|>

# -----------------------------------------------------------------------------------------
# in/out OST on/off ANTIPSYCHOs
# -----------------------------------------------------------------------------------------

master.ddi.antipsycho_all %>%
  group_by(status, ANTIPSYCHO) %>%
  summarise(PY = sum(tstop - tstart)/365.25,
            Events = sum(NFO_event),
            RR = Events/PY * 100)

# ON/OFF ANTIPSYCHOs in OST
poi_data <- master.ddi.antipsycho_all %>% mutate(diff = tstop - tstart) %>% filter(status == "in")

poi_data <- poi_data %>% 
  dplyr::select(c(patid, ANTIPSYCHO, gender, age.id, epoc, epi_n, 
                  therapy, mysipws,diff, NFO_event))

head(poi_data)

poi_data <- poi_data %>%
  group_by(patid, ANTIPSYCHO) %>%
  mutate(
    fu = sum(diff), # fu on/off OST
    NFO_count = sum(NFO_event)
  ) %>% ungroup()

poi_data <- poi_data %>% dplyr::select(-diff, -NFO_event) %>% distinct()


# unadjusted models
poi_model.0 <- glm(NFO_count ~ factor(ANTIPSYCHO) + offset(log(fu)),
                   data = poi_data, subset = fu > 0, family = poisson)

summary(poi_model.0)

# CIs
est <- cbind(Estimate = coef(poi_model.0), confint(poi_model.0))
exp(est)

with(poi_model.0, cbind(res.deviance = deviance, df = df.residual, 
                        p = pchisq(deviance, df.residual, lower.tail = FALSE)))

# Pearson's cumulative test statistic
sum(residuals(poi_model.0, 'pearson')^2) / df.residual(poi_model.0) 

# NB
nb_model.0 <- glm.nb(NFO_count ~ factor(ANTIPSYCHO) + offset(log(fu)),
                     data = poi_data, subset = fu > 0)

summary(nb_model.0)

est <- cbind(Estimate = coef(nb_model.0), confint(nb_model.0))
exp(est)

# adjusted model
poi_model.1 <- glm(NFO_count ~ factor(ANTIPSYCHO) + offset(log(fu)) + factor(therapy) + epi_n +
                     factor(gender) + age.id + factor(epoc),
                   data = poi_data, subset = fu > 0, family = poisson)

summary(poi_model.1)

est <- cbind(Estimate = coef(poi_model.1), confint(poi_model.1))
exp(est)

sum(residuals(poi_model.1, 'pearson')^2) / df.residual(poi_model.1) 


# NB
nb_model.1 <- glm.nb(NFO_count ~ factor(ANTIPSYCHO) + offset(log(fu)) + factor(therapy) +# epi_n +
                       factor(gender) + age.id + factor(epoc), data = poi_data, subset = fu > 0)

summary(nb_model.1)

est <- cbind(Estimate = coef(nb_model.1), confint(nb_model.1))
exp(est)

# weighted model
poi_model.1.w <- glm(NFO_count ~ factor(ANTIPSYCHO) + offset(log(fu)), # factor(epoc),
                     data = poi_data, subset = fu > 0, family = poisson, weights = mysipws)

summary(poi_model.1.w)

est <- cbind(Estimate = coef(poi_model.1.w), confint(poi_model.1.w))
exp(est)

# NB

# weighted model
nb_model.1.w <- glm.nb(NFO_count ~ factor(ANTIPSYCHO) + offset(log(fu)) + factor(epoc),
                     data = poi_data, subset = fu > 0, weights = mysipws)

summary(nb_model.1.w)

est <- cbind(Estimate = coef(nb_model.1.w), confint(nb_model.1.w))
exp(est)

# -------------------------- in/out OST on ANTIPSYCHO results --------------------------------

# <|>

# -----------------------------------------------------------------------------------------
# ON/OFF ANTIPSYCHOs out of OST
# -----------------------------------------------------------------------------------------

poi_data <- master.ddi.antipsycho_all %>% mutate(diff = tstop - tstart) %>% filter(status == "out")

poi_data <- poi_data %>% 
  dplyr::select(c(patid, ANTIPSYCHO, gender, age.id, epoc, epi_n, 
                  therapy, mysipws,diff, NFO_event))

head(poi_data)

poi_data <- poi_data %>%
  group_by(patid, ANTIPSYCHO) %>%
  mutate(
    fu = sum(diff), # fu on/off OST
    NFO_count = sum(NFO_event)
  ) %>% ungroup()

poi_data <- poi_data %>% dplyr::select(-diff, -NFO_event) %>% distinct()

# models
poi_model.0 <- glm(NFO_count ~ factor(ANTIPSYCHO) + offset(log(fu)),
                   data = poi_data, subset = fu > 0, family = poisson)

summary(poi_model.0)

# CIs
est <- cbind(Estimate = coef(poi_model.0), confint(poi_model.0))
exp(est)

# deviance goodness-of-fit test (p > 0.05, namely not statistically significant,
# implies a good fit):
with(poi_model.0, cbind(res.deviance = deviance, df = df.residual, 
                        p = pchisq(deviance, df.residual, lower.tail = FALSE)))

# Pearson's cumulative test statistic
sum(residuals(poi_model.0, 'pearson')^2) / df.residual(poi_model.0) 

# NB
nb_model.0 <- glm.nb(NFO_count ~ factor(ANTIPSYCHO) + offset(log(fu)),
                     data = poi_data, subset = fu > 0)

summary(nb_model.0)

# CIs
est <- cbind(Estimate = coef(nb_model.0), confint(nb_model.0))
exp(est)

# adjusted
poi_model.1 <- glm(NFO_count ~ factor(ANTIPSYCHO) + offset(log(fu)) + age.id +# epi_n +
                     factor(gender) + factor(therapy) + factor(epoc),
                   data = poi_data, subset = fu > 0, family = poisson)

# CIs
est <- cbind(Estimate = coef(poi_model.1), confint(poi_model.1))
exp(est)

summary(poi_model.1)

# adjusted NB
nb_model.1 <- glm.nb(NFO_count ~ factor(ANTIPSYCHO) + offset(log(fu)) + factor(therapy) + 
                       age.id + factor(gender) + #epi_n +
                       factor(epoc), data = poi_data, subset = fu > 0)

summary(nb_model.1)

# CIs
est <- cbind(Estimate = coef(nb_model.1), confint(nb_model.1))
exp(est)

# weighted
poi_model.1w <- glm(NFO_count ~ factor(ANTIPSYCHO) + offset(log(fu)) + factor(epoc) + epi_n ,
                    data = poi_data, subset = fu > 0, family = poisson, weights = mysipws)

# CIs
est <- cbind(Estimate = coef(poi_model.1w), confint(poi_model.1w))
exp(est)

summary(poi_model.1.w)

# weighted NB
nb_model.1w <- glm.nb(NFO_count ~ factor(ANTIPSYCHO) + offset(log(fu)) + factor(epoc),# + epi_n,
                      data = poi_data, subset = fu > 0, weights = mysipws)

# CIs
est <- cbind(Estimate = coef(nb_model.1w), confint(nb_model.1w))
exp(est)

summary(nb_model.1w)

# -------------------------- end of on/off ANTIPSYCHO out of OST results -------------------------

# <|>

# ----------------------------------------------------------------------------------------
# OST MODALITY STRATA
# ----------------------------------------------------------------------------------------

poi_data <- master.ddi.antipsycho_all %>% mutate(diff = tstop - tstart)

# ==> METHADONE
poi_data.inmeth <- poi_data %>% filter(therapy == 1 & status == "in") # in methadone only

poi_data.inmeth <- poi_data.inmeth %>% 
  dplyr::select(c(patid, ANTIPSYCHO, gender, age.id, epoc, epi_n, mysipws, diff, NFO_event))

head(poi_data.inmeth)

poi_data.inmeth <- poi_data.inmeth %>%
  group_by(patid, ANTIPSYCHO) %>%
  mutate(
    fu = sum(diff), # fu on/off OST
    NFO_count = sum(NFO_event)
  ) %>% ungroup()

poi_data.inmeth <- poi_data.inmeth %>% dplyr::select(-diff, -NFO_event) %>% distinct()  

# crude rates
# on/off ANTIPSYCHOs
poi_data.inmeth %>%
  group_by(ANTIPSYCHO) %>%
  summarise(PY = sum(fu)/365.25,
            Events = sum(NFO_count),
            RR = Events/PY * 100)

# models
# unadjusted
poi_model.0 <- glm(NFO_count ~ factor(ANTIPSYCHO) + offset(log(fu)),
                   data = poi_data.inmeth, subset = fu > 0, family = poisson)

summary(poi_model.0)

# CIs
est <- cbind(Estimate = coef(poi_model.0), confint(poi_model.0))
exp(est)

# adjusted
poi_model.inm.adj <- glm(NFO_count ~ factor(ANTIPSYCHO) + offset(log(fu)) + age.id + epi_n +
                           factor(epoc) + factor(gender),
                         data = poi_data.inmeth, subset = fu > 0, family = poisson)

summary(poi_model.inm.adj)

# CIs
est <- cbind(Estimate = coef(poi_model.inm.adj), confint(poi_model.inm.adj))
exp(est)

# weighted
poi_model.inm.w <- glm(NFO_count ~ factor(ANTIPSYCHO) + offset(log(fu)) + factor(epoc) + epi_n,
                       data = poi_data.inmeth, subset = fu > 0, family = poisson, weights = mysipws)

# CIs
est <- cbind(Estimate = coef(poi_model.inm.w), confint(poi_model.inm.w))
exp(est)

summary(poi_model.inm.w)

# == NB == #

# unadjusted
nb_model.0 <- glm.nb(NFO_count ~ factor(ANTIPSYCHO) + offset(log(fu)),
                     data = poi_data.inmeth, subset = fu > 0)

summary(nb_model.0)

# CIs
est <- cbind(Estimate = coef(nb_model.0), confint(nb_model.0))
exp(est)

# adjusted
nb_model.inm.adj <- glm.nb(NFO_count ~ factor(ANTIPSYCHO) + offset(log(fu)) + age.id + factor(epoc) +
                             factor(gender), # + epi_n, 
                           data = poi_data.inmeth, subset = fu > 0)

summary(nb_model.inm.adj)

# CIs
est <- cbind(Estimate = coef(nb_model.inm.adj), confint(nb_model.inm.adj))
exp(est)

# weighted
nb_model.inm.w <- glm.nb(NFO_count ~ factor(ANTIPSYCHO) + offset(log(fu)) + factor(epoc), # + epi_n ,
                         data = poi_data.inmeth, subset = fu > 0, weights = mysipws)

summary(nb_model.inm.w)

# CIs
est <- cbind(Estimate = coef(nb_model.inm.w), confint(nb_model.inm.w))
exp(est)

anova(nb_model.inm.w)
a = 1/nb_model.inm.w$theta # the dispersion parameter (a)
theta = 1/a
nb_model.inm.w$theta + c(-1, 1) * 1.96 * nb_model.inm.w$SE.theta # CI for theta = 1/a


# ==> BUPRENORPHINE

poi_data.inbup <- poi_data %>% filter(therapy == 2 & status == "in") # in buprenorphine only

poi_data.inbup <- poi_data.inbup %>% 
  dplyr::select(c(patid, ANTIPSYCHO, gender, age.id, epoc, epi_n, mysipws, diff, NFO_event))

head(poi_data.inbup)

poi_data.inbup <- poi_data.inbup %>%
  group_by(patid, ANTIPSYCHO) %>%
  mutate(
    fu = sum(diff), # fu on/off OST
    NFO_count = sum(NFO_event)
  ) %>% ungroup()

poi_data.inbup <- poi_data.inbup %>% dplyr::select(-diff, -NFO_event) %>% distinct()  


# crude rates
# on/off antipsychos
poi_data.inbup %>%
  group_by(ANTIPSYCHO) %>%
  summarise(PY = sum(fu)/365.25,
            Events = sum(NFO_count),
            RR = Events/PY * 100)

# models
poi_model.0 <- glm(NFO_count ~ factor(ANTIPSYCHO) + offset(log(fu)),
                   data = poi_data.inbup, subset = fu > 0, family = poisson)

summary(poi_model.0)

# CIs
est <- cbind(Estimate = coef(poi_model.0), confint(poi_model.0))
exp(est)

# adjusted
poi_model.inb.adj <- glm(NFO_count ~ factor(ANTIPSYCHO) + offset(log(fu)) + factor(gender),
                         #  age.id + factor(epoc) + epi_n,
                         data = poi_data.inbup, subset = fu > 0, family = poisson)

summary(poi_model.inb.adj)

# CIs
est <- cbind(Estimate = coef(poi_model.inb.adj), confint(poi_model.inb.adj))
exp(est)

# Pearson's cumulative test statistic
sum(residuals(poi_model.inb.adj, 'pearson')^2) / df.residual(poi_model.inb.adj)

# weighted
poi_model.inb.w <- glm(NFO_count ~ factor(ANTIPSYCHO) + offset(log(fu)),# + factor(epoc) + epi_n,
                       data = poi_data.inbup, subset = fu > 0, family = poisson, weights = mysipws)

summary(poi_model.inb.w)

# CIs
est <- cbind(Estimate = coef(poi_model.inb.w), confint(poi_model.inb.w))
exp(est)



# NB #-----------------

# unadjusted
nb_model.inb.unadj <- glm.nb(NFO_count ~ factor(ANTIPSYCHO) + offset(log(fu)),
                             data = poi_data.inbup, subset = fu > 0)

summary(nb_model.inb.unadj)

# CIs
est <- cbind(Estimate = coef(nb_model.inb.unadj), confint(nb_model.inb.unadj))
exp(est)

# adjusted
# Note: the rest of vars (gender; epi_n; age.id) could be excluded judging for the model's summary
nb_model.inb.adj <- glm.nb(NFO_count ~ factor(ANTIPSYCHO) + offset(log(fu)) + factor(epoc),
                            #age.id + factor(gender) + epi_n,
                           data = poi_data.inbup, subset = fu > 0)

summary(nb_model.inb.adj)

# CIs
est <- cbind(Estimate = coef(nb_model.inb.adj), confint(nb_model.inb.adj))
exp(est)

# weighted
nb_model.inb.w <- glm.nb(NFO_count ~ factor(ANTIPSYCHO) + offset(log(fu)) + factor(epoc),# + epi_n,
                         data = poi_data.inbup, subset = fu > 0, weights = mysipws)

# CIs
est <- cbind(Estimate = coef(nb_model.inb.w), confint(nb_model.inb.w))
exp(est)

summary(nb_model.inb.w)

# -------------------------- end of OST MODALITY STRATA results --------------------------------

# <|>


