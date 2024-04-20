#######################################################################################################
#                                           *
#                                       PhD PROJECT
#                                    ELENI DOMZARIDOU
#                                UNIVERSITY OF MANCHESTER
#                                        March 2021
#                                        CPRD Aurum
#                                           *
# 
# Aims:
# a) Transform date fields
# b) Define entry, exit, index, death (ONS) dates
# 
#######################################################################################################

library(dplyr)
library(lubridate)

load("/mnt/bmh01-rds/Ashcroft_Domzaridou_PSTRC/mbmhted6/Eleni_Aurum_JAN_2021/Env_0_initial_extr_migrators_set18.RData")

my_patients %>% summarise(Np = n_distinct(patid)) # cohort size

# variables and classes
my_patients_vars <- as.data.frame(sapply(my_patients[1:ncol(my_patients)], class)) 

# -------------------------------------------------
# NEW VARIABLE: poisoning (after ONS merging)
# -------------------------------------------------

# NAs represent non-DRDs
my_patients <- my_patients %>%
  mutate(
    poisoning = case_when(
      grepl("F11|F12|F13|F14|F15|F16|F18|F19", cause) ~ "Mental and behavioural disorders due to psychoactive substances",
      #grepl("F99", cause) ~ "Mental and behavioural disorders (Mental disorder, not otherwise specified)",
      grepl("292|304|305.2|305.3|305.4|305.5|305.6|305.7|305.8|305.9", cause) ~ "Mental and behavioural disorders due to psychoactive substances",
      grepl("X40|X41|X42|X43|X44", cause) ~ "Accidental poisoning by drugs, medicaments and biological substances",
      grepl("E850|E851|E852|E853|E854|E855|E856|E857|E858", cause) ~ "Accidental poisoning by drugs, medicaments and biological substances",
      #grepl("X49|E866.8|E866.9", cause) ~ "Accidental self-harm poisoning, other or unspecified exposure",
      #grepl("X58|X59.9", cause) ~ "Accidental self-harm (Other or unspecified means)",
      grepl("X60|X61|X62|X63|X64|E950.0|E950.1|E950.2|E950.3|E950.4|E950.5", cause) ~ "Intentional self-poisoning by drugs, medicaments and biological substances",
      #grepl("X69|E950.9", cause) ~ "Intentional self-harm poisoning, other or unspecified exposure",
      #grepl("X83|X84|E958.8|E958.9", cause) ~ "Intentional self-harm (Other or unspecified means)",
      grepl("X85|E962.0", cause) ~ "Assault by poisoning by drugs, medicaments and biological substances",
      #grepl("X90|E962.9", cause) ~ "Assault by poisoning, other or unspecified exposure",
      #grepl("Y08|Y09|E968.8|E968.9", cause) ~ "Assault by other or unspecified means",
      grepl("Y10|Y11|Y12|Y13|Y14|E980.0|E980.1|E980.2|E980.3|E980.4|E980.5", cause) ~ "Poisoning by drugs, medicaments and biological substances",
      #grepl("Y19|E980.9", cause) ~ "Self-harm by poisoning, other or unspecified exposure",
      #grepl("Y33|Y34|E988.8|E988.9", cause) ~ "Self-harm by poisoning (Other or unspecified means)",
      #grepl("R68.8|R69|R96|R97|R98|R99|798.1|798.2|798.3|798.4|798.5|798.6|798.7|798.8|798.9|
      #      799.89|799.9", cause) ~ "Ill-defined, unspecified or unknown cause",
      # T codes for external causes
      # grepl("960|961|962|963|964|965|966|967|968|969|970|971|972|973|974|975|976|977|978|979|
      #       T36|T37|T38|T39|T40|T41|T42|T43|T44|T45|T46|T47|T48|T49|T50", cause) ~ 
      #   "External cause poisoning by drugs, medicaments and biological substances",
      # grepl("989.89|989.9|T65.8|T65.9", cause) ~ "External cause poisoning, other or unspecified exposure",
      # grepl("995.89|T78.8|T78.9", cause) ~ "External cause (Other or unspecified cause)"
    )
  )


# -------------------------------
# NEW VARIABLE: poisoning_flag
# -------------------------------

my_patients <- my_patients %>% mutate(poisoning_flag = if_else(is.na(poisoning), 0, 1))



# ---------------------------------
# Merge with lookup files
# ---------------------------------

# >> Region
reg_lookup <- read.table(file = "./source_data/Lookups_2021_01_Aurum/Region.txt", header = T, sep = "\t")
names(reg_lookup) <- c("region", "region_text")
my_patients <- left_join(my_patients, reg_lookup, by = "region")

# >> Patient type
pat_lookup <- read.table(file = "./source_data/Lookups_2021_01_Aurum/PatientType.txt", header = T, sep = "\t")
names(pat_lookup) <- c("patienttypeid", "patienttype_text")
pat_lookup$patienttypeid <- as.factor(pat_lookup$patienttypeid)
my_patients <- left_join(my_patients, pat_lookup, by = "patienttypeid")

# -------------------------------------------------------------------------------------------

# <|>

# -------------------------------------------------------------------------------------------
#                                  Formulation cleaning
# -------------------------------------------------------------------------------------------

# exclude some formulations
my_patients <- my_patients %>% filter(formulation != "Transdermal patch")

my_patients %>% summarise(Np = n_distinct(patid), Nr = n())

# exclude product that are usually prescribed for pain;
# see: tephine, temgesic and buprenorphine 200/300/400 microgram
excl_drugs <- c("Tephine", "Temgesic", "Buprenorphine 200micro", 
                "Buprenorphine 300micro", "Buprenorphine 400micro", "Subutex 0.4mg",
                "Buprenorphine Injection",
                "Methadone Hydrochloride Injection 10mg/ml",
                "Methadone 10mg/ml Injection", 
                "Methadone 10mg/1ml solution for injection",
                "Methadone 20mg/ml solution for injection",
                "Methadone 20mg/2ml solution for injection ampoules",
                "Methadone 35mg/3.5ml solution for injection",
                "Methadone 50mg/5ml solution for injection ampoules",
                "Physeptone 10mg/", "Physeptone 20mg/", "Physeptone 35mg",
                "capsules", "Methadone 5mg tablets", "Physeptone 5mg tablets",
                "Methadone 2mg/5ml linctus")

my_patients$excl_drugsub <- with(my_patients, 
                                 grepl(paste(excl_drugs, collapse = "|"), termfromemis, 
                                       ignore.case = TRUE))

#table(my_patients$excl_drugsub)
my_patients <- my_patients %>% filter(excl_drugsub == FALSE)

my_patients %>% summarise(Np = n_distinct(patid), Nr = n())

# -------------------------------------------------------------------------------------------

# <|>

# -------------------------------------------------------------------------------------------
# IDEA: We calculate person-years of follow-up as the time between the first recorded 
# prescription that was written within 1998-2020 till either a) end of study; 
# b) last recorded data from GP practice (lcd); or b) CPRD GOLD death date. Note that 
# (a) and (b) are represented by the variable 'exit_date'.
# -------------------------------------------------------------------------------------------
#
#                                   *
#
# --------------------------------------------------------------------------
# NEW VARIABLES: myeventdate, mycrd, mycrd_plus1, mycrd_plus6m etc (dates)
# --------------------------------------------------------------------------
# Transform all the dates using lubridate package, taking leap years into account

# Event date
#my_patients$myeventdate <- dmy(my_patients$eventdate) # GOLD

my_patients$myeventdate <- dmy(my_patients$issuedate) # Aurum

# CPRD death date
#my_patients$mycprd_deathdate <- dmy(my_patients$deathdate) # GOLD

my_patients$mycprd_deathdate <- dmy(my_patients$cprd_ddate) # Aurum

# # Start/end of registration with GP practice relevant to CRD and Transfer-out dates in GOLD
#my_patients$mytod <- dmy(my_patients$tod) # GOLD
my_patients$mytod <- dmy(my_patients$regenddate) # Aurum


# Current registration date
# my_patients$mycrd <- dmy(my_patients$crd) # GOLD
my_patients$mycrd <- dmy(my_patients$regstartdate) # Aurum

# UTS only in GOLD
#my_patients$myuts <- dmy(my_patients$uts)
#my_patients$myuts_plus1 <- my_patients$myuts %m+% years(1)  # same as above
#my_patients$myuts_plus6m <- my_patients$myuts %m+% months(6)
#my_patients$myuts_plus3m <- my_patients$myuts %m+% months(3)
#my_patients$myuts_plus1m <- my_patients$myuts %m+% months(1)

#summary(my_patients$myuts_plus1)

# Date of the last data collection for the practice
my_patients$mylcd <- dmy(my_patients$lcd)

ls(my_patients)
#summary(my_patients)

# ---------------------------------------------------------------------------------------------------
# NEW VARIABLES: observation_period; opioid_before_studystart (2-level factor); 
#                N_opioids_before; within_obs_period; 
# ---------------------------------------------------------------------------------------------------
# >> observation_period: period that represents the study's window ;
#
# >> opioid_before_studystart: binary indicator that is 1 if a patient received opioid prescriptions
#                       before that study starts date;
#
# >> N_opioids_before: number of opioid prescriptions that were issued before the observation window
# 
# >> within_obs_period: binary indicator that is used to filter obs according to whether they belong
#                       within the observation window of interest.


# step 1: drop observations out of the observation period you examine [e.g.: 1998-2020]

# obs_period_start/end:
my_patients$obs_period_start <- ymd("1998-01-01") # start of follow-up
my_patients$obs_period_end   <- ymd("2017-12-31") #   end of follow-up

my_patients$observation_period <- my_patients$obs_period_start %--% my_patients$obs_period_end

# within_obs_period:
my_patients$within_obs_period <- with(my_patients, myeventdate %within% observation_period)

my_patients %>% count(within_obs_period) 


# --------------------------------------------------------------------------------------------
# NEW VARIABLES = exit_date, index_date (Dates); 
# --------------------------------------------------------------------------------------------
# Idea: The date of the first opioid prescription must be after the uts and the date of the 
# last opioid prescription must be before the lcd.
# 
# First, create a variable which indicates the patient's exit date (from the cohort): 
# this will be either the last opioid prescription, or the practice last data collection date 
# (lcd). We keep the minimum among those dates.
#
# The variables first take either the above calculated dates or the system date in case 
# the prescription was other than an opioid one. We then replace the system date with the 
# entry/exit dates for the whole columns.
#
# All patients have only prescriptions issued after 1998.
# Therefore, we will define the index date, as the date of the very first opioid prescription that was issued 
# per patient within the observation window. 
# Note that patients have also other medications in their records, so we first create a subset
# by filtering the data for those with opioids, in order to calculate the index date based on opioid
# prescriptions. Then, we will merge this index date with the rest cohort so as all patients will have 
# index date, which was defined based on opioid prescriptions.

# step 2: calculate index date
my_patients <- my_patients %>%
  group_by(patid) %>%
  mutate(index_date = min(myeventdate),
         index_year = year(index_date)) %>%
  ungroup()

summary(my_patients$index_date)

# step 3: calculate exit dates (last day someone appears in the cohort)
my_patients <- my_patients %>%
  group_by(patid) %>%
  mutate(
    exit_date = min(mytod, mydod, mylcd, na.rm = TRUE) # ONS date of death
  ) %>% ungroup()

summary(my_patients$exit_date)

# index date must be later than the entry date and prior to exit date:
#sum(my_patients$index_date >= my_patients$entry_date) # ok
sum(my_patients$index_date <= my_patients$exit_date) # clean
sum(my_patients$index_date == my_patients$exit_date) # patients without follow up 

# FILTERING!!!!
my_patients <- my_patients %>% filter(index_date < exit_date) # at least 1 day of follow-up
sum(my_patients$index_date == my_patients$exit_date) # ok

my_patients %>% summarise(Np = n_distinct(patid), Nr = n())

# current registration and up-to-standard dates must be earlier than index_date:
#my_patients <- my_patients %>% filter(mycrd < index_date & myuts < index_date)# GOLD
my_patients <- my_patients %>% filter(mycrd < index_date)


# ...and lcd must be later than the exit date
sum(my_patients$mylcd >= my_patients$exit_date)

# step 4: Registration with a GP for at least 6 months

# --------------------------------------
# NEW VARIABLE: prereg (numeric; months)
# --------------------------------------

# Check the difference between the patients' current registration date and index date
# to assess whether many patients had a pre-registration date
# for more than a year or 6 months:

# GOLD:
# my_patients %<>%
#   group_by(patid) %>%
#   mutate(
#     prereg = round(as.numeric(time_length(index_date - max(mycrd, myuts), unit = "months")), digits = 0)
#   ) %>%
#   ungroup()

# Aurum
my_patients %<>%
  group_by(patid) %>%
  mutate(
    prereg = round(as.numeric(time_length(index_date - mycrd, unit = "months")), digits = 0)
  ) %>%
  ungroup()

# count patient who were enrolled with a GP for at least 6 months at the index date
my_patients %>% group_by(prereg >= 6) %>% summarise(Np = n_distinct(patid))

summary(my_patients$prereg)
boxplot(my_patients$prereg)
hist(my_patients$prereg)

prereg_input <- 6 # <--- user input

# DO NOT RUN!!!
# by definition, this period has to be at least 6 months (this is a matter of sensitivity analysis)
# my_patients <- my_patients %>% filter(prereg >= prereg_input)

# cut the prereg time into intervals:
# summary(my_patients$prereg)
# cut_off_prereg <- c(-1, 1, 3, 6, 9,12,240,325)
# my_patients$prereg_cat <- cut(my_patients$prereg, cut_off_prereg)
# head(my_patients$prereg_cat)

# ..............................
#     Visualise with ggplot
# ..............................

# library(ggplot2)
# 
# my_patients %>%
#   select(c("patid", "prereg", "prereg_cat")) %>%
#   distinct() %>%
#   group_by(prereg_cat) %>% 
#   summarise(Np = n()) %>%
#   ggplot(aes(x = prereg_cat, y = Np, fill = prereg_cat, alpha = 0.2)) +
#   geom_bar(stat = "identity", show.legend = FALSE, width = 0.7) +
#   geom_text(aes(label = Np, size=3), show.legend=FALSE)+
#   ylim(0,10000) +
#   scale_x_discrete(labels = df_labels <- c("[0,1]", "(1,3]", "(3,6]", "(6,9]", "(9,12]", "(12,24]", "(>24)")) +
#   labs(title = "Pre-registration period (months) x number of patients", 
#        x = "months", y = "Frequency") +
#   theme(axis.text.x = element_text(size=12), axis.title=element_text(size=14))

# ..............................
#     end of visualisation
# ..............................

# FILTERING!!!
my_patients <- my_patients %>% filter(within_obs_period == TRUE)

my_patients %>% summarise(n_distinct(patid))
#summary(my_patients)

# if exit date is e.g. 2020-12-03, then assing 2017-12-31 as a new exit date
my_patients <- my_patients %>% mutate(
  exit_date = if_else(exit_date >= obs_period_end, obs_period_end, exit_date) 
) 

summary(my_patients$myeventdate)
summary(my_patients$exit_date)
summary(my_patients$index_date)

# Keep only OST prescriptions within the observation window
with(my_patients, sum(myeventdate >= index_date & myeventdate <= exit_date))
my_patients <- my_patients %>% filter(obs_period_start <= index_date & obs_period_end >= exit_date)
my_patients <- my_patients %>% filter(myeventdate <= exit_date)
my_patients %>% summarise(Np = n_distinct(patid))

# ------------------------------------------------------------------------------------------------

# <|>

# ----------------------------------------------------------------------------------------------
#                 Extra cleaning / new variables to define a time-dependent cohort
# ----------------------------------------------------------------------------------------------

summary(my_patients)
ls(my_patients)


# ------------------------------------------------------------------------------------------------
# NEW VARIABLES: myage; myage_cat
# ------------------------------------------------------------------------------------------------
# ->> myage (numeric; age at index date)
# ->> myage_cat (factor of 4 levels)

my_patients %<>%
  group_by(patid) %>%
  mutate(
    myage = year(index_date) - yob
  ) %>%
  ungroup()

# define age groups (ranges according to Pierce et al., 2015):
summary(my_patients$myage)

hist(my_patients$myage, breaks = 200)

my_patients$myage_cat <- cut(my_patients$myage, breaks = c(17, 24, 34, 44, 64))

my_patients %>%
  group_by(myage_cat) %>%
  summarise(Np = n_distinct(patid)) # Age_cat = NA when 18 < Age or Age > 65

# ATTENTION!!! - FILTERING
my_patients <- my_patients %>% filter(!is.na(myage_cat))
my_patients %>% summarise(Np = n_distinct(patid))

require(lattice)
histogram(~myage|gender, data = my_patients, breaks = 50, col = "lightblue") # layout = c(1,2)

# ------------------------------------------------------------------------------------------------

# <|>

# ------------------------------------------------------------------------------------------------
# NEW VARIABLE: dead (Binary indicator)
# ------------------------------------------------------------------------------------------------
# Dead = 0 -> alive patient
# Dead = 1 -> dead patient
# Note: We use the ONS date of death

# count NA values as these will be an indicator of alive patients
my_patients <- my_patients %>% 
  group_by(patid) %>%
  mutate(dead = if_else(is.na(mydod), 0, 1),
         dead = if_else(dead == 1 & mydod <= obs_period_end, 1, 0)) %>%
  ungroup()

my_patients$dead <- as.factor(my_patients$dead)

my_patients %>% group_by(dead) %>% summarise(Nr = n(), Np = n_distinct(patid)) 

# ------------------------------------------------------------------------------------------------

# <|>

# ------------------------------------------------------------------------------------------------
# NEW VARIABLE: age_at_death (numeric)
# ------------------------------------------------------------------------------------------------
# NA  = alive at end of obs period
# Number = patient's age at death date
# Note: We use the ONS date of death

my_patients <- my_patients %>% 
  group_by(patid) %>%
  mutate(age_at_death = ifelse((is.na(mydod)), NA, year(mydod) - yob)) %>%
  ungroup()

# SUMMARIES
# ...............
# all deaths
summary(my_patients$age_at_death)
hist(my_patients$age_at_death[!is.na(my_patients$age_at_death)],
     breaks = 50,
     xlim = c(10, 80),
     main = "Age at death date (all-cause mortality)",
     xlab = "Age (years)")

# ------------------------------------------------------------------------------------------------

# <|>

# ------------------------------------------------------------------------------------------------
# NEW VARIABLE: poisoning_flag (Binary indicator)
# ------------------------------------------------------------------------------------------------
# Redefine the poisoning flag according to the observation period of the study. The death date
# cannot exceed the study window.
#
# 0 -> non-DRD or no death
# 1 -> DRD

my_patients <- my_patients %>% 
  group_by(patid) %>%
  mutate(poisoning_flag = if_else(poisoning_flag == 1 & mydod <= obs_period_end, 1, 0)) %>%
  ungroup()

my_patients %>% group_by(poisoning_flag) %>% summarise(Nr = n(), Np = n_distinct(patid)) 

# ------------------------------------------------------------------------------------------------

# <|>

# ------------------------------------------------------------------------------------------------
# NEW VARIABLES: prac_pop; region_pop (numeric)
# ------------------------------------------------------------------------------------------------
# Population in OST per practice id and per region

my_patients %<>%
  group_by(pracid) %>%
  mutate(prac_pop = sum(n_distinct(patid))) %>%
  ungroup()

head(my_patients$prac_pop)
temp_prac <- my_patients %>% select(c("pracid", "region_text", "prac_pop")) %>% distinct()

# patients per practice simple stats:
prac_pop.df <- my_patients %>% 
  group_by(pracid) %>% 
  select(c("pracid", "prac_pop")) %>% 
  distinct()

boxplot(prac_pop.df$prac_pop)
summary(prac_pop.df)

# Region
my_patients %<>%
  group_by(region) %>%
  mutate(region_pop = sum(n_distinct(patid))) %>% 
  ungroup()

temp_reg <- my_patients %>% select(c("region_text", "region_pop")) %>% distinct()

# ------------------------------------------------------------------------------------------------

# <|>

# -------------------------------------------
# NEW VARIABLE: OST_dupli
# -------------------------------------------
# Identify prescriptions that have been issued at the same date, by examining
# if the prescription date of the current OST prescri is same as the next prescription date

my_patients <- my_patients %>%
  arrange(patid, myeventdate) %>%
  group_by(patid) %>%
  mutate( # the dataset is arranged by patid and event date 
    OST_dupli = if_else(myeventdate == lead(myeventdate, default = last(myeventdate)), 1, 0),
    OST_dupli = if_else(row_number() == n(), 0, OST_dupli)) %>% # the last event per patid is not duplicated since there are
  ungroup()                                                  # no more events to compare and this is the last prescription


# ------------------------------------------------------------------------------------------------
# NEW VARIABLE: expected (date)
# ------------------------------------------------------------------------------------------------
# Expected completion date of OST prescription
# Note that the date of the expected completion must be earlier than exit date; this
# needs to be double-checked with HES data.

numdays <- 14 # USER INPUT: provide number of days that indicate treatment duration

my_patients <- my_patients %>%
  group_by(patid) %>%
  mutate(
    expected = myeventdate %m+% days(numdays), # the expected completion date is equal to the date of prescription plus the treatment duration
    expected = if_else(lag(OST_dupli, default = first(OST_dupli)) == 1 & row_number() > 1, # if the are duplicates and we are not at the first line...
                       expected %m+% days(numdays), # ...then, extend the treated period by numdays
                       expected),
    expected = if_else(expected > exit_date, exit_date, expected) 
  ) %>%
  ungroup()

summary(my_patients$expected)
head(my_patients$myeventdate)
head(my_patients$expected)

# ------------------------------------------------------------------------------------------------

# <|>

# ------------------------------------------------------------------------------------------------


# --------------------------------------
# NEW VARIABLE: OST_gap (numeric, days)
# --------------------------------------
# Difference bewteen the next OST prescription and the last expected completion date

my_patients %<>%
  group_by(patid) %>%
  arrange(patid, myeventdate) %>% # sort event dates within each patient's records
  mutate(
    OST_gap = time_length(lead(myeventdate, default = last(myeventdate)) - expected, unit = "days"),
    OST_gap = if_else(myeventdate == last(myeventdate), 0, OST_gap),
    OST_gap = as.numeric(OST_gap)
  ) %>%
  ungroup()

head(my_patients$OST_gap)
summary(my_patients$OST_gap)
boxplot(my_patients$OST_gap, main = "OST_gap")


# -------------------------------------------
# NEW VARIABLE: OST_temp_daysin; OST_gap2
# -------------------------------------------
# >> OST_temp_daysin: The days that someone continuously receives OST (with gaps <= 14 days).
#                       If there are overlaps between prescriptions, calculate daysin as days 
#                       between recurrent prescription dates (see notes for figure). The variable
#                       refers to all the prescriptions that patients have been issued and not only 
#                       prescriptions of a certain OST episode.
# 
# >> OST_gap2       : Likewise with OST_gap, with the difference that this var keeps only
#                       positive number of days, replacing negative numbers by zero 

my_patients <- my_patients %>%
  group_by(patid) %>%
  arrange(patid, myeventdate) %>%
  mutate(
    # normal
    OST_temp_daysin = time_length(expected - myeventdate, unit = "days"),
    
    # nested
    OST_temp_daysin = if_else(row_number() > 1 & lag(expected, default = first(expected)) >= expected,
                              0, OST_temp_daysin),
    
    # extend the 'in-treatment' period if prescription dates differ less than numdays (OST_gap2 < 14)
    OST_temp_daysin = if_else(lag(OST_gap, default = first(OST_gap)) < numdays & lag(OST_gap, default = first(OST_gap)) > 0,
                              time_length(expected - lag(expected, default = first(expected)), unit = "days"), OST_temp_daysin),
    
    # overlapping
    OST_temp_daysin = if_else(expected > lag(expected, default = first(expected)) & myeventdate < lag(expected, default = first(expected)),
                              time_length(expected - lag(expected, default = first(expected)), unit = "days"),
                              OST_temp_daysin),
    
    # duplicated
    OST_temp_daysin = if_else(lag(OST_dupli, default = first(OST_dupli)) == 1,
                              time_length(expected - lag(expected, default = first(expected)), unit = "days"),
                              OST_temp_daysin),
    
    OST_gap2 = if_else(OST_gap <= numdays, 0, OST_gap) # Re-define OST_gap
  ) %>%
  ungroup()

summary(my_patients$OST_gap2)
summary(my_patients$OST_temp_daysin)

# --------------------------------------
# NEW VARIABLE: epi_change (factor)
# --------------------------------------
# epi_change = 1: continuous treatment episode
# epi_change = 0: time-point that indicates transion of treatment status (from 'in' to 'out')

my_patients %<>%
  group_by(patid) %>%
  mutate(epi_change = as.factor(if_else(lag(OST_gap, default = first(OST_gap)) <= numdays, 1, 0))) %>%
  ungroup()


# --------------------------------------------------------------------------------------------
# NEW VARIABLES: OST_episode (numeric); OST_N_episodes 
# --------------------------------------------------------------------------------------------
# >> OST_episode : OST episode order; diff(x) returns a vector of length: (length(x)-1)
#                   which contains the difference between one element
#                   and the next in a vector x, while cumsum(x) returns a vector of 
#                   length equal to the length of x containing the sum of the elements in x.
#
# >> OST_N_episodes: Number of distinct OST episodes (in treatment).
#
# >> OST_temp_daysin (UPDATE): impute last prescription's duration by adding numdays 
#                 as an artificial duration period

my_patients %<>%
  group_by(patid) %>%
  mutate(
    OST_episode = cumsum(c(0, diff(epi_change == 0)) < 0) + 1L, # inverse cumsum: increase the OST_episode whenever the epi_change flag turns from 1 to 0
    OST_N_episodes = max(OST_episode)
  ) %>% ungroup()


# impute last prescription's duration by adding numdays as an artificial duration period
# my_patients %<>%
#   group_by(patid, OST_episode) %>%
#   mutate(
#     OST_temp_daysin = if_else(myeventdate == last(myeventdate), numdays, OST_temp_daysin),
#     OST_temp_daysin = if_else(OST_dupli == 1, 
#                               lead(OST_temp_daysin, default = last(OST_temp_daysin)), 
#                               OST_temp_daysin)
#   ) %>% ungroup()

summary(my_patients$OST_temp_daysin)

my_patients %>% group_by(OST_episode) %>% summarise(Np = n_distinct(patid))

# --------------------------------------------------------------------------
# NEW VARIABLE: OST_daysin_epi (numeric, days)
# --------------------------------------------------------------------------
# In contrast with the var 'temp_daysin', this var calculates the days that someone
# remained in treatment within a time-length of an episode.
#
# The variable OST_daysin_epi_cat represents levels of exposure of the variable
# OST_daysin_epi and is the time-span that we will use to build the models.

# OST_daysin_epi
my_patients %<>%
  group_by(patid, OST_episode) %>%
  mutate(
    OST_daysin_epi = cumsum(OST_temp_daysin), # OST episode duration
    OST_daysin_epi = max(OST_daysin_epi)
  ) %>% ungroup()

summary(my_patients$OST_daysin_epi)
boxplot(my_patients$OST_daysin_epi)

# -----------------------------------------
# NEW VARIABLE: OST_epi_daysin_cat (factor)
# -----------------------------------------

# Stratify the days in each treatment episode to have a sense of treatment duration pre episode:

my_patients %<>%
  group_by(patid) %>%
  mutate(
    OST_daysin_epi_cat = cut(OST_daysin_epi, breaks = c(-1, 30, 90, 180, 365, 6565))
  ) %>%
  ungroup()

summary(my_patients$OST_daysin_epi_cat) # NAs correspond to out-of-treament periods

All_pats <- as.numeric(my_patients %>% summarise(a = n_distinct(patid))) #  number of all patients

# rename labels
my_patients %<>%
  group_by(patid) %>%
  mutate(
    OST_daysin_epi_cat = case_when(
      OST_daysin_epi_cat == "(-1,30]" ~ "Up to 1 month",
      OST_daysin_epi_cat == "(30,90]" ~ "1 - < 3 months",
      OST_daysin_epi_cat == "(90,180]" ~ "3 - < 6 months",
      OST_daysin_epi_cat == "(180,365]" ~ "6 - < 12 months",
      OST_daysin_epi_cat == "(365,6.56e+03]" ~ "12 months or longer"
    )) %>%
  ungroup()

my_patients$OST_daysin_epi_cat <- as.factor(my_patients$OST_daysin_epi_cat)

summary(my_patients$OST_daysin_epi_cat)

# ------------------------------------------------------------------------------------------------

# <|>

# ------------------------------------------------------------------------------------------------
# NEW VARIABLE: presc_exit_days (numeric; days)
# ------------------------------------------------------------------------------------------------
# Days between last prescription expected completion date and exit date.
# We must take into account late registration of death.

my_patients %<>% 
  group_by(patid) %>%
  mutate(
    presc_exit_days = as.numeric(time_length(exit_date - last(expected), unit = "days"))
  ) %>%
  ungroup()

summary(my_patients$presc_exit_days)
boxplot(my_patients$presc_exit_days)
head(my_patients$presc_exit_days)
head(my_patients$exit_date)

# ------------------------------------------------------------------------------------------------

# <|>

# ------------------------------------------------------------------------------------------------
# NEW VARIABLES: cumdaysin/out; daysin/out; OST_daysout_epi (numeric)
# ------------------------------------------------------------------------------------------------
# All the variables refer to each patient and neigther each row of data, nor each OST treatment episode.
# 
# >>       cumdaysin:  cumulative days in treatment represents the number of days that the patient was
#                       receiving therapy throughout the follow-up time, given that the difference
#                       between recurring OST prescriptions were less than the variable 'epi_change' 
#                       indicates;
# >>      cumdaysout:  cumulative days out of treatment;
# >>          daysin:  person-days in treatment is the maximum number of the cumulative sum of days in 
#                       treatment (because the maximum has the result of the sum of days in treatment);
# >>         daysout:  person-days out of treatment for each patient;
# >> OST_daysout_epi:  person-days that patients remained untreated after a treatment episode.

my_patients %<>%
  group_by(patid) %>%
  mutate(
     cumdaysin = cumsum(OST_temp_daysin),
        daysin = max(cumdaysin)
  ) %>%
  ungroup()

my_patients %<>%
  group_by(patid) %>% # numdays = provided by the user; represents the number of days 
  mutate(             # that each OST prescription lasts (e.g. 7; 14; 28 etc)
       daysout = if_else(OST_gap > numdays, OST_gap, 0), # if the difference is less than numdays, it implies that 
    cumdaysout = if_else(row_number() == n(), cumsum(daysout) + max(presc_exit_days), cumsum(daysout)), # the patient is still receiving OST therapy
       daysout = max(cumdaysout)) %>% # plus days until exit date (that is out of tretment)
  ungroup()


# ......................................................................
#   Visualise the distribution of days in/out of treatment with ggplot
# ......................................................................

require(ggplot2)

summary(my_patients$daysin)

# Days in treatment
my_patients %>%
  select(c("patid", "daysin")) %>% 
  distinct() %>%
  ggplot(aes(daysin)) +
  geom_histogram(aes(y = ..density..), 
                 breaks = seq(0, 7735, by = 10), # <-- adjust acoordingly
                 fill = "blue",
                 alpha = .2) +
  geom_density(col = 2, fill = "red", alpha = .2) +
  labs(title = "Person-days in treatment", x = "days", y = "density")

  
# Days out of treatment
summary(my_patients$daysout)
boxplot(my_patients$daysout)

#my_patients <- my_patients %>% filter(daysout < 538101)

my_patients %>%
  select(c("patid", "daysout")) %>% 
  distinct() %>%
  ggplot(aes(daysout)) +
  geom_histogram(aes(y = ..density..), 
                 breaks = seq(0, 6864, by = 10), 
                 fill = "blue",
                 alpha = .2) +
  geom_density(col = 2, fill = "red", alpha = .2) +
  labs(title = "Person-days out of treatment", x = "days", y = "density")

# Days in vs days out
values <- c("daysin", "daysout")

temp_long <- my_patients %>% 
  select(c("patid", "daysin", "daysout")) %>%
  distinct() %>%
  tidyr::gather(key = "patid", value = values) %>% # re-shape the dataframe in a long format
  ggplot(aes(x = patid, y = values, fill = patid)) +
  geom_boxplot(show.legend = FALSE, outlier.alpha = 0.01, alpha = 0.2) +
  geom_jitter(alpha = 0.1, color = "mediumorchid1") +
  scale_x_discrete(labels = c("in", "out")) +
  theme(axis.text = element_text(size = 12), axis.title = element_text(size = 14))

temp_long + labs(
  title = "Days in/out of treatment",
  x = "treatment stage",
  y = "days"
)

# ..............................
# end of ggplot
# ..............................

# -------------------------
# NEW VARIABLE: daysin_cat
# -------------------------

# Stratify the days in treatment to have a sense of treatment duration:

summary(my_patients$daysin)

boxplot(my_patients$daysin)

my_patients %<>%
  group_by(patid) %>%
  mutate(
    daysin_cat = cut(daysin, breaks = c(1, 30, 90, 180, 365, 7721)) 
  ) %>%
  ungroup()

summary(my_patients$daysin_cat) # NAs correspond to out-of-treament periods

# summarise results
my_patients %>%
  select(c("patid", "daysin", "daysin_cat")) %>%
  filter(!is.na(daysin_cat)) %>%
  group_by(daysin_cat) %>%
  distinct() %>%
  summarise(Np = n_distinct(patid), Pers = paste0(round(Np/All_pats*100, 1), "%"))


# rename labels
my_patients %<>%
  group_by(patid) %>%
  mutate(
    daysin_cat = case_when(
      daysin_cat == "(1,30]" ~ "Up to 1 month",
      daysin_cat == "(30,90]" ~ "1 - < 3 months",
      daysin_cat == "(90,180]" ~ "3 - < 6 months",
      daysin_cat == "(180,365]" ~ "6 - < 12 months",
      daysin_cat == "(365,7.72e+03]" ~ "12 months or longer"
    )) %>%
  ungroup()

my_patients$daysin_cat <- as.factor(my_patients$daysin_cat)

summary(my_patients$daysin_cat)


# ------------------------------------------------------
# NEW VARIABLE: OST_daysout_epi (numeric, days)
# ------------------------------------------------------
# This var calculates the days that someone remained untreated within an episode.

my_patients %<>%
  group_by(patid, OST_episode) %>%
  mutate(
    OST_daysout_epi = sum(OST_gap2)
  ) %>% ungroup()

summary(my_patients$OST_daysout_epi)
boxplot(my_patients$OST_daysout_epi)

# and redefine the same var:
my_patients <- my_patients %>%
  group_by(patid) %>%
  mutate(
    OST_daysout_epi = if_else(OST_episode == max(OST_episode), presc_exit_days, OST_daysout_epi)
  ) %>% ungroup()


# examine the time-related vars:
a <- my_patients %>%
  arrange(patid, myeventdate, OST_episode) %>%
  select(c("patid", "myeventdate", "expected", "exit_date", "presc_exit_days", 
           "OST_temp_daysin", "cumdaysin", "daysin", "OST_gap", "OST_gap2", "cumdaysout", "daysout", 
           "OST_dupli"))

View(a[1:25,])

# ------------------------------------------------------------------------------------------------

# <|>

# ------------------------------------------------------------------------------------------------
# NEW VARIABLE: pyfu = person-years of follow-up (numeric)
# ------------------------------------------------------------------------------------------------
# Calculate pyfu by substracting the index from exit date.
# The 'time_length' function is a built-in function of the package 'lubridate'. 
# It enables you to transform the output of the 
# subtraction (which was 'difftime') into years, weeks etc.
# Keep 1 decimal place.

my_patients %<>% 
  group_by(patid) %>%
  mutate(pyfu = time_length(exit_date - index_date, "days")) %>%
  ungroup()

# Some patients have longer follow up time than the observation period because they may have 
# recorded death date after 2020. Also, you probably need to censor some months after the end 
# of the observation period.

boxplot(my_patients$pyfu)
summary(my_patients$pyfu)

my_patients %>% 
  select(c("patid", "pyfu")) %>% 
  distinct() %>% 
  summarise(pyfu_sum = sum(pyfu))


# ..............................
#     Visualise with ggplot
# ..............................

require(ggplot2)

my_patients %>%
  select(c("patid", "pyfu")) %>%
  distinct() %>%
  ggplot(aes(pyfu)) +
  geom_histogram(aes(y = ..density..), 
                 breaks = seq(0, 22, by = 0.1), 
                 fill = "blue",
                 alpha = .2) +
  geom_density(col = 2, fill = "red", alpha = .2) +
  labs(title = "Person-years of follow up", 
       x = "years", y = "density")

summary(round(my_patients$pyfu*365),1) # summary of pyfu in days

boxplot(my_patients$pyfu, main = "Pyfu in GOLD", xlab = "years", horizontal = T)


# ..............................
# end of ggplot
# ..............................

require(lattice)
histogram(~pyfu|gender,  data = my_patients, col = "lightblue") # layout = c(1,2)
histogram(~pyfu|myage_cat, data = my_patients, col = "lightblue") 

# registration status:
# 0 - continuous registration
# histogram(~pyfu|regstat, data = my_patients, col = "lightblue") 


# ------------------------------------------------------------------------------------------------

# <|>

# ---------------------------------------------------------------------------
# NEW VARIABLE: myeventdiff (numeric; days)
# ---------------------------------------------------------------------------
# Difference (in days) between the first and the next issued prescription.
# This gives justification as to why we have picked 14 days as a period
# within someone is supposed to receive OST treatment + the fact that in 
# England and Wales, prescriptions of controlled drugs cannot exceed a 14-day
# interval (see here:)

my_patients %<>%
  group_by(patid) %>%
  mutate(
    myeventdiff = time_length(lead(myeventdate, default = last(myeventdate)) - myeventdate, unit = "days")
  ) %>%
  ungroup()

summary(my_patients$myeventdiff) # this looks extreme due to in/out of treatment periods

# ------------------------------------------------------------------------------------------------

# <|>

# ------------------------------------------------------------------------------------------------
# NEW VARIABLE: therapy (2-level factor)
# ------------------------------------------------------------------------------------------------
# Records whereby patients received methadone, buprenorphine, B/NLX
# NA: any other drugs except M/B/B-NLX
#  1: methadone
#  2: buprenorphine or B/NLX or NLX/B

table(my_patients$drugsubstancename)

# examine what are the codes that miss drugsubstance name:
temp2 <- my_patients %>% 
  filter(drugsubstancename == "") %>%
  group_by(termfromemis) %>%
  summarise(N = n())

meth.df <- c("Methadone Hydrochloride Diluent Oral Solution Sugar Free",
             "Methadone Hydrochloride Oral Solution, Sugar Free, Tartrazine Free 1 mg/1 ml")

my_patients <- my_patients %>%
  group_by(patid) %>%
  mutate(
    therapy = case_when(
      drugsubstancename == "Methadone hydrochloride" ~ 1,
      drugsubstancename == "" & termfromemis %in% meth.df ~ 1,
      TRUE ~ 2) # in any other case assign 2
  )%>%
  ungroup()

my_patients %>% group_by(therapy) %>% summarise(Np = n_distinct(patid), Nr = n())
my_patients %>% group_by(drugsubstancename) %>% summarise(Np = n()) # compare with above results

# ------------------------------------------------------------------------------------------------

# <|>

# ------------------------------------------------------------------------------------------------
# NEW VARIABLE: switcher (factor)
# ------------------------------------------------------------------------------------------------
# Refers to patid
# >> 0 : no switch
# >> 1 : medication switch

my_patients %<>% 
  group_by(patid) %>%
  mutate(switcher = as.factor(if_else(min(therapy) != max(therapy), 1, 0))) %>%
  ungroup()

my_patients %>% group_by(switcher) %>% summarise(Np = n_distinct(patid))

# switchers within the same treatment episode
my_patients %<>% 
  group_by(patid, OST_episode) %>%
  mutate(switcher_epi = as.factor(if_else(min(therapy) != max(therapy), 1, 0))) %>%
  ungroup()

my_patients %>% group_by(switcher_epi) %>% summarise(Np = n_distinct(patid))


# Calculate the person-years 'in' and 'out' of treatment of the cohort: 
my_patients %>% 
#  filter(switcher == 0) %>%
  select(c("patid", "daysin", "daysout", "pyfu")) %>% 
  distinct() %>% 
  summarise(pyfu_in  = sum(daysin)/365.25,
            pyfu_out = sum(daysout)/365.25,
            pyfu_in_out = sum(pyfu_in + pyfu_out),
            pyfu_all = sum(pyfu)/365.26)

# .......................
# visualise with ggplot
# .......................

library(ggplot2)

Np <- c(4873, 1535, 1232)         # <- update
percentage <- c(63.8, 20.1, 16.1) # <-- update
therapy <- c("Methadone", "Buprenorphine", "Both")
mydata <- data.frame(therapy, Np, percentage)

mydata %>%
  ggplot(aes(x = "", y = percentage, fill = therapy)) +
  geom_bar(stat = "identity") +
  scale_fill_manual(values=c("darkolivegreen3", "darkorchid1", "darksalmon")) +
  coord_polar("y", start = 0) +
  geom_text(aes(y = percentage),
            label = paste0(percentage, sep = "%"),
            size = 5,
            position = position_stack(vjust = .5))

# ..............................
# end of ggplot
# ..............................

# ------------------------------------------------------------------------------------------------

# <|>

# ------------------------------------------------------------------------------------------------
# NEW VARIABLES: period_out (time interval); period_out_start; period_out_end (Dates)
# ------------------------------------------------------------------------------------------------
# The variable refers to patid and it is the interval between the expected completion date of an
# issued prescription and the next issued prescription. The main usage of this variable is the
# identification of OST prescription gaps whereby the patients had an inpatient hospitalisation.
# Note that if the OST_gap2 > 0, it means that the patient has a therapy gap that is longer than
# the definition of the variable 'numdays'.
# 
# Values:
# >> NA: patients without OST therapy gaps
# >> Time-period: opposite
# >> 1970-01-01: missing value

# my_patients %<>%
#   group_by(patid) %>%
#   mutate(
#     period_out       = if_else(OST_gap2 > 0, expected %--% lead(myeventdate, default = last(myeventdate)), 0),
#     period_out_start = if_else(OST_gap2 > 0, expected, as.Date("1970-01-01", format = "%Y-%m-%d")),
#     period_out_end   = if_else(OST_gap2 > 0, lead(myeventdate, default = last(myeventdate)), as.Date("1970-01-01", format = "%Y-%m-%d"))
#   ) %>%
#   ungroup()

# summary(my_patients$period_out_start)
# summary(my_patients$period_out_end)

# ------------------------------------------------------------------------------------------------

# <|>

# ------------------------------------------------------------------------------------------------

# Examine drug substance and product codes summaries
# ---------------------------------------------------

ds.form.str.df <- my_patients %>% group_by(drugsubstancename, formulation, termfromemis) %>% 
  summarise(Np = n_distinct(patid))

write.csv(ds.form.str.df, "formulation.summary.aurum.csv")

#

my_patients %>% group_by(drugsubstancename) %>% summarise(Np = n_distinct(patid))

productname.df <- my_patients %>% group_by(termfromemis) %>% summarise(Np = n_distinct(patid))

write.csv(productname.df, "productname.summary.aurum.csv")

# ------------------------------------------------------------------------------------------------

# <|>

# ------------------------------------------------------------------------------------------------

# variables and classes
my_patients_vars <- as.data.frame(sapply(my_patients[1:ncol(my_patients)], class)) 

# remove vars you do not need
rm("All_pats",  "prac_pop.df", "prereg_input", candidates_JAN2021, excl_drugs, numdays,
   "temp_long", "temp_prac", "temp_reg", "ds.form.str.df", "productname.df",
   pat_lookup, reg_lookup, temp2, meth.df, Np, percentage, therapy, values, a, mydata)

# ------------------------------------------------------------------------------------------------

# <|>



