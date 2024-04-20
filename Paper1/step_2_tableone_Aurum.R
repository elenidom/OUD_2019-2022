###################################################################################################
#
#                                           *
#                                       PhD PROJECT
#                                    ELENI DOMZARIDOU
#                                UNIVERSITY OF MANCHESTER
#                                        March 2021
#                                        CPRD Aurum
#                                           *
#
# Aim: to produce table one, stratified by OST therapy type (M/B/B-NLX) and present summary stats
# that describe the cohort.
#
# Variables of interest are:
# --------------------------
# * Age     : as ranges (<18; 18-24; 25-34; 35-44; 45-64; >64; missing)
# * Gender  : as male/female
# * Therapy : ____________________________
#
# Datasets in use:
# ------------------
# - my_patients        : master cohort
#
###################################################################################################

library(dplyr)
library(tableone)

# receive the first line of every patid:
todata <- my_patients #%>% filter(prereg >= 6)

todata %>% summarise(N=n_distinct(patid))

todata <- todata[!duplicated(todata$patid),]

ls(todata)

dput(names(todata))


# myvars = a vector that keeps only the variables I need to create the baseline table
myvars <- c("myage", "myage_cat", "gender", "region_text", "dead", "N_opioids_before",
            "daysin", "daysout", "switcher", "OST_N_episodes", "OST_daysin_epi_cat","pyfu")

factorVars <- "therapy"

# produce TableOne
TOoutput <- CreateTableOne(vars = myvars, strata = factorVars, data = todata)

# print it on your screen to be able to save it on the next step
TOoutput

# save the output in the current working directory, within the current working directory
capture.output(TOoutput, file = "./Aurum_tableone_output2.txt")

# clean environment
rm(TOoutput, factorVars, myvars, todata, ther_vect)
