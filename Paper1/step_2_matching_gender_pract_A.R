#######################################################################################################
#                                           *
#                                       PhD PROJECT
#                                    ELENI DOMZARIDOU
#                                UNIVERSITY OF MANCHESTER
#                                       March  2021
#                                        CPRD GOLD
#                                           *
# 
# Aims:
# a) s
# b) c
# 
#######################################################################################################

library(dplyr)
library(lubridate)



# ------------------------------------------------------------------------------------
#                                 Matching Strategies
# ------------------------------------------------------------------------------------

# -----
# | A |
# -----

# Partition according to gender:

# Male cohort
my_pat_male <- my_patients %>% 
  filter(mygender == "Male") %>%
  select(c("patid", "mygender", "pracid")) %>%
  distinct()

# Female cohort
my_pat_fem <- my_patients %>% 
  filter(mygender == "Female") %>%
  select(c("patid", "mygender", "pracid")) %>% 
  distinct()

# Identify practice id with the largest numerical value:
class(my_patients$pracid) # string
max_pracid <- max(as.numeric(my_patients$pracid))


# Run the mathing on age in the partitioned datasets (namely male and female cohorts):
set.seed(6387526)

# ==========================================================================================

# -----
# | B | --> exact match:
# -----

table(my_patients$therapy)

library(MatchIt)

# exact match according to practice id, age and gender.
# exclude patids that switched medications.

my_patients_tomatch <- my_patients %>%
  filter(switcher == 0) %>%
  select(c("patid", "myage", "pracid", "gender", "therapy")) %>%
  distinct()

my_patients_tomatch$therapy <- if_else(my_patients_tomatch$therapy == 1, 0, 1)

table(my_patients_tomatch$gender, my_patients_tomatch$therapy)

# examine densities of age before matching:
plot(density(my_patients_tomatch$myage[my_patients_tomatch$therapy == 1]), 
     main = "Age density before matching",
     lty = 1)
lines(density(my_patients_tomatch$myage[my_patients_tomatch$therapy == 2]), lty = 2)
legend('topright', c("Methadone", "Buprenorphine"),  lty = c(1,2))

# match
m.exact <- matchit(therapy ~ myage + gender + as.factor(pracid), 
                   data = my_patients_tomatch, method = "exact")

summary(m.exact)

m.data_exact <- match.data(m.exact, distance = "distance")

# examine densities of age after matching:
plot(density(m.data_exact$myage[m.data_exact$therapy == 0]), main = "Age density after matching")
lines(density(m.data_exact$myage[m.data_exact$therapy == 1]), lty = 2)
legend('topright', c("Methadone", "Buprenorphine"),  lty = c(1,2))

# examine densities of therapy after matching:
plot(density(m.data_exact$therapy[m.data_exact$therapy == 0]), main = "Estimated PS after matching")
lines(density(m.data_exact$therapy[m.data_exact$therapy == 1]), col = "red", lty = 2)
legend('topright', c("Methadone", "Buprenorphine"),  lty = c(1,2))

table(m.data_exact$gender, m.data_exact$therapy)

# ----
# |C |
# ----

# PS matching:
# -----------------------------

my_patients_tomatch$therapy <- if_else(my_patients_tomatch$therapy == 1, 0, 1)

# glm
library(MASS)
glm.m <- glm(therapy ~ myage + gender + as.factor(pracid), 
             family = binomial, data = my_patients_tomatch)
plot(glm.m, which = 1:4)
# summary(glm.m)

glm.ps <- data.frame(my_patients_tomatch, ps = glm.m$fitted)

plot(density(subset(glm.ps, therapy == 0)[, 'ps']), main = "Estimated PS before matching")
lines(density(subset(glm.ps, therapy == 1)[, 'ps']), lty = 2)
legend('topright', c("Methadone", "Buprenorphine"),  lty = c(1,2))

# PS matching
m.ps <- matchit(therapy ~ myage + gender + as.factor(pracid),
                data = my_patients_tomatch, method = 'nearest')

summary(m.ps)

m.data_ps <- match.data(m.ps, distance = 'pscore')
dim(m.data_ps)

temp_a <- m.data_ps %>% filter(therapy == 0)
temp_b <- m.data_ps %>% filter(therapy == 1)

plot(density(temp_a$pscore), main = "Estimated PS after matching")
lines(density(temp_b$pscore), lty = 2)
legend('topright', c("Methadone", "Buprenorphine"), lty = c(1,2))

table(m.data_ps$gender, m.data_ps$therapy)


