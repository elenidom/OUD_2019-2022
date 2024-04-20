
# --------------------------------------------------------------------------------------
# >>> Time-period stratification
# --------------------------------------------------------------------------------------

                                      # ---------------------------------------
master.ddi.data <- master.ddi.bnz_all # <<-- USER INPUT: provide dataset name
                                      # ---------------------------------------

master.time_period <- master.ddi.data %>% 
  dplyr::select(-physicalHealthDis, -mentalHealthDis, -age, -age.id, -over_history) 

head(master.time_period)

# Prepare the data to split them into 28-day intervals
master.time_period$diff0 <- 1

master.time_period <- master.time_period %>% mutate(diff1 = tstop - tstart)

master.time_period <- master.time_period %>% mutate(
  diff1 = ifelse(diff0 == 1 & diff1 == 1, 2, diff1)
)


library(survival)

cut.points <- c(1, 28)

master.time_period <- survSplit(data = master.time_period, 
                    cut = cut.points, 
                    start = "diff0", 
                    end = "diff1", 
                    event = "NFO_event")

head(master.time_period)

master.time_period <- master.time_period %>% 
  mutate(
    dur = diff1 - diff0
  )

master.time_period <- master.time_period %>%
  group_by(patid, OST_episode, status) %>%
  mutate( # the '1-4 weeks' applies only to the 1st line of each episode status
    exposure_status = if_else(dur <= 28 & row_number() == 1, "1-4 weeks", "> 4 weeks")
  ) %>% ungroup() %>%
  mutate(
    # this is a fix because survSplit() cannot separate rows that differ by just one day
    # (e.g. tstart: 35, tstop: 36, then dur = NA)
    dur = ifelse(diff0 == 1 & diff1 - diff0 > 1, dur + 1, dur)
  )

head(master.time_period)

sum(master.time_period$dur)/365.25

master.time_period <- master.time_period %>%
  group_by(patid) %>%
  mutate(
    test1 = max(tstop),
    test2 = sum(dur)
  )

# check
a <- master.time_period %>% filter(test1 != test2)

# Event rate
master.time_period %>% # <<-- USER INPUT: provide drug of interest (name)
  group_by(BNZ, status, exposure_status) %>%
  summarise(PY = sum(dur)/365.25,
            Events = sum(NFO_event),
            RR = Events/PY * 100)
