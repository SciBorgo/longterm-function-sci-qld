

# Libraries
library(ggplot2)
#library(zoib)
library(janitor)
library(dplyr)
library(tidyverse)
library(visdat)
library(brms)
library(readxl)
library(modelr)
library(tidybayes)

d = read.csv("sci-survey-data.csv") %>%
  clean_names()

# Demographic variables that dont change with time
dsub_demo <- d %>%
  select(participant_id,
         gender,
         age_at_injury,
         level,
         complete) %>%
  mutate( # Relabel level of injury and completeness of injury
    level = as.factor(level),
    level_recoded = dplyr::recode_factor(level, '1' = 'tetra', '2' = 'para'),
    level_recoded = relevel(level_recoded, ref = "para"),
    complete = as.factor(complete),
    complete_recoded = dplyr::recode_factor(complete, '1' = 'incom', '2' = 'complete'),
    complete_recoded = relevel(complete_recoded, ref = "incom"),
    gender = as.factor(gender),
    gender_recoded = dplyr::recode_factor(gender, '1' = 'm', '2' = 'f'),
    gender_recoded = relevel(gender_recoded, ref = "m")
  ) %>%
  select(-level, -complete, -gender)
vis_miss(dsub_demo)

# Age wrangled
d_age <- d %>% 
  select(participant_id, age) %>%
  mutate(
    t1 = age,
    t2 = age + 1,
    t3 = age + 2,
    t4 = age + 3,
    t5 = age + 4,
    t6 = age + 14
  ) %>%
  select(-age) %>%
  pivot_longer(cols = starts_with("t"),
               names_to = "year",
               values_to = "age") %>%
  mutate(
    year = dplyr::recode(year, 't1' = '2004',
                                't2' = '2005',
                                't3' = '2006',
                                't4' = '2007',
                                't5' = '2008',
                                't6' = '2018'),
    year = as.numeric(year)
  )

# Time since injury wrangled
d_tsi <- d %>% 
  select(participant_id, time_since_injury) %>%
  mutate(
    t1 = time_since_injury,
    t2 = time_since_injury + 1,
    t3 = time_since_injury + 2,
    t4 = time_since_injury + 3,
    t5 = time_since_injury + 4,
    t6 = time_since_injury + 14
  ) %>%
  select(-time_since_injury) %>%
  pivot_longer(cols = starts_with("t"),
               names_to = "year",
               values_to = "time_since_injury") %>%
  mutate(
    year = dplyr::recode(year, 't1' = '2004',
                         't2' = '2005',
                         't3' = '2006',
                         't4' = '2007',
                         't5' = '2008',
                         't6' = '2018'),
    year = as.numeric(year)
  )

# Living status
d_living_status_2004 <- read.csv("2004 Year 1.csv")
d_living_status_2005 <- read.csv("2005 Year 2.csv")
d_living_status_2006 <- read.csv("2006 Year 3.csv")
d_living_status_2007 <- read.csv("2007 Year 4.csv")
d_living_status_2008 <- read.csv("2008 Year 5.csv")
d_living_status_2018 <- read.csv("2018 10 year followup.csv")

d_living_status <- bind_rows(d_living_status_2004,
                  d_living_status_2005,
                  d_living_status_2006,
                  d_living_status_2007,
                  d_living_status_2008,
                  d_living_status_2018)

# Merge demo and injury dataset
data_m1 <- merge(d_age, dsub_demo, by = "participant_id")
data_m2 <- merge(data_m1, d_tsi, by = c("participant_id","year"))
data_m3 <- merge(data_m2, d_living_status, by = c("participant_id","year"))

# Recode living status: 'alone' and 'support'
data_m3$living_status <- as.factor(data_m3$living_status)
data_m3$living_status_recoded <- dplyr::recode_factor(data_m3$living_status, '1' = 'alone',
                                                   '2' = 'support',
                                                   '3' = 'support',
                                                   '4' = 'support',
                                                   '5' = 'support',
                                                   '6' = 'support',
                                                   '7' = 'support',
                                                   '8' = 'support', # This is other, am i okay to code as 'support'
                                                   '9' = 'NA',
                                                   '10' = 'support')
data_m3 <- data_m3 %>% select(-living_status)
vis_miss(data_m3)


# Subset outcomes to be added in
# WHO quality of life
d_qol <- d %>% select(participant_id,
                     who_qol_t1_2004,
                     who_qol_t2_2005,
                     who_qol_t3_2006,
                     who_qol_t4_2007,
                     who_qol_t5_2008,
                     who_qol_t6_2018) %>%
  pivot_longer(cols = starts_with("who_qol"),
               names_to = "year",
               values_to = "qol")
# Recode year
d_qol$year <- dplyr::recode(d_qol$year, 'who_qol_t1_2004' = 2004, 
                           'who_qol_t2_2005' = 2005, 
                           'who_qol_t3_2006' = 2006, 
                           'who_qol_t4_2007' = 2007, 
                           'who_qol_t5_2008' = 2008,
                           'who_qol_t6_2018' = 2018)

# WHO health
d_health <- d %>% select(participant_id,
                      who_health_t1_2004,
                      who_health_t2_2005,
                      who_health_t3_2006,
                      who_health_t4_2007,
                      who_health_t5_2008,
                      who_health_t6_2018) %>%
  pivot_longer(cols = starts_with("who_health"),
               names_to = "year",
               values_to = "who_health")
# Recode year
d_health$year <- dplyr::recode(d_health$year, 'who_health_t1_2004' = 2004, 
                            'who_health_t2_2005' = 2005, 
                            'who_health_t3_2006' = 2006, 
                            'who_health_t4_2007' = 2007, 
                            'who_health_t5_2008' = 2008,
                            'who_health_t6_2018' = 2018)

# WHO physical
d_physical <- d %>% select(participant_id,
                         who_physical_t1_2004,
                         who_physical_t2_2005,
                         who_physical_t3_2006,
                         who_physical_t4_2007,
                         who_physical_t5_2008,
                         who_physical_t6_2018) %>%
  pivot_longer(cols = starts_with("who_physical"),
               names_to = "year",
               values_to = "who_physical")
# Recode year
d_physical$year <- dplyr::recode(d_physical$year, 'who_physical_t1_2004' = 2004, 
                               'who_physical_t2_2005' = 2005, 
                               'who_physical_t3_2006' = 2006, 
                               'who_physical_t4_2007' = 2007, 
                               'who_physical_t5_2008' = 2008,
                               'who_physical_t6_2018' = 2018)

# WHO psych
d_psych <- d %>% select(participant_id,
                           who_psych_t1_2004,
                           who_psych_t2_2005,
                           who_psych_t3_2006,
                           who_psych_t4_2007,
                           who_psych_t5_2008,
                           who_psych_t6_2018) %>%
  pivot_longer(cols = starts_with("who_psych"),
               names_to = "year",
               values_to = "who_psych")
# Recode year
d_psych$year <- dplyr::recode(d_psych$year, 'who_psych_t1_2004' = 2004, 
                                 'who_psych_t2_2005' = 2005, 
                                 'who_psych_t3_2006' = 2006, 
                                 'who_psych_t4_2007' = 2007, 
                                 'who_psych_t5_2008' = 2008,
                                 'who_psych_t6_2018' = 2018)

# WHO social
d_social <- d %>% select(participant_id,
                        who_social_t1_2004,
                        who_social_t2_2005,
                        who_social_t3_2006,
                        who_social_t4_2007,
                        who_social_t5_2008,
                        who_social_t6_2018) %>%
  pivot_longer(cols = starts_with("who_social"),
               names_to = "year",
               values_to = "who_social")
# Recode year
d_social$year <- dplyr::recode(d_social$year, 'who_social_t1_2004' = 2004, 
                              'who_social_t2_2005' = 2005, 
                              'who_social_t3_2006' = 2006, 
                              'who_social_t4_2007' = 2007, 
                              'who_social_t5_2008' = 2008,
                              'who_social_t6_2018' = 2018)

# WHO environment
d_environment <- d %>% select(participant_id,
                         who_environment_t1_2004,
                         who_environment_t2_2005,
                         who_environment_t3_2006,
                         who_environment_t4_2007,
                         who_environment_t5_2008,
                         who_environment_t6_2018) %>%
  pivot_longer(cols = starts_with("who_environment"),
               names_to = "year",
               values_to = "who_environment")
# Recode year
d_environment$year <- dplyr::recode(d_environment$year, 'who_environment_t1_2004' = 2004, 
                               'who_environment_t2_2005' = 2005, 
                               'who_environment_t3_2006' = 2006, 
                               'who_environment_t4_2007' = 2007, 
                               'who_environment_t5_2008' = 2008,
                               'who_environment_t6_2018' = 2018)

# SCSI
d_scsi <- d %>% select(participant_id,
                              scsi_t1_2004,
                              scsi_t2_2005,
                              scsi_t3_2006,
                              scsi_t4_2007,
                              scsi_t5_2008,
                              scsi_t6_2018) %>%
  pivot_longer(cols = starts_with("scsi"),
               names_to = "year",
               values_to = "scsi")
# Recode year
d_scsi$year <- dplyr::recode(d_scsi$year, 'scsi_t1_2004' = 2004, 
                                    'scsi_t2_2005' = 2005, 
                                    'scsi_t3_2006' = 2006, 
                                    'scsi_t4_2007' = 2007, 
                                    'scsi_t5_2008' = 2008,
                                    'scsi_t6_2018' = 2018)

# FIM motor subscale
d_fim <- d %>% select(participant_id,
                       fim_t1_2004,
                       fim_t2_2005,
                       fim_t3_2006,
                       fim_t4_2007,
                       fim_t5_2008,
                       fim_t6_2018) %>%
  pivot_longer(cols = starts_with("fim"),
               names_to = "year",
               values_to = "fim")
# Recode year
d_fim$year <- dplyr::recode(d_fim$year, 'fim_t1_2004' = 2004, 
                             'fim_t2_2005' = 2005, 
                             'fim_t3_2006' = 2006, 
                             'fim_t4_2007' = 2007, 
                             'fim_t5_2008' = 2008,
                             'fim_t6_2018' = 2018)

# Community integration measure
d_cim <- d %>% select(participant_id,
                      cim_t1_2004,
                      cim_t2_2005,
                      cim_t3_2006,
                      cim_t4_2007,
                      cim_t5_2008,
                      cim_t6_2018) %>%
  pivot_longer(cols = starts_with("cim"),
               names_to = "year",
               values_to = "cim")
# Recode year
d_cim$year <- dplyr::recode(d_cim$year, 'cim_t1_2004' = 2004, 
                            'cim_t2_2005' = 2005, 
                            'cim_t3_2006' = 2006, 
                            'cim_t4_2007' = 2007, 
                            'cim_t5_2008' = 2008,
                            'cim_t6_2018' = 2018)

# Merge the full dataset
data_sci <- merge(data_m3, d_qol, by = c("participant_id","year"), all.x = T) # Merge qol
data_sci_m1 <- merge(data_sci, d_health, by = c("participant_id","year"), all.x = T) # Merge health
data_sci_m2 <- merge(data_sci_m1, d_physical, by = c("participant_id","year"), all.x = T) # Merge physical
data_sci_m3 <- merge(data_sci_m2, d_psych, by = c("participant_id","year"), all.x = T) # Merge psych
data_sci_m4 <- merge(data_sci_m3, d_social, by = c("participant_id","year"), all.x = T) # Merge social
data_sci_m5 <- merge(data_sci_m4, d_environment, by = c("participant_id","year"), all.x = T) # Merge environment
data_sci_m6 <- merge(data_sci_m5, d_scsi, by = c("participant_id","year"), all.x = T) # Merge scsi
data_sci_m7 <- merge(data_sci_m6, d_fim, by = c("participant_id","year"), all.x = T) # Merge fim
data_sci_m8 <- merge(data_sci_m7, d_cim, by = c("participant_id","year"), all.x = T) # Merge cim

vis_miss(data_sci_m8)

#write.csv(x = data_sci_m8, file = "data-sci-main.csv", row.names = F)