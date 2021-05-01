

#### Descriptive table
library(dplyr)
library(janitor)
library(tidyverse)
library(readxl)
library(brms)

d = read.csv("data-sci-main.csv") %>%
  clean_names() %>%
  drop_na(living_status_recoded)

# Age and age at injury
d$variable = d$age
d$variable = d$age_at_injury

d %>%
  filter(year %in% c("2004","2008","2018")) %>%
  group_by(year) %>%
  summarise(#mean_age = mean(variable),
    #sd_age = sd(variable),
    median_age = median(variable),
    quanitle_25 = quantile(variable, probs = 0.25),
    quanitle_75 = quantile(variable, probs = 0.75))

# Model for age
d_fit = d %>% filter(year %in% c("2004","2008","2018"))
fit <- brm(
  formula = age ~ as.factor(year) + (1|participant_id),
  seed = 123,
  data = d_fit)
pp_check(fit, re_formula = NULL, nsamples = 100)
print(summary(fit), digits = 2)



# Gender, living status, level, completeness
d$variable = d$gender_recoded
d$variable = d$living_status_recoded
d$variable = d$level_recoded
d$variable = d$complete_recoded

d %>%
  filter(year %in% c("2004","2008","2018")) %>%
  group_by(year, variable) %>%
  summarise(n = n()) %>%
  mutate(freq = n / sum(n))


#### Place of residence
d_res <- read_xlsx("data dave.xlsx",2) %>% 
  select(participant_id, residence_2004, residence_2008, residence_2018) %>%
  pivot_longer(cols = starts_with("re"),
               names_to = "year",
               values_to = "residence") %>%
  mutate(
    residence = as.factor(residence)
  )

# Summarise (1 = metro; 2 = non-metro)
d_res %>%
  group_by(year, residence) %>%
  summarise(n = n()) %>%
  mutate(freq = n / sum(n))

# Model
fit <- brm(
  formula = residence ~ year + (1|participant_id),
  family = bernoulli(link = "logit"),
  seed = 123,
  data = d_res)

# Predictive check and summary
pp_check(fit, re_formula = NULL, nsamples = 100)
print(summary(fit), digits = 2)




#### Marital
d_marital <- read_xlsx("data dave.xlsx",2) %>% 
  select(participant_id, marital_2004,marital_2008,marital_2018) %>%
  pivot_longer(cols = starts_with("m"),
               names_to = "year",
               values_to = "marital") %>%
  mutate(
    marital = as.factor(marital),
    marital = recode_factor(marital,'2' = 'mar','3' = 'mar',
                            '1' = 'not_mar','4' = 'not_mar','5' = 'not_mar','6' = 'not_mar'))

# Summarise
d_marital %>%
  group_by(year, marital) %>%
  summarise(n = n()) %>%
  mutate(freq = n / sum(n))

# Model
fit <- brm(
  formula = marital ~ year + (1|participant_id),
  family = bernoulli(link = "logit"),
  seed = 123,
  data = d_marital)

# Predictive check and summary
pp_check(fit, re_formula = NULL, nsamples = 100)
print(summary(fit), digits = 2)
conditional_effects(fit)


#### Living
d = read.csv("data-sci-main.csv") %>%
  clean_names() %>%
  drop_na(living_status_recoded) %>%
  filter(year %in% c("2004","2008","2018")) %>%
  mutate(
    year = as.factor(year),
    year = relevel(year, ref = "2008")
  )

d %>%
  group_by(year, living_status_recoded) %>%
  summarise(n = n()) %>%
  mutate(freq = n / sum(n))

# Model
fit <- brm(
  formula = living_status_recoded ~ year + (1|participant_id),
  family = bernoulli(link = "logit"),
  prior = set_prior("normal(0,0.5)", class = 'b'),
  seed = 123,
  data = d)
pp_check(fit, re_formula = NULL, nsamples = 100)
print(summary(fit), digits = 2)
conditional_effects(fit)

d_revel <- d %>% mutate(year = relevel(year, ref = "2008"))
fit <- brm(
  formula = living_status_recoded ~ year + (1|participant_id),
  family = bernoulli(link = "logit"),
  prior = set_prior("normal(0,0.5)", class = 'b'),
  seed = 123,
  data = d_revel)
print(summary(fit), digits = 2)
conditional_effects(fit)

#### Employment
d_work <- read_xlsx("data dave.xlsx",2) %>% 
  select(participant_id, employ_2004, employ_2008, employ_2018) %>%
  pivot_longer(cols = starts_with("em"),
               names_to = "year",
               values_to = "employ") %>%
  mutate(
    employ = as.factor(employ),
    employ = recode_factor(employ, '1' = 'true','2' = 'true','3' = 'true',
                           '4' = 'false','5' = 'false','6' = 'false','7' = 'false',
                           '8' = 'false','9' = 'false'))

# Summarise
d_work %>%
  group_by(year, employ) %>%
  summarise(n = n()) %>%
  mutate(freq = n / sum(n))
