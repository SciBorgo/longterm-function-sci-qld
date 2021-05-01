
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

#### Quality of life
d = read.csv("data-sci-main.csv") %>%
#d = data_sci_m8 %>%
  drop_na(qol) %>%
  select(participant_id,
         year,
         time_since_injury,
         living_status_recoded,
         complete_recoded,
         level_recoded,
         age_at_injury,
         qol)
vis_dat(d)

# Plot outcome
table(d$qol)
ggplot() + 
  geom_histogram(data = d, aes(x = qol))

# Plot
ggplot() +
  geom_histogram(data = d,
       aes(x = qol)) +
  facet_wrap(~year) +
  theme_bw()

# Standardize continuous co-variates
d$time_since_injury_s <- scale(d$time_since_injury, center = T, scale = T)
d$age_at_injury_s <- scale(d$age_at_injury, center = T, scale = T)

# Model
qol_formula = qol ~ year + (1|year) + level_recoded + complete_recoded + living_status_recoded +
  age_at_injury_s*time_since_injury_s + (1|participant_id)

# Model
fit <- brm(
  formula = qol_formula,
  family = cumulative(link = "logit"),
  prior = c(set_prior("normal(0,1)", class = "b")),
  chains = 4, cores = 8, iter = 20000,
  control = list(adapt_delta = 0.99),
  thin = 10,
  seed = 123,
  data = d)

## PP check
pp_check(fit, re_formula = NULL, nsamples = 100)

# Summary
summary(fit)

# Prior summary
prior_summary(fit)

# Prob effect of year >0
prob_year_effect <- 
  gather_draws(fit, b_year) %>%
  pivot_wider(names_from = .variable, values_from = .value) %>%
  mutate(year_effect = b_year)
prob_year_effect %>% ggplot() + geom_histogram(aes(x = year_effect))
prob_year_effect %>% mean_qi(year_effect>0, .width = c(.95))
prob_year_effect %>% mean_qi(year_effect<0, .width = c(.95))
prob_year_effect %>% mean_qi(exp(year_effect), .width = c(.95))


# Marginal effects
conditional_effects(fit, categorical = T)
conditional_effects(fit, categorical = F)








#### WHO Health
# Range 1 to 5
# Higher rating = better
d = read.csv("data-sci-main.csv") %>%
  #d = data_sci_m8 %>%
  drop_na(who_health) %>%
  select(participant_id,
         year,
         time_since_injury,
         living_status_recoded,
         complete_recoded,
         level_recoded,
         age_at_injury,
         who_health)
vis_dat(d)

# Plot outcome
table(d$who_health)
ggplot() + 
  geom_histogram(data = d, aes(x = who_health))

# Plot
ggplot() +
  geom_histogram(data = d,
                 aes(x = who_health)) +
  facet_wrap(~year) +
  theme_bw()

# Standardize continuous co-variates
d$time_since_injury_s <- scale(d$time_since_injury, center = T, scale = T)
d$age_at_injury_s <- scale(d$age_at_injury, center = T, scale = T)

# Model
health_formula = who_health ~ year + (1|year) + level_recoded + complete_recoded + living_status_recoded +
  age_at_injury_s*time_since_injury_s + (1|participant_id)

# Model
fit <- brm(
  formula = health_formula,
  family = cumulative(link = "logit"),
  prior = c(set_prior("normal(0,1)", class = "b")),
  chains = 4, cores = 8, iter = 20000,
  control = list(adapt_delta = 0.99),
  thin = 10,
  seed = 123,
  data = d)

# PP check
pp_check(fit, re_formula = NULL, nsamples = 100)

# Summary
summary(fit)

# Prob effect of year >0
prob_year_effect <- 
  gather_draws(fit, b_year) %>%
  pivot_wider(names_from = .variable, values_from = .value) %>%
  mutate(year_effect = b_year)
prob_year_effect %>% ggplot() + geom_histogram(aes(x = year_effect))
prob_year_effect %>% mean_qi(year_effect>0, .width = c(.95))
prob_year_effect %>% mean_qi(year_effect<0, .width = c(.95))
prob_year_effect %>% mean_qi(exp(year_effect), .width = c(.95))

# Prior summary
prior_summary(fit)

# Marginal effects
conditional_effects(fit, categorical = T)
conditional_effects(fit, categorical = F)










#### WHO Physical
# Range 0  to 100
# Higher scores = better
d = read.csv("data-sci-main.csv") %>%
  #data = data_sci_m8 %>%
  drop_na(who_physical) %>%
  select(participant_id,
         year,
         time_since_injury,
         living_status_recoded,
         complete_recoded,
         level_recoded,
         age_at_injury,
         who_physical)
vis_dat(d)
vis_miss(d)

# Plot outcome
table(d$who_physical)
ggplot() + 
  geom_histogram(data = d, aes(x = who_physical))

# Plot
ggplot() +
  geom_boxplot(data = d,
               aes(y = who_physical, x = as.factor(year))) +
  theme_bw()

# Standardize continuous covariates
d$time_since_injury_s <- scale(d$time_since_injury, center = T, scale = T)
d$age_at_injury_s <- scale(d$age_at_injury, center = T, scale = T)

# Look at a subset of participants
d %>% 
  filter(participant_id<20) %>%
  ggplot(aes(y = who_physical, x = year)) +
  geom_point() +
  facet_wrap(~participant_id, scales="free") +
  stat_smooth(method = "lm", se = F)

# Transform to 0-1 interval
hist(d$who_physical)
d$who_physical_beta <- d$who_physical/100
hist(d$who_physical_beta, breaks = 100)

# Shift 1s
table(d$who_physical_beta)
d$who_physical_beta[d$who_physical_beta == 1] <- 0.99999
table(d$who_physical_beta)
hist(d$who_physical_beta)

# Model
physical_formula = bf(who_physical_beta ~ year + (1|year) + level_recoded + complete_recoded + living_status_recoded +
                      age_at_injury_s + time_since_injury_s + (1|participant_id))

# Model
fit <- brm(
  formula = physical_formula,
  family = Beta(),
  prior = c(set_prior("normal(0,0.5)", class = "b")),
  chains = 4, cores = 8, iter = 20000,
  control = list(adapt_delta = 0.95),
  thin = 10,
  seed = 123,
  data = d)

# Posterior predictive check
pp_check(fit, re_formula = NULL, nsamples = 100)

# Summary
print(summary(fit), digits = 3)
# Conclusion: no effect of any variable on WHO physical.

# Prob of year effect
prob_year_effect <- 
  gather_draws(fit, b_year) %>%
  pivot_wider(names_from = .variable, values_from = .value) %>%
  mutate(year_effect = b_year)
prob_year_effect %>% ggplot() + geom_histogram(aes(x = year_effect))
prob_year_effect %>% mean_qi(year_effect>0, .width = c(.95))
prob_year_effect %>% mean_qi(year_effect<0, .width = c(.95))
prob_year_effect %>% mean_qi(exp(year_effect), .width = c(.95))

# Plot model estimates
mcmc_hist(fit, pars = c("b_year"))

# Prior summary
prior_summary(fit)

# Marginal effects
conditional_effects(fit) 

# Values by year
d %>%
  group_by(participant_id) %>%
  data_grid(year,level_recoded, complete_recoded, living_status_recoded, age_at_injury_s,time_since_injury_s) %>%
  add_fitted_draws(fit) %>%
  mutate(
    year_facet = year > 2009
  ) %>%
  ggplot(aes(x = year, y = who_physical_beta)) +
  stat_eye(aes(y = .value*100), .width = c(0.50,0.95)) +
  ylab("WHO Physical (0-100)") +
  xlab("Year") +
  theme_bw() +
  facet_grid(. ~ year_facet, scales = "free", space = "free") +
  theme(axis.text=element_text(size = 10),
        axis.title=element_text(size = 12),
        legend.text=element_text(size = 10),
        panel.grid.major.x=element_blank(),
        panel.grid.minor.x=element_blank(),
        strip.text.x = element_blank()) +
  scale_x_continuous(breaks = seq(2004, 2018, 1))
ggsave(file = "who-physical.png", units="in", width = 7, height = 3.5, dpi = 300)












#### WHO Psychological
# Range 0  to 100
# Higher scores = better
d = read.csv("data-sci-main.csv") %>%
  #data = data_sci_m8 %>%
  drop_na(who_psych) %>%
  select(participant_id,
         year,
         time_since_injury,
         living_status_recoded,
         complete_recoded,
         level_recoded,
         age_at_injury,
         who_psych)
vis_dat(d)
vis_miss(d)

# Plot outcome
table(d$who_psych)
ggplot() + 
  geom_histogram(data = d, aes(x = who_psych))

# Plot
ggplot() +
  geom_boxplot(data = d,
               aes(y = who_psych, x = as.factor(year))) +
  theme_bw()

# Standardize continuous covariates
d$time_since_injury_s <- scale(d$time_since_injury, center = T, scale = T)
d$age_at_injury_s <- scale(d$age_at_injury, center = T, scale = T)

# Look at a subset of participants
d %>% 
  filter(participant_id<20) %>%
  ggplot(aes(y = who_psych, x = year)) +
  geom_point() +
  facet_wrap(~participant_id, scales="free") +
  stat_smooth(method = "lm")

# Transform to 0-1 interval
hist(d$who_psych)
d$who_psych_beta <- d$who_psych/100
hist(d$who_psych_beta, breaks = 100)

# Shift 1s
table(d$who_psych_beta)
d$who_psych_beta[d$who_psych_beta == 1] <- 0.99999
table(d$who_psych_beta)
hist(d$who_psych_beta)

# Model
psych_formula = bf(who_psych_beta ~ year + (1|year) + level_recoded + complete_recoded + living_status_recoded +
                      age_at_injury_s*time_since_injury_s + (1|participant_id))

# Model
fit <- brm(
  formula = psych_formula,
  family = Beta(),
  prior = c(set_prior("normal(0,0.5)", class = "b")),
  chains = 4, cores = 8, iter = 20000,
  control = list(adapt_delta = 0.95),
  thin = 10,
  seed = 123,
  data = d)

# Posterior predictive check
pp_check(fit, re_formula = NULL, nsamples = 100)

# Summary
print(summary(fit), digits = 3)
# Conclusion: Effect of injury level and age-at-injury by time-since-injury interaction.

# Prob of year effect
prob_year_effect <- 
  gather_draws(fit, b_year) %>%
  pivot_wider(names_from = .variable, values_from = .value) %>%
  mutate(odds_ratio = b_year )
prob_year_effect %>% ggplot() + geom_histogram(aes(x = odds_ratio))
prob_year_effect %>% mean_qi(odds_ratio>0, .width = c(.95))
prob_year_effect %>% mean_qi(exp(odds_ratio), .width = c(.95))

# Plot model estimates
mcmc_hist(fit, pars = c("b_year"))

# Conclusion:

# Prior summary
prior_summary(fit)

# Marginal effects
conditional_effects(fit) 

# Values by year
d %>%
  group_by(participant_id) %>%
  data_grid(year,level_recoded, complete_recoded, living_status_recoded, age_at_injury_s,time_since_injury_s) %>%
  add_fitted_draws(fit) %>%
  mutate(
    year_facet = year > 2009
  ) %>%
  ggplot(aes(x = year, y = who_psych_beta)) +
  stat_eye(aes(y = .value*100), .width = c(0.50,0.95)) +
  ylab("WHO Psychological (0-100)") +
  xlab("Year") +
  theme_bw() +
  facet_grid(. ~ year_facet, scales = "free", space = "free") +
  theme(axis.text=element_text(size = 10),
        axis.title=element_text(size = 12),
        legend.text=element_text(size = 10),
        panel.grid.major.x=element_blank(),
        panel.grid.minor.x=element_blank(),
        strip.text.x = element_blank()) +
  scale_x_continuous(breaks = seq(2004, 2018, 1))
ggsave(file = "who-psych.png", units="in", width = 7, height = 3.5, dpi = 300)











#### WHO Social
# Range 0  to 100
# Higher scores = better
d = read.csv("data-sci-main.csv") %>%
  #data = data_sci_m8 %>%
  drop_na(who_social) %>%
  select(participant_id,
         year,
         time_since_injury,
         living_status_recoded,
         complete_recoded,
         level_recoded,
         age_at_injury,
         who_social)
vis_dat(d)
vis_miss(d)

# Plot outcome
table(d$who_social)
ggplot() + 
  geom_histogram(data = d, aes(x = who_social))

# Plot
ggplot() +
  geom_boxplot(data = d,
               aes(y = who_social, x = as.factor(year))) +
  theme_bw()

# Standardize continuous covariates
d$time_since_injury_s <- scale(d$time_since_injury, center = T, scale = T)
d$age_at_injury_s <- scale(d$age_at_injury, center = T, scale = T)

# Look at a subset of participants
d %>% 
  filter(participant_id<20) %>%
  ggplot(aes(y = who_social, x = year)) +
  geom_point() +
  facet_wrap(~participant_id, scales="free") +
  stat_smooth(method = "lm")

# Transform to 0-1 interval
hist(d$who_social)
d$who_social_beta <- d$who_social/100
hist(d$who_social_beta, breaks = 100)

# Shift 1s
table(d$who_social_beta)
d$who_social_beta[d$who_social_beta == 1] <- 0.99999
d$who_social_beta[d$who_social_beta == 0] <- 0.00001
table(d$who_social_beta)
hist(d$who_social_beta)

# Model
social_formula = bf(who_social_beta ~ year + (1|year) + level_recoded + complete_recoded + living_status_recoded +
                      + age_at_injury_s + time_since_injury_s + (1|participant_id))

# Model
fit <- brm(
  formula = social_formula,
  family = Beta(),
  prior = c(set_prior("normal(0,0.5)", class = "b")),
  chains = 4, cores = 8, iter = 20000,
  control = list(adapt_delta = 0.99),
  thin = 10,
  seed = 123,
  data = d)

# Posterior predictive check
pp_check(fit, re_formula = NULL, nsamples = 100)

# Summary
print(summary(fit), digits = 3)

# Prob of year effect
prob_year_effect <- 
  gather_draws(fit, b_year) %>%
  pivot_wider(names_from = .variable, values_from = .value) %>%
  mutate(odds_ratio = b_year )
prob_year_effect %>% ggplot() + geom_histogram(aes(x = odds_ratio))
prob_year_effect %>% mean_qi(odds_ratio>0, .width = c(.95))
prob_year_effect %>% mean_qi(exp(odds_ratio), .width = c(.95))

# Plot model estimates
mcmc_hist(fit, pars = c("b_year"))

# Conclusion:

# Prior summary
prior_summary(fit)

# Marginal effects
conditional_effects(fit) 

# Values by year
d %>%
  group_by(participant_id) %>%
  data_grid(year,level_recoded, complete_recoded, living_status_recoded, age_at_injury_s,time_since_injury_s) %>%
  add_fitted_draws(fit) %>%
  mutate(
    year_facet = year > 2009
  ) %>%
  ggplot(aes(x = year, y = who_social_beta)) +
  stat_eye(aes(y = .value*100), .width = c(0.50,0.95)) +
  ylab("WHO Social (0-100)") +
  xlab("Year") +
  theme_bw() +
  facet_grid(. ~ year_facet, scales = "free", space = "free") +
  theme(axis.text=element_text(size = 10),
        axis.title=element_text(size = 12),
        legend.text=element_text(size = 10),
        panel.grid.major.x=element_blank(),
        panel.grid.minor.x=element_blank(),
        strip.text.x = element_blank()) +
  scale_x_continuous(breaks = seq(2004, 2018, 1))
ggsave(file = "who-social.png", units="in", width = 7, height = 3.5, dpi = 300)













#### WHO Environment – scale = 0 to 100, with higher = better
# Range 0 to 100
d = read.csv("data-sci-main.csv") %>%
  #data = data_sci_m8 %>%
  drop_na(who_environment) %>%
  select(participant_id,
         year,
         time_since_injury,
         living_status_recoded,
         complete_recoded,
         level_recoded,
         age_at_injury,
         who_environment)
vis_dat(d)
vis_miss(d)

# Plot outcome
table(d$who_environment)
ggplot() + 
  geom_histogram(data = d, aes(x = who_environment))

# Plot
ggplot() +
  geom_boxplot(data = d,
               aes(y = who_environment, x = as.factor(year))) +
  theme_bw()

# Standardize continuous covariates
d$time_since_injury_s <- scale(d$time_since_injury, center = T, scale = T)
d$age_at_injury_s <- scale(d$age_at_injury, center = T, scale = T)

# Look at a subset of participants
d %>% 
  filter(participant_id<20) %>%
  ggplot(aes(y = who_environment, x = year)) +
  geom_point() +
  facet_wrap(~participant_id, scales="free") +
  stat_smooth(method = "lm")

# Transform to 0-1 interval
hist(d$who_environment)
d$who_environment_beta <- d$who_environment/100
hist(d$who_environment_beta, breaks = 100)

# Shift 1s
table(d$who_environment_beta)
d$who_environment_beta[d$who_environment_beta == 1] <- 0.99999
table(d$who_environment_beta)
hist(d$who_environment_beta)

# Model
environment_formula = bf(who_environment_beta ~ year + (1|year) + level_recoded + complete_recoded + living_status_recoded +
                    age_at_injury_s*time_since_injury_s + (1|participant_id))

# Model
fit <- brm(
  formula = environment_formula,
  family = Beta(),
  prior = c(set_prior("normal(0,0.5)", class = "b")),
  chains = 4, cores = 8, iter = 20000,
  control = list(adapt_delta = 0.99),
  thin = 10,
  seed = 123,
  data = d)

# Posterior predictive check
pp_check(fit, re_formula = NULL, nsamples = 100)

# Summary
print(summary(fit), digits = 3)

# Prob of year effect
prob_year_effect <- 
  gather_draws(fit, b_year) %>%
  pivot_wider(names_from = .variable, values_from = .value) %>%
  mutate(odds_ratio = b_year )
prob_year_effect %>% ggplot() + geom_histogram(aes(x = odds_ratio))
prob_year_effect %>% mean_qi(odds_ratio>0, .width = c(.95))
prob_year_effect %>% mean_qi(exp(odds_ratio), .width = c(.95))

# Plot model estimates
mcmc_hist(fit, pars = c("b_year"))

# Conclusion:

# Prior summary
prior_summary(fit)

# Look at residuals
res <- residuals(fit)
hist(res[,1])

# Marginal effects
conditional_effects(fit) 

# Values by year
d %>%
  group_by(participant_id) %>%
  data_grid(year,level_recoded, complete_recoded, living_status_recoded, age_at_injury_s,time_since_injury_s) %>%
  add_fitted_draws(fit) %>%
  mutate(
    year_facet = year > 2009
  ) %>%
  ggplot(aes(x = year, y = who_environment_beta)) +
  stat_eye(aes(y = .value*100), .width = c(0.50,0.95)) +
  ylab("WHO Environment (0-100)") +
  xlab("Year") +
  theme_bw() +
  facet_grid(. ~ year_facet, scales = "free", space = "free") +
  theme(axis.text=element_text(size = 10),
        axis.title=element_text(size = 12),
        legend.text=element_text(size = 10),
        panel.grid.major.x=element_blank(),
        panel.grid.minor.x=element_blank(),
        strip.text.x = element_blank()) +
  scale_x_continuous(breaks = seq(2004, 2018, 1))
ggsave(file = "who-environment.png", units="in", width = 7, height = 3.5, dpi = 300)





#### FIM motor subscale
# Range 7 to 91
d = read.csv("data-sci-main.csv") %>%
#data = data_sci_m8 %>%
  drop_na(qol) %>%
  select(participant_id,
         year,
         time_since_injury,
         time_since_injury,
         living_status_recoded,
         complete_recoded,
         level_recoded,
         age_at_injury,
         fim)
vis_dat(d)
vis_miss(d)

# Plot outcome
table(d$fim)
ggplot() + 
  geom_histogram(data = d, aes(x = fim))

# Plot
ggplot() +
  geom_boxplot(data = d,
                 aes(y = fim, x = as.factor(year))) +
  theme_bw()

# Standardize continuous co-variates
d$time_since_injury_s <- scale(d$time_since_injury, center = T, scale = T)
d$age_at_injury_s <- scale(d$age_at_injury, center = T, scale = T)

ggplot() +
  geom_point(data = d, aes(x = time_since_injury, y = fim, colour = age_at_injury))

# Look at a subset of participants
d %>% 
  filter(participant_id<20) %>%
  ggplot(aes(y = fim, x = year),
         scales = free) +
  geom_point() +
  facet_wrap(~participant_id) +
  stat_smooth(method = "lm")

# Transform to 0-1 interval
hist(d$fim)
d$fim_beta <- (d$fim-7)/(91-7)
hist(d$fim_beta, breaks = 50)

# Shift 1s
table(d$fim_beta)
d$fim_beta[d$fim_beta == 1] <- 0.99999
table(d$fim_beta)
hist(d$fim_beta)

# Model
fim_formula = bf(fim_beta ~ year + (1|year) + level_recoded + complete_recoded + living_status_recoded +
                   time_since_injury_s + age_at_injury_s + (1|participant_id))

# Model
fit <- brm(
  formula = fim_formula,
  family = Beta(),
  prior = c(set_prior("normal(0,0.5)", class = "b")),
  chains = 4, cores = 8, iter = 20000,
  control = list(adapt_delta = 0.99),
  thin = 10,
  seed = 123,
  data = d)

# PP check
pp_check(fit, re_formula = NULL, nsamples = 100)

# Summary
print(summary(fit), digits = 3)
# Conclusion: effects of level, incomplete, living status on FIM.

# Odds ratios
level_effect <- 
  gather_draws(fit, b_level_recodedtetra) %>%
  pivot_wider(names_from = .variable, values_from = .value) %>%
  mutate(effect = b_level_recodedtetra)
level_effect %>% ggplot() + geom_histogram(aes(x = exp(effect)))
level_effect %>% mean_qi(exp(effect), .width = c(.95))

incomplete_effect <- 
  gather_draws(fit, b_complete_recodedincom) %>%
  pivot_wider(names_from = .variable, values_from = .value) %>%
  mutate(effect = b_complete_recodedincom)
incomplete_effect %>% ggplot() + geom_histogram(aes(x = exp(effect)))
incomplete_effect %>% mean_qi(exp(effect), .width = c(.95))

living_effect <- 
  gather_draws(fit, b_living_status_recodedsupport) %>%
  pivot_wider(names_from = .variable, values_from = .value) %>%
  mutate(effect = b_living_status_recodedsupport)
living_effect %>% ggplot() + geom_histogram(aes(x = exp(effect)))
living_effect %>% mean_qi(exp(effect), .width = c(.95))

# Prior summary
prior_summary(fit)

# Marginal effects
conditional_effects(fit) 

# Values by year
d %>%
  group_by(participant_id) %>%
  data_grid(year,level_recoded, complete_recoded, living_status_recoded, age_at_injury_s, time_since_injury_s) %>%
  add_fitted_draws(fit) %>%
  mutate(
    year_facet = year > 2009
  ) %>%
  ggplot(aes(x = year, y = fim_beta)) +
  stat_eye(aes(y = (.value*(91-7))+7), .width = c(0.66,0.95)) +
  ylab("FIM Motor Subscale (7-91)") +
  xlab("Year") +
  theme_bw() +
  facet_grid(. ~ year_facet, scales = "free", space = "free") +
  theme(axis.text=element_text(size = 14),
        axis.title=element_text(size = 16),
        legend.text=element_text(size = 14),
        panel.grid.major.x=element_blank(),
        panel.grid.minor.x=element_blank(),
        strip.text.x = element_blank()) +
  scale_x_continuous(breaks = seq(2004, 2018, 1)) -> plotfim
plotfim
ggsave(file = "fim.png", units="in", width = 7, height = 3.5, dpi = 300)








#### SCSI
# Range 0 to 45
d = read.csv("data-sci-main.csv") %>%
  drop_na(scsi) %>%
  select(participant_id,
         year,
         time_since_injury,
         living_status_recoded,
         complete_recoded,
         level_recoded,
         age_at_injury,
         scsi)
vis_dat(d)
vis_miss(d)

# Plot outcome
table(d$scsi)
ggplot() + 
  geom_histogram(data = d, aes(x = scsi))

# Plot
ggplot() +
  geom_boxplot(data = d,
               aes(y = scsi, x = as.factor(year))) +
  theme_bw()

# Standardize continuous co-variates
d$time_since_injury_s <- scale(d$time_since_injury, center = T, scale = T)
d$age_at_injury_s <- scale(d$age_at_injury, center = T, scale = T)

# Look at a subset of participants
d %>% 
  filter(participant_id<20) %>%
  ggplot(aes(y = scsi, x = year)) +
  geom_point() +
  facet_wrap(~participant_id, scales="free") +
  stat_smooth(method = "lm")

# Transform to 0-1 interval
hist(d$scsi)
d$scsi_beta <- d$scsi/45
hist(d$scsi_beta, breaks = 45)

# Shift 0s
table(d$scsi_beta)
d$scsi_beta[d$scsi_beta == 0] <- 0.00001
table(d$scsi_beta)
hist(d$scsi_beta)

# Model
scsi_formula = bf(scsi_beta ~ year + (1|year) + level_recoded + complete_recoded + living_status_recoded +
                   time_since_injury_s + age_at_injury_s + (1|participant_id))

# Model
fit <- brm(
  formula = scsi_formula,
  family = Beta(),
  prior = c(set_prior("normal(0,0.5)", class = "b")),
  chains = 4, cores = 8, iter = 20000,
  control = list(adapt_delta = 0.99),
  thin = 10,
  seed = 123,
  data = d)

# Posterior predictive check
pp_check(fit, re_formula = NULL, nsamples = 100)

# Summary
print(summary(fit), digits = 3)

# Plot model estimates
mcmc_hist(post, pars = c("b_time_since_injury_s"))

# Odds ratio of time since injury effect
time_since_injury_effect <- 
  gather_draws(fit, b_time_since_injury_s) %>%
  pivot_wider(names_from = .variable, values_from = .value) %>%
  mutate(tsi_effect = b_time_since_injury_s)
time_since_injury_effect %>% ggplot() + geom_histogram(aes(x = exp(tsi_effect)))
time_since_injury_effect %>% mean_qi(exp(tsi_effect), .width = c(.95))

# Check time effect
time_effect <- 
  gather_draws(fit, b_year) %>%
  pivot_wider(names_from = .variable, values_from = .value) %>%
  mutate(effect = b_year)
time_effect %>% ggplot() + geom_histogram(aes(x = exp(effect)))
time_effect %>% mean_qi(effect>0, .width = c(.95))

# Conclusion: effect of time since injury.

# Prior summary
prior_summary(fit)

# Marginal effects
conditional_effects(fit) 

# Values by year
d %>%
  group_by(participant_id) %>%
  data_grid(year,level_recoded, complete_recoded, living_status_recoded, age_at_injury_s, time_since_injury_s) %>%
  add_fitted_draws(fit) %>%
  mutate(
    year_facet = year > 2009
  ) %>%
  ggplot(aes(x = year, y = scsi_beta)) +
  stat_eye(aes(y = .value*45), .width = c(0.66,0.95)) +
  ylab("SCSI (0-45)") +
  xlab("Year") +
  theme_bw() +
  facet_grid(. ~ year_facet, scales = "free", space = "free") +
  theme(axis.text=element_text(size = 14),
        axis.title=element_text(size = 16),
        legend.text=element_text(size = 14),
        panel.grid.major.x=element_blank(),
        panel.grid.minor.x=element_blank(),
        strip.text.x = element_blank()) +
  scale_x_continuous(breaks = seq(2004, 2018, 1)) -> plotscsi
plotscsi
ggsave(file = "scsi.png", units="in", width = 7, height = 3.5, dpi = 300)

# Jitter check
ggplot()+
  geom_jitter(data = d, aes(x = as.factor(year), y = scsi))

# Boxplot check
ggplot()+
  geom_boxplot(data = d, aes(x = as.factor(year), y = scsi))









#### Community integration measure
# Range 10 to 50
d = read.csv("data-sci-main.csv") %>%
  #data = data_sci_m8 %>%
  drop_na(cim) %>%
  select(participant_id,
         year,
         time_since_injury,
         living_status_recoded,
         complete_recoded,
         level_recoded,
         age_at_injury,
         cim)
vis_dat(d)
vis_miss(d)

# Plot outcome
table(d$cim)
ggplot() + 
  geom_histogram(data = d, aes(x = cim))

# Plot
ggplot() +
  geom_boxplot(data = d,
               aes(y = cim, x = as.factor(year))) +
  theme_bw()

# Standardize continuous co-variates
d$time_since_injury_s <- scale(d$time_since_injury, center = T, scale = T)
d$age_at_injury_s <- scale(d$age_at_injury, center = T, scale = T)

# Look at a subset of participants
d %>% 
  filter(participant_id<20) %>%
  ggplot(aes(y = cim, x = year),
         scales = free) +
  geom_point() +
  facet_wrap(~participant_id) +
  stat_smooth(method = "lm")

# Transform to 0-1 interval
hist(d$cim)
d$cim_beta <- (d$cim-10)/(40)
hist(d$cim_beta, breaks = 50)

# Shift 1s
table(d$cim_beta)
d$cim_beta[d$cim_beta == 1] <- 0.99999
table(d$cim_beta)
hist(d$cim_beta)

# Model
cim_formula = bf(cim_beta ~ year + (1|year) + level_recoded + complete_recoded + living_status_recoded +
                   age_at_injury_s + time_since_injury_s + (1|participant_id))

# Model
fit <- brm(
  formula = cim_formula,
  family = Beta(),
  prior = c(set_prior("normal(0,0.5)", class = "b")),
  chains = 4, cores = 8, iter = 20000,
  control = list(adapt_delta = 0.99),
  thin = 10,
  seed = 123,
  data = d)

# PP check
pp_check(fit, re_formula = NULL, nsamples = 100)

# Summary
print(summary(fit), digits = 3)

# Conclusion: no effect of any variable on CIM.

# Odds ratios
effect_of_year <- 
  gather_draws(fit, b_year) %>%
  pivot_wider(names_from = .variable, values_from = .value) %>%
  mutate(year_effect = b_year)
effect_of_year %>% ggplot() + geom_histogram(aes(x = exp(year_effect)))
effect_of_year %>% mean_qi(exp(year_effect), .width = c(.95))
effect_of_year %>% mean_qi(year_effect>0, .width = c(.95))

# Prior summary
prior_summary(fit)

# Marginal effects
conditional_effects(fit) 

# Values by year
d %>%
  group_by(participant_id) %>%
  data_grid(year,level_recoded, complete_recoded, living_status_recoded, age_at_injury_s, time_since_injury_s) %>%
  add_fitted_draws(fit) %>%
  mutate(
    year_facet = year > 2009
  ) %>%
  ggplot(aes(x = year, y = cim_beta)) +
  stat_eye(aes(y = (.value*(50-10))+10), .width = c(0.66,0.95)) +
  ylab("CIM (10-50)") +
  xlab("Year") +
  theme_bw() +
  facet_grid(. ~ year_facet, scales = "free", space = "free") +
  theme(axis.text=element_text(size = 14),
        axis.title=element_text(size = 16),
        legend.text=element_text(size = 14),
        panel.grid.major.x=element_blank(),
        panel.grid.minor.x=element_blank(),
        strip.text.x = element_blank()) +
  scale_x_continuous(breaks = seq(2004, 2018, 1)) -> plotcim
plotcim
ggsave(file = "cim.png", units="in", width = 7, height = 3.5, dpi = 300) 









#### END


