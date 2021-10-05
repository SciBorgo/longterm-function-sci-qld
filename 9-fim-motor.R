

# SCI study
# Borg DN
# September, 2021

# Load data
d = read_csv("sci-dataset-22-09-21.csv") %>%
  clean_names() %>%
  mutate(
    level = as.factor(level_recoded),
    complete = as.factor(complete_recoded),
    living_status = as.factor(living_status_recoded),
    year = year-2004
  ) %>%
  drop_na(fim)

# Functional independence measure
# By year
dsum = d %>% group_by(year) %>%
  summarise(mean = mean(fim, na.rm = T))
d %>% ggplot() +
  geom_line(aes(x = year, y = fim, group = participant_id), alpha = .2) +
  geom_line(data = dsum, aes(x = year, y = mean), colour = "black", size = 1) +
  theme_bw()

# Transform to 0-1 interval
hist(d$fim)
d$fim_beta = (d$fim-7)/(91-7)
d$fim_beta[d$fim_beta == 1] = 0.99999 # Shift 1s off the boundary
hist(d$fim_beta, breaks = 20)

# Demean
d = cbind(d, demean(d, select = "time_since_injury", group = "participant_id"))

# Center age at injury
mu = mean(d$age_at_injury, na.rm = T)
d$age_at_injury_s = d$age_at_injury-mu

# Beta regression model
fit = gam(fim_beta ~ age_at_injury_s + year + weight + level + complete + living_status +
            time_since_injury_within + time_since_injury_between + s(participant_id, bs = 're') + s(participant_id, year, bs = 're'),
          family = betar(link = "logit"),
          data = d)

# Diagnostics
hist(residuals(fit))

# Summary
summary(fit)

# Save estimates
mgcv.helper::confint.gam(fit) %>%
  clean_names() %>%
  select(-statistic) %>%
  slice(-1) %>%
  write.csv("fim-parameter-estimates.csv", row.names = F)


# Plot fitted values
plot_data = (emmeans({ref_grid(fit, at = list(year = c(0,1,2,3,4,14)), transform = T)}, ~ year, adjust = "none")) %>%
  as_tibble() %>%
  clean_names()

plot_data %>% ggplot() +
  geom_errorbar(aes(x = as.factor(year+2004), ymin = (lower_cl*(91-7))+7, ymax = (upper_cl*(91-7))+7), width = 0) +
  geom_point(aes(x = as.factor(year+2004), y = (response*(91-7))+7), size = 2) +
  labs(x = "Year", y = "FIM Motor Subscale (7-91)") + 
  theme_bw(base_size = 11)


# Effect of level
effect_lvl = pairs(emmeans({ref_grid(fit, at = list(level = c('para','tetra')), transform = T)}, ~ level, adjust = "none")) %>%
  as_tibble() %>%
  clean_names()
effect_lvl
effect_lvl$estimate*(91-7)

z = 1.96 # 95% CI of effect
lower = effect_lvl$estimate+(z*effect_lvl$se)
lower*(91-7)
upper = effect_lvl$estimate-(z*effect_lvl$se)
upper*(91-7)


# Effect of completeness
effect_comp = pairs(emmeans({ref_grid(fit, at = list(complete = c('complete','incom')), transform = T)}, ~ complete, adjust = "none")) %>%
  as_tibble() %>%
  clean_names()
effect_comp
effect_comp$estimate*(91-7)

z = 1.96 # 95% CI of effect
lower = effect_comp$estimate+(z*effect_comp$se)
lower*(91-7)
upper = effect_comp$estimate-(z*effect_comp$se)
upper*(91-7)

# Effect of living status
effect_living = pairs(emmeans({ref_grid(fit, at = list(living_status = c('alone','support')), transform = T)}, ~ living_status, adjust = "none")) %>%
  as_tibble() %>%
  clean_names()
effect_living
effect_living$estimate*(91-7)

z = 1.96 # 95% CI of effect
lower = effect_living$estimate+(z*effect_living$se)
lower*(91-7)
upper = effect_living$estimate-(z*effect_living$se)
upper*(91-7)
             


#### END