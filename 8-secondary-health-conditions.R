

# SCI study
# Borg DN
# September, 2021

# Load data
d = read_csv("sci-dataset-22-09-21.csv") %>%
  clean_names() %>%
  mutate(
    level = as.factor(level_recoded),
    complete = as.factor(complete_recoded),
    age_at_injury_s = scale(age_at_injury, center = T, scale = F),
    living_status = as.factor(living_status_recoded),
    year = year-2004
  ) %>%
  drop_na(scsi)

# Secondary health conditions measure
# Collapsed
d %>% ggplot(aes(x = scsi)) + geom_histogram() +
  theme_bw()

# By year
dsum = d %>% group_by(year) %>%
  summarise(mean = mean(scsi, na.rm = T))
d %>% ggplot() +
  geom_line(aes(x = year, y = scsi, group = participant_id), alpha = .2) +
  geom_line(data = dsum, aes(x = year, y = mean), colour = "black", size = 1) +
  theme_bw()

# Model
dsub = panel_data(d, id = participant_id, wave = year)

fit = wbm(scsi ~ time_since_injury | age_at_injury_s + year + weight + level + complete + living_status | (year | participant_id),
          data = dsub,
          model = "w-b",
          family = poisson(link = "log"),
          control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 2e5)))

# Diagnostics
res = residuals(fit)
hist(res)
car::qqPlot(res)

# Summary
summary(fit)
tidy(fit, conf.int = T)

# Save estimates
tidy(fit, conf.int = T) %>%
  filter(group %in% c("within","between")) %>%
  select(-statistic,-group) %>%
  relocate(term, estimate, std.error, conf.low, conf.high, p.value) %>%
  slice(-2) %>%
  write.csv("scsi-parameter-estimates.csv", row.names = F)

# Plot fitted values by year
plot_data = emmeans({ref_grid(fit, at = list(year = c(0,1,3,4,14)), transform = T)}, ~ year, adjust = "none") %>%
  as_tibble() %>%
  clean_names()
plot_data %>% ggplot() +
  geom_errorbar(aes(x = as.factor(year+2004), ymin = asymp_lcl, ymax = asymp_ucl), width = 0) +
  geom_point(aes(x = as.factor(year+2004), y = rate), size = 2) +
  labs(x = "Year", y = "") +
  theme_bw(base_size = 11)

# Time since injury (within-subject) effect
confint(pairs(emmeans({ref_grid(fit, at = list(year = c(0,1)), transform = T)}, ~ year, adjust = "none")))

# Slope for year
confint(pairs(emmeans({ref_grid(fit, at = list(year = c(0,1)), transform = T)}, ~ year, adjust = "none")))

# Complete effect
confint(pairs(emmeans({ref_grid(fit, at = list(complete = c('complete','incom')), transform = T)}, ~ completeincom, adjust = "none")))



#### END