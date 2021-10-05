
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
  drop_na(who_physical)

# WHO Physical
# By year
dsum = d %>% group_by(year) %>%
  summarise(mean = mean(who_physical, na.rm = T))
d %>% ggplot() +
  geom_line(aes(x = year, y = who_physical, group = participant_id), alpha = .2) +
  geom_line(data = dsum, aes(x = year, y = mean), colour = "black", size = 1) +
  theme_bw()


# Model
dsub = panel_data(d, id = participant_id, wave = year)

fit = wbm(who_physical ~ time_since_injury | age_at_injury_s + year + weight + level + complete + living_status | (year | participant_id),
          data = dsub, model = "w-b")

# Diagnostics
res = residuals(fit)
hist(res)
car::qqPlot(res)

# Summary
summary(fit)
tidy(fit)
confint(fit)
anova(fit)

# Save estimates
tidy(fit, conf.int = T) %>%
  filter(group %in% c("within","between")) %>%
  select(-statistic,-group) %>%
  relocate(term, estimate, std.error, conf.low, conf.high, p.value) %>%
  slice(-2) %>%
  write.csv("physical-parameter-estimates.csv", row.names = F)

# Plot fitted values
plot_data = (emmeans({ref_grid(fit, at = list(year = c(0,1,2,3,4,14)), transform = T)}, ~ year, adjust = "none")) %>%
  as_tibble() %>%
  clean_names()

plot_data %>% ggplot() +
  geom_errorbar(aes(x = as.factor(year+2004), ymin = lower_cl, ymax = upper_cl), width = 0) +
  geom_point(aes(x = as.factor(year+2004), y = emmean), size = 2) +
  labs(x = "Year", y = "WHO Physical (0-100)") +
  theme_bw(base_size = 11)


#### END