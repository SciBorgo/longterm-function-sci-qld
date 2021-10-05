

# SCI study
# Borg DN
# September, 2021

# Load data
d = read_csv("sci-dataset-22-09-21.csv") %>%
  clean_names() %>%
  mutate(
    level = as.factor(level_recoded),
    complete = as.factor(complete_recoded),
    time_since_injury_s = scale(time_since_injury, center = T, scale = T),
    age_at_injury_s = scale(age_at_injury, center = T, scale = T),
    living_status = as.factor(living_status_recoded),
    year = year-2004
  ) %>%
  drop_na(cim)

# Community integration measure
# By year
dsum = d %>% group_by(year) %>%
  summarise(mean = mean(cim, na.rm = T))
d %>% ggplot() +
  geom_line(aes(x = year, y = cim, group = participant_id), alpha = .2) +
  geom_line(data = dsum, aes(x = year, y = mean), colour = "black", size = 1) +
  theme_bw()


# Transform to 0-1 interval
hist(d$cim)
d$cim_beta = (d$cim-10)/(50-10)
d$cim_beta[d$cim_beta == 1] = 0.99999 # Shift 1s off the boundary
hist(d$cim_beta, breaks = 20)

# Demean
d = cbind(d, demean(d, select = "time_since_injury", group = "participant_id"))

# Standardize age
mu = mean(d$age_at_injury, na.rm = T)
d$age_at_injury_s = d$age_at_injury-mu

# Beta regression model
fit = gam(cim_beta ~ age_at_injury_s + year + weight + level + complete + living_status +
            time_since_injury_within + time_since_injury_between + s(participant_id, bs = 're') + s(participant_id, year, bs = 're'),
          family = betar(link = "logit"),
          data = d)

# Diagnostics
res = residuals(fit)
hist(res, breaks = 50)
car::qqPlot(res)

# Summary
summary(fit)

# Save estimates
mgcv.helper::confint.gam(fit) %>%
  clean_names() %>%
  select(-statistic) %>%
  slice(-1) %>%
  write.csv("cim-parameter-estimates.csv", row.names = F)

# Plot fitted values
plot_data = (emmeans({ref_grid(fit, at = list(year = c(0,1,2,3,4,14)), transform = T)}, ~ year, adjust = "none")) %>%
  as_tibble() %>%
  clean_names()

plot_data %>% ggplot() +
  geom_errorbar(aes(x = as.factor(year+2004), ymin = (lower_cl*(50-10))+10, ymax = (upper_cl*(50-10))+10), width = 0) +
  geom_point(aes(x = as.factor(year+2004), y = (response*(50-10))+10), size = 2) +
  labs(x = "Year", y = "CIM (10-50)") + 
  theme_bw(base_size = 11)



#### END