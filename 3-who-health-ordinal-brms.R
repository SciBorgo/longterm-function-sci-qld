
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
  drop_na(who_health)

# Plot outcome
ggplot() + geom_histogram(data = d, aes(x = who_health))
ggplot() + geom_histogram(data = d, aes(x = who_health)) + facet_wrap(~year)

# Demean
d = cbind(d, demean(d, select = "time_since_injury", group = "participant_id"))

# Model
# Fit under proportional odds assumption
fit_cumul = brm(who_health ~ year + level + complete + age_at_injury_s + weight + time_since_injury_within + time_since_injury_between + living_status + (1+year|participant_id),
               prior = set_prior("normal(0, 1)", class = "b", coef = ""),
               chains = 8,
               cores = 8,
               iter = 2000,
               control = list(adapt_delta = 0.95),
               data = d,
               cumulative(link = "logit"))
save(fit_cumul, file = "fit_cumul_who_health.RData")
fit_cumul <- add_criterion(fit_cumul, "loo")


# Fit adjacent category model
fit_acat = brm(who_health ~ year + level + complete + age_at_injury_s + weight + time_since_injury_within + time_since_injury_between + living_status + (1+year|participant_id),
              prior = set_prior("normal(0, 1)", class = "b", coef = ""),
              chains = 8,
              cores = 8,
              iter = 2000,
              control = list(adapt_delta = 0.95),
              data = d,
              acat(link = "logit"))
save(fit_acat, file = "fit_acat_who_health.RData")
fit_acat <- add_criterion(fit_acat, "loo")


# Compare models
loo(fit_cumul)
loo(fit_acat)

# Posterior predictive check
pp_check(fit_cumul, re_formula = NULL, ndraws = 100)

# Summary
summary(fit_cumul)
print(fit_cumul, digits = 3)

# Plot
d_plot = d %>% select(participant_id, year, who_health) %>%
  mutate(year = year + 2004) %>%
  group_by(year,who_health) %>%
  count() %>%
  group_by(year) %>% 
  mutate(freq=n/sum(n)) %>%
  ungroup() %>%
  mutate_if(is.numeric, round, 2)

d_plot %>%
  mutate(year = as.factor(year),
         `Health satisfaction` = factor(who_health, levels = c("5","4","3","2","1"))) %>%
  ggplot(aes(y = year, x = freq, fill = `Health satisfaction`)) +
  geom_col(position = "fill", colour = "black", size = 0.25) +
  theme_light(base_size = 11) +
  labs(x = "Percentage", y = "Year") +
  scale_x_continuous(labels = scales::percent_format()) +
  scale_y_discrete() +
  scale_fill_manual(values = c("white","grey80","grey60","grey50","black"),
                    labels = c("Very\nsatisfied", "Satisfied","Neutral","Dissatisfied","Very\ndissatisfied"),
                    name = "Health\nsatisfaction") +
  theme(legend.position = "bottom") +
  guides(fill = guide_legend(reverse = T)) +
  geom_text(aes(label = freq*100), color = "black", size = 2.75, position = position_stack(vjust = 0.5))
#ggsave("health.png", units = "in", width = 6, height = 3, dpi = 300)
ggsave("Figure-2.tiff", units = "in", width = 6, height = 3, dpi = 600, compression = "lzw")

