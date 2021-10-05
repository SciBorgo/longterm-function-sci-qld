
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

# Model
dsub = panel_data(d, id = participant_id, wave = year)

fit = wbm(who_physical ~ time_since_injury | age_at_injury_s + year + weight + level + complete + living_status | (year | participant_id),
          data = dsub, model = "w-b")

# Plot fitted values
plot_data = (emmeans({ref_grid(fit, at = list(year = c(0,1,2,3,4,14)), transform = T)}, ~ year, adjust = "none")) %>%
  as_tibble() %>%
  clean_names()

plot_data %>% ggplot() +
  geom_errorbar(aes(x = as.factor(year+2004), ymin = lower_cl, ymax = upper_cl), width = 0.15) +
  geom_point(aes(x = as.factor(year+2004), y = emmean), size = 2) +
  labs(x = "Year", y = "Physical (0-100)") +
  ylim(35,100) +
  theme_bw(base_size = 11) +
  theme(strip.text.x = element_text(size = 10)) +
  facet_grid(~"WHOQOL-Bref Physical") -> p_physical

# WHO Psych
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
  drop_na(who_psych)

# Model
dsub = panel_data(d, id = participant_id, wave = year)

fit = wbm(who_psych ~ time_since_injury | age_at_injury_s + year + weight + level + complete + living_status | (year | participant_id),
          data = dsub, model = "w-b",
          control = lmerControl(optimizer = "Nelder_Mead"))

# Plot fitted values
plot_data = (emmeans({ref_grid(fit, at = list(year = c(0,1,2,3,4,14)), transform = T)}, ~ year, adjust = "none")) %>%
  as_tibble() %>%
  clean_names()
plot_data[plot_data > 100] = 100

plot_data %>% ggplot() +
  geom_errorbar(aes(x = as.factor(year+2004), ymin = lower_cl, ymax = upper_cl), width = 0.15) +
  geom_point(aes(x = as.factor(year+2004), y = emmean), size = 2) +
  labs(x = "Year", y = "Psychological (0-100)") +
  ylim(50,100) +
  theme_bw(base_size = 11) +
  theme(strip.text.x = element_text(size = 10))  +
  facet_grid(~"WHOQOL-Bref Psychological") -> p_psych
p_psych


# WHO Social
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
  drop_na(who_social)

# Model
dsub = panel_data(d, id = participant_id, wave = year)

fit = wbm(who_social ~ time_since_injury | age_at_injury_s + year + weight + level + complete + living_status | (year | participant_id),
          data = dsub, model = "w-b")

# Plot fitted values
plot_data = (emmeans({ref_grid(fit, at = list(year = c(0,1,2,3,4,14)), transform = T)}, ~ year, adjust = "none")) %>%
  as_tibble() %>%
  clean_names()

plot_data %>% ggplot() +
  geom_errorbar(aes(x = as.factor(year+2004), ymin = lower_cl, ymax = upper_cl), width = 0.15) +
  geom_point(aes(x = as.factor(year+2004), y = emmean), size = 2) +
  labs(x = "Year", y = "Social (0-100)") +
  ylim(40,100) +
  theme_bw(base_size = 11) +
  theme(strip.text.x = element_text(size = 10)) +
  facet_grid(~"WHOQOL-Bref Social") -> p_social


# WHO Environment
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
  drop_na(who_environment)

# Model
dsub = panel_data(d, id = participant_id, wave = year)

fit = wbm(who_environment ~ time_since_injury | age_at_injury_s + year + weight + level + complete + living_status | (year | participant_id),
          data = dsub, model = "w-b",
          control = lmerControl(optimizer = "Nelder_Mead"))

# Plot fitted values
plot_data = (emmeans({ref_grid(fit, at = list(year = c(0,1,2,3,4,14)), transform = T)}, ~ year, adjust = "none")) %>%
  as_tibble() %>%
  clean_names()

plot_data %>% ggplot() +
  geom_errorbar(aes(x = as.factor(year+2004), ymin = lower_cl, ymax = upper_cl), width = 0.15) +
  geom_point(aes(x = as.factor(year+2004), y = emmean), size = 2) +
  labs(x = "Year", y = "Environment (0-100)") +
  ylim(60,100.5) +
  theme_bw(base_size = 11) +
  theme(strip.text.x = element_text(size = 10)) +
  facet_grid(~"WHOQOL-Bref Environment") -> p_enviro


# Panel plot
plot_grid(p_physical,
          p_psych,
          p_social,
          p_enviro,
          ncol = 2,
          nrow = 2, 
          scale = 0.95,
          align = 'v',
          axis = "lr")
#ggsave(file = "figure3-whoqol.png", units="in", width = 7, height = 5.5, dpi = 300)
ggsave(file = "Figure-3.tiff", units="in", width = 7, height = 5.5, dpi = 300, compression = "lzw")




#### END