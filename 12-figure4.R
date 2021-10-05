
# SCI study
# Borg DN
# September, 2021

# Secondary conditions
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

# Model
dsub = panel_data(d, id = participant_id, wave = year)

fit = wbm(scsi ~ time_since_injury | age_at_injury_s + year + weight + level + complete + living_status | (year | participant_id),
          data = dsub,
          model = "w-b",
          family = poisson(link = "log"),
          control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 2e5)))

# Plot fitted values
plot_data = (emmeans({ref_grid(fit, at = list(year = c(0,1,2,3,4,14)), transform = T)}, ~ year, adjust = "none")) %>%
  as_tibble() %>%
  clean_names()

plot_data %>% ggplot() +
  geom_errorbar(aes(x = as.factor(year+2004), ymin = asymp_lcl, ymax = asymp_ucl), width = 0.15) +
  geom_point(aes(x = as.factor(year+2004), y = rate), size = 2) +
  labs(x = "Year", y = "SCSI (0-45)") +
  theme_bw(base_size = 11) +
  scale_y_continuous(limit = c(0,16.25), n.breaks = 6) +
  theme(strip.text.x = element_text(size = 10)) +
  facet_grid(~"Secondary Conditions") -> p_scsi




# Functional independence measure
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

# Transform to 0-1 interval
hist(d$fim)
d$fim_beta = (d$fim-7)/(91-7)
d$fim_beta[d$fim_beta == 1] = 0.99999 # Shift 1s off the boundary
hist(d$fim_beta, breaks = 20)

# Demean
d = cbind(d, demean(d, select = "time_since_injury", group = "participant_id"))

# Standardize age at injury
mu = mean(d$age_at_injury, na.rm = T)
d$age_at_injury_s = d$age_at_injury-mu


# Beta regression model
fit = gam(fim_beta ~ age_at_injury_s + year + weight + level + complete + living_status +
            time_since_injury_within + time_since_injury_between + s(participant_id, bs = 're') + s(participant_id, year, bs = 're'),
          family = betar(link = "logit"),
          data = d)

# Plot fitted values
plot_data = (emmeans({ref_grid(fit, at = list(year = c(0,1,2,3,4,14)), transform = T)}, ~ year, adjust = "none")) %>%
  as_tibble() %>%
  clean_names()

plot_data %>% ggplot() +
  geom_errorbar(aes(x = as.factor(year+2004), ymin = (lower_cl*(91-7))+7, ymax = (upper_cl*(91-7))+7), width = 0.15) +
  geom_point(aes(x = as.factor(year+2004), y = (response*(91-7))+7), size = 2) +
  labs(x = "Year", y = "FIM Motor (7-91)") + 
  theme_bw(base_size = 11) +
  ylim(55,77.5) + 
  theme(strip.text.x = element_text(size = 10)) +
  facet_grid(~"Physical Functioning") -> p_fim





# Community integration measure
# Load data
d = read_csv("sci-dataset-22-09-21.csv") %>%
  clean_names() %>%
  mutate(
    level = as.factor(level_recoded),
    complete = as.factor(complete_recoded),
    living_status = as.factor(living_status_recoded),
    year = year-2004
  ) %>%
  drop_na(cim)

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

# Plot fitted values
plot_data = (emmeans({ref_grid(fit, at = list(year = c(0,1,2,3,4,14)), transform = T)}, ~ year, adjust = "none")) %>%
  as_tibble() %>%
  clean_names()

plot_data %>% ggplot() +
  geom_errorbar(aes(x = as.factor(year+2004), ymin = (lower_cl*(50-10))+10, ymax = (upper_cl*(50-10))+10), width = 0.15) +
  geom_point(aes(x = as.factor(year+2004), y = (response*(50-10))+10), size = 2) +
  labs(x = "Year", y = "CIM (10-50)") +
  ylim(36,46) +
  theme_bw(base_size = 11) +
  theme(strip.text.x = element_text(size = 10)) +
  facet_grid(~"Community Integration Measure") -> p_cim



# Panel plot
plot_grid(p_scsi ,
          p_fim,
          p_cim,
          ncol = 2,
          nrow = 2, 
          scale = 0.95,
          align = 'v',
          axis = "lr")
#ggsave(file = "figure4-scsi-fim-cim.png", units="in", width = 7, height = 5.5, dpi = 300)
ggsave(file = "Figure-4.tiff", units="in", width = 7, height = 5.5, dpi = 300, compression = "lzw")




#### END