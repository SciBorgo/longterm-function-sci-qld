

# SCI study
# Borg DN
# September, 2021

# WD
setwd("~/Downloads/Project - MK SCI analysis")

# Load data
d = read_csv("sci-dataset-10-09-21.csv") %>%
  clean_names() %>%
  mutate(
    level = as.factor(level_recoded),
    complete = as.factor(complete_recoded),
    year = year-2004,
    age_at_injury_s = scale(age_at_injury, center = T, scale = T),
    gender = as.factor(gender_recoded)
  ) %>%
  select(participant_id, year, complete, level, age_at_injury_s, gender, qol, time_since_injury)

# Labeling for loss to follow up
d$qol[is.na(d$qol)] <- 0

# Demean
d = cbind(d, demean(d, select = "time_since_injury", group = "participant_id"))
d$time_since_injury_between_s = scale(d$time_since_injury_between, center = T, scale = T)
d$time_since_injury_within_s = scale(d$time_since_injury_within, center = F, scale = T)

d_ipcw = d %>%
  mutate(
    qol_cut = cut(qol, breaks = c(-1,0,5)),
    qol_cut = as.factor(qol_cut),
    lost_to_follow_up = recode_factor(qol_cut, `(0,5]` = "0", `(-1,0]` = "1")
  )


# Model
inverse_prob_cens_weights = ipw(Surv(year, lost_to_follow_up==0) ~ level + complete + gender + age_at_injury_s + time_since_injury_within_s + time_since_injury_between_s,
                                cluster = "participant_id",
                                data = d_ipcw,
                                weight.name = "weight")

# Plot weights
inverse_prob_cens_weights %>%
  ggplot(aes(x = as.factor(year), y = weight)) +
  geom_boxplot() +
  geom_jitter(width = 0.15)

# Save weights
d_weights = inverse_prob_cens_weights %>%
  select(participant_id, weight, year) %>%
  mutate(year = year+2004)

d_full = read_csv("sci-dataset-10-09-21.csv") %>%
  clean_names() %>%
  select(-weight)

merge(d_full, d_weights, by = c("participant_id","year")) %>%
  write.csv("sci-dataset-22-09-21.csv", row.names = F)


#### END



