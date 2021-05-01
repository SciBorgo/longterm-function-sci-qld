

#### Loss to follow-up table
library(dplyr)
library(janitor)
library(tidyverse)
library(readxl)
library(brms)
library(tidyverse)
library(cowplot)

d = read.csv("attrition-table-analysis.csv") %>%
  clean_names()

# 2008
d %>% ggplot(aes(x = att_2008, y = fim)) + geom_boxplot() + geom_jitter(width = 0.1) + theme_bw()
table(d$att_2008)

# Age and age at injury
d$variable = d$age
d$variable = d$age_at_injury

d %>% group_by(att_2008) %>%
  summarise(
    median_age = median(variable),
    quanitle_25 = quantile(variable, probs = 0.25),
    quanitle_75 = quantile(variable, probs = 0.75)
  )

d %>% group_by(att_2008, gender_recoded) %>%
  summarise(n = n()) %>%
  mutate(freq = n / sum(n))

d %>% group_by(att_2008, level_recoded) %>%
  summarise(n = n()) %>%
  mutate(freq = n / sum(n))

d %>% group_by(att_2008, complete_recoded) %>%
  summarise(n = n()) %>%
  mutate(freq = n / sum(n))

d %>% group_by(att_2008) %>%
  summarise(
    median_age = median(scsi),
    quanitle_25 = quantile(scsi, probs = 0.25),
    quanitle_75 = quantile(scsi, probs = 0.75)
  )

d %>% group_by(att_2008) %>%
  summarise(
    median_age = median(fim),
    quanitle_25 = quantile(fim, probs = 0.25),
    quanitle_75 = quantile(fim, probs = 0.75)
  )

# Analysis
d_2008 <- d %>% mutate(
  att_2008 = as.factor(att_2008),
  att_2008 = recode_factor(att_2008, 'complete' = 'complete', 'lost' = 'non-complete', 'dead' = 'non-complete')
)

# SCSI
d_2008 %>% group_by(att_2008) %>%
  summarise(median=median(scsi))
fit = wilcox.test(scsi ~ att_2008, data = d_2008, exact = F)
fit$p.value
effsize::cliff.delta(d = d_2008$scsi, f = d_2008$att_2008, conf.level = .95)

# FIM
fit = wilcox.test(fim ~ att_2008, data = d_2008, exact = F)
fit$p.value
effsize::cliff.delta(d = d_2008$fim, f = d_2008$att_2008, conf.level = .95)



# 2018
d %>% ggplot(aes(x = att_2018, y = fim)) + geom_boxplot() + geom_jitter(width = 0.1) + theme_bw()
table(d$att_2018)

# Age and age at injury
d$variable = d$age
d$variable = d$age_at_injury

d %>% group_by(att_2018) %>%
  summarise(
    median_age = median(variable),
    quanitle_25 = quantile(variable, probs = 0.25),
    quanitle_75 = quantile(variable, probs = 0.75)
  )

d %>% group_by(att_2018, gender_recoded) %>%
  summarise(n = n()) %>%
  mutate(freq = n / sum(n))

d %>% group_by(att_2018, level_recoded) %>%
  summarise(n = n()) %>%
  mutate(freq = n / sum(n))

d %>% group_by(att_2018, complete_recoded) %>%
  summarise(n = n()) %>%
  mutate(freq = n / sum(n))

d %>% group_by(att_2018) %>%
  summarise(
    median_age = median(scsi),
    quanitle_25 = quantile(scsi, probs = 0.25),
    quanitle_75 = quantile(scsi, probs = 0.75)
  )

d %>% group_by(att_2018) %>%
  summarise(
    median_age = median(fim),
    quanitle_25 = quantile(fim, probs = 0.25),
    quanitle_75 = quantile(fim, probs = 0.75)
  )

# Analysis
d_2018 <- d %>% mutate(
  att_2018 = as.factor(att_2018),
  att_2018 = recode_factor(att_2018, 'complete' = 'complete', 'lost' = 'non-complete', 'dead' = 'non-complete')
)

# SCSI
d_2018 %>% group_by(att_2018) %>%
  summarise(median=median(scsi))

fit = wilcox.test(scsi ~ att_2018, data = d_2018, exact = F)
fit$p.value
effsize::cliff.delta(d = d_2018$scsi, f = d_2018$att_2018, conf.level = .95)

# FIM
fit = wilcox.test(fim ~ att_2018, data = d_2018, exact = F)
fit$p.value
effsize::cliff.delta(d = d_2018$fim, f = d_2018$att_2018, conf.level = .95)




# Supplement plot for attrition analysis
d_2008 <- read.csv("attrition-table-analysis.csv") %>%
  mutate(
    group_2008 = as.factor(group_2008),
    group_2008 = relevel(group_2008, ref = "TRUE")
  )

ggplot(data = d_2008, aes(x = group_2008, y = scsi)) + 
  geom_boxplot(outlier.size = -1) +
  geom_jitter(width = 0.1, size = 3, alpha = 0.25) +
  theme_bw(base_size = 14) +
  labs(x = "Survey Response in 2008", y = "SCSI in 2004 (0-45)") -> plot1

ggplot(data = d_2008, aes(x = group_2008, y = fim)) + 
  geom_boxplot(outlier.size = -1) +
  geom_jitter(width = 0.1, size = 3, alpha = 0.25) +
  theme_bw(base_size = 14) +
  labs(x = "Survey Response in 2008", y = "FIM Motor in 2004 (7-91)") -> plot2

# 2018
d_2018 <- read.csv("attrition-table-analysis.csv") %>%
  mutate(
    group_2018 = as.factor(group_2018),
    group_2018 = relevel(group_2018, ref = "TRUE")
  )

ggplot(data = d_2018, aes(x = group_2018, y = scsi)) + 
  geom_boxplot(outlier.size = -1) +
  geom_jitter(width = 0.1, size = 3, alpha = 0.25) +
  theme_bw(base_size = 14) +
  labs(x = "Survey Response in 2018", y = "SCSI in 2004 (0-45)") -> plot3

ggplot(data = d_2018, aes(x = group_2018, y = fim)) + 
  geom_boxplot(outlier.size = -1) +
  geom_jitter(width = 0.1, size = 3, alpha = 0.25) +
  theme_bw(base_size = 14) +
  labs(x = "Survey Response in 2018", y = "FIM Motor in 2004 (7-91)") -> plot4


# Panel plot
plot_grid(plot1, plot2, plot3, plot4,
          ncol = 2, nrow = 2,
          labels = c('(A)','(B)','(C)','(D)'),
          align = 'v', axis = "lr",
          label_size = 16, scale = 0.95)
ggsave(file = "supplement-plot-attrition.png", units="in", width = 7.5, height = 7, dpi = 300)
