


#### Plot model coefficients
library(bayesplot)
library(ggplot2)
library(cowplot)

color_scheme_set("gray")

# Note: run all models and save them; they will need to be called here, before each plot.

# Load saved from model
plot1 <- mcmc_areas(fit, pars = c("b_year"), point_est = "mean", prob = 0.66, prob_outer = 0.95, transformations = "exp") + xlab("Effect of year (odds ratio)") + ylab("Density") + theme_bw(base_size = 16) +
  theme(panel.grid.minor.x = element_blank(),
        panel.grid.minor.y = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.grid.major.y = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank()) + ggtitle("WHOQOL Global Quality of Life") +
  vline_at(1, linetype = "dashed") + scale_x_continuous(limits = c(0.93,1.05), breaks = seq(0.93,1.05, by = 0.02)) 
# Load saved from model
plot2 <- mcmc_areas(fit, pars = c("b_year"), point_est = "mean", prob = 0.66, prob_outer = 0.95, transformations = "exp") + xlab("Effect of year (odds ratio)") + ylab("Density") + theme_bw(base_size = 16) +
  theme(panel.grid.minor.x = element_blank(),
        panel.grid.minor.y = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.grid.major.y = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank()) + ggtitle("WHOQOL Health Satisfaction") +
  vline_at(1, linetype = "dashed") + scale_x_continuous(limits = c(0.91,1.03), breaks = seq(0.91,1.03, by = 0.02))
# Load saved from model
plot3 <- mcmc_areas(fit, pars = c("b_year"), point_est = "mean", prob = 0.66, prob_outer = 0.95, transformations = "exp") + xlab("Effect of year (odds ratio)") + ylab("Density") + theme_bw(base_size = 16) +
  theme(panel.grid.minor.x = element_blank(),
        panel.grid.minor.y = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.grid.major.y = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank()) + ggtitle("WHOQOL Physical") +
  vline_at(1, linetype = "dashed") + scale_x_continuous(limits = c(0.98,1.01), breaks = seq(0.98,1.01, by = 0.01))
# Load saved from model
plot4 <- mcmc_areas(fit, pars = c("b_year"), point_est = "mean", prob = 0.66, prob_outer = 0.95, transformations = "exp") + xlab("Effect of year (odds ratio)") + ylab("Density") + theme_bw(base_size = 16) +
  theme(panel.grid.minor.x = element_blank(),
        panel.grid.minor.y = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.grid.major.y = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank()) + ggtitle("WHOQOL Psychological") +
  vline_at(1, linetype = "dashed") + scale_x_continuous(limits = c(0.98,1.02), breaks = seq(0.98,1.02, by = 0.01))
# Load saved from model
plot5 <- mcmc_areas(fit, pars = c("b_year"), point_est = "mean", prob = 0.66, prob_outer = 0.95, transformations = "exp") + xlab("Effect of year (odds ratio)") + ylab("Density") + theme_bw(base_size = 16) +
  theme(panel.grid.minor.x = element_blank(),
        panel.grid.minor.y = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.grid.major.y = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank()) + ggtitle("WHOQOL Social") +
  vline_at(1, linetype = "dashed") + scale_x_continuous(limits = c(0.975,1.025), breaks = seq(0.98,1.02, by = 0.01))
# Load saved from model
plot6 <- mcmc_areas(fit, pars = c("b_year"), point_est = "mean", prob = 0.66, prob_outer = 0.95, transformations = "exp") + xlab("Effect of year (odds ratio)") + ylab("Density") + theme_bw(base_size = 16) +
  theme(panel.grid.minor.x = element_blank(),
        panel.grid.minor.y = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.grid.major.y = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank()) + ggtitle("WHOQOL Environment") +
  vline_at(1, linetype = "dashed") + scale_x_continuous(limits = c(0.99,1.03), breaks = seq(0.99,1.03, by = 0.01))
# Load saved from model
plot7 <- mcmc_areas(fit, pars = c("b_year"), point_est = "mean", prob = 0.66, prob_outer = 0.95, transformations = "exp") + xlab("Effect of year (odds ratio)") + ylab("Density") + theme_bw(base_size = 16) +
  theme(panel.grid.minor.x = element_blank(),
        panel.grid.minor.y = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.grid.major.y = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank()) + ggtitle("SCSI") +
  vline_at(1, linetype = "dashed") + scale_x_continuous(limits = c(0.98,1.04), breaks = seq(0.98,1.04, by = 0.01))
# Load saved from model
plot8 <- mcmc_areas(fit, pars = c("b_year"), point_est = "mean", prob = 0.66, prob_outer = 0.95, transformations = "exp") + xlab("Effect of year (odds ratio)") + ylab("Density") + theme_bw(base_size = 16) +
  theme(panel.grid.minor.x = element_blank(),
        panel.grid.minor.y = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.grid.major.y = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank()) + ggtitle("FIM Motor") +
  vline_at(1, linetype = "dashed") + scale_x_continuous(limits = c(0.97,1.02), breaks = seq(0.97,1.02, by = 0.01))
# Load saved from model
plot9 <- mcmc_areas(fit, pars = c("b_year"), point_est = "mean", prob = 0.66, prob_outer = 0.95, transformations = "exp") + xlab("Effect of year (odds ratio)") + ylab("Density") + theme_bw(base_size = 16) +
  theme(panel.grid.minor.x = element_blank(),
        panel.grid.minor.y = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.grid.major.y = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank()) + ggtitle("CIM") +
  vline_at(1, linetype = "dashed") + scale_x_continuous(limits = c(0.97,1.04), breaks = seq(0.97,1.04, by = 0.01))
plot_grid(plot1, plot2, plot3, plot4, plot5, plot6, plot7, plot8, plot9,
          ncol = 3, nrow = 3, align = 'v', axis = "lr",
          labels = c('(A)','(B)','(C)','(D)','(E)','(F)','(G)','(H)','(I)'), scale = 0.9, label_size = 22)
ggsave(file = "effect-of-year.png", units="in", width = 15, height = 9.5, dpi = 300)
