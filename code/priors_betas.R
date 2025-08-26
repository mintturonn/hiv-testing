
library(dplyr)
library(tidyr)
library(ggplot2)

######### PRIOR
n_prior_draws <- 50
pop_groups <- c("Women", "MSW", "MSM")
age_levels <- c("15-24", "25-34", "35-44", "45-54", "55-64", "65-74", "75+")
ag <- length(age_levels)
risk_levels <- c("LR", "HR")
yrs0 <- 1999:2024
nyears <- length(yrs0)

# Priors: shape [draw, age, group]
prior_beta_scr <- array( c(rbeta(n_prior_draws * 4, 2, 10), 
                           rbeta(n_prior_draws * 3, 1, 10), 
                           rbeta(n_prior_draws * 4, 2, 10), 
                           rbeta(n_prior_draws * 3, 1, 10),
                           rbeta(n_prior_draws * 5, 2, 5), 
                           rbeta(n_prior_draws * 2, 1, 5)), 
                          dim = c(n_prior_draws, ag, 3))

# RR for 3 HR intervals
prior_beta_rr1 <- matrix(rbeta(n_prior_draws, 1, 10), nrow=n_prior_draws, ncol=3)
prior_beta_rr2 <- matrix(rbeta(n_prior_draws, 1, 10), nrow=n_prior_draws, ncol=3)
prior_beta_rr3 <- matrix(rbeta(n_prior_draws, 1, 10), nrow=n_prior_draws, ncol=3)

# RR"LR"
prior_rr_lr     <- matrix(rbeta(n_prior_draws*3, 1, 3), nrow=n_prior_draws, ncol=3) # low risk, always <1

prior_list <- list()

for (i in 1:n_prior_draws) {
  for (a in 1:ag) {
    age_lab <- age_levels[a]
    for (g in 1:3) {
      group_lab <- pop_groups[g]
      beta_scr <- prior_beta_scr[i,a,g]

      beta_hr1 <- beta_scr * (1 + prior_beta_rr1[i,g])
      beta_hr2 <- beta_hr1 * (1 + prior_beta_rr2[i,g])
      beta_hr3 <- beta_hr2 * (1 + prior_beta_rr3[i,g])
      
      for (r in 1:2) {
        risk_lab <- risk_levels[r]
        for (y in 1:nyears) {
          year <- yrs0[y]
          if(year <= 2005) {
            beta_hr <- beta_scr + (beta_hr1 - beta_scr) * (year - 1999) / (2005 - 1999)
          } else if(year <= 2014) {
            beta_hr <- beta_hr1 + (beta_hr2 - beta_hr1) * (year - 2005) / (2014 - 2005)
          } else {
            beta_hr <- beta_hr2 + (beta_hr3 - beta_hr2) * (year - 2014) / (2024 - 2014)
          }
          # LR/HR
          if(risk_lab == "HR") {
            beta_val <- beta_hr
          } else {
            beta_val <- prior_rr_lr[i,g] * beta_hr
          }
          prior_list[[length(prior_list)+1]] <- data.frame(
            draw = i,
            Age  = age_lab,
            Group = group_lab,
            Risk = risk_lab,
            Year = year,
            beta = beta_val,
            beta2_retest_neg =  beta_val,
            beta2_retest_pos = prior_rr_hiv[i,g] * beta_val
          )
        }
      }
    }
  }
}
prior_df <- bind_rows(prior_list)
prior_df$draw <- as.factor(prior_df$draw)
prior_df$source <- "Prior"
prior_df$Age <- factor(prior_df$Age, levels = age_levels)

####### POSTERIOR

params_needed <- c(
  "beta_scr",        # Should be samples x age_groups x group (say N x ag x 3)
  "beta_rr1_msm","beta_rr2_msm","beta_rr3_msm",
  "beta_rr1_msw","beta_rr2_msw","beta_rr3_msw",
  "beta_rr1_w", "beta_rr2_w", "beta_rr3_w",
  "rr_beta2_msm",  "rr_lr_msm",
  "rr_beta2_msw", "rr_lr_msw",
  "rr_beta2_w",  "rr_lr_w") # "rr_hiv_msm","rr_hiv_msw", "rr_hiv_w",

post <- rstan::extract(fit_test_model, pars = params_needed)
# Now post is a list with one entry per parameter, each a matrix/array of posterior draws.

n_post_draws <- dim(post$beta_scr)[1] # number of posterior draws
ag <- dim(post$beta_scr)[2]           # number of age groups
pop_groups <- c("Women", "MSW", "MSM")
age_levels <- c("15-24", "25-34", "35-44", "45-54", "55-64", "65-74", "75+")
yrs0 <- 1999:2024
nyears <- length(yrs0)
risk_levels <- c("LR", "HR")

posterior_list <- list()

for (i in 1:n_post_draws) {
  for (a in 1:ag) {
    for (g in 1:3) {
      # Group-specific RRs
      if (g==1) { # Women
        beta_scr <- post$beta_scr[i,a,g]
        beta_rr1 <- post$beta_rr1_w[i]
        beta_rr2 <- post$beta_rr2_w[i]
        beta_rr3 <- post$beta_rr3_w[i]
        rr_beta2 <- post$rr_beta2_w[i]
        rr_hiv   <- post$rr_hiv_w[i]
        rr_lr    <- post$rr_lr_w[i]
      } else if (g==2) { # MSW
        beta_scr <- post$beta_scr[i,a,g]
        beta_rr1 <- post$beta_rr1_msw[i]
        beta_rr2 <- post$beta_rr2_msw[i]
        beta_rr3 <- post$beta_rr3_msw[i]
        rr_beta2 <- post$rr_beta2_msw[i]
        rr_hiv   <- post$rr_hiv_msw[i]
        rr_lr    <- post$rr_lr_msw[i]
      } else if (g==3) { # MSM
        beta_scr <- post$beta_scr[i,a,g]
        beta_rr1 <- post$beta_rr1_msm[i]
        beta_rr2 <- post$beta_rr2_msm[i]
        beta_rr3 <- post$beta_rr3_msm[i]
        rr_beta2 <- post$rr_beta2_msm[i]
        rr_hiv   <- post$rr_hiv_msm[i]
        rr_lr    <- post$rr_lr_msm[i]
      }
      
      beta_hr1 <- beta_scr * (1 + beta_rr1)
      beta_hr2 <- beta_hr1 * (1 + beta_rr2)
      beta_hr3 <- beta_hr2 * (1 + beta_rr3)
      
      for (r in 1:2) {
        for (y in 1:nyears) {
          year <- yrs0[y]
          # Piecewise HR trajectory
          if (year <= 2005) {
            beta_hr <- beta_scr + (beta_hr1 - beta_scr) * (year - 1999) / (2005 - 1999)
          } else if (year <= 2014) {
            beta_hr <- beta_hr1 + (beta_hr2 - beta_hr1) * (year - 2005) / (2014 - 2005)
          } else {
            beta_hr <- beta_hr2 + (beta_hr3 - beta_hr2) * (year - 2014) / (2024 - 2014)
          }
          # Risk level
          if (r == 2) { # HR
            beta_val <- beta_hr
            risk_name <- "HR"
          } else {      # LR
            beta_val <- rr_lr * beta_hr
            risk_name <- "LR"
          }
          posterior_list[[length(posterior_list)+1]] <- data.frame(
            draw  = i,
            Age   = age_levels[a],
            Group = pop_groups[g],
            Risk  = risk_name,
            Year  = year,
            beta  = beta_val,
            beta2_retest_neg =  beta_val
            #beta2_retest_pos = rr_hiv * beta_val
          )
        }
      }
    }
  }
}

posterior_df <- dplyr::bind_rows(posterior_list)
posterior_df$draw <- as.factor(posterior_df$draw)
posterior_df$source <- "Posterior"
posterior_df$aes_color <- as.character(posterior_df$Group)
posterior_df$Age <- factor(posterior_df$Age, levels=age_levels)

#### PLOT

plot_df <- bind_rows(prior_df, posterior_df)


plot_df %>%
  filter(Risk == "HR") %>%
ggplot(aes(x=Year, y=beta, group=interaction(draw, source), color=source)) +
  geom_line(size=0.3, alpha=0.5) +
  facet_wrap(~Group+Age, ncol=7) +
  scale_color_manual(values = c("Posterior" = "steelblue1", "Prior" = "grey70")) +
  labs(y = "Test probability",
       title = "Annual test probability in population with increased HIV testing need", color = "Group", linetype="") +
  theme_minimal(base_size = 14) +
  theme(panel.grid = element_blank(), 
        legend.position = "right",
        axis.text.x = element_text(angle = 90)) +
  coord_cartesian(ylim = c(0, 1)) -> betas_hr_plot

plot_df %>%
  filter(Risk == "LR") %>%
  ggplot(aes(x=Year, y=beta, group=interaction(draw, source), color=source)) +
  geom_line(size=0.3, alpha=0.5) +
  facet_wrap(~Group+Age, ncol=7) +
  scale_color_manual(values = c("Posterior" = "steelblue1", "Prior" = "grey80")) +
  labs(y = "Test probability",
       title = "Annual test probability in population with lower HIV testing need", color = "Group", linetype="") +
  theme_minimal(base_size = 14) +
  theme(panel.grid = element_blank(), 
        legend.position = "right",
        axis.text.x = element_text(angle = 90)) +
  coord_cartesian(ylim = c(0, 1)) -> betas_lr_plot
  
ggsave(
  filename = here('output/prior-post-betas-hr.png'),
  plot = betas_hr_plot,
  width = 30,# 15,
  height = 20,
  units = "cm",
  bg = "white",
)

ggsave(
  filename = here('output/prior-post-betas-lr.png'),
  plot = betas_lr_plot,
  width = 30,# 15,
  height = 20,
  units = "cm",
  bg = "white",
)



