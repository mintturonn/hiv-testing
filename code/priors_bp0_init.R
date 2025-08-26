

age_groups0 <- c("15-24", "25-34", "35-44", "45-54", "55-64", "65-74", "75+")
pop_groups <- c("Women", "MSW", "MSM")

params1 <- c("bp0[1,1]", "bp0[2,1]", "bp0[3,1]", "bp0[4,1]", "bp0[5,1]",
             "bp0[6,1]", "bp0[7,1]", "bp0[1,2]", "bp0[2,2]", "bp0[3,2]",
             "bp0[4,2]", "bp0[5,2]", "bp0[6,2]", "bp0[7,2]", "bp0[1,3]",
             "bp0[2,3]", "bp0[3,3]", "bp0[4,3]", "bp0[5,3]", "bp0[6,3]",
             "bp0[7,3]")

as.data.frame(rstan::extract(fit_test_model, pars = params1)) %>%
  pivot_longer(everything(), names_to = "parameter", values_to = "value") %>% 
  mutate(
    age_idx = as.integer(str_match(parameter, "bp0\\.(\\d+)\\.(\\d+)\\.")[,2]),
    pop_idx = as.integer(str_match(parameter, "bp0\\.(\\d+)\\.(\\d+)\\.")[,3]),
    Age = age_groups0[age_idx],
    Population = pop_groups[pop_idx]) %>%
  mutate(type="Posterior") -> posts1_long

# For pop_idx 1
alpha1 <- c(400, 400, 400, 200, 80, 60, 60)
beta1  <- c(600, 600, 600, 600, 600, 600, 600)

# For pop_idx 2
alpha2 <- c(190, 200, 200, 190, 80, 60, 60)
beta2  <- c(570, 240, 240, 570, 500, 500, 500)

# For pop_idx 3 (all the same)
alpha3 <- c(300, 300, 300, 300, 300, 200, 200)
beta3  <- rep(150, 7)

expand.grid(
  age_idx = 1:7,
  pop_idx = 1:3
) %>%
  # Join in the appropriate alpha, beta based on indices
  mutate(
    alpha = case_when(
      pop_idx == 1 ~ alpha1[age_idx],
      pop_idx == 2 ~ alpha2[age_idx],
      pop_idx == 3 ~ alpha3[age_idx]
    ),
    beta = case_when(
      pop_idx == 1 ~ beta1[age_idx],
      pop_idx == 2 ~ beta2[age_idx],
      pop_idx == 3 ~ beta3[age_idx]
    ),
    Age = age_groups0[age_idx],
    Population = pop_groups[pop_idx],
    parameter = sprintf("bp0[%d,%d]", age_idx, pop_idx),
    lower = qbeta(0.025, alpha, beta),
    median = qbeta(0.5, alpha, beta),
    upper = qbeta(0.975, alpha, beta),
    type = "Prior"
  ) %>%
  select(Population, Age, parameter, type, lower, median, upper) -> priors_params 


posts1_long %>%
  group_by(Population, Age, parameter, type) %>%
  summarize(
    lower = quantile(value, 0.025),
    median = median(value),
    upper = quantile(value, 0.975),
    .groups = "drop") %>%
  bind_rows(priors_params) -> summary_df

summary_df$Age <- factor(summary_df$Age, levels = rev(age_groups0))
summary_df$Population <- factor(summary_df$Population, levels = pop_groups)

summary_df <- summary_df %>%
  mutate(label = sprintf("%.3f [%.3f, %.3f]", median, lower, upper))

ggplot(summary_df, aes( x = median, xmin = lower, xmax = upper, y = Age, color = type)) +
  geom_pointrange(position = position_dodge(width = 0.2),size = 0.4) +
  geom_text(aes(label = label, x = 0), position = position_dodge(width = 0.6), size = 3, hjust = 0) +
  facet_grid(. ~ Population) +
  labs( title = "Initial conditions: Ever tested in population with increased HIV testing need",
        x = "Value",
        y = "Age group" ) +
  scale_color_manual(values = c(Prior = "#009E73", Posterior = "#D55E00")) +
  theme_minimal() +
  coord_cartesian(xlim = c(0, 0.8)) -> bp0_plot

ggsave(
  filename = here('output/prior-post-bp0.png'),
  plot = bp0_plot,
  width = 30,# 15,
  height = 20,
  units = "cm",
  bg = "white",
)


