
library(scales)
source(here("code/figure_specs.R"))
# 
# fit_df %>%
#   dplyr::select(starts_with("hiv_init")) %>%
#   dplyr::select(ends_with(c("1]"))) -> test

############################
# Never tested by risk group


test_long_r1 <-  c(rep(NA, 14), d_tested_f_r1[1], rep(NA, 10) ,rep(NA, 14), d_tested_m_r1[1], rep(NA, 10) , rep(NA, 25), 
                   rep(NA, 14), d_tested_f_r1[2], rep(NA, 10) ,rep(NA, 14), d_tested_m_r1[2], rep(NA, 10) , rep(NA, 25),
                   rep(NA, 14), d_tested_f_r1[3], rep(NA, 10) ,rep(NA, 14), d_tested_m_r1[3], rep(NA, 10) , rep(NA, 25),
                   rep(NA, 14), d_tested_f_r1[4], rep(NA, 10) ,rep(NA, 14), d_tested_m_r1[4], rep(NA, 10) , rep(NA, 25),
                   rep(NA, 14), d_tested_f_r1[5], rep(NA, 10) ,rep(NA, 14), d_tested_m_r1[5], rep(NA, 10) , rep(NA, 25),
                   rep(NA, 25*3*2))

test_long_r2 <-  c(rep(NA, 14), d_tested_f_r2[1], rep(NA, 10) ,rep(NA, 14), d_tested_m_r2[1], rep(NA, 10) , rep(NA, 25), 
                   rep(NA, 14), d_tested_f_r2[2], rep(NA, 10) ,rep(NA, 14), d_tested_m_r2[2], rep(NA, 10) , rep(NA, 25),
                   rep(NA, 14), d_tested_f_r2[3], rep(NA, 10) ,rep(NA, 14), d_tested_m_r2[3], rep(NA, 10) , rep(NA, 25),
                   rep(NA, 14), d_tested_f_r2[4], rep(NA, 10) ,rep(NA, 14), d_tested_m_r2[4], rep(NA, 10) , rep(NA, 25),
                   rep(NA, 14), d_tested_f_r2[5], rep(NA, 10) ,rep(NA, 14), d_tested_m_r2[5], rep(NA, 10) , rep(NA, 25),
                   rep(NA, 25*3*2))

smr_testrisk_r1 <- cbind(as.data.frame( summary(
  fit_test_model, pars = "pr_tested_r1", probs = c(0.025, 0.5, 0.975))$summary ) , 
  age = age_string, population =  pop_string , t= year_string,  data = test_long_r1)
colnames(smr_testrisk_r1) <- make.names(colnames(smr_testrisk_r1)) # to remove % in the col names
smr_testrisk_r1$Testing <- "Once per lifetime"

smr_testrisk_r2 <- cbind(as.data.frame( summary(
  fit_test_model, pars = "pr_tested_r2", probs = c(0.025, 0.5, 0.975))$summary ) , 
  age = age_string, population =  pop_string , t= year_string,  data = test_long_r2)
colnames(smr_testrisk_r2) <- make.names(colnames(smr_testrisk_r2)) # to remove % in the col names
smr_testrisk_r2$Testing <- "Annual"

rbind(smr_testrisk_r1, smr_testrisk_r2) %>%
  # filter(population != "MSM") %>%
  filter(t ==2023) %>%
    ggplot() +
      geom_pointrange(aes(x=age, y = 100-100*X50., ymax = 100-100*X2.5., ymin = 100-100*X97.5., color=population), 
                      position = position_dodge(width = 0.2), alpha=0.5) +
      facet_wrap(~Testing, ncol = 2) +
      ylim(c(0,100)) +
      labs(x = "Year", y = "Percent") + mytheme3 +
      theme(legend.position = "bottom",
            axis.text.x=element_text(angle=45, size = 9)) -> p_not_tested_risk

ggsave(
  filename = here('output/not_tested_risk_msw_w.png'),
  plot = p_not_tested_risk,
  width = 30,# 15,
  height = 15,
  units = "cm",
  bg = "white",
)

############################
# Not tested in the past 12 months

smr_testrisk_r1 <- cbind(as.data.frame( summary(
  fit_test_model, pars = "pr_rec_tested_r1", probs = c(0.025, 0.5, 0.975))$summary ) , 
  age = age_string, population =  pop_string , t= year_string)
colnames(smr_testrisk_r1) <- make.names(colnames(smr_testrisk_r1)) # to remove % in the col names
smr_testrisk_r1$Testing <- "Once per lifetime"

smr_testrisk_r2 <- cbind(as.data.frame( summary(
  fit_test_model, pars = "pr_rec_tested_r2", probs = c(0.025, 0.5, 0.975))$summary ) , 
  age = age_string, population =  pop_string , t= year_string)
colnames(smr_testrisk_r2) <- make.names(colnames(smr_testrisk_r2)) # to remove % in the col names
smr_testrisk_r2$Testing <- "Annual"

smr_testrisk_r1 %>%
  filter(t ==2023) %>%
  ggplot() +
  geom_pointrange(aes(x=age, y = 100-100*X50., ymax = 100-100*X2.5., ymin = 100-100*X97.5., color=population),  alpha=0.4) + 
                #  position = position_jitter(width = 0.15, height = 0), alpha=0.5) +
  facet_wrap(~population, ncol = age_groups) +
  ylim(c(0,100)) +
  labs(x = "Year", y = "Percent") + mytheme3 +
  theme(legend.position = "bottom",
        axis.text.x=element_text(angle=45, size = 9)) -> p_not_tested_risk1

ggsave(
  filename = here('output/not_tested_risk_1.png'),
  plot = p_not_tested_risk1,
  width = 25,# 15,
  height = 10,
  units = "cm",
  bg = "white",
)

smr_testrisk_r2 %>%
  filter(t ==2023) %>%
  ggplot() +
  geom_pointrange(aes(x=age, y = 100-100*X50., ymax = 100-100*X2.5., ymin = 100-100*X97.5., color=population),  alpha=0.4) + 
  #  position = position_jitter(width = 0.15, height = 0), alpha=0.5) +
  facet_wrap(~population, ncol = age_groups) +
  ylim(c(0,100)) +
  labs(x = "Year", y = "Percent") + mytheme3 +
  theme(legend.position = "bottom",
        axis.text.x=element_text(angle=45, size = 9)) -> p_not_tested_risk2

ggsave(
  filename = here('output/not_tested_risk_2.png'),
  plot = p_not_tested_risk2,
  width = 25,# 15,
  height = 10,
  units = "cm",
  bg = "white",
)



############################
# Number of tests


# fit_tidy = fit |>
#   recover_types(df) |>
#   spread_draws(P[year, race],
#                Sens, Spec,
#                Test[year, race],
#                theta[year, race],
#                pred_Positivities[year, race]) |>
#   ungroup()

# Step 1: Extract all parameter names
param_names <- names(fit_test_model)

# Step 2: Filter parameter names that start with "test_nums[" and end with ",25]"
filtered_params <- param_names %>%
  .[grepl("^test_nums\\[", .)] %>%    # Starts with "test_nums["
  .[grepl(",25\\]$", .)]              # Ends with ",25]"


filtered_params_pop <- param_names %>%
  .[grepl("^Ntot_risk\\[", .)] %>%    # Starts with "Ntot_risk["
  .[grepl(",25\\]$", .)]              # Ends with ",25]"

age_string6 <- rep(c("15-24","25-34", "35-44", "45-54","55-64","65-74","75+"),3)
pop_string2 <- rep(c("Women", "MSW", "MSM"), each=7*2)
pop_string_pop <- rep(rep(c("Women", "MSW", "MSM"), each=7),2)
testing <- rep(rep(c("Once per lifetime", "Annual"), each=7), 3)
testing_pop <- rep(c("Once per lifetime", "Annual"), each=7*3)

smr_test <- cbind(as.data.frame( summary(
  fit_test_model, pars = filtered_params, probs = c(0.025, 0.5, 0.975))$summary ) , 
  age = age_string6, pop =  pop_string2, testing=testing )
colnames(smr_test) <- make.names(colnames(smr_test)) # to remove % in the col names

smr_pop_r <- cbind( as.data.frame( summary(
  fit_test_model, pars = filtered_params_pop, probs = c(0.025, 0.5, 0.975))$summary ) , 
  age = age_string6, pop =  pop_string_pop, testing=testing_pop ) #
colnames(smr_pop_r) <- make.names(colnames(smr_pop_r)) # to remove % in the col names

smr_test %>%
  ggplot() +
  geom_pointrange(aes(x=age, y = X50., ymin = X2.5., ymax = X97.5., color=testing), 
   position = position_jitter(width = 0.15, height = 0), alpha=0.5) +
  facet_wrap(~pop, ncol = age_groups, scales = "free") +
  ylim(c(0,NA)) +
  labs(x = "Year", y = "Percent") + mytheme3 +
  scale_y_continuous(labels = comma) +
  theme(legend.position = "bottom",
        axis.text.x=element_text(angle=45, size = 9))  -> p_num_tests

ggsave(
  filename = here('output/p_num_tests.png'),
  plot = p_num_tests,
  width = 28,# 15,
  height = 10,
  units = "cm",
  bg = "white",
)

## SOMETHING WEIRD HERE!! 
smr_pop_r %>%
  left_join(smr_test, by=c("pop", "age", "testing")) %>%
  mutate(rate = X50..y/X50..x,
         rate_ll = X2.5..y/X2.5..x,
         rate_ul = X97.5..y/X97.5..x) %>%
  filter(age!="75+") %>%
  mutate(age=ifelse(age=="65-74", "65+", age)) %>%
  ggplot() +
  geom_pointrange(aes(x=age, y = rate, ymin = rate_ll, ymax = rate_ul, color=testing), 
                  position = position_jitter(width = 0.15, height = 0), alpha=0.5) +
  facet_wrap(~pop, ncol = age_groups, scales = "free") +
  ylim(c(0,NA)) +
  labs(x = "Year", y = "Percent") + mytheme3 +
  scale_y_continuous(labels = comma) +
  theme(legend.position = "bottom",
        axis.text.x=element_text(angle=45, size = 9))  -> p_num_tests_prop

ggsave(
  filename = here('output/p_num_tests_prop.png'),
  plot = p_num_tests_prop,
  width = 28,# 15,
  height = 10,
  units = "cm",
  bg = "white",
)


############################
# Reason for testing

filtered_params2 <- param_names %>%
  .[grepl("^test_reas_", .)] 

age_string7 <- rep(c("15-24","25-34", "35-44", "45-54","55-64","65-74","75+"),2*(5+4+4))
pop_string6 <- c(rep(c("Women"), 7*5*2), rep(c("MSW", "MSM"), each=7*4*2))
testing2 <- c( rep(c("Once per lifetime", "Annual"), each=5*7), 
               rep(c("Once per lifetime", "Annual"), each=4*7),
               rep(c("Once per lifetime", "Annual"), each=4*7))
test_reas <-c(rep(rep(c("Screening", "Testing among known PHIV", "PrEP related HIV testing", "Pregnancy related HIV testing", "PWID"), each=7),2),
              rep(rep(c("Screening", "Testing among known PHIV", "PrEP related HIV testing", "PWID"), each=7),2),
              rep(rep(c("Screening", "Testing among known PHIV", "PrEP related HIV testing", "PWID"), each=7),2))


smr_test2 <- cbind(as.data.frame( summary(
  fit_test_model, pars = filtered_params2, probs = c(0.025, 0.5, 0.975))$summary ) , 
  age = age_string7, population =  pop_string6, testing=testing2, reason=test_reas )
colnames(smr_test2) <- make.names(colnames(smr_test2)) # to remove % in the col names

smr_test2 %>%
  filter(reason != "PrEP related HIV testing" | testing != "Once per lifetime") %>%
  ggplot() +
  geom_pointrange(aes(x=age, y = X50., ymin = X2.5., ymax = X97.5., color=reason), 
                  position = position_dodge(width = 0.2), alpha=0.5, size=0.2) +
  facet_wrap(~population+testing, ncol = 2) +
  ylim(c(0,NA)) +
  labs(x = "Year", y = "Percent") + mytheme3 +
  scale_y_continuous(labels = comma) +
  scale_color_manual(values = c("#009999", "#800080", "#00FFFF", "#FFA500", "blue")) +
  theme(legend.position = "bottom",
        axis.text.x=element_text(angle=45, size = 9))  -> p_reason_num_tests

ggsave(
  filename = here('output/reason_num_tests.png'),
  plot = p_reason_num_tests,
  width = 30,# 15,
  height = 20,
  units = "cm",
  bg = "white",
)

smr_test2 %>%
  filter(reason != "PrEP related HIV testing" | testing != "Once per lifetime") %>%
  ggplot() +
  geom_pointrange(aes(x=age, y = X50., ymin = X2.5., ymax = X97.5., color=population), 
                  position = position_dodge(width = 0.5), alpha=0.5) +
  facet_wrap(~testing+reason, scales="free", ncol = 5) +
  ylim(c(0,NA)) +
  labs(x = "Year", y = "Percent") + mytheme3 +
  scale_y_continuous(labels = comma) +
  scale_color_manual(values = c("#009999", "#FFA500",  "blue")) +
  theme(legend.position = "bottom",
        axis.text.x=element_text(angle=45, size = 9))  -> p_reason_num_tests_2

ggsave(
  filename = here('output/reason_num_tests_2.png'),
  plot = p_reason_num_tests_2,
  width = 30,# 15,
  height = 20,
  units = "cm",
  bg = "white",
)

smr_test2 %>%
  filter(reason == "Testing among known PHIV") -> phiv

colSums(phiv[,4:6])

