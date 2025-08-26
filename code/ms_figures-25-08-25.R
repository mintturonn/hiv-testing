
library(scales)
source(here("code/figure_specs.R"))
# 
# fit_df %>%
#   dplyr::select(starts_with("hiv_init")) %>%
#   dplyr::select(ends_with(c("1]"))) -> test

############################
# Never tested by risk group


smr_testrisk_r1 <- cbind(as.data.frame( summary(
  fit_test_model, pars = "pr_tested_r1", probs = c(0.025, 0.5, 0.975))$summary ) , 
  age = age_string, population =  pop_string , t= year_string)
colnames(smr_testrisk_r1) <- make.names(colnames(smr_testrisk_r1)) # to remove % in the col names
smr_testrisk_r1$Testing <- "Once per lifetime"

smr_testrisk_r2 <- cbind(as.data.frame( summary(
  fit_test_model, pars = "pr_tested_r2", probs = c(0.025, 0.5, 0.975))$summary ) , 
  age = age_string, population =  pop_string , t= year_string)
colnames(smr_testrisk_r2) <- make.names(colnames(smr_testrisk_r2)) # to remove % in the col names
smr_testrisk_r2$Testing <- "Annual"

rbind(smr_testrisk_r1) %>%
  filter(t == 2024) %>%
  arrange(population, age) %>%
  mutate(pop_tot = c(0.05*(d_popsize_m[2:8,24]), 0.95*(d_popsize_m[2:8,24]), d_popsize_f[2:8,24]),
         risk_pr = c(risk_pr[,3], risk_pr[,2], risk_pr[,1]),
         no_test = pop_tot * (1-risk_pr) * (1-`X50.`),
         no_testul = pop_tot * (1-risk_pr) * (1-`X2.5.`),
         no_testll = pop_tot * (1-risk_pr) * (1-`X97.5.`) ) -> out_lr

rbind(smr_testrisk_r2) %>%
  filter(t == 2024) %>%
  arrange(population, age) %>%
  mutate(pop_tot = c(0.05*(d_popsize_m[2:8,24]), 0.95*(d_popsize_m[2:8,24]), d_popsize_f[2:8,24]),
         risk_pr = c(risk_pr[,3], risk_pr[,2], risk_pr[,1]),
         no_test = pop_tot * (risk_pr) * (1-`X50.`),
         no_testul = pop_tot * (risk_pr) * (1-`X2.5.`),
         no_testll = pop_tot * (risk_pr) * (1-`X97.5.`) ) -> out_hr

out_lr %>%
  group_by(population) %>%
  summarize(no_test = sum(no_test),
            no_testul = sum(no_testul),
            no_testll = sum(no_testll))

out_hr %>%
  group_by(population) %>%
  summarize(no_test = sum(no_test),
            no_testul = sum(no_testul),
            no_testll = sum(no_testll))


rbind(smr_testrisk_r1) %>%
  filter(t == 2024) %>%
  ggplot() +
  geom_col(aes(x = age, y = 100, fill = "Not tested"), 
           position = position_dodge(width = 0.9),  width = 0.7, alpha=0.5) +
  geom_col(aes(x = age, y = 100*X50., fill = "Tested"), 
           position = position_dodge(width = 0.9), width = 0.7, alpha=0.5) +
  geom_errorbar(aes(x = age, ymin = 100 * X97.5., ymax = 100 * X2.5., group = population),
                position = position_dodge(width = 0.9), width = 0.1) +
  facet_wrap(~population, ncol=3) +
  ylim(c(0, 100)) +
  labs(x = "Year", y = "Percent") + 
  scale_fill_manual(values = c("Not tested" = "steelblue3", "Tested" = "navy")) +
  mytheme3 +
  theme(legend.position = "bottom",
        axis.text.x = element_text(angle = 45, size = 9, hjust=1, vjust=1.5 ),
        panel.spacing = unit(0.5, "cm"),
        axis.text.y = element_text(hjust = 1)) +
  guides(fill = guide_legend(title = NULL)) -> p_not_tested_risk_r1

ggsave(
  filename = here('output/ever_tested_risk_r1.png'),
  plot = p_not_tested_risk_r1,
  width = 25,# 15,
  height = 10,
  units = "cm",
  bg = "white",
)

rbind(smr_testrisk_r2) %>%
  filter(t == 2024) %>%
  ggplot() +
  geom_col(aes(x = age, y = 100, fill = "Not tested"), 
           position = position_dodge(width = 0.9),  width = 0.7, alpha=0.5) +
  geom_col(aes(x = age, y = 100*X50., fill = "Tested"), 
           position = position_dodge(width = 0.9), width = 0.7, alpha=0.5) +
  geom_errorbar(aes(x = age, ymin = 100 * X97.5., ymax = 100 * X2.5., group = population),
                position = position_dodge(width = 0.9), width = 0.1) +
  facet_wrap(~population, ncol=3) +
  ylim(c(0, 100)) +
  labs(x = "Year", y = "Percent") + 
  scale_fill_manual(values = c("Not tested" = "steelblue3", "Tested" = "navy")) +
  mytheme3 +
  theme(legend.position = "bottom",
        axis.text.x = element_text(angle = 45, size = 9, hjust=1, vjust=1.5 ),
        panel.spacing = unit(0.5, "cm"),
        axis.text.y = element_text(hjust = 1)) +
  guides(fill = guide_legend(title = NULL)) -> p_not_tested_risk_r2


ggsave(
  filename = here('output/ever_tested_risk_r2.png'),
  plot = p_not_tested_risk_r2,
  width = 25,# 15,
  height = 10,
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

rbind(smr_testrisk_r1) %>%
  filter(t == 2024) %>%
  arrange(population, age) %>%
  mutate(pop_tot = c(0.05*(d_popsize_m[2:8,24]), 0.95*(d_popsize_m[2:8,24]), d_popsize_f[2:8,24]),
         risk_pr = c(risk_pr[,3], risk_pr[,2], risk_pr[,1]),
         no_test = pop_tot * (1-risk_pr) * (1-`X50.`),
         no_testul = pop_tot * (1-risk_pr) * (1-`X2.5.`),
         no_testll = pop_tot * (1-risk_pr) * (1-`X97.5.`) ) -> out_lr

rbind(smr_testrisk_r2) %>%
  filter(t == 2024) %>%
  arrange(population, age) %>%
  mutate(pop_tot = c(0.05*(d_popsize_m[2:8,24]), 0.95*(d_popsize_m[2:8,24]), d_popsize_f[2:8,24]),
         risk_pr = c(risk_pr[,3], risk_pr[,2], risk_pr[,1]),
         no_test = pop_tot * (risk_pr) * (1-`X50.`),
         no_testul = pop_tot * (risk_pr) * (1-`X2.5.`),
         no_testll = pop_tot * (risk_pr) * (1-`X97.5.`) ) -> out_hr

out_lr %>%
  group_by(population) %>%
  summarize(no_test = sum(no_test),
            no_testul = sum(no_testul),
            no_testll = sum(no_testll))

out_hr %>%
  group_by(population) %>%
  summarize(no_test = sum(no_test),
            no_testul = sum(no_testul),
            no_testll = sum(no_testll))


smr_testrisk_r1 %>%
  filter(t == 2024) %>%
  ggplot() +
  geom_col(aes(x = age, y = 100, fill = "Not tested"), 
           position = position_dodge(width = 0.9),  width = 0.7, alpha=0.5) +
  geom_col(aes(x = age, y = 100*X50., fill = "Tested"), 
           position = position_dodge(width = 0.9), width = 0.7, alpha=0.5) +
  geom_errorbar(aes(x = age, ymin = 100 * X97.5., ymax = 100 * X2.5., group = population),
                position = position_dodge(width = 0.9), width = 0.1) +
  facet_wrap(~population, ncol = 3) +
  ylim(c(0, 100)) +
  labs(x = "Year", y = "Percent") + 
  scale_fill_manual(values = c("Not tested" = "steelblue3", "Tested" = "navy")) +
  mytheme3 +
  theme(legend.position = "bottom",
        axis.text.x = element_text(angle = 45, size = 9, hjust=1, vjust=1.5 ),
        panel.spacing = unit(0.5, "cm"),
        axis.text.y = element_text(hjust = 1)) +
  guides(fill = guide_legend(title = NULL)) -> p_not_tested12m_risk1


ggsave(
  filename = here('output/past12m_not_tested_risk_1.png'),
  plot = p_not_tested12m_risk1,
  width = 25,# 15,
  height = 10,
  units = "cm",
  bg = "white",
)

#########################

smr_testrisk_r2 %>%
  filter(t == 2024) %>%
  ggplot() +
  geom_col(aes(x = age, y = 100, fill = "Not tested"), 
           position = position_dodge(width = 0.9),  width = 0.7, alpha=0.5) +
  geom_col(aes(x = age, y = 100*X50., fill = "Tested"), 
           position = position_dodge(width = 0.9), width = 0.7, alpha=0.5) +
  geom_errorbar(aes(x = age, ymin = 100 * X97.5., ymax = 100 * X2.5., group = population),
                position = position_dodge(width = 0.9), width = 0.1) +
  facet_wrap(~population, ncol = 3) +
  ylim(c(0, 100)) +
  labs(x = "Year", y = "Percent") + 
  scale_fill_manual(values = c("Not tested" = "steelblue3", "Tested" = "navy")) +
  mytheme3 +
  theme(legend.position = "bottom",
        axis.text.x = element_text(angle = 45, size = 9, hjust=1, vjust=1.5 ),
        panel.spacing = unit(0.5, "cm"),
        axis.text.y = element_text(hjust = 1)) +
  guides(fill = guide_legend(title = NULL)) -> p_not_tested12m_risk2


ggsave(
  filename = here('output/past12m_not_tested_risk_2.png'),
  plot = p_not_tested12m_risk2,
  width = 25,# 15,
  height = 10,
  units = "cm",
  bg = "white",
)

############################
# Reason for testing - totals
param_names <- names(fit_test_model)

# filtered_params3 <- param_names %>%
#   .[grepl("^test_totals", .)] 

filtered_params3 <- param_names %>%
  .[grepl("^test_totals(?!2)", ., perl = TRUE)]

test_reas0 <- rep(c("Other reasons for testing", "Testing among known PLHIV", "PrEP related testing", "Pregnancy related testing", "Testing among PWID"), 3)
sex0 <- rep(c("Women", "MSW", "MSM"), each=5)
pop_tot = rep(c( sum(d_popsize_f[2:8,24]), 0.95*sum(d_popsize_m[2:8,24]), 0.05*sum(d_popsize_m[2:8,24])), each=5)

smr_test3 <- cbind(as.data.frame( summary(
  fit_test_model, pars = filtered_params3, probs = c(0.025, 0.5, 0.975))$summary ) , reason=test_reas0, population=sex0, popsize=pop_tot )
colnames(smr_test3) <- make.names(colnames(smr_test3)) # to remove % in the col names

smr_test3 %>%
  mutate(reason=factor(reason, levels = c("Pregnancy related testing", "Testing among PWID", "PrEP related testing", "Testing among known PLHIV", "Other reasons for testing"))) %>%
  filter(mean !=0) %>%
  ggplot() +
  geom_col( aes(x = reason, y =  X50., fill = reason),  position = position_dodge(width = 0.9), alpha = 0.5, width = 0.7) +
  geom_errorbar( aes(x = reason, ymin = X97.5., ymax =  X2.5., group = reason),
                 position = position_dodge(width = 0.9), width = 0.1) +
  ylim(c(0,NA)) +
  labs(x = "Year", y = "Percent") + mytheme3 +
 # scale_y_log10(labels = comma) + 
  facet_wrap(~population, scales = "free_x") +
 scale_y_continuous(labels = comma) +
  scale_fill_manual(values = c("#009999", "#800080", "darkorange", "magenta", "navy")) +
  theme(legend.position = "none",
        axis.text.x=element_text(angle=60, size = 10, hjust=1, vjust=1),
        axis.text.y = element_text(hjust = 1) )  -> p_reason_num_tests

cbind(smr_test3$reason, signif(smr_test3$X50., digits=3),signif(smr_test3$X2.5., digits=3), signif(smr_test3$X97.5., digits=3))
cbind(smr_test3$reason, signif(smr_test3$X50., digits=3),signif(smr_test3$X2.5., digits=3), signif(smr_test3$X97.5., digits=3))

ggsave(
  filename = here('output/reason_num_tests_total-2.png'),
  plot = p_reason_num_tests,
  width = 20,# 15,
  height = 10,
  units = "cm",
  bg = "white",
)




smr_test3 %>%
  mutate(reason=factor(reason, levels = c("Pregnancy related testing", "Testing among PWID", "PrEP related testing", "Testing among known PLHIV", "Other reasons for testing"))) %>%
  filter(mean !=0) %>%
  ggplot() +
  geom_col( aes(x = reason, y =  100*X50./popsize, fill = reason),  position = position_dodge(width = 0.9), alpha = 0.5, width = 0.7) +
  geom_errorbar( aes(x = reason, ymin = 100*X97.5./popsize, ymax =  100*X2.5./popsize, group = reason),
                 position = position_dodge(width = 0.9), width = 0.1) +
  ylim(c(0,NA)) +
  labs(x = "Year", y = "Percent") + mytheme3 +
  # scale_y_log10(labels = comma) + 
  facet_wrap(~population, scales = "free_x") +
  scale_y_continuous(labels = comma) +
  scale_fill_manual(values = c("#009999", "#800080", "darkorange", "magenta", "navy")) +
  theme(legend.position = "none",
        axis.text.x=element_text(angle=60, size = 10, hjust=1, vjust=1),
        axis.text.y = element_text(hjust = 1) )  -> p_reason_rate_tests

smr_test3 %>%
  mutate(reason=factor(reason, levels = c("Pregnancy related testing", "Testing among PWID", "PrEP related testing", "Testing among known PLHIV", "Other reasons for testing")),
         r = 100*X50./popsize,
         rll = 100*X2.5./popsize,
         rul = 100*X97.5./popsize,) %>%
  filter(mean !=0) 

ggsave(
  filename = here('output/reason_rate_tests_total-2.png'),
  plot = p_reason_rate_tests,
  width = 20,# 15,
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


# filtered_params_pop <- param_names %>%
#   .[grepl("^Ntot_risk\\[", .)] %>%    # Starts with "Ntot_risk["
#   .[grepl(",25\\]$", .)]              # Ends with ",25]"

age_string6 <- rep(c("15-24","25-34", "35-44", "45-54","55-64","65-74","75+"),3)
pop_string2 <- rep(c("Women", "MSW", "MSM"), each=7*2)
pop_string_pop <- rep(rep(c("Women", "MSW", "MSM"), each=7),2)
testing <- rep(rep(c("Without ", "Reported PrEP indications"), each=7), 3)
testing_pop <- rep(c("Without increased HIV acquisition risk behaviors", 
                     "With increased HIV acquisition risk behaviors"), each=7*3)

smr_test <- cbind(as.data.frame( summary(
  fit_test_model, pars = filtered_params, probs = c(0.025, 0.5, 0.975))$summary ) , 
  age = age_string6, pop =  pop_string2, testing=testing )
colnames(smr_test) <- make.names(colnames(smr_test)) # to remove % in the col names

# smr_pop_r <- cbind( as.data.frame( summary(
#   fit_test_model, pars = filtered_params_pop, probs = c(0.025, 0.5, 0.975))$summary ) , 
#   age = age_string6, pop =  pop_string_pop, testing=testing_pop ) #
# colnames(smr_pop_r) <- make.names(colnames(smr_pop_r)) # to remove % in the col names

smr_test %>%
  ggplot() +
  # geom_pointrange(aes(x=age, y = X50., ymin = X2.5., ymax = X97.5., color=testing), 
  #  position = position_jitter(width = 0.15, height = 0), alpha=0.5) +
  geom_col( aes(x = age, y =  X50., fill = testing),  position = position_dodge(width = 0.9), alpha = 0.5, width = 0.7) +
  geom_errorbar( aes(x = age, ymin = X97.5., ymax =  X2.5., group = testing),
                 position = position_dodge(width = 0.9), width = 0.1) +
  facet_wrap(~pop, ncol = age_groups) +
  ylim(c(0,NA)) +
  labs(x = "Year", y = "Percent") + mytheme3 +
  scale_y_continuous(labels = comma) +
  theme(legend.position = "bottom",
        axis.text.x=element_text(angle=45, size = 9, hjust=1, vjust=1.4))  -> p_num_tests

ggsave(
  filename = here('output/p_num_tests.png'),
  plot = p_num_tests,
  width = 20,# 15,
  height = 10,
  units = "cm",
  bg = "white",
)

########################################################
## SOMETHING WEIRD HERE!! 
# smr_pop_r %>%
#   left_join(smr_test, by=c("pop", "age", "testing")) %>%
#   mutate(rate = X50..y/X50..x,
#          rate_ll = X2.5..y/X2.5..x,
#          rate_ul = X97.5..y/X97.5..x) %>%
#   filter(age!="75+") %>%
#   mutate(age=ifelse(age=="65-74", "65+", age)) %>%
#   ggplot() +
#   geom_pointrange(aes(x=age, y = rate, ymin = rate_ll, ymax = rate_ul, color=testing), 
#                   position = position_jitter(width = 0.15, height = 0), alpha=0.5) +
#   facet_wrap(~pop, ncol = age_groups, scales = "free") +
#   ylim(c(0,NA)) +
#   labs(x = "Year", y = "Percent") + mytheme3 +
#   scale_y_continuous(labels = comma) +
#   theme(legend.position = "bottom",
#         axis.text.x=element_text(angle=45, size = 9))  -> p_num_tests_prop
# 
# ggsave(
#   filename = here('output/p_num_tests_prop.png'),
#   plot = p_num_tests_prop,
#   width = 28,# 15,
#   height = 10,
#   units = "cm",
#   bg = "white",
# )




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
  # geom_pointrange(aes(x=age, y = X50., ymin = X2.5., ymax = X97.5., color=reason), 
  #                 position = position_dodge(width = 0.2), alpha=0.5, size=0.2) +
  geom_col( aes(x = age, y =  X50., fill = interaction(testing,population)),  position = position_dodge(width = 0.9), alpha = 0.5, width = 0.7) +
  geom_errorbar( aes(x = age, ymin = X97.5., ymax =  X2.5., group = interaction(testing,population)),
                 position = position_dodge(width = 0.9), width = 0.1) +
  facet_wrap(~reason, ncol = 5, scales="free") +
  ylim(c(0,NA)) +
  labs(x = "Year", y = "Percent") + mytheme3 +
  scale_y_continuous(labels = comma) +
  scale_fill_manual(values = c("#009999", "#800080", "#00FFFF", "#FFA500", "blue", "darkorange")) +
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


