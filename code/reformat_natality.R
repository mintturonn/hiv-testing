
library(here)
library(tidyverse)
library(rriskDistributions)

source("~/hiv-testing/code/helper_funs.R")

## rate of pregnancy by age group

read.delim( here("data", "Natality, 1995-2002.txt"), header=TRUE) %>%
  rename(Age.of.Mother.9 = Age.of.Mother ) %>%
  rename(Age.of.Mother.9.Code = Age.of.Mother.Code) -> births1

births2 <- read.delim(here("data", "Natality, 2003-2006.txt"), header=TRUE)
births3 <- read.delim(here("data", "Natality, 2007-2022.txt"), header=TRUE)

rbind(births1, births2, births3) %>%
  mutate(age = ifelse(Age.of.Mother.9=="15-19 years" | Age.of.Mother.9=="20-24 years", "15-24", 
                      ifelse(Age.of.Mother.9=="25-29 years" | Age.of.Mother.9=="30-34 years", "25-34", 
                             ifelse(Age.of.Mother.9=="35-39 years" | Age.of.Mother.9=="40-44 years", "35-44", 
                                    ifelse(Age.of.Mother.9=="45-49 years" | Age.of.Mother.9=="50 years and over", "45-54", 
                                          NA ))))) %>%
  group_by(Year, age) %>%
  summarize(births = sum(Births)) %>%
  rename(year = Year) %>%
  filter(!is.na(births)) -> births

read.csv(here("data/AtlasPlusTableData_population_2000_2022.csv"), skip=7)  %>%
  mutate(year = ifelse(Year == "2020 (COVID-19 Pandemic)", 2020, as.numeric(Year))) %>%
  mutate(age = ifelse(Age.Group == "14-May", "5-14", Age.Group)) %>%
  mutate(cases = as.numeric(gsub(",", "", Cases))) %>%
  mutate(rate= as.numeric(gsub(",", "",  `Rate.per.100000`))) %>%
  mutate(population= as.numeric(gsub(",", "",  `Population`))) %>%
  select(Sex, age, year, population) %>%
  filter(Sex == "Female") %>%
  left_join(births, by = c("year", "age")) %>%
  filter(!is.na(births)) %>%
  mutate(birth_rate = births/population) -> birth_rate


 ## HIV testing in pregnancy
 
 # NSFFG: olakunde 2020 Int J STD AIDS, Anderson 2006 MatChild Health [2002]
 hivt1 <- data.frame(year = c(2002, 2011:2017),
                        ll = c(0.650, 0.433, 0.670, 0.692, 0.705, 0.659, 0.705, 0.636),
                        est = c(0.692, 0.637, 0.759, 0.77, 0.778, 0.752, 0.797, 0.760),
                        ul = c(0.734, 0.841, 0.847, 0.848, 0.851, 0.844, 0.889, 0.884))
 
 # PRAMS: Nwangwu-Ike 2023
 hivt2 <-  data.frame(year = c(2017),
                        ll = c(0.653),
                        est = c(0.661),
                        ul = c(0.670))
 
   
## Rate of HIV screening due to pregancy   
   
  birth_rate %>%
     mutate(hivt_ll = colMeans(hivt1)[2]*birth_rate,
            hivt_est = colMeans(hivt1)[3]*birth_rate,
            hivt_ul = colMeans(hivt1)[4]*birth_rate, 
            hivt_sd = (hivt_est-hivt_ll)/1.96,
            alpha = betavars(birth_rate, birth_rate/2000)$alpha,
            beta = betavars(birth_rate, birth_rate/2000)$beta )  %>%
    arrange(age, year) ->  birth_rate_wtest_all
  
  birth_rate_wtest_all[birth_rate_wtest_all$year ==2000 | birth_rate_wtest_all$year ==2022,] -> birth_rate_wtest
  
  # # 1999 = 2000 and 2023 = 2022
  # 
  # birth_rate_wtest[birth_rate_wtest$year ==2000 | birth_rate_wtest$year ==2022,] -> birth_rate_wtest_dpl
  # birth_rate_wtest_dpl$year[birth_rate_wtest_dpl$year==2000] <- 1999
  # birth_rate_wtest_dpl$year[birth_rate_wtest_dpl$year==2022] <- 2023
  # 
  # rbind(birth_rate_wtest, birth_rate_wtest_dpl) %>%
  #   arrange(age, year)-> birth_rate_wtest

write.csv(birth_rate_wtest, here("data/birth_rate_wtest.csv"), row.names = FALSE) 
write.csv(birth_rate_wtest_all, here("data/prg_data_ests.csv"), row.names = FALSE) 



hiv_testing_pregn <- round(get.gamma.par(c(0.025, 0.5, 0.975), as.vector(c(0.56, 1.4, 2)), show.output = FALSE, tol = 0.0001),3)
quantile(rgamma(10^6, hiv_testing_pregn[1], hiv_testing_pregn[2]), probs = c(0.025, 0.5, 0.975))

# https://www.ncbi.nlm.nih.gov/pmc/articles/PMC7933054/
hiv_prev_women1 <- round(get.beta.par(c(0.025, 0.5, 0.975), as.vector(c(0.0001, 0.0007,	0.0013)), show.output = FALSE, tol = 0.0001),3)
quantile(rbeta(10^6, hiv_prev_women1[1], hiv_prev_women1[2]), probs = c(0.025, 0.5, 0.975))

hiv_prev_women <- round(get.beta.par(c(0.025, 0.5, 0.975), as.vector(c(0.0007, 0.0013,	0.0023)), show.output = FALSE, tol = 0.0001),3)
quantile(rbeta(10^6, hiv_prev_women[1], hiv_prev_women[2]), probs = c(0.025, 0.5, 0.975))

hiv_prev_msm <- round(get.beta.par(c(0.025, 0.5, 0.975), as.vector(c(0.055, 0.0884,	0.1411)), show.output = FALSE, tol = 0.0001),3)
quantile(rbeta(10^6, hiv_prev_msm[1], hiv_prev_msm[2]), probs = c(0.025, 0.5, 0.975))

hiv_prev_msm <- round(get.beta.par(c(0.025, 0.5, 0.975), as.vector(c(0.0884,	0.1411, 0.2)), show.output = FALSE, tol = 0.0001),3)
quantile(rbeta(10^6, hiv_prev_msm[1], hiv_prev_msm[2]), probs = c(0.025, 0.5, 0.975))


######################################
# 
birth_rate %>%
  ggplot(aes(x=year, y=100*birth_rate, color=age)) +
  geom_point() + 
  geom_line() +
  theme_minimal() + ylab("Births per 100 women") + theme(legend.position = "bottom")  + theme(legend.position = "bottom") -> fig_births

ggsave(
  filename = here('output/fig_births.png'),  # 
  plot = fig_births,
  width = 10,
  height = 10,
  units = "cm",
  bg = "white",
)


ggplot() +
  geom_pointrange(data=hivt1, aes(x=year, y=100*est, ymin = 100*ll, ymax= 100*ul), color="blue", size=0.5) + 
  geom_pointrange(data=hivt2, aes(x=year, y=100*est, ymin = 100*ll, ymax= 100*ul), color="orange", size=0.5) + 
  geom_ribbon(aes(x=2000:2023, ymin=100*0.75,  ymax=100*1.4), alpha =0.1, fill = "blue") +
  theme_minimal() + ylim(c(0, 150))  + xlim(c(2000, 2023)) + ylab("HIV testing in pregnant women (%)")  + theme(legend.position = "bottom") -> fig_hiv_test_pregn

ggsave(
  filename = here('output/fig_hiv_test_pregn.png'),  # 
  plot = fig_hiv_test_pregn,
  width = 10,
  height = 10,
  units = "cm",
  bg = "white",
)

birth_rate_wtest %>%
  ggplot() +
  geom_ribbon(aes(x=year, ymin=10^2*hivt_ll, ymax=10^2*hivt_ul, fill=age), alpha=0.5) + 
  geom_line(aes(x=year, y= 10^2*hivt_est, color=age)) + 
  theme_minimal() + ylab("HIV testing per 100 women") + theme(legend.position = "bottom") -> fig_births_hiv

ggsave(
  filename = here('output/fig_births_hiv.png'),  # 
  plot = fig_births_hiv,
  width = 10,
  height = 10,
  units = "cm",
  bg = "white",
)

birth_rate %>%
  mutate(hivt_ll = colMeans(hivt1)[2]*birth_rate,
         hivt_est = colMeans(hivt1)[3]*birth_rate,
         hivt_ul = colMeans(hivt1)[4]*birth_rate, 
         hivtests = hivt_est*population,
         hivtests_ll = hivt_ll*population,
         hivtests_ul = hivt_ul*population) %>%
  group_by(year) %>%
  summarize(test = sum(hivtests),
            test_ll = sum(hivtests_ll),
            test_ul = sum(hivtests_ul)) %>%
  ggplot(aes(x=year, ymin=test_ll, ymax=test_ul)) +
  geom_ribbon( alpha=0.3, fill="blue") + 
 # geom_line(aes(x=year, y=test)) +
  theme_minimal() + ylab("HIV tests in pregnany, total") + theme(legend.position = "bottom")+
  ylim(c(0, NA)) -> fig_hivtests_prg


ggsave(
  filename = here('output/fig_hivtests_prg.png'),  # 
  plot = fig_hivtests_prg,
  width = 15,
  height = 10,
  units = "cm",
  bg = "white",
)






   
   
 
  