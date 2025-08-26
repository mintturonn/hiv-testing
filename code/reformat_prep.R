
library(here)
library(tidyverse)

source("~/hiv-testing/code/helper_funs.R")

# population size

read.csv(here("data/AtlasPlusTableData_population_2000_2022.csv"), skip=7)  %>%
  mutate(year = ifelse(Year == "2020 (COVID-19 Pandemic)", 2020, as.numeric(Year))) %>%
  mutate(age = ifelse(Age.Group == "14-May", "5-14", Age.Group)) %>%
  mutate(population = as.numeric(gsub(",", "",  `Population`))) %>%
  select(Sex, age, year, population)  -> pop_temp

pop_temp %>%
  filter(Sex=="Male") %>%
  mutate(msm = 0.05 * population,
         msw = 0.95 * population) %>%
  select(-population) %>%
  pivot_longer(cols = c("msm", "msw"), values_to = "population", names_to = "transcat") -> pop_male

pop_temp %>%
  filter(Sex=="Female") %>%
  mutate(transcat = "women")-> pop_female


## prep use

read.csv(here("data/AtlasPlusTableData_prep_users.csv"), skip=8)  %>%
  mutate(year = ifelse(Year == "2020 (COVID-19 Pandemic)", 2020, as.numeric(Year))) %>%
  mutate(cases = as.numeric(gsub(",", "", Cases))) %>%
  #mutate(population = as.numeric(gsub(",", "",  `Population`))) %>%
  mutate(age = Age.Group) %>%
  filter(!is.na(cases)) %>%
  select(Sex, year:age) -> prep_use_temp

prep_use_temp$age[prep_use_temp$age=="55+"] <- "55-64"
prep_use_temp$age[prep_use_temp$age=="13-24"] <- "15-24"


prep_use_temp %>%
  filter(Sex=="Male") %>%
  mutate(msm = 0.98 * cases,
         msw = 0.02 * cases) %>%
  select(-cases) %>%
  pivot_longer(cols = c("msm", "msw"), values_to = "cases", names_to = "transcat") -> prep_use_male

prep_use_temp %>%
  filter(Sex=="Female") %>%
  mutate(transcat = "women")-> prep_use_female


# risk population
data.frame( age =  c("15-24", "25-34", "35-44", "45-54", "55-64", "65-74", "75+"),
            women = c(0.29, 0.15, 0.11, 0.07, 0.07, 0.01, 0.0001),
            msw =   c(0.34, 0.19, 0.11, 0.09, 0.06, 0.03, 0.015),
            msm =   c(0.35, 0.35, 0.35, 0.18, 0.18, 0.09, 0.045)) %>%
  pivot_longer(cols = c("msm", "msw", "women"), values_to = "higher_risk", names_to = "transcat") -> risk_pr_long
  

# hiv testing among prep users
# Huang 2018, JID
# baseline

# OFID
# getting HIV test every 3mo
hivt1 <- data.frame(year = c(2018:2021),
                    ll =  rep(3/5, 4),
                    est = rep(2/3, 4),
                    ul =  rep(9/10,4))

########################################

rbind(prep_use_female, prep_use_male) %>%
  left_join(risk_pr_long, by = c("transcat", "age")) %>%
  left_join( rbind(pop_female, pop_male), by = c("transcat", "age", "year")) %>%
  mutate(pop_hr = higher_risk * population) %>%
  mutate(prep_pr = cases / pop_hr,
         prep_sd = sqrt(prep_pr*(1-prep_pr)/pop_hr)) %>%
  filter(year %in% c(2017, 2022)) %>%
  arrange(transcat, age, year) %>%
  select(Sex.x, year, age, transcat, prep_pr, prep_sd) %>%
  pivot_wider(names_from = year, values_from = c(prep_pr, prep_sd), names_prefix = "year_") %>%
  mutate(relrate = prep_pr_year_2017/prep_pr_year_2022,
         alpha_2022 = ifelse( transcat=="msm", betavars(prep_pr_year_2022, prep_pr_year_2022/1000)$alpha,
                              betavars(prep_pr_year_2022, prep_pr_year_2022/10000)$alpha),
         beta_2022  = ifelse( transcat=="msm", betavars(prep_pr_year_2022, prep_pr_year_2022/1000)$beta,
                              betavars(prep_pr_year_2022, prep_pr_year_2022/10000)$beta),
         alpha_2017rr = betavars(relrate, relrate/500)$alpha,
         beta_2017rr  = betavars(relrate, relrate/500)$beta)  -> prep_use

# 1999 = 2000 and 2023 = 2022
# 
# prep_use[prep_use$year ==2000 | prep_use$year ==2022,] -> prep_use_dpl
# prep_use_dpl$year[prep_use_dpl$year==2000] <- 1999
# prep_use_dpl$year[prep_use_dpl$year==2022] <- 2023
# 
# rbind(prep_use, prep_use_dpl) %>%
#   arrange(transcat, age, year)-> prep_use

write.csv(prep_use, here("data/prep_pr.csv"), row.names = FALSE) 

rbind(prep_use_female, prep_use_male) %>%
  left_join(risk_pr_long, by = c("transcat", "age")) %>%
  left_join( rbind(pop_female, pop_male), by = c("transcat", "age", "year")) %>%
  mutate(pop_hr = higher_risk * population) %>%
  mutate(prep_pr = cases / pop_hr,
         prep_sd = sqrt(prep_pr*(1-prep_pr)/pop_hr)) -> prep_use_data_ests

write.csv(prep_use_data_ests, here("data/prep_use_data_ests.csv"), row.names = FALSE) 

###################################
# HIV testing

library(stats4)

# Define the given probabilities
prob_3_months <- 0.75
prob_12_months <- 0.93

# Define the log-likelihood function for binomial distribution
log_likelihood <- function(lambda) {
  # Calculate the probability of testing at least once in 3 months
  prob_3m <- 1 - exp(-lambda * 3/12)
  
  # Calculate the probability of testing at least once in 12 months
  prob_12m <- 1 - exp(-lambda)
  
  # Log-likelihood for the given probabilities
  ll <- dbinom(1, size=1, prob=prob_3m, log=TRUE) * prob_3_months + 
    dbinom(0, size=1, prob=prob_3m, log=TRUE) * (1 - prob_3_months) +
    dbinom(1, size=1, prob=prob_12m, log=TRUE) * prob_12_months + 
    dbinom(0, size=1, prob=prob_12m, log=TRUE) * (1 - prob_12_months)
  
  return(-ll)  # Return the negative log-likelihood for minimization
}

# Estimate lambda using maximum likelihood estimation (MLE)
mle_result <- mle(log_likelihood, start=list(lambda=1))

# Extract the estimated lambda (rate parameter)
coef(mle_result)


hiv_testing_prep <- round(get.gamma.par(c(0.025, 0.5, 0.975), as.vector(c(0.56, 0.93, coef(mle_result))), show.output = FALSE, tol = 0.0001),3)
quantile(rgamma(10^6, hiv_testing_prep[1], hiv_testing_prep[2]), probs = c(0.025, 0.5, 0.975))

hiv_test_posit <- betavars(0.01, 0.0001)




###################

test_linear_interp <- function(N0, N1, x0, x1, y0, y1, t){

  x <- numeric(N1)
  for (n in N0:N1) {
      x[n] = x0 + (x1 - x0) / (y1 - y0) * (t[n] - y0);
  }

  return(x)
}

# 
# # 0 to X between 2014:2017
# test_linear_interp(4, 0, 4.529976e-02, 2014, 2017, 2014:2017)
# 
# # X0 to X1 between 2017:2018
 test_linear_interp(1, 7, 0.06177222, 0.0001191526, 2017, 2022, 2017:2024)
# 
#   
########################################

prep_use %>%
  ggplot(aes(x=year, y=cases, color=Age.Group)) +
  geom_line() +
  facet_wrap(~Sex, scales="free_y") + ylim(c(0, NA)) + xlim(c(2017,2022)) +
  ylab("number of PrEP users") + theme_bw()

######################################
# 
prep_use %>%
  ggplot(aes(x=year, y=100*prep_pr, color=age)) +
  geom_point() + 
  geom_line() +
  facet_wrap(~transcat, scales= "free") +
  theme_minimal() + ylab("per 100 persons w/indicators for PrEP") + theme(legend.position = "bottom")  + theme(legend.position = "bottom") -> fig_prep_use

ggsave(
  filename = here('output/fig_prep_use.png'),  # 
  plot = fig_prep_use,
  width = 19,
  height = 10,
  units = "cm",
  bg = "white",
)


ggplot() +
  geom_pointrange(data=hivt1, aes(x=year, y=100*est, ymin = 100*ll, ymax= 100*ul), color="blue", size=0.5) + 
 # geom_pointrange(data=hivt2, aes(x=year, y=100*est, ymin = 100*ll, ymax= 100*ul), color="orange", size=0.5) + 
  geom_ribbon(aes(x=2000:2023, ymin=100*colMeans(hivt1)[2],  ymax=100*colMeans(hivt1)[4]), alpha =0.1, fill = "blue") +
  theme_minimal() + ylim(c(0, 100))  + xlim(c(2000, 2023)) + ylab("HIV testing in prep women (%)")  + theme(legend.position = "bottom") -> fig_hiv_test_prep

ggsave(
  filename = here('output/fig_hiv_test_prep.png'),  # 
  plot = fig_hiv_test_prep,
  width = 10,
  height = 10,
  units = "cm",
  bg = "white",
)

birth_rate_wtest %>%
  ggplot() +
  geom_ribbon(aes(x=year, ymin=10^2*hivt_ll, ymax=10^2*hivt_ul, fill=age), alpha=0.5) + 
  geom_line(aes(x=year, y= 10^2*hivt_est, color=age)) + 
  theme_minimal() + ylab("HIV testing per 100 women") + theme(legend.position = "bottom") -> fig_prep_use_hiv

ggsave(
  filename = here('output/fig_prep_use_hiv.png'),  # 
  plot = fig_prep_use_hiv,
  width = 10,
  height = 10,
  units = "cm",
  bg = "white",
)


