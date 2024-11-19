
library(here)
library(rstan)
library(readxl)
library(reshape2)
library(tidyverse)


#  rm(list = ls())
#  .rs.restartR()

timespan = 25 # years the model is run
tstep = 1
timestep = 25*(1/tstep)+1
age_groups <- 7
gen_groups <- 3

source(here("code/calibration_data.R"))
source(here("code/calibration_data_2.R"))

tlength <- seq(0, timespan, by=1) #  by=0.25
# times
n_years <- length(tested_f[1,]) 


t0 <- seq(1, 25, by=1)
t1 <- seq(1, length(tlength), by=1)
# t0 <- seq(1, 97, by=4)
# t1 <- seq(1, length(tlength), by=4)

# tlength[t1+4]-tlength[t1]
# for (i in 2:length(t1)){
#   
#   test[i-1] <-tlength[t1[i]]-tlength[t1[i-1]]
# 
# }

init_fun <- function() { list(
  beta_prg_pr = array(c(runif((age_groups-3)*2,0.00001,0.1)), dim = c((age_groups-3), 2)),
  beta_prep_w = array(c(runif((age_groups-2),0.00001,0.1), runif((age_groups-2),0.01,0.06)), dim = c((age_groups-2), 2)),
  beta_prep_msw = array(c(runif((age_groups-2),0.00001,0.1), runif((age_groups-2),0.01,0.06)), dim = c((age_groups-2), 2)),
  beta_prep_msm = array(c(runif((age_groups-2),0.00001,0.1), runif((age_groups-2),0.01,0.06)), dim = c((age_groups-2), 2)),
  beta_scr  = array(c(runif((age_groups)*3*2,0.00001,0.1)), dim = c(age_groups, gen_groups, 2)),
  beta2_scr = array(c(runif((age_groups)*3*2*2,0.00001,0.1)), dim = c(age_groups,2, gen_groups, 2)),
  tst_prg = runif(1, 0.1,0.8),
  tst_prep = runif(1, 0.1,0.8),
  p0 = runif(3, 0.001,0.01),
  p1 = runif(3, 0.0001,0.001)
  # no_pnc_pos =  apply(fit.no_pnc_pospriors[1:7,], 1, function(x) rbeta(1, x[1], x[2])),
  # pnc_pos =  apply(fit.pnc_pospriors[1:7,], 1, function(x) rbeta(1, x[1], x[2])),
  # no_pnc = apply(cbind(fit.nopnc_priors$alpha, fit.nopnc_priors$beta), 1, function(x) rbeta(1, x[1], x[2]))
) }

aging = c(1/10, 1/10, 1/10, 1/10, 1/10, 1/10) # c(0, 0, 0, 0, 0, 0) #
# data for Stan
data_test <- list(n_years = n_years, n0 = n0, ag = age_groups, d=2, r=2, s=gen_groups, gamma = 1, 
                  aging = aging, timespan = timespan, timestep = timestep, tstep = tstep,  t1=t1, popgr=popgr, yrs=1999:2024,
                  risk_pr = risk_pr, risk_ageout=risk_ageout, d_denom = d_denom,
                  # this block for testing by reason
                  beta_prg_alpha = prg_priors$alpha, beta_prg_beta=prg_priors$beta, 
                  beta_pwid_shape= c(6.752, 433.754 ),
                  test_pr_pwid = c(43.329, 52.884), 
                  test_pr_prg = c(26.96, 19.02), test_pr_prep=c(17.824, 18.808 ),
                  hiv_test_posit_wm = c(0.98, 97.02), hiv_test_posit_msm = c(8.7,281.3), 
                  beta_prep_w_alpha = prep_priors_w$alpha_2022,     beta_prep_w_rr_alpha = prep_priors_w$alpha_2017rr, 
                  beta_prep_w_beta = prep_priors_w$beta_2022,       beta_prep_w_rr_beta = prep_priors_w$beta_2017rr, 
                  beta_prep_msw_alpha = prep_priors_msw$alpha_2022, beta_prep_msw_rr_alpha = prep_priors_msw$alpha_2017rr, 
                  beta_prep_msw_beta = prep_priors_msw$beta_2022,   beta_prep_msw_rr_beta = prep_priors_msw$beta_2017rr, 
                  beta_prep_msm_alpha = prep_priors_msm$alpha_2022, beta_prep_msm_rr_alpha = prep_priors_msm$alpha_2017rr, 
                  beta_prep_msm_beta = prep_priors_msm$beta_2022,   beta_prep_msm_rr_beta = prep_priors_msm$beta_2017rr, 
                  d_tested_f_r1=d_tested_f_r1, d_tested_f_r2 = d_tested_f_r2,  d_tested_m_r1=d_tested_m_r1, d_tested_m_r2 = d_tested_m_r2,
                  d_popsize_f=d_popsize_f, d_popsize_m = d_popsize_m, d_testvol = c(testvol_2019,testvol_2020,testvol_2021), mort=mort,
                   d_hiv_diag= d_hivdiagnoses_all,  d_hiv_known = d_hivknownprev_all, #d_hivdeaths = d_hivmortality_all, 
                  #d_deaths_f = d_mortality_f[,2:22], d_deaths_m = d_mortality_m[,2:22], 
                  d_tested_f = tested_f, d_tested_m = tested_m, d_tested_msm = tested_msm,
                  d_tested_f2 = tested_f2, d_tested_m2 = tested_m2, # d_tested_msm2 = tested_msm2,
                  d_tested_recent_f2 = tested_recent_f2, d_tested_recent_m2 = tested_recent_m2, d_tested_recent_msm = tested_recent_msm,
                  dv = c(2,4,6,8,10,12,14,16,18), dmsmv = c(10,13,16,19,23), dm=c(14,16,18,20))

# number of MCMC steps
## 4000 (3900+100) takes 272 mins ~ 4h 30 mins
# 4*3000 16,5 h [old]
# 1*2000 3.1h

niter <- 1000

test_model <- stan_model(here("stan_model/base_model_sex_risk.stan"))

fit_test_model <- sampling(test_model,
                           data = data_test,
                           iter = niter,
                           init = init_fun,
                           chains = 1, 
                           warmup = niter - 100,
                           control = list(adapt_delta = 0.98))

save(fit_test_model, file="fit_test_model.RData")

pars=c( "p","bp0", "hiv_init")
print(fit_test_model, pars = pars)

fit_df <- as.data.frame(fit_test_model)

stan_dens(fit_test_model, pars = pars, separate_chains = TRUE)

source(here("code/calibration_figures.R"))


fit_df %>%
  dplyr::select(starts_with("beta2_scr")) -> test

fit_df %>%
  dplyr::select(starts_with("beta_prep_msm")) -> test

fit_df %>%
  dplyr::select(starts_with("beta_prep_msm[1,")) -> test

test[,1] <- test[,1]/ n0[5]
test[,2] <- test[,2]/ n0[6]
test[,3] <- test[,3]/ n0[7]

fit_df %>%
  dplyr::select(starts_with("test_nums[")) -> test2

fit_df %>%
  dplyr::select(starts_with("Ntot[")) %>%
  dplyr::select(ends_with(c(",1]", ",15]"))) -> test

test$yr1 <- rowSums(test[,1:21])
test$yrX <- rowSums(test[,22:42])

tesntot <- tibble(rowSums(test[, 1:7]),  rowSums(test[, 8:14]),  rowSums(test[, 15:21]))

fit_df %>%
  dplyr::select(starts_with("B")) -> test #%>%

any(test<0)

  #dplyr::select(ends_with(c(",18]", ",19]", ",20]"))) -> test

testR <- tibble(rowSums(test[, 319]),  rowSums(test[, 8:14]),  rowSums(test[, 15:21]))


