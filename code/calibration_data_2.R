
read.csv(here("data/prep_pr.csv")) %>%
  filter(transcat == "msm") %>%
  select(age, alpha_2022, beta_2022, alpha_2017rr, beta_2017rr) -> prep_priors_msm

read.csv(here("data/prep_pr.csv")) %>%
  filter(transcat == "women") %>%
  select(age, alpha_2022, beta_2022, alpha_2017rr, beta_2017rr) -> prep_priors_w

read.csv(here("data/prep_pr.csv")) %>%
  filter(transcat == "msw") %>%
  select(age, alpha_2022, beta_2022, alpha_2017rr, beta_2017rr) -> prep_priors_msw


read.csv(here("data/birth_rate_wtest.csv")) %>%
  select(year, age, alpha, beta) -> prg_priors

# adult population 18-64 in 2020
# based on NHANES PrEP estimates
risk_pr <- matrix(0, age_groups,gen_groups)
risk_pr[,1] <- c(0.29, 0.15, 0.11, 0.07, 0.07, 0.01, 0.0001)
risk_pr[,2] <- c(0.34, 0.19, 0.11, 0.09, 0.06, 0.03, 0.015)
risk_pr[,3] <- c(0.35, 0.35, 0.35, 0.18, 0.09, 0.045, 0.02)

# https://www.cdc.gov/mmwr/volumes/71/wr/mm7148a1.htm#T1_down

testvol_2019 <- 2101633 + 2523317 + 2572963 + 2451303
testvol_2020 <- 2471614 + 1682578 + 2325554 + 2274593
testvol_2021 <- 2346191 + 2646562 + 2643539 + 2453114

s0 <- b0 <- r0 <- numeric(age_groups) 

# this is wieird 
n0 <- matrix(NA, age_groups, 3)

n0[,1] <-  d_popsize_f[2:8,1]
n0[,2] <-  0.95*d_popsize_m[2:8,1]
n0[,3] <-  0.05*d_popsize_m[2:8,1]

d_denom_obs <- 1000
d_denom <- 250
d_divide <- d_denom_obs/d_denom

risk_pr <- matrix(0, age_groups,gen_groups)
risk_pr[,1] <- c(0.29, 0.15, 0.11, 0.07, 0.07, 0.01, 0.0001)
risk_pr[,2] <- c(0.34, 0.19, 0.11, 0.09, 0.06, 0.03, 0.015)
risk_pr[,3] <- c(0.35, 0.35, 0.35, 0.18, 0.09, 0.045, 0.02)

risk_ageout <- matrix(0, age_groups-1,gen_groups)
risk_ageout[,1] <- 1-risk_pr[2:7,1]/risk_pr[1:6,1]
risk_ageout[,2] <- 1-risk_pr[2:7,2]/risk_pr[1:6,2]
risk_ageout[,3] <- 1-risk_pr[2:7,3]/risk_pr[1:6,3]

mort <- matrix(NA, 4,3)
mort[,3] <- c(0.003, 0.004, 0.043, 0.08) # msm
mort[,2] <- c(0.003, 0.004, 0.043, 0.08) # msw
mort[,1] <- c(0.003, 0.004, 0.023, 0.08) # women

popgr <- 1.7

tested_f <- tested_m <-  matrix(NA, age_groups, 9) 
tested_msm <- matrix(NA, age_groups-2, 5)

# 2000, 2002, 2004, 2006, 2008, 2010, 2012, 2014, 2016
tested_f[1,] <- c(506, 420, 333, 422, 380, 388, 359, 284, 345)  # ever tested_f
tested_f[2,] <- c(642, 600, 612, 645, 650, 558, 532, 530, 596) # ever tested_f
tested_f[3,] <- c(494, 478, 548, 578, 547, 604, 588, 571, 591)  # ever tested_f
tested_f[4,] <- c(317, 259, 312, 426, 402, 428, 422, 433, 434) # ever tested_f
tested_f[5,] <- c(225, 170, 222, 204, 240, 294, 258, 235, 313)
tested_f[6,] <- c(133, 144, 75, 120, 162, 135, 139, 156, 158)
tested_f[7,] <- c(105, 70, 46, 31, 98, 85, 78, 80,81)

tested_m[1,] <- c(243, 212, 240, 247, 320, 221, 208, 210, 203)  # ever tested
tested_m[2,] <- c(453, 485, 427, 370, 461, 445, 372, 368, 354) # ever tested
tested_m[3,] <- c(500, 473, 490, 452, 501, 462, 468, 481, 444)  # ever tested
tested_m[4,] <- c(395, 321, 310, 375, 351, 440, 398, 400, 374) # ever tested
tested_m[5,] <- c(288, 268, 228, 305, 387, 326, 403, 324, 315)
tested_m[6,] <- c(183, 202, 205, 168, 274, 220, 292, 216, 203)
tested_m[7,] <- c(154 ,144, 118, 158, 181, 115, 133, 115, 94)

read_excel(here("data/nhbs-test-data.xlsx")) -> d_nhbs_msm
# OLD 2007, 2011, 2015
# nhbs: 2008, 2011, 2014, 2017, 2021
tested_msm[1,] <- d_nhbs_msm$n_ever[d_nhbs_msm$Age=="18-24"][2:6]  # ever tested
tested_msm[2,] <- d_nhbs_msm$n_ever[d_nhbs_msm$Age=="25-29"][2:6] # ever tested
tested_msm[3,] <- d_nhbs_msm$n_ever[d_nhbs_msm$Age=="30-39"][2:6]   # ever tested
tested_msm[4,] <- d_nhbs_msm$n_ever[d_nhbs_msm$Age=="40-49"][2:6]
tested_msm[5,] <- d_nhbs_msm$n_ever[d_nhbs_msm$Age=="50+"][2:6]

tested_m <- round(tested_m/d_divide )
tested_f <- round(tested_f/d_divide )
tested_msm <- round(tested_msm/d_divide )

# 2012, 2014, 2016, 2018
d_tested_f_r1 <- c(398,558, 599, 428,341)
d_tested_f_r2 <- c(351,668, 610, 533,421)
d_tested_m_r1 <- c(209, 356, 454, 383, 317)
d_tested_m_r2 <- c(262, 569, 645, 599, 564)

d_tested_m_r1 <- round(d_tested_m_r1/d_divide )
d_tested_m_r2 <- round(d_tested_m_r2/d_divide )
d_tested_f_r1 <- round(d_tested_f_r1/d_divide )
d_tested_f_r2 <- round(d_tested_f_r2/d_divide )

# NSFG ever (Only 3 first age categories)
tested_f2 <- tested_m2 <- tested_msm2 <- tested_recent_m2 <- tested_recent_f2 <- tested_recent_msm2  <-  matrix(NA, 3, 4)
tested_recent_msm  <- matrix(NA, age_groups-2, 5)
# 2012, 2014, 2016, 2018
# scaled to 1000 per group
# rescale to 200 per group (d_denom)
tested_m2[1,] <- c( 280, 223, 207, 210)
tested_m2[2,] <- c(543, 522, 540, 480)
tested_m2[3,] <- c(578, 598, 593, 552)
tested_f2[1,] <- c(355, 360, 359, 301)
tested_f2[2,] <- c(759, 704, 702, 705)
tested_f2[3,] <- c(726, 750, 738, 781)

tested_msm2[1,] <-  c(537, 455, 562, 548)
tested_msm2[2,] <-  c(869, 750, 765, 839)
tested_msm2[3,] <-  c(805, 913, 917, 982)

tested_m2 <- round(tested_m2/d_divide )
tested_f2 <- round(tested_f2/d_divide )
tested_msm2 <- round(tested_msm2/d_divide )

tested_recent_m2[1,] <-  c(158, 111, 112, 106)
tested_recent_m2[2,] <-  c(197, 174, 173, 130)
tested_recent_m2[3,] <-  c(129, 133, 177, 123)
tested_recent_f2[1,] <-c(187, 216, 236, 164)
tested_recent_f2[2,] <-c(250, 254, 283, 258)
tested_recent_f2[3,] <-c(157, 147, 191, 186)
tested_recent_msm2[1,] <- c(406, 317, 263, 255)
tested_recent_msm2[2,] <- c(257, 570, 485, 476)
tested_recent_msm2[3,] <- c(545, 450, 376, 485)

tested_recent_msm[1,] <- d_nhbs_msm$n_12m[d_nhbs_msm$Age=="18-24"][2:6]  
tested_recent_msm[2,] <- d_nhbs_msm$n_12m[d_nhbs_msm$Age=="25-29"][2:6] 
tested_recent_msm[3,] <- d_nhbs_msm$n_12m[d_nhbs_msm$Age=="30-39"][2:6]   
tested_recent_msm[4,] <- d_nhbs_msm$n_12m[d_nhbs_msm$Age=="40-49"][2:6]
tested_recent_msm[5,] <- d_nhbs_msm$n_12m[d_nhbs_msm$Age=="50+"][2:6]

tested_recent_m2 <- round(tested_recent_m2/d_divide )
tested_recent_f2 <- round(tested_recent_f2/d_divide )
tested_recent_msm2 <- round(tested_recent_msm2/d_divide )
tested_recent_msm <- round(tested_recent_msm/d_divide )


