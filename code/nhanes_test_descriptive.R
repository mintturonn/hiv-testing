

library(survey)
library(nhanesA)
library(labelled)
library(ggpubr)
library(geofacet)
library(here)
library(readxl)
library(scales)
library(tidycensus)
library(tidyverse)

# nhanes
source("~/prep_denominator/code/nhanes_import_funs.R")

# will download the nhanes cycles -- takes couple minutes
source("~/prep_denominator/code/nhanes_import_hiv.R")


## NEED TO CONFIRM THIS IS CORRECT
nhns$mec18year[nhns$SDDSRVYR == 1 | nhns$SDDSRVYR == 2] <- 1/4.5 * nhns$WTMEC4YR[nhns$SDDSRVYR == 1 | nhns$SDDSRVYR == 2]
nhns$mec18year[nhns$SDDSRVYR > 2] <- 1/9 * nhns$WTMEC2YR[nhns$SDDSRVYR > 2]

nhns$int18year[nhns$SDDSRVYR == 1 | nhns$SDDSRVYR == 2] <- 1/4.5 * nhns$WTINT4YR[nhns$SDDSRVYR == 1 | nhns$SDDSRVYR == 2]
nhns$int18year[nhns$SDDSRVYR > 2] <- 1/9 * nhns$WTINT2YR[nhns$SDDSRVYR > 2]

nhns_all <- svydesign(id      = ~SDMVPSU,
                      strata  = ~SDMVSTRA,
                      weights = ~int18year,
                      nest    = TRUE,
                      data    = nhns)


## need to know which time span to select
nhns <- subset(nhns_all, SDDSRVYR >6) # 2012, 2014, 2016

# nhns$int18year[nhns$SDDSRVYR == 1 | nhns$SDDSRVYR == 2] <- 1/4.5 * nhns$WTINT4YR[nhns$SDDSRVYR == 1 | nhns$SDDSRVYR == 2]
# nhns$int18year[nhns$SDDSRVYR > 2] <- 1/9 * nhns$WTINT2YR[nhns$SDDSRVYR > 2]

# prepneed (among all)

# women
svyby(~prep_fem, nhns, by = ~age3, svyciprop,  vartype="ci", method="logit") %>%
  arrange(., age1) %>%
  as_tibble() %>% 
  rename(Age.Group = age1) -> tab_fem

#MSM [prep_msm2 include AI only, not VI]
svyby(~prep_msm2, nhns, by = ~age3, svyciprop,  vartype="ci", method="logit") %>%
  arrange(., age1) %>%
  as_tibble() %>% 
  rename(Age.Group = age1) -> tab_msm

# MSW
svyby(~prep_msw, nhns, by = ~age3, svyciprop,  vartype="ci", method="logit") %>%
  arrange(., age1) %>%
  as_tibble() %>% 
  rename(Age.Group = age1)-> tab_msw

svyby(~prep_all, nhns, by = ~age3+sexorient, svyciprop,  vartype="ci", method="logit") %>%
  arrange(., age3) %>%
  as_tibble() 


yearfun <- function(var1, varname){
  
  svyby(~hivtest, nhns_all, by = ~get(var1)+year, svyciprop,  vartype="ci", method="logit") %>%
     as_tibble() %>%
    rename(group = `get(var1)`) %>%
    ggplot(aes(x=year, y=hivtest, color=group)) +
    geom_point() +
    geom_line() +
    geom_linerange(aes(x=year, ymin=ci_l, ymax=ci_u)) +
    scale_color_manual(values = c( "forestgreen", "steelblue2", "#E69F00", "maroon", "grey20")) +
    scale_y_continuous(labels = scales::percent_format(accuracy = 1), limits = c(0, 1)) +
    ylab("HIV test (ever)") +
    theme_minimal() +
    labs(color = varname)
}

yearfun("race", "Race/ethnicity")
yearfun("age1", "Age")
yearfun("gender", "Gender")
yearfun("sexorient", "Sex/Orientation")



svyby(~hivtest, nhns_all, by = ~gender+year, svyciprop,  vartype="ci", method="logit") %>%  arrange(., gender)

svyby(~hivtest, nhns_all, by = ~sexorient+year+age3, svyciprop,  vartype="ci", method="logit") %>%  arrange(., sexorient) -> hiv_test_age

svyby(~hivtest, nhns_all, by = ~age3+year, svyciprop,  vartype="ci", method="logit") %>%  arrange(., age3) -> agenhanes

svyby(~hivtest, nhns_all, by = ~age3+year+prep_all+sexorient, svyciprop,  vartype="ci", method="logit") %>%  arrange(., prep_all, sexorient, age3) -> age_prep_sexor_nhanes

write.csv( age_prep_sexor_nhanes, here("output/nhanes_age_prep_sexor_year.csv"), row.names=FALSE)

svyby(~prep_all, nhns_all, by = ~age3, svyciprop,  vartype="ci", method="logit") %>%  arrange(., age3) -> risk_age_prep_nhanes

svyby(~hivtest, nhns_all, by = ~age3+prep_all+sexorient, svyciprop,  vartype="ci", method="logit") %>%  arrange(., prep_all, sexorient, age3) -> age_prep_sexor

write.csv( age_prep_sexor, here("output/age_prep_sexor.csv"), row.names=FALSE)

svyby(~hivtest, nhns_all, by = ~prep_all+sexorient, svyciprop,  vartype="ci", method="logit") %>%  arrange(., prep_all, sexorient) -> prep_sexor

write.csv( age_prep_sexor, here("output/prep_sexor.csv"), row.names=FALSE)



svyby(~hivtest, nhns_all, by = ~age3+year2+prep_all+sexorient, svyciprop,  vartype="ci", method="logit") %>%  arrange(., prep_all, sexorient, age3) %>%
  as_tibble() %>%
  filter(sexorient == "women" | sexorient == "msw" ) %>%
  filter(!is.nan(ci_u)) %>%
  ggplot(aes(x=year2, y=hivtest, color=as.factor(prep_all))) +
  geom_point() +
  geom_line() +
  geom_linerange(aes(x=year2, ymin=ci_l, ymax=ci_u)) + 
  facet_wrap(~sexorient+age3, ncol=5) + theme_minimal() + theme(legend.position = "none")

svyby(~hivtest, nhns_all, by = ~age3+prep_all+sexorient, svyciprop,  vartype="ci", method="logit") %>%  arrange(., prep_all, sexorient, age3) %>%
  as_tibble() %>%
  filter(!is.nan(ci_u)) %>%
  ggplot(aes(x=age3, y=hivtest, color=as.factor(prep_all))) +
  geom_point(position = position_dodge(0.2)) +
  geom_line() +
  geom_linerange(aes(x=age3, ymin=ci_l, ymax=ci_u), position = position_dodge(0.2)) + ylim(c(0,1)) +
  facet_wrap(~sexorient, ncol=5) + theme_minimal() + theme(legend.position = "none")

svyby(~hivtest, nhns_all, by = ~prep_all+sexorient, svyciprop,  vartype="ci", method="logit") %>%  arrange(., prep_all, sexorient)  %>%
  as_tibble() %>%
  filter(!is.nan(ci_u)) %>%
  ggplot(aes(x=sexorient, y=hivtest, color=as.factor(prep_all))) +
  geom_point(position = position_dodge(0.2)) +
  geom_line() +
  geom_linerange(aes(x=sexorient, ymin=ci_l, ymax=ci_u), position = position_dodge(0.2)) + ylim(c(0,1)) +
  theme_minimal() + theme(legend.position = "none")

# AGE
svyby(~hivtest, nhns_all, by = ~age1+year, svyciprop,  vartype="ci", method="logit") %>%
  as_tibble() %>%
  ggplot(aes(x=year, y=hivtest, color=age1)) +
  geom_point() +
  geom_line() +
  geom_linerange(aes(x=year, ymin=ci_l, ymax=ci_u))

# 
svyby(~hivtest, nhns_all, by = ~race+year, svyciprop,  vartype="ci", method="logit") %>%
  as_tibble() %>%
  ggplot(aes(x=year, y=hivtest, color=race)) +
  geom_point() +
  geom_line() +
  geom_linerange(aes(x=year, ymin=ci_l, ymax=ci_u))


# OLD
# #MSM [prep_msm2 include AI only, not VI]
# svyby(~prep_msm2, nhns_all, by = ~age1, svyciprop,  vartype="ci", method="logit") %>%
#   arrange(., age1) %>%
#   as_tibble() %>% 
#   rename(Age.Group = age1) %>%
#   mutate(Age.Group = ifelse(Age.Group=="18-24", "13-24", Age.Group)) -> tab_msm
# 
# tab_msm %>%
#   ggplot() +
#   geom_point((aes(x=Age.Group, y=prep_msm2))) +
#   geom_linerange(aes(x=Age.Group, y=prep_msm2, ymin=ci_l, ymax=ci_u)) +
#   ylim(c(0, 0.7)) +
#   theme_bw()
# 
# svyby(~prep_msm2, nhns_all, by = ~age2, svyciprop,  vartype="ci", method="logit") %>%
#   arrange(., age2) %>%
#   as_tibble() %>% 
#   rename(Age.Group = age2)  -> tab_msm2
# 
# # unstable older age group estimates -> aggregate at higher level
# tab_msm$prep_msm2[tab_msm$Age.Group=="45-54" | tab_msm$Age.Group=="55+"] <- tab_msm2$prep_msm2[tab_msm2$Age.Group=="45-59"]
# tab_msm$ci_l[tab_msm$Age.Group=="45-54" | tab_msm$Age.Group=="55+"] <- tab_msm2$ci_l[tab_msm2$Age.Group=="45-59"]
# tab_msm$ci_u[tab_msm$Age.Group=="45-54" | tab_msm$Age.Group=="55+"] <- tab_msm2$ci_u[tab_msm2$Age.Group=="45-59"]
# 
# # MSW
# svyby(~prep_msw, nhns_all, by = ~age1, svyciprop,  vartype="ci", method="logit") %>%
#   arrange(., age1) %>%
#   as_tibble() %>% 
#   rename(Age.Group = age1) %>%
#   mutate(Age.Group = ifelse(Age.Group=="18-24", "13-24", Age.Group)) -> tab_msw
# 
# tab_msw %>%
#   ggplot() +
#   geom_point((aes(x=Age.Group, y=prep_msw))) +
#   geom_linerange(aes(x=Age.Group, y=prep_msw, ymin=ci_l, ymax=ci_u)) +
#   ylim(c(0, 0.5)) +
#   theme_bw()
# #####
# # women
# svyby(~SXQ727, nhns_fem, by = ~age1, svymean,  vartype="ci", method="logit") %>%
#   arrange(., age1) %>%
#   as_tibble() 
# 
# # prop which uses condom always, at least half the time, or halfthe time
# svyby(~condom2, nhns_fem, by = ~age1, svyciprop,  vartype="ci", method="logit") %>%
#   arrange(., age1) %>%
#   as_tibble() 
# 
# # MSW
# 
# svyby(~SXD510, nhns_msw, by = ~age1, svymean,  vartype="ci", method="logit") %>%
#   arrange(., age1) %>%
#   as_tibble() 
# 
# # prop which uses condom always, at least half the time, or halfthe time
# svyby(~condom2, nhns_msw, by = ~age1, svyciprop,  vartype="ci", method="logit") %>%
#   arrange(., age1) %>%
#   as_tibble() 
# 
# # MSM
# 
# 
# svyby(~SXQ550, nhns_msm, by = ~age1, svymean,  vartype="ci", method="logit") %>%
#   arrange(., age1) %>%
#   as_tibble() -> test
# 
# # prop which uses condom always, at least half the time, or halfthe time
# svyby(~condom2, nhns_msm, by = ~age1, svyciprop,  vartype="ci", method="logit") %>%
#   arrange(., age1) %>%
#   as_tibble()  -> test
# 
# svyby(~condom2, nhns_msm, by = ~age2, svyciprop,  vartype="ci", method="logit") %>%
#   arrange(., age2) %>%
#   as_tibble()  -> test

