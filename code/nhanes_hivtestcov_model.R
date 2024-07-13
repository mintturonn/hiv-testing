
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

#        rm(list = ls())
#        .rs.restartR()


########################################
# nhanes
source("~/prep_denominator/code/nhanes_import_funs.R")

# will download the nhanes cycles -- takes couple minutes
source("~/prep_denominator/code/nhanes_import_hiv.R")

# statefun(hivdat, beta0, nact, safe, safe_eff, contact)

nhns_d <- svydesign(id        = ~SDMVPSU,
                    strata  = ~SDMVSTRA,
                    weights = ~WTMEC2YR, #~WTINT2YR, # all info from interviews
                    nest    = TRUE,
                    data    = nhns)

nhns_all <- subset(nhns_d, year>2008)library(survey)
library(nhanesA)
library(labelled)
library(ggpubr)
library(geofacet)
library(here)
library(readxl)
library(scales)
library(tidycensus)
library(tidyverse)

#        rm(list = ls())
#        .rs.restartR()

source(here('code/prep_model_funs.R'))
source(here('code/acs_fun.R'))
# will give warnings:
source(here('code/prep_model_pars.R'))
source(here('code/figure_specs.R'))

## check this is OK

prep.eff <- par$base_case[par$params == "prep_eff"]

########################################
# nhanes
source("~/prep_denominator/code/nhanes_import_funs.R")

# will download the nhanes cycles -- takes couple minutes
source("~/prep_denominator/code/nhanes_import_hiv.R")

# statefun(hivdat, beta0, nact, safe, safe_eff, contact)

nhns_d <- svydesign(id        = ~SDMVPSU,
                    strata  = ~SDMVSTRA,
                    weights = ~WTMEC2YR, #~WTINT2YR, # all info from interviews
                    survey.lonely.psu = "adjust",
                    nest    = TRUE,
                    data    = nhns)

nhns_all <- subset(nhns_d, year>2008)

svyby(~prep_msm2, nhns_all, by = ~age_msm, svyciprop,  vartype="ci", method="logit") %>%
  arrange(., age_msm) %>%
  as_tibble() %>% 
  rename(Age.Group = age_msm) %>%
  add_row(Age.Group= "45-54") %>%
  add_row(Age.Group= "55+") %>%
  add_row(Age.Group= "13-17", .before = 1) %>%
  add_row(Age.Group= "13-24", .before = 3)

# https://stats.oarc.ucla.edu/r/seminars/survey-data-analysis-with-r/

# by risk group
svyby(~hivtest, nhns_all, by = ~age3+year+prep_all+sexorient, svyciprop,  vartype="ci", method="logit") %>%  
  arrange(., prep_all, sexorient, age3) -> age_prep_sexor_nhanes
evertested_all <- svyglm(hivtest~sexorient*year+age3+prep_all, design=nhns_all, family=quasibinomial())

evertested_all_pred <- cbind(age_prep_sexor_nhanes, predict(evertested_all, age_prep_sexor_nhanes, type="response"))

evertested_all_pred$ll <- evertested_all_pred$response - 1.96*evertested_all_pred$SE
evertested_all_pred$ul <- evertested_all_pred$response + 1.96*evertested_all_pred$SE

ggplot(evertested_all_pred) +
  geom_pointrange(aes(x=year, y=hivtest, ymin=ci_l, ymax=ci_u, color=as.factor(prep_all)), size=0.5, alpha=0.6) +
  geom_line(aes(x=year, y=hivtest, color=as.factor(prep_all)), alpha=0.6) +
  geom_pointrange(aes(x=year, y=response, ymin=ll, ymax=ul, color=as.factor(prep_all)), shape=21, position = position_dodge(width = 0.9)) +
  geom_line(aes(x=year, y=response, color=as.factor(prep_all)), linetype = "dashed", position = position_dodge(width = 0.9)) +
  facet_wrap(~sexorient+age3, ncol=5) + theme_bw() -> p0

ggsave(
  filename = here('output_test/nhanes_cov-new.png'),
  plot = p0,
  width = 35,
  height = 25,
  units = "cm",
  bg = "white",
)

# all
svyby(~hivtest, nhns_all, by = ~age3+year+sexorient, svyciprop,  vartype="ci", method="logit") %>%  
  arrange(., sexorient, age3) -> age_sexor_nhanes
evertested_all2 <- svyglm(hivtest~sexorient*year+age3, design=nhns_all, family=quasibinomial())
evertested_all2_pred <- cbind(age_sexor_nhanes, predict(evertested_all2, age_sexor_nhanes, type="response"))

evertested_all2_pred$ll <- evertested_all2_pred$response - 1.96*evertested_all2_pred$SE
evertested_all2_pred$ul <- evertested_all2_pred$response + 1.96*evertested_all2_pred$SE

ggplot(evertested_all2_pred) +
  geom_pointrange(aes(x=year, y=hivtest, ymin=ci_l, ymax=ci_u), size=0.5, alpha=0.6) +
  geom_line(aes(x=year, y=hivtest), alpha=0.6) +
  geom_pointrange(aes(x=year, y=response, ymin=ll, ymax=ul), shape=21, position = position_dodge(width = 0.9)) +
  geom_line(aes(x=year, y=response), linetype = "dashed", position = position_dodge(width = 0.9)) +
  facet_wrap(~sexorient+age3, ncol=5) + theme_bw() -> p1

ggsave(
  filename = here('output_test/nhanes_cov_overall-new.png'),
  plot = p1,
  width = 35,
  height = 25,
  units = "cm",
  bg = "white",
)







