
library(cowplot)
library(here)
library(nhanesA)
library(survey)
library(tidyverse)
library(labelled)

source(here('code/nhanes_import_funs.R'))


# need to separate the figure functions 
source(here('code/nhanes_import_funs.R'))


svyby(~ctt, nsfg_design, by = ~ age, svyciprop, vartype="ci", method="logit") %>%
  # arrange(., age, SDDSRVYR) %>%
  ggplot() +
  geom_linerange(aes(x=age, ymin=100*ci_l, ymax=100*ci_u, color=age), position = position_dodge2(width = 0.5), size=1) +
  geom_point(aes(x=age, y = 100*ctt, color=age), position = position_dodge2(width = 0.5)) + 
  ylim(c(0, 40)) +
  ylab("CT test (%)") +
  xlab("age") +
  theme_minimal() -> fig1.1

svyby(~ctd, nsfg_design, by = ~ age, svyciprop, vartype="ci", method="logit") %>%
  # arrange(., age, SDDSRVYR) %>%
  ggplot() +
  geom_linerange(aes(x=age, ymin=100*ci_l, ymax=100*ci_u, color=age), position = position_dodge2(width = 0.5), size=1) +
  geom_point(aes(x=age, y = 100*ctd, color=age), position = position_dodge2(width = 0.5)) + 
  ylim(c(0, 10)) +
  ylab("CT diagnosis (%)") +
  xlab("age") +
  theme_minimal() -> fig1.2

comb_figs(fig1.1, fig1.2, "nsfg_fage")
