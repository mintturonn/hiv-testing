
library(dplyr)
library(ggpubr)
library(here)
library(readxl)

# testing data from NHBS
read_excel(here("data/nhbs-test-data.xlsx")) %>%
  ggplot(aes(x=Year, y=`ever-percent`, color=Age)) +
  geom_line() + 
  geom_pointrange(aes(ymin=100*(p_ever-1.96*se_ever), ymax=100*(p_ever+1.96*se_ever))) + 
  theme_bw() + ylim(c(50,100)) + ggtitle("Ever tested (%)")

# testing data from NHBS
read_excel(here("data/nhbs-test-data.xlsx")) %>%
  ggplot(aes(x=Year, y=`past12m-percent`, color=Age)) +
  geom_line() + 
  geom_pointrange(aes(ymin=100*(p_12m-1.96*se_12m), ymax=100*(p_12m+1.96*se_12m))) +
  theme_bw() + ylim(c(40,100)) + ggtitle("Tested in past 12m (%)")