


read.csv(here("data/AtlasPlusTableData_prep_users.csv"), skip=8)  %>%
  mutate(year = ifelse(Year == "2020 (COVID-19 Pandemic)", 2020, as.numeric(Year))) %>%
  mutate(cases = as.numeric(gsub(",", "", Cases))) %>%
  mutate(rate_per100= as.numeric(gsub(",", "",  `Percent`))) %>%
  mutate(population= as.numeric(gsub(",", "",  `Population`)))   -> prep_use



prep_use %>%
  ggplot(aes(x=year, y=cases, color=Age.Group)) +
  geom_line()+
  facet_wrap(~Sex, scales="free_y") + ylim(c(0, NA)) + xlim(c(2017,2022)) +
  ylab("number of PrEP users") + theme_bw()