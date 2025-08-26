
# read.csv(here("data/AtlasPlusTableData_national_popsize.csv"), skip=10)  %>%
#   mutate(year = ifelse(Year == "2020 (COVID-19 Pandemic)", 2020, as.numeric(Year))) %>%
#   mutate(cases = as.numeric(gsub(",", "", Cases))) %>%
#   mutate(rate= as.numeric(gsub(",", "",  `Rate.per.100000`))) %>%
#   mutate(population= as.numeric(gsub(",", "",  `Population`))) %>%
#   select(Indicator, Sex, Age.Group, year, cases, rate, population) %>%
#   arrange(Sex, year) %>%
#   pivot_wider(names_from = Age.Group, values_from = c("cases", "rate", "population")) %>%
#   filter(Indicator=="HIV deaths") -> data_popsize_all
# 
# d_popsize_f <- t(rev(data_popsize_all[data_popsize_all$Sex=="Female",14:18]))
# d_popsize_m <- t(rev(data_popsize_all[data_popsize_all$Sex=="Male",14:18]))

read.csv(here("data/AtlasPlusTableData_population_2000_2022.csv"), skip=7)  %>%
  mutate(year = ifelse(Year == "2020 (COVID-19 Pandemic)", 2020, as.numeric(Year))) %>%
  mutate(age = ifelse(Age.Group == "14-May", "5-14", Age.Group)) %>%
  mutate(cases = as.numeric(gsub(",", "", Cases))) %>%
  mutate(rate= as.numeric(gsub(",", "",  `Rate.per.100000`))) %>%
  mutate(population= as.numeric(gsub(",", "",  `Population`))) %>%
  select(Sex, age, year, population) %>%
  arrange(Sex, year, age) %>%
  pivot_wider(names_from = age, values_from =population)  -> data_popsize_all0

d_popsize_f <- t(data_popsize_all0[data_popsize_all0$Sex=="Female",c(2,4,5,6,7,9,10)])
d_popsize_f <- cbind(d_popsize_f, d_popsize_f[,23]) # 2023 assumed the same as 2022
d_popsize_f <- rbind(d_popsize_f ,d_popsize_f[7,]) # extrapolate to 
d_popsize_f[c(7,8),] <- d_popsize_f[c(7,8),] /2

d_popsize_m <- t(data_popsize_all0[data_popsize_all0$Sex=="Male",c(2,4,5,6,7,9,10)])
d_popsize_m <- cbind(d_popsize_m, d_popsize_m[,23]) # 2023 assumed the same as 2022
d_popsize_m <- rbind(d_popsize_m ,d_popsize_m[7,])
d_popsize_m[c(7,8),] <- d_popsize_m[c(7,8),] /2

d_popdiff_f <- d_popdiff_m <- matrix(NA, 7,22)

for (i in 1:(ncol(d_popsize_f)-2)){
  for (j in 1:(nrow(d_popsize_f)-1)){
  
  d_popdiff_f[j,i] <- d_popsize_f[j+1,i+2] -  d_popsize_f[j+1,i+1] 
  d_popdiff_m[j,i] <- d_popsize_m[j+1,i+2] -  d_popsize_m[j+1,i+1] 
  }
}

read.csv(here("data/AtlasPlusTableData_national_popsize.csv"), skip=10)  %>%
  mutate(year = ifelse(Year == "2020 (COVID-19 Pandemic)", 2020, as.numeric(Year))) %>%
  mutate(population= as.numeric(gsub(",", "",  `Population`))) %>%
  filter(Sex == "Male" & Indicator=="HIV diagnoses") %>%
  select(Age.Group, Sex, year, population) -> male_pop_d

read.csv(here("data/AtlasPlusTableData_national_men_hiv_year.csv"), skip=11)  %>%
  mutate(year = ifelse(Year == "2020 (COVID-19 Pandemic)", 2020, as.numeric(Year))) %>%
  mutate(cases = as.numeric(gsub(",", "", Cases))) %>%
  mutate(rate= as.numeric(gsub(",", "",  `Rate.per.100000`))) %>%
  select(Indicator, Age.Group, year, cases, rate, `Transmission.Category`) %>%
  mutate(msm = ifelse(`Transmission.Category` == "Male-to-male sexual contact" | 
                      `Transmission.Category` == "Male-to-male sexual contact and injection drug use", 1, 0)) %>%
  pivot_wider(names_from = Indicator, values_from = c("cases", "rate")) %>%
  group_by(Age.Group, msm, year) %>%
  summarize(
    deaths = sum(`cases_HIV deaths`),
    diagnoses = sum(`cases_HIV diagnoses`),
    prevalence = sum(`cases_HIV prevalence`)
  ) %>%
  ungroup() %>%
  arrange(Age.Group, year) %>%
  left_join(male_pop_d, by=c("Age.Group", "year")) %>%
  mutate(population_adjusted = ifelse(msm==1, population*0.05, population*0.95)) %>%
  mutate( rate = diagnoses/population_adjusted) -> hivdata_m

read.csv(here("data/AtlasPlusTableData_national_women_hiv_year.csv"), skip=11)  %>%
  mutate(year = ifelse(Year == "2020 (COVID-19 Pandemic)", 2020, as.numeric(Year))) %>%
  mutate(cases = as.numeric(gsub(",", "", Cases))) %>%
  mutate(rate= as.numeric(gsub(",", "",  `Rate.per.100000`))/10^5) %>%
  mutate(population= as.numeric(gsub(",", "",  `Population`))) %>%
  select(Indicator, Age.Group, year, cases, rate, population) %>%
  pivot_wider(names_from = Indicator, values_from = c("cases", "rate")) %>%
  group_by(Age.Group, year) %>%
  summarize(
    deaths = sum(`cases_HIV deaths`),
    diagnoses = sum(`cases_HIV diagnoses`),
    prevalence = sum(`cases_HIV prevalence`),
    population = sum(population)
  ) %>%
  ungroup() %>%
  arrange(Age.Group, year) %>%
  mutate(rate = diagnoses / population) -> hivdata_f


d_popsize <- d_hivmortality_all <- d_hivdiagnoses_all <- d_hivknownprev_all <- d_hivdiagnrate_all <- array(NA, dim = c(5, timespan-12, gen_groups))

d_hivmortality_all[,,1] <- t(matrix(hivdata_f$deaths,nrow=timespan-12))
d_hivmortality_all[,,2] <- t(matrix(hivdata_m$deaths[hivdata_m$msm==0],nrow=timespan-12))
d_hivmortality_all[,,3] <- t(matrix(hivdata_m$deaths[hivdata_m$msm==1],nrow=timespan-12))

d_hivdiagnoses_all[,,1] <- t(matrix(hivdata_f$diagnoses,nrow=timespan-12))
d_hivdiagnoses_all[,,2] <- t(matrix(hivdata_m$diagnoses[hivdata_m$msm==0],nrow=timespan-12))
d_hivdiagnoses_all[,,3] <- t(matrix(hivdata_m$diagnoses[hivdata_m$msm==1],nrow=timespan-12))

d_hivdiagnrate_all[,,1] <- t(matrix(hivdata_f$rate,nrow=timespan-12))
d_hivdiagnrate_all[,,2] <- t(matrix(hivdata_m$rate[hivdata_m$msm==0],nrow=timespan-12))
d_hivdiagnrate_all[,,3] <- t(matrix(hivdata_m$rate[hivdata_m$msm==1],nrow=timespan-12))

d_hivknownprev_all[,,1] <- t(matrix(hivdata_f$prevalence,nrow=timespan-12))
d_hivknownprev_all[,,2] <- t(matrix(hivdata_m$prevalence[hivdata_m$msm==0],nrow=timespan-12))
d_hivknownprev_all[,,3] <- t(matrix(hivdata_m$prevalence[hivdata_m$msm==1],nrow=timespan-12))

d_popsize[,,1] <- matrix(10^6,5,14) 
d_popsize[,,2] <- matrix(10^6,5,14) 
d_popsize[,,3] <- matrix(10^6,5,14) 

# hivdata %>%
#   select( Age.Group, year, `rate_HIV diagnoses`) %>%
#   mutate(rate = round(`rate_HIV diagnoses`,0))  %>%
#   select(rate, year, Age.Group) %>%
#   pivot_wider(names_from = year, values_from = c("rate")) -> d_hiv_diag
# 
# hivdata %>%
#   select( Age.Group, year, `rate_HIV prevalence`) %>%
#   mutate(rate = round(`rate_HIV prevalence`,0))  %>%
#   select(rate, year, Age.Group) %>%
#   pivot_wider(names_from = year, values_from = c("rate")) -> d_hiv_prev


 read.delim("~/prep_denominator/data/Underlying Cause of Death, 1999-2020.txt")  %>%
   filter(Year > 1999 ) %>%
   mutate(age_code = as.numeric(Single.Year.Ages.Code)) %>%
   mutate(age_groups = ifelse(age_code>12 & age_code<25, "13-24", 
                              ifelse(age_code>24 & age_code<35, "25-34",
                                     ifelse(age_code>34 & age_code<45, "35-44",
                                            ifelse(age_code>44 & age_code<55, "45-54", 
                                                   ifelse(age_code>54 & age_code<65, "55-64",
                                                          ifelse(age_code>64 & age_code<75, "65-74",
                                                                 ifelse(age_code>74 & age_code<85, "75-84", 
                                                                        ifelse(age_code>74, "75+", NA ))))))))) %>%
   group_by(age_groups, Gender, Year) %>%
   summarize(deaths = sum(Deaths),
             pop = sum(as.numeric(Population)),
             death.rate = deaths/pop) %>%
   ungroup()  %>%
   filter(!is.na(age_groups)) %>%
   filter(age_groups!= "75-84") -> d_mortality
 
 d_mortality %>%
   filter(Gender=="Female") %>%
   select(age_groups, Year, deaths) %>%
   pivot_wider(names_from =Year, values_from = deaths ) -> d_mortality_f
 
 d_mortality %>%
   filter(Gender=="Male") %>%
   select(age_groups, Year, deaths) %>%
   pivot_wider(names_from =Year, values_from = deaths ) -> d_mortality_m
 
 
 # d_mortality %>% 
 #   ggplot(aes(x=Year, y=deaths, color=age_groups)) +
 #    geom_line() +
 #   facet_wrap(~Gender)
 
 
# read.delim("~/prep_denominator/data/Compressed Mortality, 1999-2016.txt")  -> tab2