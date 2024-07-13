
age_string <- c(rep("15-24",timespan*gen_groups), rep("25-34",timespan*gen_groups), rep("35-44", timespan*gen_groups), rep("45-54",timespan*gen_groups),
  rep("55-64",timespan*gen_groups), rep("65-74",timespan*gen_groups), rep("75+",timespan*gen_groups))

age_string2 <- c(rep("15-24",timespan*gen_groups), rep("25-34",timespan*gen_groups), rep("35-44", timespan*gen_groups), rep("45-54",timespan*gen_groups),
                rep("55+",timespan*gen_groups))

age_string3 <- c(rep("15-24",timespan*(gen_groups-1)), rep("25-34",timespan*(gen_groups-1)), rep("35-44", timespan*(gen_groups-1)), rep("45-54",timespan*(gen_groups-1)),
                rep("55-64",timespan*(gen_groups-1)), rep("65-74",timespan*(gen_groups-1)), rep("75+",timespan*(gen_groups-1)))

age_string4 <- c(rep("15-24",timespan*(gen_groups-1)), rep("25-34",timespan*(gen_groups-1)), rep("35-44", timespan*(gen_groups-1)), rep("45-54",timespan*(gen_groups-1)),
                 rep("55+",timespan*(gen_groups-1)))

age_string5 <- c(rep("15-24",gen_groups), rep("25-34", gen_groups), rep("35-44",gen_groups), rep("45-54",gen_groups),
                 rep("55-64",gen_groups), rep("65-74",gen_groups), rep("75+",gen_groups))

pop_string <- rep( rep(c("Women", "MSW", "MSM"), each=timespan), age_groups)
pop_string2 <- rep( rep(c("women", "MSW", "MSM"), each=timespan), age_groups-2)
pop_string3 <- rep( rep(c("women", "men"), each=timespan), age_groups)
pop_string4 <- rep( rep(c("women", "men"), each=timespan), age_groups-2)
pop_string5 <-  rep(c("Women", "MSW", "MSM"), age_groups)

year_string <- rep(1999:2023, gen_groups*age_groups )
year_string2 <-rep(1999:2023, gen_groups*(age_groups-2))
year_string3 <-rep(1999:2023, (gen_groups-1)*age_groups)
year_string4 <-rep(1999:2023, (gen_groups-1)*(age_groups-2))

################################

## EVER TESTED -- the output is organized as [i,k,y] [age, orsex, year]

spread_tested <- function(tested){
    tested_new <- cbind(tested[,1], rep(NA, 7), tested[,2], rep(NA, 7),tested[,3], rep(NA, 7),tested[,4], rep(NA, 7),
                        tested[,5], rep(NA, 7),tested[,6], rep(NA, 7),tested[,7], rep(NA, 7),tested[,8], rep(NA, 7),tested[,9])
    
    tested_new
}

# nhanes
test_long <- as.vector( t(  cbind(matrix(NA, nrow=7, ncol=1), spread_tested(tested_f), matrix(NA, nrow=7, ncol=7),  
                                matrix(NA, nrow=7, ncol=1), spread_tested(tested_m), matrix(NA, nrow=7, ncol=7),
                                matrix(NA, nrow=7, ncol=9), 
                                matrix(NA, nrow=7, ncol=7),  matrix(NA, nrow=7, ncol=9)  ) ))

# nsfg
test_long2 <- as.vector( t(  cbind(matrix(NA, nrow=7, ncol=13), c(tested_f2[,1],rep(NA, 4)), rep(NA, 7),
                                                                  c(tested_f2[,2],rep(NA, 4)), rep(NA, 7),
                                                                  c(tested_f2[,3],rep(NA, 4)), rep(NA, 7),
                                                                  c(tested_f2[,4],rep(NA, 4)), rep(NA, 7),matrix(NA, nrow=7, ncol=4),
                                     matrix(NA, nrow=7, ncol=13), c(tested_m2[,1],rep(NA, 4)), rep(NA, 7),
                                                                  c(tested_m2[,2],rep(NA, 4)), rep(NA, 7),
                                                                  c(tested_m2[,3],rep(NA, 4)), rep(NA, 7),
                                                                  c(tested_m2[,4],rep(NA, 4)), rep(NA, 7),matrix(NA, nrow=7, ncol=4),
                                     matrix(NA, nrow=7, ncol=13), matrix(NA, nrow=7, ncol=8), matrix(NA, nrow=7, ncol=4)) ))

# nhbs
# nhbs: 2008, 2011, 2014, 2017, 2021
test_long3 <- as.vector( t(  cbind(matrix(NA, nrow=7, ncol=59), c(tested_msm[,1], rep(NA, 2)), matrix(NA, nrow=7, ncol=2), 
                                                                c(tested_msm[,2], rep(NA, 2)), matrix(NA, nrow=7, ncol=2), 
                                                                c(tested_msm[,3], rep(NA, 2)), matrix(NA, nrow=7, ncol=2), 
                                                                c(tested_msm[,4], rep(NA, 2)), matrix(NA, nrow=7, ncol=3), 
                                                                c(tested_msm[,5], rep(NA, 2)), matrix(NA, nrow=7, ncol=2) )))

smr_pred <- cbind(as.data.frame( summary(
  fit_test_model, pars = "pr_tested", probs = c(0.025, 0.5, 0.975))$summary ) , 
  age = age_string, population =  pop_string , t= year_string,  data_nhns = test_long,  data_nsfg = test_long2, data_nhbs = test_long3)
colnames(smr_pred) <- make.names(colnames(smr_pred)) # to remove % in the col names

ggplot(smr_pred, mapping = aes(x = t)) +
  geom_ribbon(aes(ymin = X2.5., ymax = X97.5., fill=population), alpha = 0.35) +
  geom_line(mapping = aes(x = t, y = X50., color=population)) + 
  geom_point(mapping = aes(y = data_nhns/d_denom), color="black", shape=21) +
  geom_point(mapping = aes(y = data_nsfg/d_denom), color="red", shape=21) +
  geom_point(mapping = aes(y = data_nhbs/d_denom), color="blue", shape=21) +
  facet_wrap(~population+age, ncol = age_groups) +
  ylim(c(0,1)) +
  labs(x = "Year", y = "Ever tested") + theme_minimal() +
  theme(legend.position = "none",
        axis.text.x=element_text(angle=45, size = 8)) +
  xlim(c(2000, 2023)) -> p_ever_tested

ggsave(
  filename = here('output_test/calib_ever_tested-new.png'),
  plot = p_ever_tested,
  width = 25,# 15,
  height = 10,
  units = "cm",
  bg = "white",
)

################################

## EVER TESTED, by RISK GROUP -- the output is organized as [i,k,y] [age, orsex, year]

test_long_r1 <-  c(rep(NA, 14), d_tested_f_r1[1], rep(NA, 10) ,rep(NA, 14), d_tested_m_r1[1], rep(NA, 10) , rep(NA, 25), 
                   rep(NA, 14), d_tested_f_r1[2], rep(NA, 10) ,rep(NA, 14), d_tested_m_r1[2], rep(NA, 10) , rep(NA, 25),
                   rep(NA, 14), d_tested_f_r1[3], rep(NA, 10) ,rep(NA, 14), d_tested_m_r1[3], rep(NA, 10) , rep(NA, 25),
                   rep(NA, 14), d_tested_f_r1[4], rep(NA, 10) ,rep(NA, 14), d_tested_m_r1[4], rep(NA, 10) , rep(NA, 25),
                   rep(NA, 14), d_tested_f_r1[5], rep(NA, 10) ,rep(NA, 14), d_tested_m_r1[5], rep(NA, 10) , rep(NA, 25),
                   rep(NA, 25*3*2))

test_long_r2 <-  c(rep(NA, 14), d_tested_f_r2[1], rep(NA, 10) ,rep(NA, 14), d_tested_m_r2[1], rep(NA, 10) , rep(NA, 25), 
                   rep(NA, 14), d_tested_f_r2[2], rep(NA, 10) ,rep(NA, 14), d_tested_m_r2[2], rep(NA, 10) , rep(NA, 25),
                   rep(NA, 14), d_tested_f_r2[3], rep(NA, 10) ,rep(NA, 14), d_tested_m_r2[3], rep(NA, 10) , rep(NA, 25),
                   rep(NA, 14), d_tested_f_r2[4], rep(NA, 10) ,rep(NA, 14), d_tested_m_r2[4], rep(NA, 10) , rep(NA, 25),
                   rep(NA, 14), d_tested_f_r2[5], rep(NA, 10) ,rep(NA, 14), d_tested_m_r2[5], rep(NA, 10) , rep(NA, 25),
                   rep(NA, 25*3*2))

smr_testrisk_r1 <- cbind(as.data.frame( summary(
  fit_test_model, pars = "pr_tested_r1", probs = c(0.025, 0.5, 0.975))$summary ) , 
  age = age_string, population =  pop_string , t= year_string,  data = test_long_r1)
colnames(smr_testrisk_r1) <- make.names(colnames(smr_testrisk_r1)) # to remove % in the col names
smr_testrisk_r1$risk <- "lower"

smr_testrisk_r2 <- cbind(as.data.frame( summary(
  fit_test_model, pars = "pr_tested_r2", probs = c(0.025, 0.5, 0.975))$summary ) , 
  age = age_string, population =  pop_string , t= year_string,  data = test_long_r2)
colnames(smr_testrisk_r2) <- make.names(colnames(smr_testrisk_r2)) # to remove % in the col names
smr_testrisk_r2$risk <- "higher"

ggplot(rbind(smr_testrisk_r1, smr_testrisk_r2), mapping = aes(x = t)) +
  geom_ribbon(aes(ymin = X2.5., ymax = X97.5., fill=risk), alpha = 0.35) +
  geom_line(mapping = aes(x = t, y = X50., color=risk)) + 
  geom_point(mapping = aes(y = data/d_denom , color=risk), fill="white", size=2, shape=21) +
  facet_wrap(~population+age, ncol = age_groups) +
  ylim(c(0,1)) +
  labs(x = "Year", y = "Ever tested - by risk") + theme_minimal() +
  theme(legend.position = "bottom",
        axis.text.x=element_text(angle=45, size = 8)) +
  xlim(c(2000, 2023))  -> p_tested_risk

ggsave(
  filename = here('output_test/calib_tested_risk-new.png'),
  plot = p_tested_risk,
  width = 25,# 15,
  height = 10,
  units = "cm",
  bg = "white",
)

# ggplot(smr_recpred, map

#################################
## RECENTLY TESTED -- by sex only
# nsfg
testrec_long <- as.vector( t(cbind(matrix(NA, nrow=7, ncol=13), c(tested_recent_f2[,1],rep(NA, 4)), rep(NA, 7),
                                                                c(tested_recent_f2[,2],rep(NA, 4)), rep(NA, 7),
                                                                c(tested_recent_f2[,3],rep(NA, 4)), rep(NA, 7),
                                                                c(tested_recent_f2[,4],rep(NA, 4)), rep(NA, 7),matrix(NA, nrow=7, ncol=4),
                                   matrix(NA, nrow=7, ncol=13), c(tested_recent_m2[,1],rep(NA, 4)), rep(NA, 7),
                                                                c(tested_recent_m2[,2],rep(NA, 4)), rep(NA, 7),
                                                                c(tested_recent_m2[,3],rep(NA, 4)), rep(NA, 7),
                                                                c(tested_recent_m2[,4],rep(NA, 4)), rep(NA, 7),matrix(NA, nrow=7, ncol=4),
                                   matrix(NA, nrow=7, ncol=13), matrix(NA, nrow=7, ncol=8)  , matrix(NA, nrow=7, ncol=4)) ))

testrec_long3 <- as.vector( t(  cbind(matrix(NA, nrow=7, ncol=59), c(tested_recent_msm[,1], rep(NA, 2)), matrix(NA, nrow=7, ncol=2), 
                                   c(tested_recent_msm[,2], rep(NA, 2)), matrix(NA, nrow=7, ncol=2), 
                                   c(tested_recent_msm[,3], rep(NA, 2)), matrix(NA, nrow=7, ncol=2), 
                                   c(tested_recent_msm[,4], rep(NA, 2)), matrix(NA, nrow=7, ncol=3), 
                                   c(tested_recent_msm[,5], rep(NA, 2)), matrix(NA, nrow=7, ncol=2) )))


smr_recpred <- cbind(as.data.frame( summary(
  fit_test_model, pars = "pr_rec_tested", probs = c(0.025, 0.5, 0.975))$summary ) , 
  population= rep(c("women", "msw", "msm"), each =length(1999:2023)),
  age = age_string, t= rep(1999:2023, gen_groups),  data = testrec_long, nhbs = testrec_long3) # ,
 colnames(smr_recpred) <- make.names(colnames(smr_recpred)) # to remove % in the col names
 
 ggplot(smr_recpred, mapping = aes(x = t)) +
   geom_ribbon(aes(ymin = X2.5., ymax = X97.5., fill=population), alpha = 0.35) +
   geom_line(mapping = aes(x = t, y = X50., color=population)) + 
   geom_point(mapping = aes(y = data/d_denom), color="red", shape=21) +
   geom_point(mapping = aes(y = nhbs/d_denom), color="blue", shape=21) +
   facet_wrap(~population+age, ncol = age_groups) +
   ylim(c(0,1)) +
   labs(x = "Year", y = "Tested <12 months ago") + theme_minimal() +
   theme(legend.position = "none",
         axis.text.x=element_text(angle=45, size = 8)) +
   xlim(c(2000, 2023)) -> p_rect
 
 ggsave(
   filename = here('output_test/calib_rec_tested-new.png'),
   plot = p_rect,
   width = 25,# 15,
   height = 10,
   units = "cm",
   bg = "white",
 )

# ggplot(smr_recpred, mapping = aes(x = t)) +
#   geom_ribbon(aes(ymin = X2.5., ymax = X97.5., fill=population), alpha = 0.35) +
#   geom_line(mapping = aes(x = t, y = X50.)) + 
#   facet_wrap(~population+age, ncol = age_groups) +
#   #geom_point(mapping = aes(y = data/1000, color=sex)) +
#   # geom_point(mapping = aes(y = tested_recent/N)) +
#   labs(x = "Year", y = "Tested <12 months ago") + theme_minimal() 

################################
# POPULATION SIZE

pop_long <- as.vector( t( cbind(matrix(NA, nrow=7, ncol=1), d_popsize_f[2:8,], 
                                matrix(NA, nrow=7, ncol=1), 0.95*d_popsize_m[2:8,],
                                matrix(NA, nrow=7, ncol=1), 0.05*d_popsize_m[2:8,])))

smr_pop <- cbind( as.data.frame( summary(
  fit_test_model, pars = "Ntot", probs = c(0.025, 0.5, 0.975))$summary ) , 
  age = age_string, pop =  pop_string, t= year_string, data = pop_long) #
colnames(smr_pop) <- make.names(colnames(smr_pop)) # to remove % in the col names

ggplot(smr_pop, mapping = aes(x = t)) +
  geom_ribbon(aes(ymin = X2.5., ymax = X97.5., fill=pop), alpha = 0.35) +
  geom_line(mapping = aes(x = t, y = X50., color=pop)) + 
  geom_point(mapping = aes(y = data), size=0.3, shape=21, color="gray50") +
  facet_wrap(~pop+age, ncol = 7, scales = "free") + # 
  labs(x = "Year", y = "Population size") + theme_minimal() +
  theme(legend.position = "none",
        axis.text.x=element_text(angle=45, size = 8)) +
  xlim(c(2000, 2023)) + ylim(c(0, NA)) -> p_pop

ggsave(
  filename = here('output_test/pop.png'),
  plot = p_pop,
  width = 25,
  height = 15,
  units = "cm",
  bg = "white",
)


################################

# MORTALITY
mort_long <- as.vector( t(cbind(matrix(NA, nrow=7, ncol=1), d_mortality_f[,2:22], matrix(NA, nrow=7, ncol=3),
                                matrix(NA, nrow=7, ncol=1), d_mortality_m[,2:22], matrix(NA, nrow=7, ncol=3))))

smr_mort <- cbind(as.data.frame( summary(
  fit_test_model, pars = "deaths", probs = c(0.025, 0.5, 0.975))$summary ) , 
  age = age_string3, pop =  pop_string3, t= year_string3, data = mort_long)
colnames(smr_mort) <- make.names(colnames(smr_mort)) # to remove % in the col names

ggplot(smr_mort, mapping = aes(x = t)) +
  geom_ribbon(aes(ymin = X2.5., ymax = X97.5., fill=pop), alpha = 0.35) +
  geom_line(mapping = aes(x = t, y = X50., color=pop)) + 
  geom_point(mapping = aes(y = data, color=pop)) +
  facet_wrap(~pop+age, ncol = age_groups, scales = "free") +
 # ylim(c(0,1)) +
  labs(x = "Year", y = "Deaths") + theme_minimal() +
  xlim(c(2000, 2023)) + ylim(c(0, NA)) 

###############################
## DIAGNOSES
## modify data to fit the model output
# women
diag_long <- as.vector( t(cbind(matrix(NA, nrow=5, ncol=9), d_hivdiagnoses_all[,,1], matrix(NA, nrow=5, ncol=2),  
                                matrix(NA, nrow=5, ncol=9), d_hivdiagnoses_all[,,2], matrix(NA, nrow=5, ncol=2),
                                matrix(NA, nrow=5, ncol=9), d_hivdiagnoses_all[,,3], matrix(NA, nrow=5, ncol=2) )))

smr_diag <- cbind( as.data.frame( summary(
  fit_test_model, pars = "hiv_diag", probs = c(0.025, 0.5, 0.975))$summary ) , 
  age = age_string2, population =  pop_string2, t= year_string2, data = diag_long) #
colnames(smr_diag) <- make.names(colnames(smr_diag)) # to remove % in the col names

ggplot(smr_diag, mapping = aes(x = t)) +
  geom_ribbon(aes(ymin = X2.5., ymax = X97.5., fill=population), alpha = 0.35) +
  geom_line(mapping = aes(x = t, y = X50., color=population)) + 
  geom_point(mapping = aes(y = data), color="black", shape=21, size=1) +
  facet_wrap(~population+age, ncol = 5,  scales = "free" ) + #,
  #ylim(c(0,0.0005)) +
  labs(x = "Year", y = "HIV diagnoses") + theme_minimal() +
  theme(legend.position = "none",
        axis.text.x=element_text(angle=45, size = 8)) +
  xlim(c(2000, 2023)) + ylim(c(0, NA))  -> p_dhiv

ggsave(
  filename = here('output_test/calb_dhiv-new.png'),  # -scalesfree
  plot = p_dhiv,
  width = 15,
  height = 10,
  units = "cm",
  bg = "white",
)

###############################
## KNOWN LIVING WITH HIV
## modify data to fit the model output
# women
prev_long <- as.vector( t(cbind(matrix(NA, nrow=5, ncol=9), d_hivknownprev_all[,,1], matrix(NA, nrow=5, ncol=2), 
                                matrix(NA, nrow=5, ncol=9), d_hivknownprev_all[,,2], matrix(NA, nrow=5, ncol=2), 
                                matrix(NA, nrow=5, ncol=9), d_hivknownprev_all[,,3], matrix(NA, nrow=5, ncol=2) )))

smr_prev <- cbind( as.data.frame( summary(
  fit_test_model, pars = "known_hiv_prev", probs = c(0.025, 0.5, 0.975))$summary ) , 
  age = age_string2, population =  pop_string2, t= year_string2, data = prev_long)
colnames(smr_prev) <- make.names(colnames(smr_prev)) # to remove % in the col names


ggplot(smr_prev, mapping = aes(x = t)) +
  geom_ribbon(aes(ymin = X2.5., ymax = X97.5., fill=population), alpha = 0.35) +
  geom_line(mapping = aes(x = t, y = X50., color=population)) + 
  geom_point(mapping = aes(y = data), color="black", shape=21, size=1) +
  facet_wrap(~population+age, ncol = 5, scales = "free") + # 
 # ylim(c(0,0.01)) +
  labs(x = "Year", y = "Known PHIV") + theme_minimal() +
  theme(legend.position = "none",
        axis.text.x=element_text(angle=45, size = 8)) +
  xlim(c(2000, 2023)) + ylim(c(0, NA))   -> p_plhiv


ggsave(
  filename = here('output_test/calb_plhiv-new.png'),
  plot = p_plhiv,
  width = 15,
  height = 10,
  units = "cm",
  bg = "white",
)
#######################

## TESTING VOLUME
smr_vols <- cbind( as.data.frame( summary(
  fit_test_model, pars = "testvol_est", probs = c(0.025, 0.5, 0.975))$summary ) ,  
  t= c(2019, 2020, 2021))
colnames(smr_vols) <- make.names(colnames(smr_vols)) # to remove % in the col names

smr_vols %>%
  ggplot() + 
  geom_pointrange(aes(x=t, y=X50., ymin = X2.5., ymax = X97.5.), color = "red") +
  geom_point(aes(x=c(2019, 2020, 2021), y=c(testvol_2019, testvol_2020, testvol_2021)), color = "black", size=3, shape=21) +
  labs(x = "", y = "Testing volume") + theme_minimal() +
  scale_x_continuous(limits = c(2018, 2022), breaks = c(2019, 2020, 2021)) + ylim(c(0, NA)) -> p_testvol

ggsave(
  filename = here('output_test/calb_tesvol.png'),
  plot = p_testvol,
  width = 10,
  height = 6,
  units = "cm",
  bg = "white",
)



#######################

###############################
## HIV DEATHS
## modify data to fit the model output
# women
hivdth_long <- as.vector( t(cbind(matrix(NA, nrow=5, ncol=9), d_hivmortality_all[,,1], matrix(NA, nrow=5, ncol=2),  
                                  matrix(NA, nrow=5, ncol=9), d_hivmortality_all[,,2], matrix(NA, nrow=5, ncol=2),
                                  matrix(NA, nrow=5, ncol=9), d_hivmortality_all[,,3], matrix(NA, nrow=5, ncol=2) )))

smr_hivdth <- cbind( as.data.frame( summary(
  fit_test_model, pars = "hivdeaths", probs = c(0.025, 0.5, 0.975))$summary ) , 
  age = age_string2, population =  pop_string2, t= year_string2, data = hivdth_long) #
colnames(smr_hivdth) <- make.names(colnames(smr_hivdth)) # to remove % in the col names

ggplot(smr_hivdth, mapping = aes(x = t)) +
  geom_ribbon(aes(ymin = X2.5., ymax = X97.5., fill=population), alpha = 0.35) +
  geom_line(mapping = aes(x = t, y = X50., color=population)) + 
  geom_point(mapping = aes(y = data, color=population)) +
  facet_wrap(~population+age, ncol = 5, scales = "free") +
  #ylim(c(0,0.0005)) +
  labs(x = "Year", y = "HIV deaths") + theme_minimal() +
  xlim(c(2000, 2023)) + ylim(c(0, NA))


###############################
## HIV INCIDENCE
# 
# smr_incid <- cbind( as.data.frame( summary(
#   fit_test_model, pars = "incid", probs = c(0.025, 0.5, 0.975))$summary ) , 
#   age = age_string, pop =  pop_string, t= year_string) #
# colnames(smr_incid) <- make.names(colnames(smr_incid)) # to remove % in the col names

smr_incid <- cbind( as.data.frame( summary(
  fit_test_model, pars = "incid", probs = c(0.025, 0.5, 0.975))$summary ) , 
  age = age_string5, pop =  pop_string5) #
colnames(smr_incid) <- make.names(colnames(smr_incid)) # to remove % in the col names

ggplot(smr_incid, mapping = aes(x = age, group=pop, color=age)) +
  geom_linerange(aes( ymin = X2.5., ymax = X97.5.)) + 
  geom_point(mapping = aes(y = X50.)) +
  facet_wrap(~pop, ncol = 3) +
  labs(x = "Age", y = "Estimated HIV incidence") + theme_minimal() +
  ylim(c(0, NA))


#######################


## RECENTLY TESTED -- by age
smr_recpred2 <- cbind(as.data.frame( summary(
  fit_test_model, pars = "rec_tested", probs = c(0.025, 0.5, 0.975))$summary ) , 
  age= c(rep("15-23",timespan), rep("25-34",timespan), rep("35-44", timespan), rep("45-54",timespan),
         rep("55-64",timespan), rep("65-74",timespan), rep("75+",timespan)),  
  t= rep(1999:2023, age_groups*gen_groups))
colnames(smr_recpred2) <- make.names(colnames(smr_recpred2)) # to remove % in the col names

ggplot(smr_recpred2, mapping = aes(x = t)) +
  geom_ribbon(aes(ymin = X2.5., ymax = X97.5.), alpha = 0.35) +
  geom_line(mapping = aes(x = t, y = X50.)) + 
  facet_wrap(~age, ncol = 4) +
  # geom_point(mapping = aes(y = tested_recent/N)) +
  labs(x = "Year", y = "Tested <12 months ago") + theme_minimal() 

## RECENTLY TESTED
params <- lapply(t, function(i){sprintf("y[%s,2]", i)}) #number of infected for each day
smr_y <- as.data.frame(summary(fit_test_model, 
                               pars = params, probs = c(0.05, 0.5, 0.95))$summary)
colnames(smr_y) <- make.names(colnames(smr_y)) # to remove % in the col names

ggplot(smr_y, mapping = aes(x = t)) +
  geom_ribbon(aes(ymin = X5./N, ymax = X95./N), alpha = 0.35) +
  geom_line(mapping = aes(x = t, y = X50./N)) + 
  labs(x = "Year", y = "Recently tested")

## EVER TESTED
smr_pred <- cbind(as.data.frame( summary(
  fit_test_model, pars = "pr_tested", probs = c(0.025, 0.5, 0.975))$summary ) , 
  age= c(rep("15-23",timespan*gen_groups), rep("25-34",timespan*gen_groups), rep("35-44", timespan*gen_groups), rep("45-54",timespan*gen_groups),
         rep("55-64",timespan*gen_groups), rep("65-74",timespan*gen_groups), rep("75+",timespan*gen_groups)),  
  pop =  rep( rep(c("women", "men", "msm"), each=timespan), age_groups),
  t= rep(2000:2020, age_groups*gen_groups),  tested = tested_long)
colnames(smr_pred) <- make.names(colnames(smr_pred)) # to remove % in the col names

ggplot(smr_pred, mapping = aes(x = t)) +
  geom_ribbon(aes(ymin = X2.5., ymax = X97.5., fill=pop), alpha = 0.35) +
  geom_line(mapping = aes(x = t, y = X50., color=pop)) + 
  geom_point(mapping = aes(y = tested/1000, color=pop)) +
  facet_wrap(~pop+age, ncol = age_groups) +
  ylim(c(0,1)) +
  labs(x = "Year", y = "Ever tested") + theme_minimal() +
  xlim(c(2000, 2020))


## TESTING VOLUME
smr_vols <- cbind(as.data.frame( summary(
  fit_test_model, pars = "test_nums", probs = c(0.025, 0.5, 0.975))$summary ) ,  
  age= c(rep("15-23",timespan*gen_groups), rep("25-34",timespan*gen_groups), rep("35-44", timespan*gen_groups), rep("45-54",timespan*gen_groups),
         rep("55-64",timespan*gen_groups), rep("65-74",timespan*gen_groups), rep("75+",timespan*gen_groups)),  
  t= rep(2010:2020, age_groups*gen_groups))
colnames(smr_vols) <- make.names(colnames(smr_vols)) # to remove % in the col names

t(colSums(smr_vols[c(20, 40, 60, 80), c(1, 4, 5, 6)])) %>%
  as_tibble() %>%
  ggplot() + 
  geom_pointrange(aes(x=2019, y=X50., ymin = X2.5., ymax = X97.5.)) +
  ylim(c(0,2*10^7)) +
  geom_hline(yintercept= testvol, color = "red", linetype = "dashed") +   
  labs(x = "", y = "Testing volume") + theme_minimal() +
  scale_x_continuous(limits = c(2019), breaks = c(2019))



## RECENTLY TESTED
smr_recpred <- cbind(as.data.frame( summary(
  fit_test_model, pars = "rec_tested", probs = c(0.025, 0.5, 0.975))$summary ) , 
  age= c(rep("15-23",timespan), rep("25-34",timespan), rep("35-44", timespan), rep("45-54",timespan),
         rep("55-64",timespan), rep("65-74",timespan), rep("75+",timespan)),  
  t= rep(2000:2019, age_groups))
colnames(smr_recpred) <- make.names(colnames(smr_recpred)) # to remove % in the col names

ggplot(smr_recpred, mapping = aes(x = t)) +
  geom_ribbon(aes(ymin = X2.5., ymax = X97.5.), alpha = 0.35) +
  geom_line(mapping = aes(x = t, y = X50.)) + 
  facet_wrap(~age, ncol = 4) +
  # geom_point(mapping = aes(y = tested_recent/N)) +
  labs(x = "Year", y = "Tested <12 months ago") + theme_minimal() 

## RECENTLY TESTED
params <- lapply(t, function(i){sprintf("y[%s,2]", i)}) #number of infected for each day
smr_y <- as.data.frame(summary(fit_test_model, 
                               pars = params, probs = c(0.05, 0.5, 0.95))$summary)
colnames(smr_y) <- make.names(colnames(smr_y)) # to remove % in the col names

ggplot(smr_y, mapping = aes(x = t)) +
  geom_ribbon(aes(ymin = X5./N, ymax = X95./N), alpha = 0.35) +
  geom_line(mapping = aes(x = t, y = X50./N)) + 
  labs(x = "Year", y = "Recently tested")

