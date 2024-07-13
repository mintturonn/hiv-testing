
# after calibrattion_figures.R

smr_pred %>%
  filter(t==2023) %>%
  ggplot() +
  geom_pointrange(aes(x=age, y= X50., ymin = X2.5., ymax = X97.5., color=age), size=0.2) +
  facet_wrap(~population, ncol = 1) +
  ylim(c(0,1)) +
  labs(x = "Age", y = "Proportion ever tested") + theme_minimal() +
  theme(legend.position = "none",
        axis.text.x=element_text(angle=45, size = 12))



rbind(smr_testrisk_r1, smr_testrisk_r2) %>%   
  filter(t==2023) %>%
  ggplot() +
  geom_pointrange(aes(x=age, y= X50., ymin = X2.5., ymax = X97.5., color=risk), size=0.2, position = position_dodge(width = 0.20)) +
  facet_wrap(~population, ncol = 1) +
  ylim(c(0,1)) +
  labs(x = "Year", y = "Ever tested - by HIV testing recommendation") + theme_minimal() +
  theme(legend.position = "bottom",
        axis.text.x=element_text(angle=45, size = 12)) 

smr_recpred %>%
  filter(t==2023) %>%
  ggplot() +
  geom_pointrange(aes(x=age, y= X50., ymin = X2.5., ymax = X97.5., color=age), size=0.2) +
  facet_wrap(~population, ncol = 1) +
  ylim(c(0,1)) +
  labs(x = "Age", y = "Proportion tested in <12m") + theme_minimal() +
  theme(legend.position = "none",
        axis.text.x=element_text(angle=45, size = 12))