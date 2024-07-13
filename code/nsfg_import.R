
# library(devtools)
# install_github("ajdamico/lodown" , dependencies = TRUE)

library(here)
library(lodown)
library(survey)
library(tidyverse)

# examine all available NSFG microdata files
nsfg_cat <-
  get_catalog( "nsfg" ,
               output_dir = file.path( path.expand( "~" ) , "NSFG" ) )

# 2011-19 only
# nsfg_cat2 <- subset( nsfg_cat , grepl( "2011_2019" , full_url ) )

# download the microdata to your local computer
 # nsfg_cat <- lodown( "nsfg", nsfg_cat )

## TEST
# nsfg_cat <- subset( nsfg_cat , grepl( "2013_2015" , full_url ) )
# # download the microdata to your local computer
# nsfg_cat <- lodown( "nsfg" , nsfg_cat )

# Construct a complex sample survey design:
options( survey.lonely.psu = "adjust" )

readRDS( file.path( path.expand( "~" ) , "NSFG" , "2002FemResp.rds" ) ) %>%
  mutate(year = "2002") %>%
  mutate(weight = finalwgt) %>%
  mutate(secu = secu_r) -> nsg0

readRDS( file.path( path.expand( "~" ) , "NSFG" , "2006_2010_FemResp.rds" ) ) %>%
  mutate(year = "2006-2010") %>%
  mutate(weight = wgtq1q16) -> nsg1

readRDS( file.path( path.expand( "~" ) , "NSFG" , "2011_2013_FemRespData.rds" ) ) %>%
  mutate(year = "2011_2013") %>%
  mutate(weight = wgt2011_2013) -> nsg2

readRDS( file.path( path.expand( "~" ) , "NSFG" , "2013_2015_FemRespData.rds" ) ) %>%
  mutate(year = "2013_2015") %>%
  mutate(weight = wgt2013_2015) -> nsg3

readRDS( file.path( path.expand( "~" ) , "NSFG" , "2015_2017_FemRespData.rds" ) ) %>%
  mutate(year = "2015_2017") %>%
  mutate(weight = wgt2015_2017) -> nsg4

readRDS( file.path( path.expand( "~" ) , "NSFG" , "2017_2019_FemRespData.rds" ) ) %>%
  mutate(year = "2017_2019") %>%
  mutate(weight = wgt2017_2019) -> nsg5


readRDS( file.path( path.expand( "~" ) , "NSFG" , "XXXXXXXXXXXXXXXXXXMale.rds" ) ) %>%
  mutate(year = "2002") -> nsg00

readRDS( file.path( path.expand( "~" ) , "NSFG" , "2006_2010_Male.rds" ) ) %>%
   mutate(year = "2006_2010") -> nsg01

readRDS( file.path( path.expand( "~" ) , "NSFG" , "2011_2013_MaleData.rds" ) ) %>%
  mutate(year = "2011_2013") -> nsg02

readRDS( file.path( path.expand( "~" ) , "NSFG" , "2013_2015_MaleData.rds" ) ) %>%
  mutate(year = "2013_2015") -> nsg03

readRDS( file.path( path.expand( "~" ) , "NSFG" , "2015_2017_MaleData.rds" ) ) %>%
  mutate(year = "2015_2017") -> nsg04

readRDS( file.path( path.expand( "~" ) , "NSFG" , "2017_2019_MaleData.rds" ) ) %>%
  mutate(year = "2017_2019") -> nsg05


nsg0[,c("age_r", "stdtst12",            "evhivtst", "hivtest", "hivtst",            "year", "hisprace",               "parts1yr", "sest", "finalwgt", "secu_r", "secu", "weight")] %>%
  bind_rows(., nsg1[,c("age_r", "stdsvc12", "hivsoon" , "evhivtst", "hivtest", "hivtst", "hivtstyr", "year", "hisprace", "hisprace2",             "sest", "wgtq1q16", "secu", "weight")]) %>%
  bind_rows(., nsg2[,c("age_r", "stdsvc12", "stdtrt12", "hivresult", "hivmal12", "evhivtst", "hivtest", "hivtst", "hivtstyr", "year", "sest", "wgt2011_2013", "secu", "weight")]) %>% 
  bind_rows(., nsg3[,c("age_r", "stdsvc12", "stdtrt12", "chlam", "chlamtst", "hivresult", "hivmal12", "evhivtst", "hivtest", "hivtst", "hivtstyr", "year", "sest", "wgt2013_2015", "secu", "weight")]) %>%
  bind_rows(., nsg4[,c("age_r", "stdsvc12", "stdtrt12", "chlam", "chlamtst", "hivresult", "hivmal12", "evhivtst", "hivtest", "hivtst", "hivtstyr", "year", "sest", "wgt2015_2017", "secu", "weight")]) %>% 
  bind_rows(., nsg5[,c("age_r", "stdsvc12", "stdtrt12", "chlam", "chlamtst", "hivresult", "hivmal12", "evhivtst", "hivtest", "hivtst", "hivtstyr", "year", "sest", "wgt2017_2019", "secu", "weight")]) -> nsgf 

nsgf$hivtest


nsfg_fem <-
  svydesign(
    id = ~ secu ,
    strata = ~ sest ,
    data = nsgf ,
    weights = ~ weight ,
    nest = TRUE
  )


# nsg4 <- readRDS( file.path( path.expand( "~" ) , "NSFG" , "2013_2015_MaleData.rds" ) )
#  
# nsfg_design <-
#   svydesign(
#     id = ~ secu ,
#     strata = ~ sest ,
#     data = nsg ,
#     weights = ~ wgt2013_2015 ,
#     nest = TRUE
#   )

# i think this is WRONG!
# readRDS( file.path( path.expand( "~" ) , "NSFG" , "2011_2019_FemaleWgtData.rds" ) ) %>%
#   filter(!is.na(wgt2011_2019)) -> nsg


# recode age
nsg$age[nsg$age_r < 25] <- "<25"
nsg$age[nsg$age_r > 24 & nsg$age_r < 40] <- ">24"

# tested 12m
nsg$ctt[nsg$chlamtst==1] <- 1
nsg$ctt[nsg$chlamtst==5] <- 0

# diagnosed 12 m
nsg$ctd[nsg$chlam ==1] <- 1
nsg$ctd[nsg$chlam ==5] <- 0

# combined
nsg$anyct[nsg$ctt==0 & nsg$ctd==0] <- 0
nsg$anyct[nsg$ctt==1 & nsg$ctd==0] <- 1
nsg$anyct[nsg$ctt==0 & nsg$ctd==1] <- 2
nsg$anyct[nsg$ctt==1 & nsg$ctd==1] <- 3

############## MEN
# recode age
nsgm$age[nsgm$age_r < 25] <- "<25"
nsgm$age[nsgm$age_r > 24] <- ">24"

# tested 12m -- any sstd
nsgm$stdt[nsgm$stdtst12==1] <- 1
nsgm$stdt[nsgm$stdtst12==5] <- 0

# diagnosed 12 m
nsgm$ctd[nsgm$chlam ==1] <- 1
nsgm$ctd[nsgm$chlam ==5] <- 0

# combined
nsgm$anyct[nsgm$stdt==0 & nsgm$ctd==0] <- 0
nsgm$anyct[nsgm$stdt==1 & nsgm$ctd==0] <- 1
nsgm$anyct[nsgm$stdt==0 & nsgm$ctd==1] <- 2
nsgm$anyct[nsgm$stdt==1 & nsgm$ctd==1] <- 3


#############################

nsg %>%
  group_by(anyct) %>%
  summarise(n=n()) %>%
  spread(anyct, n) %>%
  knitr::kable()



nsfg_design <- 
  svydesign( 
    id = ~ caseid , 
    strata = ~ sest , 
    data = nsg , 
    weights = ~ wgt2011_2019 ,  
    nest = TRUE 
  )

# Add new columns to the data set:

nsfg_design <- 
  update( 
    nsfg_design , 
    
    one = 1 ,
    
    birth_control_pill = as.numeric( constat1 == 6 ) ,
    
    age_categories = 
      factor( findInterval( ager , c( 15 , 20 , 25 , 30 , 35 , 40 ) ) ,
              labels = c( '15-19' , '20-24' , '25-29' , '30-34' , '35-39' , '40-44' ) ) ,
    
    marstat =
      factor( marstat , levels = c( 1:6 , 8:9 ) ,
              labels = c(
                "Married to a person of the opposite sex" ,
                "Not married but living together with a partner of the opposite sex" ,
                "Widowed" ,
                "Divorced or annulled" ,
                "Separated, because you and your spouse are not getting along" ,
                "Never been married" ,
                "Refused" ,
                "Don't know" )
      )
  )

# Count the unweighted number of records in the survey sample, overall and by groups:
sum( weights( nsfg_design , "sampling" ) != 0 )
svyby( ~ one , ~ age_categories , nsfg_design , unwtd.count )

# Count the weighted size of the generalizable population, overall and by groups:
svytotal( ~ one , nsfg_design )

#####
# Descriptive Statistics
# Calculate the mean (average) of a linear variable, overall and by groups:
  
  svymean( ~ npregs_s , nsfg_design , na.rm = TRUE )

svyby( ~ npregs_s , ~ age_categories , nsfg_design , svymean , na.rm = TRUE )
# Calculate the distribution of a categorical variable, overall and by groups:
  
  svymean( ~ marstat , nsfg_design )

svyby( ~ marstat , ~ age_categories , nsfg_design , svymean )
# Calculate the sum of a linear variable, overall and by groups:
  
  svytotal( ~ npregs_s , nsfg_design , na.rm = TRUE )

svyby( ~ npregs_s , ~ age_categories , nsfg_design , svytotal , na.rm = TRUE )
# Calculate the weighted sum of a categorical variable, overall and by groups:
  
  svytotal( ~ marstat , nsfg_design )

svyby( ~ marstat , ~ age_categories , nsfg_design , svytotal )
# Calculate the median (50th percentile) of a linear variable, overall and by groups:
  
  svyquantile( ~ npregs_s , nsfg_design , 0.5 , na.rm = TRUE )

svyby( 
  ~ npregs_s , 
  ~ age_categories , 
  nsfg_design , 
  svyquantile , 
  0.5 ,
  ci = TRUE ,
  keep.var = TRUE ,
  na.rm = TRUE
)
# Estimate a ratio:
  
  svyratio( 
    numerator = ~ npregs_s , 
    denominator = ~ nbabes_s , 
    nsfg_design ,
    na.rm = TRUE
  )

## Subsetting
# Restrict the survey design to ever cohabited:
  
  sub_nsfg_design <- subset( nsfg_design , timescoh > 0 )
# Calculate the mean (average) of this subset:
  
  svymean( ~ npregs_s , sub_nsfg_design , na.rm = TRUE )
# Measures of Uncertainty
# Extract the coefficient, standard error, confidence interval, and coefficient of variation from any descriptive statistics function result, overall and by groups:
  
  this_result <- svymean( ~ npregs_s , nsfg_design , na.rm = TRUE )

coef( this_result )
SE( this_result )
confint( this_result )
cv( this_result )

grouped_result <-
  svyby( 
    ~ npregs_s , 
    ~ age_categories , 
    nsfg_design , 
    svymean ,
    na.rm = TRUE 
  )

coef( grouped_result )
SE( grouped_result )
confint( grouped_result )
cv( grouped_result )
# Calculate the degrees of freedom of any survey design object:
  
  degf( nsfg_design )
# Calculate the complex sample survey-adjusted variance of any statistic:
  
  svyvar( ~ npregs_s , nsfg_design , na.rm = TRUE )
# Include the complex sample design effect in the result for a specific statistic:
  
  # SRS without replacement
  svymean( ~ npregs_s , nsfg_design , na.rm = TRUE , deff = TRUE )

# SRS with replacement
svymean( ~ npregs_s , nsfg_design , na.rm = TRUE , deff = "replace" )
# Compute confidence intervals for proportions using methods that may be more accurate near 0 and 1. See ?svyciprop for alternatives:
  
  svyciprop( ~ birth_control_pill , nsfg_design ,
             method = "likelihood" , na.rm = TRUE )
# 
