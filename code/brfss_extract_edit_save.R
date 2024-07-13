
path <- "~/BRFSS/"


brfss.col2010 = read_excel(paste0(path, "col_names_manual.xlsx"), sheet="2010")
brfss.col2011 = read_excel(paste0(path, "col_names_manual.xlsx"), sheet="2011")
brfss.col2012 = read_excel(paste0(path, "col_names_manual.xlsx"), sheet="2012")
brfss.col2013 = read_excel(paste0(path, "col_names_manual.xlsx"), sheet="2013")
brfss.col2014 = read_excel(paste0(path, "col_names_manual.xlsx"), sheet="2014")
brfss.col2015 = read_excel(paste0(path, "col_names_manual.xlsx"), sheet="2015")
brfss.col2016 = read_excel(paste0(path, "col_names_manual.xlsx"), sheet="2016")
brfss.col2017 = read_excel(paste0(path, "col_names_manual.xlsx"), sheet="2017")
brfss.col2018 = read_excel(paste0(path, "col_names_manual.xlsx"), sheet="2018")
brfss.col2019 = read_excel(paste0(path, "col_names_manual.xlsx"), sheet="2019")
brfss.col2020 = read_excel(paste0(path, "col_names_manual.xlsx"), sheet="2020")


# Note that the variable layout includes group fields that encompass multiple subsequent columns and 
# have no independent width of their own (example: IDATE includes IMONTH, IDAY, and IYEAR). 
# There are also blank columns between some fields. This code creates a data frame of columns that 
# removes the group fields and calculates actual field widths in the file (File_Width) for each field, 
# which will be needed to parse the file.

brfss.col2020$File_Width = sapply(1:nrow(brfss.col2020), function(y) ifelse(y < nrow(brfss.col2020), 
                                                                            brfss.col2020$`Starting Column`[y + 1] - brfss.col2020$`Starting Column`[y], 1))

brfss.col2020 = brfss.col2020[brfss.col2020$`Field Length` > 0,]


# data 
# X_LLCPWT can be used for weighting when calculating crude means and proportions at the state or national level.

## NOT_FINISHED writing the function!
brfss_extract_fun <- function(){    
    
    responses = read.fwf(paste0(path, "LLCP2020.ASC "), widths = brfss.col2020$File_Width, col.names = brfss.col2020$`Variable Name`)
    
    name_list = c("X_STATE", "X_URBSTAT", "X_PSU",  "X_STSTR", "X_LLCPWT", "X_LLCPWT2", "X_AGEG5YR", "DISPCODE", "NUMADULT", "SEXVAR", "GENHLTH",
                  "PHYSHLTH", "MENTHLTH", "HLTHPLN1", "EXERANY2", "SLEPTIM1", "CVDINFR4", "MARITAL", 
                  "EDUCA", "RENTHOM1", "VETERAN3", "EMPLOY1", "CHILDREN",
                  "INCOME2", "PREGNANT", "WEIGHT2", "HTIN4", "DEAF", "BLIND", "SMOKDAY2",
                  "STOPSMK2", "AVEDRNK3", "DRNK3GE5", "COLNSCPY", "HIVTST7", "HIVTSTD3", "HIVRISK5", "X_LLCPWT")
    
    responses2 = responses[, name_list]
    
    responses2$hivrisk[responses2$HIVRISK5==1] <- 1
    responses2$hivrisk[responses2$HIVRISK5==2] <- 0
    
    responses2$hivtest[responses2$HIVTST7==1] <- 1
    responses2$hivtest[responses2$HIVTST7==2] <- 0
    
  #  responses2$hivtestdate[responses2$HIVTSTD3 < 700000 ] <- responses2$HIVTSTD3[responses2$HIVTSTD3 < 700000 ] 

    
    write.csv(responses2, here("data/brfss_2020.csv"), row.names=FALSE)
    
}

