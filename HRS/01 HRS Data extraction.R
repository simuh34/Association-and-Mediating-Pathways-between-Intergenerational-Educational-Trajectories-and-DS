library(tidyverse) 
library(leaps)
library(ggplot2) 
library(corrplot) 
library(plyr) 
library(dplyr) 
library(glmnet) 
library(caret) 
library(xgboost) 
library(randomForest) 
library(pander)
library(mice) 
library(ggalluvial)
library(haven)
library(MCM)
library(ggsci)
library(reshape2)

data_r <- read_dta('randhrs1992_2020v2.dta')
data_h <- read_dta('H_HRS_c.dta')

subdat_r <- subset(data_r,
                   select = c(inw10,r11iwstat,r12iwstat,r13iwstat,r14iwstat,r15iwstat,r10wtresp,hhid,hhidpn,
                              r10iwmid,r11iwmid,r12iwmid,r13iwmid,r14iwmid,r15iwmid,#interview time
                              #demographics
                              rabyear,   #birth year
                              r10agey_e, #age
                              ragender,  #gender
                              raracem,rahispan, #race and ethnicity
                              #socioeconomic
                              h10atotb, #household wealth
                              h10hhres, #people living with
                              r10lbrf, #labour force status
                              r10mstath, #marital status
                              #health related
                              r10conde, #number of chronic diseases (0-8)
                              r10tr20, #word recall
                              #behavioral
                              r10smokev, r10smoken,#ever and current smoking
                              r10drink, r10drinkd, #currently drinks, and number of day/week drinks
                              r10vgactx,#vigorous activity
                              r10mdactx,#moderate activity
                              #exposre
                              raedyrs, #years of education
                              rameduc, #mother's years education
                              rafeduc, #father's years education
                              #outcome
                              r10cesd, #0-8
                              r11cesd, #2012
                              r12cesd, #2014
                              r13cesd, #2016
                              r14cesd, #2018
                              r15cesd  #2020
                              ))

subdat_h <- subset(data_h,
                   select = c(hhid,hhidpn,
                              #covaraites
                              r10orient, #orientation
                              h10rural,  #urbanicity
                              r10socmn, r10mbmi, #"soc_activity","BMI"
                              #exposure
                              raeducl, #r harmonized education level
                              ramomeducl, #r mother harmonized education level
                              radadeducl, #r father harmonized education level 
                              radadoccup, #father's occupation
                              r10mheight #height measurement in meters
                             ))  
                              
data01 <- merge(subdat_r, subdat_h, by = c("hhid","hhidpn"))

names(data01) <- c("hhid","hhidpn", "inw10","inw11","inw12","inw13","inw14","inw15", "s_weight", "r10iwmid","r11iwmid","r12iwmid","r13iwmid","r14iwmid","r15iwmid",
                   "birth_year", "age", "gender","race", 
                   "hispanic", "household_wealth", "people_living_with", "lb_status", 
                   "marital_status","n_chronic", "word_recall", 
                   "smoke_ever","smoke_now","drink_ever","drink_frq","vigo_act", "mode_act",
                   "years_edu","years_edu_m","years_edu_f",
                   "cesd2010", "cesd2012","cesd2014","cesd2016","cesd2018","cesd2020",
                   "orient", "urbanicity","soc_activity","BMI","H_edu", "H_edu_m","H_edu_f","f_occupation","height")

#interview time
base_date <- as.Date("1960-01-01") 
date_from_r10iwmid <- base_date + data01$r10iwmid 
date_from_r11iwmid <- base_date + data01$r11iwmid 
date_from_r12iwmid <- base_date + data01$r12iwmid 
date_from_r13iwmid <- base_date + data01$r13iwmid 
date_from_r14iwmid <- base_date + data01$r14iwmid 
date_from_r15iwmid <- base_date + data01$r15iwmid 

data01$r10iwmid_year <- as.numeric(format(date_from_r10iwmid, "%Y"))
data01$r10iwmid_month <- as.numeric(format(date_from_r10iwmid, "%m"))

data01$r11iwmid_year <- as.numeric(format(date_from_r11iwmid, "%Y"))
data01$r11iwmid_month <- as.numeric(format(date_from_r11iwmid, "%m"))

data01$r12iwmid_year <- as.numeric(format(date_from_r12iwmid, "%Y"))
data01$r12iwmid_month <- as.numeric(format(date_from_r12iwmid, "%m"))

data01$r13iwmid_year <- as.numeric(format(date_from_r13iwmid, "%Y"))
data01$r13iwmid_month <- as.numeric(format(date_from_r13iwmid, "%m"))

data01$r14iwmid_year <- as.numeric(format(date_from_r14iwmid, "%Y"))
data01$r14iwmid_month <- as.numeric(format(date_from_r14iwmid, "%m"))

data01$r15iwmid_year <- as.numeric(format(date_from_r15iwmid, "%Y"))
data01$r15iwmid_month <- as.numeric(format(date_from_r15iwmid, "%m"))


data01$riwmid_w0<-  data01$r10iwmid_year + data01$r10iwmid_month/12
data01$riwmid_w1<-  data01$r11iwmid_year + data01$r11iwmid_month/12
data01$riwmid_w2<-  data01$r12iwmid_year + data01$r12iwmid_month/12
data01$riwmid_w3<-  data01$r13iwmid_year + data01$r13iwmid_month/12
data01$riwmid_w4<-  data01$r14iwmid_year + data01$r14iwmid_month/12
data01$riwmid_w5<-  data01$r15iwmid_year + data01$r15iwmid_month/12


write.csv(data01,'hrs.csv')

