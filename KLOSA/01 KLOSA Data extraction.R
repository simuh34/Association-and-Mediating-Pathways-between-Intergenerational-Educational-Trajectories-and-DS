library(tidyverse) #data tidying
library(leaps) #data tidying
library(ggplot2) #visualizing data
library(corrplot) #visualizing data
library(plyr) #data tidying
library(dplyr) 
library(glmnet) #for modeling
library(caret) #for modeling
library(xgboost) #for modeling
library(randomForest) #for modeling
library(pander) #data collation
library(mice) #check missing values
library(ggalluvial)
library(haven)
library(MCM)
library(ggsci)
library(reshape2)

data <- read_dta('D:\\R project\\data\\H_KLoSA_e2.dta')

#wave3
subdat <- subset(data,
                   select = c(inw3,r3wtresp,r4iwstat,r5iwstat,r6iwstat,r7iwstat,r8iwstat,pid,
                              r3iwy,r4iwy,r5iwy,r6iwy,r7iwy,r8iwy,r3iwm,r4iwm,r5iwm,r6iwm,r7iwm,r8iwm,#interview time
                              #demographics
                              rabyear,   #birth year
                              r3agey, #age
                              ragender,  #gender
                              #socioeconomic
                              r3atotb, #household wealth
                              hh3hhresp, #people living with
                              r3lbrf_k, #labour force status
                              r3mstat, #marital status
                              r3rural,#urbanicity
                              #health related
                              r3hibpe,
                              r3diabe,
                              r3cancre,
                              r3lunge,
                              r3hearte,
                              r3stroke,
                              r3arthre,
                              r3psyche,
                              #number of chronic diseases (0-8)
                              #behavioral
                              r3smokev, r3smoken,#ever and current smoking
                              r3drinkev, r3drinkn_k, #currently drinks, and number of day/week drinks
                              r3vigact,#vigorous activity
                              #exposre
                              raeduc_k, #education levels
                              rameduc_k,#mother's level of education
                              rafeduc_k,  #father's level of education
                              raeducl,#harmonize education
                              ramomeducl,#harmonize mother
                              radadeducl,#harmonized father
                              #radadoccup, #father's occupation
                              r3bmi,#bmi 
                              r3socwk, #r any weekly social activities
                              r3height, #height measurement in meters
                              #childhood
                              
                              #outcome
                              r3cesd10a,#depression,0-30
                              r4cesd10a,#depression
                              r5cesd10b,#depression
                              r6cesd10b,#depression
                              r7cesd10b, #depression
                              r8cesd10b
                              ))

names(subdat)
names(subdat) <- c("inw3", "s_weight", "inw4","inw5","inw6","inw7","inw8","ID",
                   "r3iwy","r4iwy","r5iwy","r6iwy","r7iwy","r8iwy","r3iwm","r4iwm","r5iwm","r6iwm","r7iwm","r8iwm",
                   "birth_year", "age", "gender", 
                   "household_wealth", "people_living_with", "lb_status", 
                   "marital_status","urbanicity",
                   "r3hibpe","r3diabe","r3cancre","r3lunge",
                   "r3hearte","r3stroke","r3arthre","r3psyche",
                   "smoke_ever","smoke_now","drink_ever","drink_frq",
                   "vigo_act", 
                   "levels_edu","levels_edu_m","levels_edu_f","H_edu", "H_edu_m","H_edu_f",
                   "BMI","soc_activity","height",
                   "depression2010", "depression2012", "depression2014", "depression2016", "depression2018","depression2020")

#interview time
subdat$riwmid_w3<-  subdat$r3iwy + subdat$r3iwm/12
subdat$riwmid_w4<-  subdat$r4iwy + subdat$r4iwm/12
subdat$riwmid_w5<-  subdat$r5iwy + subdat$r5iwm/12
subdat$riwmid_w6<-  subdat$r6iwy + subdat$r6iwm/12
subdat$riwmid_w7<-  subdat$r7iwy + subdat$r7iwm/12
subdat$riwmid_w8<-  subdat$r7iwy + subdat$r8iwm/12


