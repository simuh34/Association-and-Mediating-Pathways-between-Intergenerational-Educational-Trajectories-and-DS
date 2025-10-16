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

setwd('D:\\R project\\Educational mobility and depression\\SHARE')
data <- read_dta('D:\\R project\\data\\H_SHARE_f.dta')

#wave4
subdat <- subset(data,
                   select = c(inw4,r4wtresp,mergeid,r5iwstat,r6iwstat,r7iwstat,r8iwstat,
                              r4iwy,r5iwy,r6iwy,r7iwy,r8iwy,r4iwm,r5iwm,r6iwm,r7iwm,r8iwm,#interview time
                              #demographics
                              rabyear,   #birth year
                              r4agey, #age
                              ragender,  #gender
                              rabcountry, #born in interview country
                              #socioeconomic
                              h4atotb, #household wealth
                              hh4hhresp, #people living with
                              r4lbrf_s, #labour force status
                              r4mstat, #marital status
                              h4rural,
                              #health related
                              r4hibpe,
                              r4diabe,
                              r4cancre,
                              r4lunge,
                              r4hearte,
                              r4stroke,
                              r4arthre,
                              r4psyche,
                              #number of chronic diseases (0-8)
                              r4tr20, #word recall
                              r4orient, #orient
                              #behavioral
                              r4smokev, r4smoken,#ever and current smoking
                              r4drinkev, r4drinkn, #currently drinks, and number of day/week drinks
                              r4vgactx,#vigorous activity
                              r4mdactx,#moderate activity
                              #exposre
                              raedyrs, #years of education
                              ramomedisced,#mother's years education
                              radadedisced,#father's years education
                              raeducl,#harmonize education
                              ramomeducl,#harmonize mother
                              radadeducl,#harmonized father
                              radadoccup, #father's occupation
                              r4bmi, #self report bmi
                              r4socyr, #any yearly social activities
                              r4height, #height measurement in meters
                              #childhood
                              raccrooms, raccnpeople, raccbath, raccwaterc, raccwaterh,
                              racctoilet, raccheating, raccbooks, raccmathperf, 
                              racclangperf, ramaoccup,rachshlt,
                              #childhood stress
                              ramischlth, rasfnhch,racsevent_s,
                              #warmth
                              rapadrug,
                              #outcome
                              r4eurod,#depression
                              r5eurod,#depression
                              r6eurod,#depression
                              r7eurod,#depression
                              r8eurod #depression
                              ))

names(subdat)

names(subdat) <- c("inw4", "s_weight", "ID", "inw5","inw6","inw7","inw8",
                   "r4iwy","r5iwy","r6iwy","r7iwy","r8iwy","r4iwm","r5iwm","r6iwm","r7iwm","r8iwm",
                   "birth_year", "age", "gender","migration",
                   "household_wealth", "people_living_with", "lb_status", 
                   "marital_status","urbanicity",
                   "r4hibpe","r4diabe","r4cancre","r4lunge",
                   "r4hearte","r4stroke","r4arthre","r4psyche",
                   "word_recall", "orient","smoke_ever","smoke_now","drink_ever","drink_frq",
                   "vigo_act", "mode_act",
                   "years_edu","years_edu_m","years_edu_f","H_edu", "H_edu_m","H_edu_f",
                   "f_occupation","BMI","soc_activity","height",
                   "raccrooms", "raccnpeople", "raccbath", "raccwaterc", 
                   "raccwaterh", "racctoilet", "raccheating", "raccbooks", 
                   "raccmathperf", "racclangperf", "ramaoccup","rachshlt",
                   "ramischlth", "rasfnhch","racsevent_s","rapadrug",
                   "depression2010", "depression2012", "depression2014", "depression2016", "depression2018")

#interview time
subdat$riwmid_w4<-  subdat$r4iwy + subdat$r4iwm/12
subdat$riwmid_w5<-  subdat$r5iwy + subdat$r5iwm/12
subdat$riwmid_w6<-  subdat$r6iwy + subdat$r6iwm/12
subdat$riwmid_w7<-  subdat$r7iwy + subdat$r7iwm/12
subdat$riwmid_w8<-  subdat$r8iwy + subdat$r8iwm/12


write.csv(subdat,'share.csv')

