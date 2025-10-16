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

data_h <- read_dta('H_ELSA_g3.dta')

subdat_h <- subset(data_h, select = c(inw5,idauniq,hh5hhid,
                                      r5iwindy, r6iwindy,r7iwindy,r8iwindy,r9iwindy,#interview time year
                                      r5iwindm,r6iwindm,r7iwindm,r8iwindm,r9iwindm,#interview time month
                                      #demographics
                                      rabyear,#birth year
                                      r5agey , #age at interview
                                      ragender,#gender
                                      raracem, #race
                                      #socioeconomic
                                      h5atotb, #Asset: r+s total wealth
                                      h5hhres, #number of people living in this household(in here not done)
                                      r5inlbrf, # r labor force status
                                      r5mstat, #marital status
                                      ramaoccup, #r main carer occupation at age 14
                                      #health related
                                      r6mheight,#height in meters
                                      r5tr20,# words recall
                                      r5orient, #orientation
                                      r5arthre, #  r ever had arthritis
                                      r5cancre, #  r ever had cancer
                                      r5hibpe,  #  r Ever had high blood pressure
                                      r5diabe,  #  r ever had diabetes
                                      r5lunge,  # r ever had lung disease
                                      r5hearte, #  r ever had heart problem
                                      r5stroke, #  r ever had stroke
                                      r5psyche,  # r ever had psych problem
                                      r5drink,  #w5 R ever drinks any alcohol
                                      r5drinkwn_e,  #w5 R # drinks/week
                                      r5smoken,  #w5 R smokes now
                                      r5smokef,  #w5 R # cigarettes/day
                                      r5socyr,  #w5 R participate in social activities
                                      r6mbmi,  #w6 r measured Body Mass Index (kg/m2)
                                      #childhoood variables,Childhood/Lifetime Stressful Events
                                      ralsevent_e,  # r's summary count of lifetime stressful events
                                      #childhoood variables,carer relationship
                                      ramarela, # main carer relationship to the respondent
                                      #childhoood variables,Childhood Health
                                      rachshlt, # r childhood health status
                                      rapwarm_e, # r's parental warmth summary mean score
                                      #exposre
                                      raedyrs_e,raeducl,#edu
                                      ramomeduage,#r age mother finished education
                                      radadeduage, #r age father finished education
                                      #outcome
                                      r5cesd,#2010
                                      r6cesd,#2012
                                      r7cesd,#2014
                                      r8cesd,#2016
                                      r9cesd #2018
                                      
))

names(subdat_h) <- c("inw5","ID","hhid",
                     "r5iwy","r6iwy","r7iwy","r8iwy","r9iwy","r5iwm","r6iwm","r7iwm","r8iwm","r9iwm",
                     "birth_year", 
                     "age1", "gender", "race",
                     "household_wealth", "people_living_with", "lb_status","marital_status","par_occupation",
                     "height","word_recall","orient",
                     "arthre","cancre","hibpe","diabe","lunge","hearte","stroke","psyche",
                     "drink_now",  "num_drink", "smoke_now","num_smoke","soc_activity","BMI",
                     "count_stress_events",
                     "carer_rel", "child_health","rapwarm_e",
                     "years_edu","H_edu", "years_edu_m","years_edu_f",
                     "cesd2010", "cesd2012", "cesd2014","cesd2016","cesd2018")


#interview time

subdat_h$wave5_time<-  subdat_h$r5iwy + subdat_h$r5iwm/12
subdat_h$wave6_time<-  subdat_h$r6iwy + subdat_h$r6iwm/12
subdat_h$wave7_time<-  subdat_h$r7iwy + subdat_h$r7iwm/12
subdat_h$wave8_time<-  subdat_h$r8iwy + subdat_h$r8iwm/12
subdat_h$wave9_time<-  subdat_h$r9iwy + subdat_h$r9iwm/12

