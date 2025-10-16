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

data_h <- read_dta('D:\\code book\\Educational mobility\\MHAS\\H_MHAS_c.dta')

subdat_h <- subset(data_h, select = c(inw3,unhhidnp,r3wtresp,r4wtresp,r5wtresp,h3hhid,
                                      r3iwy,r4iwy,r5iwy,#interview time year
                                      r3iwm,r4iwm,r5iwm,#interview time month
                                      r3iwstat, r4iwstat,r5iwstat,#wave status, interview status
                                      #demographics
                                      rabyear,rabmonth, #birth year and month
                                      r3agey, r4agey,r5agey,#age at interview
                                      ragender,#sex
                                      h3rural, #live in rural/urban
                                      #socioeconomic
                                      h3atotb, #Asset: r+s total wealth
                                      h3hhresp, #number of people living in this household
                                      r3lbrf_m, # r labor force status
                                      r3mstat, #marital status
                                      #health related
                                      r3height,#height in meters
                                      r3tr8_m,# words recall
                                      r3orient_m, #orientation
                                      r3arthre, #  r ever had arthritis
                                      r3cancre, #  r ever had cancer
                                      r3hibpe,  #  r Ever had high blood pressure
                                      r3diabe,  #  r ever had diabetes
                                      r3lunge_m,  # r ever had lung disease
                                      r3hrtatte, #  r Ever had heart attack
                                      r3stroke, #  r ever had stroke
                                      r3drink, # w3 R Ever drinks any alcohol
                                      r3drinkd, # w3 R Number of days/week drinks
                                      r3smoken, #w3 R Smokes now
                                      r3smokef, #w3 R Number of cigarettes/day
                                      r3socwk, #w3 R Any weekly social activities
                                      r3bmi, #w3 R Measured Body Mass Index=kg/m2
                                      #exposre
                                      raedyrs,raeducl,#edu
                                      rameduc_m,rafeduc_m,#parental edu level,1.None,2.Some primary,3.Primary,4.More than primary
                                      #outcome
                                      r3cesd_m,#2012
                                      r4cesd_m,#2015
                                      r5cesd_m#2018
                                      
))

names(subdat_h) <- c("inw3","ID","r3wtresp","r4wtresp","r5wtresp","hhid",
                     "r3iwy","r4iwy","r5iwy","r3iwm","r4iwm","r5iwm",
                     "r3iwstat","r4iwstat","r5iwstat",
                     "birth_year", "birth_month",
                     "age","age4","age5", "gender", "urbanicity",
                     "household_wealth", "people_living_with", "lb_status","marital_status",
                     "height","r1tr20","orient",
                     "r3arthre","r3cancre","r3hibpe","r3diabe","r3lunge","r3hearte","r3stroke",
                     "drink_ever",  "num_drink", "smoke_now","num_smoke","soc_activity","BMI",
                     "years_edu","H_edu","H_edu_m","H_edu_f",
                     "cesd2012", "cesd2015","cesd2018")


#interview time

subdat_h$wave3_time<-  subdat_h$r3iwy + subdat_h$r3iwm/12
subdat_h$wave4_time<-  subdat_h$r4iwy + subdat_h$r4iwm/12
subdat_h$wave5_time<-  subdat_h$r5iwy + subdat_h$r5iwm/12

