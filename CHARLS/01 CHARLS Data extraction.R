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

data_h <- read_dta('D:\\R project\\data\\charls\\H_CHARLS_D_Data.dta')

subdat_h <- subset(data_h, select = c(inw1,ID,r1wtresp,r2wtresp,r3wtresp,r4wtresp,hhid,
                                      r1iwy, r2iwy,r3iwy,r4iwy,#interview time year
                                      r1iwm,r2iwm,r3iwm,r4iwm,#interview time month
                                      r1iwstat, r2iwstat, r3iwstat, r4iwstat,#wave status, interview status
                                      #demographics
                                      rabyear,rabmonth, #birth year and month
                                      r1agey, r2agey, r3agey, r4agey,#age at interview
                                      ragender,#sex
                                      r1hukou, #hukou
                                      h1rural, #live in rural/urban
                                      #socioeconomic
                                      hh1atotb, #Asset: r+s total wealth
                                      h1hhres, #number of people living in this household
                                      r1lbrf_c, # r labor force status
                                      r1mstat, #marital status
                                      radadoccup_c, #father's occupation,1.Farming,2 Non-Agricultural
                                      #health related
                                      r1shlta,#r Self-report of health alt
                                      r1mheight,#height in meters
                                      r1tr20,# words recall
                                      r1orient, #orientation
                                      r1arthre, #  r ever had arthritis
                                      r1cancre, #  r ever had cancer
                                      r1hibpe,  #  r Ever had high blood pressure
                                      r1diabe,  #  r ever had diabetes
                                      r1lunge,  # r ever had lung disease
                                      r1hearte, #  r ever had heart problem
                                      r1stroke, #  r ever had stroke
                                      r1psyche,  # r ever had psych problem
                                      r1vgact_c,  #w1 r any vigorous physical activity 
                                      r1mdact_c,  #w1 r any moderate physical activity
                                      r1vgactx_c,  # r days/wk vigorous physical activity or exer
                                      r1mdactx_c, #r # days/wk moderate physical activity or exer
                                      r1drinkl, #r ever drinks any alcohol last year
                                      r1drinkr_c, # r range of # drinks/day
                                      r1smoken, # w1 r smoke now
                                      r1smokef, # w1 r # cigarettes/day
                                      r1socwk, # w1 r participate in social activities
                                      r1mbmi, #w1 r measured Body Mass Index (kg/m2)
                                      #childhoood variables,Childhood/Lifetime Stressful Events
                                      ramomdrug, # female guardian had an alcohol and/or drug issue
                                      radaddrug, # people living in hh when 10,male guardian had an alcohol and/or drug issue
                                      rapadrug, #r guardians had an alcohol and/or drug issue
                                      r1lifethe, # r ever experienced serious traffic accident/inj
                                      ramischlth, # r missed school for 1+ mo due to health 
                                      #childhoood variables,Self-Rated Childhood Health and Finances
                                      rahltcom, # health condition compared to other children
                                      rafinacom, # financial status compared to avg family in same
                                      #childhoood variables,Parental Warmth
                                      ramomeft, # female guardian put effort into watching
                                      ramomatt_c, # r received female guardian's love and affection
                                      ramomgrela, # r good relationship with female guardian
                                      radadgrela, # r good relationship with male guardian 
                                      #exposre
                                      raeduc_c,raeducl,#edu
                                      rameduc_c,#mother's years education
                                      rafeduc_c,#father's years education
                                      ramomeducl,radadeducl,#parental edu level
                                      #outcome
                                      r1cesd10,#0-10
                                      r2cesd10,#2014
                                      r3cesd10,#2016
                                      r4cesd10#2018
                                      
))

names(subdat_h) <- c("inw10","ID","r1wtresp","r2wtresp","r3wtresp","r4wtresp","hhid",
                     "r1iwy","r2iwy","r3iwy","r4iwy","r1iwm","r2iwm","r3iwm","r4iwm",
                   "r1iwstat","r2iwstat","r3iwstat","r4iwstat",
                   "birth_year", "birth_month",
                   "age1","age2","age3","age4", "gender", "hukou","urbanicity",
                   "household_wealth", "people_living_with", "lb_status","marital_status","f_occupation",
                   "SHLT","height","r1tr20","orient",
                   "r1arthre","r1cancre","r1hibpe","r1diabe","r1lunge","r1hearte","r1stroke","r1psyche",
                   "vigor_anyact", "moder_anyact","vigor_act", "moder_act", "drink_now",  "num_drink", "smoke_now","num_smoke","soc_activity","BMI",
                   "fe_drug", "ma_drug","guardians_drug","accident","missed_sch",
                   "chil_condition", "fin_status",
                   "fe_watching","fe_love","fe_guardian", "ma_guardian",
                   "years_edu","H_edu", "years_edu_m","years_edu_f","H_edu_m","H_edu_f",
                   "cesd2012", "cesd2014","cesd2016","cesd2018")


#interview time

subdat_h$wave1_time<-  subdat_h$r1iwy + subdat_h$r1iwm/12
subdat_h$wave2_time<-  subdat_h$r2iwy + subdat_h$r2iwm/12
subdat_h$wave3_time<-  subdat_h$r3iwy + subdat_h$r3iwm/12
subdat_h$wave4_time<-  subdat_h$r4iwy + subdat_h$r4iwm/12
