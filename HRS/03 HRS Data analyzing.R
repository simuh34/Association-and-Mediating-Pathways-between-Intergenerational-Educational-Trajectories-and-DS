library(ggplot2) 
library(dplyr) 
library(ggalluvial)
library(ggsci)
library(tableone)
library(kableExtra)
library(sjPlot)
library(geepack)
library(tidyr)
library(DT)

df_long  <- df %>%
  pivot_longer(cols = c("cesd2010_depression","cesd2012_depression", "cesd2014_depression","cesd2016_depression","cesd2018_depression","cesd2020_depression"), 
               names_to = "time", 
               values_to = "depression") %>%
  mutate(time = case_when(
    time == "cesd2010_depression" ~ riwmid_w0 - riwmid_w0,
    time == "cesd2012_depression" ~ riwmid_w1 - riwmid_w0,
    time == "cesd2014_depression" ~ riwmid_w2 - riwmid_w0,
    time == "cesd2016_depression" ~ riwmid_w3 - riwmid_w0,
    time == "cesd2018_depression" ~ riwmid_w4 - riwmid_w0,
    time == "cesd2020_depression" ~ riwmid_w5 - riwmid_w0,
    TRUE ~ NA_real_  
  ))
df_long$educational_mobility_H <- factor(df_long$educational_mobility_H, levels = c("Stably low", 
                                                            "Downward mobility", 
                                                            "Stably middle",
                                                            "Upward mobility", 
                                                            "Stably high"))
df_long$educational_mobility_pr <- factor(df_long$educational_mobility_pr, levels = c("Stably low", 
                                                                                    "Downward mobility", 
                                                                                    "Stably middle",
                                                                                    "Upward mobility", 
                                                                                    "Stably high"))



####longitudinal study####

df_long <- df_long[!(df_long$time == 0 & df_long$depression == 1 &!is.na(df_long$depression)), ]

####GEE####
# unadjusted model
##EM1
gee_model_1 <- geeglm(depression ~ educational_mobility_residual, 
                    data = df_long, 
                    id = hhidpn, 
                    family = binomial(link = "logit"),
                    corstr = "exchangeable")
summary(gee_model_1)

##EM2
gee_model_2 <- geeglm(depression ~ educational_mobility_H , 
                      data = df_long, 
                      id = hhidpn, 
                      family = binomial(link = "logit"), 
                      corstr = "exchangeable")
summary(gee_model_2)

##EM3
gee_model_3 <- geeglm(depression ~ educational_mobility_pr, 
                      data = df_long, 
                      id = hhidpn, 
                      family = binomial(link = "logit"), 
                      corstr = "exchangeable")
summary(gee_model_3)
tab_model(gee_model_1,gee_model_2,gee_model_3)

# adjusted for C1 
##EM4
gee_model_4 <- geeglm(depression ~ educational_mobility_residual + age + gender + Race + urbanicity, 
                      data = df_long, 
                      id = hhidpn, 
                      family = binomial(link = "logit"),
                      corstr = "exchangeable")

gee_model_4 <- geeglm(depression ~ educational_mobility_residual + age + gender + Race + urbanicity, 
                      data = df_long, 
                      id = hhidpn, 
                      family = binomial(link = "logit"),
                      corstr = "exchangeable")
summary(gee_model_4)

##EM5
gee_model_5 <- geeglm(depression ~ educational_mobility_H + age + gender + Race + urbanicity, 
                      data = df_long, 
                      id = hhidpn, 
                      family = binomial(link = "logit"), 
                      corstr = "exchangeable")
summary(gee_model_5)

##EM6
gee_model_6 <- geeglm(depression ~ educational_mobility_pr + age + gender + Race + urbanicity, 
                      data = df_long, 
                      id = hhidpn, 
                      family = binomial(link = "logit"), 
                      corstr = "exchangeable")
summary(gee_model_6)
tab_model(gee_model_4,gee_model_5,gee_model_6)

##EM10
gee_model_10 <- geeglm(depression ~ educational_mobility_residual + age + gender + Race + urbanicity + stress + health + financial + warmth + height, 
                       data = df_long, 
                       id = hhidpn, 
                       family = binomial(link = "logit"),
                       corstr = "exchangeable")
summary(gee_model_10)

##EM11
gee_model_11 <- geeglm(depression ~ educational_mobility_H + age + gender + Race + urbanicity+ stress + health + financial + warmth+ height,
                       data = df_long, 
                       id = hhidpn, 
                       family = binomial(link = "logit"), 
                       corstr = "exchangeable")
summary(gee_model_11)

##EM12
gee_model_12 <- geeglm(depression ~ educational_mobility_pr + age + gender + Race + urbanicity+ stress + health + financial + warmth+ height,
                       data = df_long, 
                       id = hhidpn, 
                       family = binomial(link = "logit"), 
                       corstr = "exchangeable")
summary(gee_model_12)
tab_model(gee_model_10,gee_model_11,gee_model_12)

# adjusted for C1&C2 * gender
##EM13
gee_model_13 <- geeglm(depression ~ educational_mobility_residual * gender + age  + Race + urbanicity + stress + health + financial + warmth+ height, 
                       data = df_long, 
                       id = hhidpn, 
                       family = binomial(link = "logit"),
                       corstr = "exchangeable")
summary(gee_model_13)

##EM14
gee_model_14 <- geeglm(depression ~ educational_mobility_H * gender + age  + Race + urbanicity+ stress + health + financial + warmth+ height,
                       data = df_long, 
                       id = hhidpn, 
                       family = binomial(link = "logit"), 
                       corstr = "exchangeable")
summary(gee_model_14)

##EM15
gee_model_15 <- geeglm(depression ~ educational_mobility_pr * gender + age  + Race + urbanicity+ stress + health + financial + warmth+ height,
                       data = df_long, 
                       id = hhidpn, 
                       family = binomial(link = "logit"), 
                       corstr = "exchangeable")
summary(gee_model_15)
tab_model(gee_model_13,gee_model_14,gee_model_15) 

#common variables
gee_model_16 <- geeglm(depression ~ educational_mobility_residual*gender +age+height,
                       data = df_long, 
                       id = hhidpn, 
                       family = binomial(link = "logit"),
                       corstr = "exchangeable")
tab_model(gee_model_16) 

gee_model_17 <- geeglm(depression ~ educational_mobility_pr*gender +age+height,
                       data = df_long, 
                       id = hhidpn, 
                       family = binomial(link = "logit"),
                       corstr = "exchangeable")
tab_model(gee_model_17) 

#model 18
gee_model_18 <- geeglm(depression ~ educational_mobility_pr + gender + age  + Race + urbanicity,
                       data = df_long, 
                       id = hhidpn, 
                       family = binomial(link = "logit"),
                       corstr = "exchangeable")
tab_model(gee_model_18) 


####Moderating effect####
#adjusted for DAGs
##EM resi_drink_now
gee_resi_drink_now <- geeglm(depression ~ educational_mobility_residual*drink_now + age + gender +Race+ urbanicity +height
                             +stress + health + financial + warmth, 
                             data = df_long, 
                             id = hhidpn, 
                             family = binomial(link = "logit"),
                             corstr = "exchangeable")
summary(gee_resi_drink_now)


##EM pr_drink_now
gee_pr_drink_now <- geeglm(depression ~ educational_mobility_pr*drink_now + age + gender +Race+ urbanicity +height
                           +stress + health + financial + warmth,
                           data = df_long, 
                           id = hhidpn, 
                           family = binomial(link = "logit"), 
                           corstr = "exchangeable")
summary(gee_pr_drink_now)
tab_model(gee_resi_drink_now,gee_pr_drink_now)

##EM resi_smoke_now
gee_resi_smoke_now <- geeglm(depression ~ educational_mobility_residual*smoke_now + age + gender +Race+ urbanicity +height
                             +stress + health + financial + warmth, 
                             data = df_long, 
                             id = hhidpn, 
                             family = binomial(link = "logit"),
                             corstr = "exchangeable")
summary(gee_resi_smoke_now)


##EM pr_smoke_now
gee_pr_smoke_now <- geeglm(depression ~ educational_mobility_pr*smoke_now + age + gender +Race+ urbanicity +height
                           +stress + health + financial + warmth,
                           data = df_long, 
                           id = hhidpn, 
                           family = binomial(link = "logit"), 
                           corstr = "exchangeable")
summary(gee_pr_smoke_now)
tab_model(gee_resi_smoke_now,gee_pr_smoke_now)

##EM resi_soc_activity
gee_resi_soc_activity <- geeglm(depression ~ educational_mobility_residual*soc_activity + age + gender +Race+ urbanicity +height
                                +stress + health + financial + warmth, 
                                data = df_long, 
                                id = hhidpn, 
                                family = binomial(link = "logit"),
                                corstr = "exchangeable")
summary(gee_resi_soc_activity)


##EM pr_soc_activity
gee_pr_soc_activity <- geeglm(depression ~ educational_mobility_pr*soc_activity + age + gender +Race+ urbanicity +height
                              +stress + health + financial + warmth,
                              data = df_long, 
                              id = hhidpn, 
                              family = binomial(link = "logit"), 
                              corstr = "exchangeable")
summary(gee_pr_soc_activity)
tab_model(gee_resi_soc_activity,gee_pr_soc_activity)

##EM resi_BMI
gee_resi_BMI <- geeglm(depression ~ educational_mobility_residual*BMI + age + gender +Race+ urbanicity +height
                       +stress + health + financial + warmth, 
                       data = df_long, 
                       id = hhidpn, 
                       family = binomial(link = "logit"),
                       corstr = "exchangeable")
summary(gee_resi_BMI)


##EM pr_BMI
gee_pr_BMI <- geeglm(depression ~ educational_mobility_pr*BMI + age + gender +Race+ urbanicity +height
                     +stress + health + financial + warmth,
                     data = df_long, 
                     id = hhidpn, 
                     family = binomial(link = "logit"), 
                     corstr = "exchangeable")
summary(gee_pr_BMI)
tab_model(gee_resi_BMI,gee_pr_BMI)

##EM resi_marital_status
gee_resi_marital_status <- geeglm(depression ~ educational_mobility_residual*marital_status + age + gender +Race+ urbanicity +height
                                  +stress + health + financial + warmth, 
                                  data = df_long, 
                                  id = hhidpn, 
                                  family = binomial(link = "logit"),
                                  corstr = "exchangeable")
summary(gee_resi_marital_status)


##EM pr_marital_status
gee_pr_marital_status <- geeglm(depression ~ educational_mobility_pr*marital_status + age + gender +Race+ urbanicity +height
                                +stress + health + financial + warmth,
                                data = df_long, 
                                id = hhidpn, 
                                family = binomial(link = "logit"), 
                                corstr = "exchangeable")
summary(gee_pr_marital_status)
tab_model(gee_resi_marital_status,gee_pr_marital_status)

##EM resi_n_chronic_category
gee_resi_n_chronic_category <- geeglm(depression ~ educational_mobility_residual*n_chronic_category + age + gender +Race+ urbanicity +height
                                      +stress + health + financial + warmth, 
                                      data = df_long, 
                                      id = hhidpn, 
                                      family = binomial(link = "logit"),
                                      corstr = "exchangeable")
summary(gee_resi_n_chronic_category)


##EM pr_n_chronic_category
gee_pr_n_chronic_category <- geeglm(depression ~ educational_mobility_pr*n_chronic_category + age + gender +Race+ urbanicity +height
                                    +stress + health + financial + warmth,
                                    data = df_long, 
                                    id = hhidpn, 
                                    family = binomial(link = "logit"), 
                                    corstr = "exchangeable")
summary(gee_pr_n_chronic_category)
tab_model(gee_resi_n_chronic_category,gee_pr_n_chronic_category)

##EM resi_wealth_quartile
gee_resi_wealth_quartile <- geeglm(depression ~ educational_mobility_residual*wealth_quartile + age + gender +Race+ urbanicity +height
                                   +stress + health + financial + warmth, 
                                   data = df_long, 
                                   id = hhidpn, 
                                   family = binomial(link = "logit"),
                                   corstr = "exchangeable")
summary(gee_resi_wealth_quartile)


##EM pr_wealth_quartile
gee_pr_wealth_quartile <- geeglm(depression ~ educational_mobility_pr*wealth_quartile + age + gender +Race+ urbanicity +height
                                 +stress + health + financial + warmth,
                                 data = df_long, 
                                 id = hhidpn, 
                                 family = binomial(link = "logit"), 
                                 corstr = "exchangeable")
summary(gee_pr_wealth_quartile)
tab_model(gee_resi_wealth_quartile,gee_pr_wealth_quartile)

####mediation effect analysis####
# drink now residual
gee_med_resi <- geeglm(drink_now ~ educational_mobility_residual + age + gender + Race +height +
                       +stress + health + warmth + financial + urbanicity, 
                       data = df_long, 
                       id = hhidpn, 
                       family = binomial(link = "probit"),
                       corstr = "exchangeable")

gee_resi_drink <- geeglm(depression ~ educational_mobility_residual * drink_now + age + gender + Race +height
                         +stress + health + warmth + financial + urbanicity, 
                         data = df_long, 
                         id = hhidpn,
                         family = binomial(link = "probit"),
                         corstr = "exchangeable")

med_drink_resi <- mediate(model.m = gee_med_resi, model.y = gee_resi_drink, 
                          treat = 'educational_mobility_residual', mediator = 'drink_now', data = df_long)
summary(med_drink_resi)

# drink now pr
# Downward mobility 
gee_med_downward <- geeglm(drink_now ~ downward_mobility + age + gender + Race +height
                           +stress + health + warmth + financial + urbanicity, 
                           data = df_downward_mobility,
                           id = hhidpn,
                           family = binomial(link = "probit"),
                           corstr = "exchangeable")

gee_pr_drink_downward <- geeglm(depression ~ downward_mobility * drink_now + age + gender + Race +height
                                +stress + health + warmth + financial + urbanicity, 
                                data = df_downward_mobility,
                                id = hhidpn,
                                family = binomial(link = "probit"),
                                corstr = "exchangeable")

med_drink_downward <- mediate(model.m = gee_med_downward, model.y = gee_pr_drink_downward,
                              treat = 'downward_mobility', mediator = 'drink_now', data = df_downward_mobility)

summary(med_drink_downward)

# Stably middle 
gee_med_stably_middle <- geeglm(drink_now ~ stably_middle + age + gender + Race +height
                                +stress + health + warmth + financial + urbanicity, 
                                data = df_stably_middle,
                                id = hhidpn,
                                family = binomial(link = "probit"),
                                corstr = "exchangeable")

gee_pr_drink_stably_middle <- geeglm(depression ~ stably_middle * drink_now + age + gender + Race +height
                                     +stress + health + warmth + financial + urbanicity, 
                                     data = df_stably_middle,
                                     id = hhidpn,
                                     family = binomial(link = "probit"),
                                     corstr = "exchangeable")

med_drink_stably_middle <- mediate(model.m = gee_med_stably_middle, model.y = gee_pr_drink_stably_middle,
                                   treat = 'stably_middle', mediator = 'drink_now', data = df_stably_middle)

summary(med_drink_stably_middle)

# upward_mobility
gee_med_upward_mobility <- geeglm(drink_now ~ upward_mobility + age + gender + Race +height
                                  +stress + health + warmth + financial + urbanicity, 
                                  data = df_upward_mobility,
                                  id = hhidpn,
                                  family = binomial(link = "probit"),
                                  corstr = "exchangeable")

gee_pr_drink_upward_mobility <- geeglm(depression ~ upward_mobility * drink_now + age + gender + Race +height
                                       +stress + health + warmth + financial + urbanicity, 
                                       data = df_upward_mobility,
                                       id = hhidpn,
                                       family = binomial(link = "probit"),
                                       corstr = "exchangeable")

med_drink_upward_mobility <- mediate(model.m = gee_med_upward_mobility, model.y = gee_pr_drink_upward_mobility,
                                     treat = 'upward_mobility', mediator = 'drink_now', data = df_upward_mobility)

summary(med_drink_upward_mobility)


# stably_high
gee_med_stably_high <- geeglm(drink_now ~ stably_high + age + gender + Race +height +stress + health + warmth + financial + urbanicity, 
                              data = df_stably_high,
                              id = hhidpn,
                              family = binomial(link = "probit"),
                              corstr = "exchangeable")

gee_pr_drink_stably_high <- geeglm(depression ~ stably_high * drink_now + age + gender + Race +height +stress + health + warmth + financial + urbanicity,
                                   data = df_stably_high,
                                   id = hhidpn,
                                   family = binomial(link = "probit"),
                                   corstr = "exchangeable")

med_drink_stably_high <- mediate(model.m = gee_med_stably_high, model.y = gee_pr_drink_stably_high,
                                 treat = 'stably_high', mediator = 'drink_now', data = df_stably_high)

summary(med_drink_stably_high)

### smoke now residual ###

gee_med_resi2 <- geeglm(smoke_now ~ educational_mobility_residual + age + gender + Race +height +stress + health + warmth + financial + urbanicity,
                        data = df_long, 
                        id = hhidpn, 
                        family = binomial(link = "probit"),
                        corstr = "exchangeable")

gee_resi_smoke <- geeglm(depression ~ educational_mobility_residual * smoke_now + age + gender + Race +height +stress + health + warmth + financial + urbanicity,
                         data = df_long, 
                         id =hhidpn,
                         family = binomial(link = "probit"),
                         corstr = "exchangeable")

med_smoke_resi <- mediate(model.m = gee_med_resi2, model.y = gee_resi_smoke, 
                          treat = 'educational_mobility_residual', mediator = 'smoke_now', data = df_long)
summary(med_smoke_resi)

# smoke now pr
# Downward mobility 
gee_med_downward <- geeglm(smoke_now ~ downward_mobility + age + gender + Race +height +stress + health + warmth + financial + urbanicity,
                           data = df_downward_mobility,
                           id = hhidpn,
                           family = binomial(link = "probit"),
                           corstr = "exchangeable")

gee_pr_smoke_downward <- geeglm(depression ~ downward_mobility * smoke_now + age + gender + Race +height +stress + health + warmth + financial + urbanicity,
                                data = df_downward_mobility,
                                id = hhidpn,
                                family = binomial(link = "probit"),
                                corstr = "exchangeable")

med_smoke_downward <- mediate(model.m = gee_med_downward, model.y = gee_pr_smoke_downward,
                              treat = 'downward_mobility', mediator = 'smoke_now', data = df_downward_mobility)

summary(med_smoke_downward)

# Stably middle 
gee_med_stably_middle <- geeglm(smoke_now ~ stably_middle + age + gender + Race +height +stress + health + warmth + financial + urbanicity,
                                data = df_stably_middle,
                                id = hhidpn,
                                family = binomial(link = "probit"),
                                corstr = "exchangeable")

gee_pr_smoke_stably_middle <- geeglm(depression ~ stably_middle * smoke_now + age + gender + Race +height +stress + health + warmth + financial + urbanicity,
                                     data = df_stably_middle,
                                     id = hhidpn,
                                     family = binomial(link = "probit"),
                                     corstr = "exchangeable")

med_smoke_stably_middle <- mediate(model.m = gee_med_stably_middle, model.y = gee_pr_smoke_stably_middle,
                                   treat = 'stably_middle', mediator = 'smoke_now', data = df_stably_middle)

summary(med_smoke_stably_middle)

# upward_mobility
gee_med_upward_mobility <- geeglm(smoke_now ~ upward_mobility + age + gender + Race +height +stress + health + warmth + financial + urbanicity,
                                  data = df_upward_mobility,
                                  id = hhidpn,
                                  family = binomial(link = "probit"),
                                  corstr = "exchangeable")

gee_pr_smoke_upward_mobility <- geeglm(depression ~ upward_mobility * smoke_now + age + gender + Race +height +stress + health + warmth + financial + urbanicity,
                                       data = df_upward_mobility,
                                       id = hhidpn,
                                       family = binomial(link = "probit"),
                                       corstr = "exchangeable")

med_smoke_upward_mobility <- mediate(model.m = gee_med_upward_mobility, model.y = gee_pr_smoke_upward_mobility,
                                     treat = 'upward_mobility', mediator = 'smoke_now', data = df_upward_mobility)

summary(med_smoke_upward_mobility)


# stably_high
gee_med_stably_high <- geeglm(smoke_now ~ stably_high + age + gender + Race +height +stress + health + warmth + financial + urbanicity,
                              data = df_stably_high,
                              id = hhidpn,
                              family = binomial(link = "probit"),
                              corstr = "exchangeable")

gee_pr_smoke_stably_high <- geeglm(depression ~ stably_high * smoke_now + age + gender + Race +height +stress + health + warmth + financial + urbanicity,
                                   data = df_stably_high,
                                   id = hhidpn,
                                   family = binomial(link = "probit"),
                                   corstr = "exchangeable")

med_smoke_stably_high <- mediate(model.m = gee_med_stably_high, model.y = gee_pr_smoke_stably_high,
                                 treat = 'stably_high', mediator = 'smoke_now', data = df_stably_high)

summary(med_smoke_stably_high)

# soc_activity residual
gee_med_resi3 <- geeglm(soc_activity ~ educational_mobility_residual + age + gender + Race +height +stress + health + warmth + financial + urbanicity,
                        data = df_long, 
                        id = hhidpn, 
                        family = binomial(link = "probit"),
                        corstr = "exchangeable")

gee_resi_activity <- geeglm(depression ~ educational_mobility_residual * soc_activity + age + gender + Race +height +stress + health + warmth + financial + urbanicity,
                            data = df_long, 
                            id = hhidpn,
                            family = binomial(link = "probit"),
                            corstr = "exchangeable")

med_activity_resi <- mediate(model.m = gee_med_resi3, model.y = gee_resi_activity, 
                             treat = 'educational_mobility_residual', mediator = 'soc_activity', data = df_long)
summary(med_activity_resi)

# soc_activity pr
# Downward mobility 
gee_med_downward <- geeglm(soc_activity ~ downward_mobility + age + gender + Race +height +stress + health + warmth + financial + urbanicity,
                           data = df_downward_mobility,
                           id = hhidpn,
                           family = binomial(link = "probit"),
                           corstr = "exchangeable")

gee_pr_activity_downward <- geeglm(depression ~ downward_mobility * soc_activity + age + gender + Race +height +stress + health + warmth + financial + urbanicity,
                                   data = df_downward_mobility,
                                   id = hhidpn,
                                   family = binomial(link = "probit"),
                                   corstr = "exchangeable")

med_activity_downward <- mediate(model.m = gee_med_downward, model.y = gee_pr_activity_downward,
                                 treat = 'downward_mobility', mediator = 'soc_activity', data = df_downward_mobility)

summary(med_activity_downward)

# Stably middle 
gee_med_stably_middle <- geeglm(soc_activity ~ stably_middle + age + gender + Race +height +stress + health + warmth + financial + urbanicity,
                                data = df_stably_middle,
                                id = hhidpn,
                                family = binomial(link = "probit"),
                                corstr = "exchangeable")

gee_pr_activity_stably_middle <- geeglm(depression ~ stably_middle * soc_activity + age + gender + Race +height +stress + health + warmth + financial + urbanicity,
                                        data = df_stably_middle,
                                        id = hhidpn,
                                        family = binomial(link = "probit"),
                                        corstr = "exchangeable")

med_activity_stably_middle <- mediate(model.m = gee_med_stably_middle, model.y = gee_pr_activity_stably_middle,
                                      treat = 'stably_middle', mediator = 'soc_activity', data = df_stably_middle)

summary(med_activity_stably_middle)

# upward_mobility
gee_med_upward_mobility <- geeglm(soc_activity ~ upward_mobility + age + gender + Race +height +stress + health + warmth + financial + urbanicity,
                                  data = df_upward_mobility,
                                  id = hhidpn,
                                  family = binomial(link = "probit"),
                                  corstr = "exchangeable")

gee_pr_activity_upward_mobility <- geeglm(depression ~ upward_mobility * soc_activity + age + gender + Race +height +stress + health + warmth + financial + urbanicity,
                                          data = df_upward_mobility,
                                          id = hhidpn,
                                          family = binomial(link = "probit"),
                                          corstr = "exchangeable")

med_activity_upward_mobility <- mediate(model.m = gee_med_upward_mobility, model.y = gee_pr_activity_upward_mobility,
                                        treat = 'upward_mobility', mediator = 'soc_activity', data = df_upward_mobility)

summary(med_activity_upward_mobility)


# stably_high
gee_med_stably_high <- geeglm(soc_activity ~ stably_high + age + gender + Race +height +stress + health + warmth + financial + urbanicity,
                              data = df_stably_high,
                              id = hhidpn,
                              family = binomial(link = "probit"),
                              corstr = "exchangeable")

gee_pr_activity_stably_high <- geeglm(depression ~ stably_high * soc_activity + age + gender + Race +height +stress + health + warmth + financial + urbanicity,
                                      data = df_stably_high,
                                      id = hhidpn,
                                      family = binomial(link = "probit"),
                                      corstr = "exchangeable")

med_activity_stably_high <- mediate(model.m = gee_med_stably_high, model.y = gee_pr_activity_stably_high,
                                    treat = 'stably_high', mediator = 'soc_activity', data = df_stably_high)

summary(med_activity_stably_high)


# BMI residual
gee_med_resi4<- geeglm(BMI ~ educational_mobility_residual + age + gender + Race +height +stress + health + warmth + financial + urbanicity,
                       data = df_long, 
                       id = hhidpn, 
                       family = binomial(link = "probit"),
                       corstr = "exchangeable")

gee_resi_BMI <- geeglm(depression ~ educational_mobility_residual * BMI + age + gender + Race +height +stress + health + warmth + financial + urbanicity,
                       data = df_long, 
                       id = hhidpn,
                       family = binomial(link = "probit"),
                       corstr = "exchangeable")

med_BMI_resi <- mediate(model.m = gee_med_resi4, model.y = gee_resi_BMI, 
                        treat = 'educational_mobility_residual', mediator = 'BMI', data = df_long)
summary(med_BMI_resi)

# BMI pr
# Downward mobility 
gee_med_downward <- geeglm(BMI ~ downward_mobility + age + gender + Race +height +stress + health + warmth + financial + urbanicity,
                           data = df_downward_mobility,
                           id = hhidpn,
                           family = binomial(link = "probit"),
                           corstr = "exchangeable")

gee_pr_BMI_downward <- geeglm(depression ~ downward_mobility * BMI + age + gender + Race +height +stress + health + warmth + financial + urbanicity,
                              data = df_downward_mobility,
                              id = hhidpn,
                              family = binomial(link = "probit"),
                              corstr = "exchangeable")

med_BMI_downward <- mediate(model.m = gee_med_downward, model.y = gee_pr_BMI_downward,
                            treat = 'downward_mobility', mediator = 'BMI', data = df_downward_mobility)

summary(med_BMI_downward)

# Stably middle 
gee_med_stably_middle <- geeglm(BMI ~ stably_middle + age + gender + Race +height +stress + health + warmth + financial + urbanicity,
                                data = df_stably_middle,
                                id = hhidpn,
                                family = binomial(link = "probit"),
                                corstr = "exchangeable")

gee_pr_BMI_stably_middle <- geeglm(depression ~ stably_middle * BMI + age + gender + Race +height +stress + health + warmth + financial + urbanicity,
                                   data = df_stably_middle,
                                   id = hhidpn,
                                   family = binomial(link = "probit"),
                                   corstr = "exchangeable")

med_BMI_stably_middle <- mediate(model.m = gee_med_stably_middle, model.y = gee_pr_BMI_stably_middle,
                                 treat = 'stably_middle', mediator = 'BMI', data = df_stably_middle)

summary(med_BMI_stably_middle)

# upward_mobility
gee_med_upward_mobility <- geeglm(BMI ~ upward_mobility + age + gender + Race +height +stress + health + warmth + financial + urbanicity,
                                  data = df_upward_mobility,
                                  id = hhidpn,
                                  family = binomial(link = "probit"),
                                  corstr = "exchangeable")

gee_pr_BMI_upward_mobility <- geeglm(depression ~ upward_mobility * BMI + age + gender + Race +height +stress + health + warmth + financial + urbanicity,
                                     data = df_upward_mobility,
                                     id = hhidpn,
                                     family = binomial(link = "probit"),
                                     corstr = "exchangeable")

med_BMI_upward_mobility <- mediate(model.m = gee_med_upward_mobility, model.y = gee_pr_BMI_upward_mobility,
                                   treat = 'upward_mobility', mediator = 'BMI', data = df_upward_mobility)

summary(med_BMI_upward_mobility)


# stably_high
gee_med_stably_high <- geeglm(BMI ~ stably_high + age + gender + Race +height +stress + health + warmth + financial + urbanicity,
                              data = df_stably_high,
                              id = hhidpn,
                              family = binomial(link = "probit"),
                              corstr = "exchangeable")

gee_pr_BMI_stably_high <- geeglm(depression ~ stably_high * BMI + age + gender + Race +height +stress + health + warmth + financial + urbanicity,
                                 data = df_stably_high,
                                 id = hhidpn,
                                 family = binomial(link = "probit"),
                                 corstr = "exchangeable")

med_BMI_stably_high <- mediate(model.m = gee_med_stably_high, model.y = gee_pr_BMI_stably_high,
                               treat = 'stably_high', mediator = 'BMI', data = df_stably_high)

summary(med_BMI_stably_high)

# wealth_numeric residual
gee_med_resi5<- geeglm(wealth_numeric ~ educational_mobility_residual + age + gender + Race +height +stress + health + warmth + financial + urbanicity,
                       data = df_long, 
                       id = hhidpn, 
                       family = binomial(link = "probit"),
                       corstr = "exchangeable")

gee_resi_wealth <- geeglm(depression ~ educational_mobility_residual * wealth_numeric + age + gender + Race +height +stress + health + warmth + financial + urbanicity,
                          data = df_long, 
                          id = hhidpn,
                          family = binomial(link = "probit"),
                          corstr = "exchangeable")

med_wealth_resi <- mediate(model.m = gee_med_resi5, model.y = gee_resi_wealth, 
                           treat = 'educational_mobility_residual', mediator = 'wealth_numeric', data = df_long)
summary(med_wealth_resi)

# wealth_numeric pr
# Downward mobility 
gee_med_downward <- geeglm(wealth_numeric ~ downward_mobility + age + gender + Race +height +stress + health + warmth + financial + urbanicity,
                           data = df_downward_mobility,
                           id = hhidpn,
                           family = binomial(link = "probit"),
                           corstr = "exchangeable")

gee_pr_wealth_downward <- geeglm(depression ~ downward_mobility * wealth_numeric + age + gender + Race +height +stress + health + warmth + financial + urbanicity,
                                 data = df_downward_mobility,
                                 id = hhidpn,
                                 family = binomial(link = "probit"),
                                 corstr = "exchangeable")

med_wealth_downward <- mediate(model.m = gee_med_downward, model.y = gee_pr_wealth_downward,
                               treat = 'downward_mobility', mediator = 'wealth_numeric', data = df_downward_mobility)

summary(med_wealth_downward)

# Stably middle 
gee_med_stably_middle <- geeglm(wealth_numeric ~ stably_middle + age + gender + Race +height +stress + health + warmth + financial + urbanicity,
                                data = df_stably_middle,
                                id = hhidpn,
                                family = binomial(link = "probit"),
                                corstr = "exchangeable")

gee_pr_wealth_stably_middle <- geeglm(depression ~ stably_middle * wealth_numeric + age + gender + Race +height +stress + health + warmth + financial + urbanicity,
                                      data = df_stably_middle,
                                      id = hhidpn,
                                      family = binomial(link = "probit"),
                                      corstr = "exchangeable")

med_wealth_stably_middle <- mediate(model.m = gee_med_stably_middle, model.y = gee_pr_wealth_stably_middle,
                                    treat = 'stably_middle', mediator = 'wealth_numeric', data = df_stably_middle)

summary(med_wealth_stably_middle)

# upward_mobility
gee_med_upward_mobility <- geeglm(wealth_numeric ~ upward_mobility + age + gender + Race +height +stress + health + warmth + financial + urbanicity,
                                  data = df_upward_mobility,
                                  id = hhidpn,
                                  family = binomial(link = "probit"),
                                  corstr = "exchangeable")

gee_pr_wealth_upward_mobility <- geeglm(depression ~ upward_mobility * wealth_numeric + age + gender + Race +height +stress + health + warmth + financial + urbanicity,
                                        data = df_upward_mobility,
                                        id = hhidpn,
                                        family = binomial(link = "probit"),
                                        corstr = "exchangeable")

med_wealth_upward_mobility <- mediate(model.m = gee_med_upward_mobility, model.y = gee_pr_wealth_upward_mobility,
                                      treat = 'upward_mobility', mediator = 'wealth_numeric', data = df_upward_mobility)

summary(med_wealth_upward_mobility)


# stably_high
gee_med_stably_high <- geeglm(wealth_numeric ~ stably_high + age + gender + Race +height +stress + health + warmth + financial + urbanicity,
                              data = df_stably_high,
                              id = hhidpn,
                              family = binomial(link = "probit"),
                              corstr = "exchangeable")

gee_pr_wealth_stably_high <- geeglm(depression ~ stably_high * wealth_numeric + age + gender + Race +height +stress + health + warmth + financial + urbanicity,
                                    data = df_stably_high,
                                    id = hhidpn,
                                    family = binomial(link = "probit"),
                                    corstr = "exchangeable")

med_wealth_stably_high <- mediate(model.m = gee_med_stably_high, model.y = gee_pr_wealth_stably_high,
                                  treat = 'stably_high', mediator = 'wealth_numeric', data = df_stably_high)

summary(med_wealth_stably_high)

d0_sims <- med_wealth_stably_high$d0.sims  # control组(Stably low)的ACME模拟值
d1_sims <- med_wealth_stably_high$d1.sims  # treated组(Stably high)的ACME模拟值

# 计算组间差异
diff_sims <- d1_sims - d0_sims

# 计算点估计和95%CI
diff_estimate <- mean(diff_sims)
diff_ci <- quantile(diff_sims, probs = c(0.025, 0.975))

# 计算p值
p_value <- 2 * min(mean(diff_sims > 0), mean(diff_sims < 0))

cat(sprintf("\nACME差异(Stably high - Stably low) = %.4f (95%% CI: [%.4f, %.4f]), p = %.4f",
            diff_estimate, diff_ci[1], diff_ci[2], p_value))

library(ggplot2)
library(ggridges)

# 准备数据
plot_data <- data.frame(
  Group = rep(c("Stably low", "Stably high"), each = length(d0_sims)),
  ACME = c(d0_sims, d1_sims)
)
# 绘制分布图
ggplot(plot_data, aes(x = ACME, y = Group, fill = Group)) +
  geom_density_ridges(alpha = 0.7, scale = 0.9) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "red") +
  scale_fill_manual(values = c("#1f78b4", "#33a02c")) +
  labs(title = "Group Differences in Wealth Mediation Effects",
       subtitle = "Stably Low Group Shows Significantly Stronger Mediation",
       x = "ACME (Change in Depressive Symptoms Probability)", 
       y = "",
       caption = "Note: Density distributions of ACME estimates by group") +
  theme_minimal(base_size = 14) +
  theme(
    legend.position = "none",
    plot.title = element_text(face = "bold"),
    plot.caption = element_text(hjust = 0, color = "grey40")
  ) +
  # Repositioned annotation to top-left
  annotate("text", 
           x = min(plot_data$ACME) * 0.35, 
           y = 2.3,  # Adjusted for ridge plot y-scale
           label = sprintf("Difference = %.4f\n95%% CI: [%.4f, %.4f]", 
                           diff_estimate, diff_ci[1], diff_ci[2]),
           color = "darkred", 
           size = 5,
           hjust = 0) 


# marital_status residual
gee_med_resi6<- geeglm(marital_status_numeric ~ educational_mobility_residual + age + gender + Race +height +stress + health + warmth + financial + urbanicity,
                       data = df_long, 
                       id = hhidpn, 
                       family = binomial(link = "probit"),
                       corstr = "exchangeable")

gee_resi_mari <- geeglm(depression ~ educational_mobility_residual * marital_status_numeric + age + gender + Race +height +stress + health + warmth + financial + urbanicity,
                        data = df_long, 
                        id = hhidpn,
                        family = binomial(link = "probit"),
                        corstr = "exchangeable")

med_mari_resi <- mediate(model.m = gee_med_resi6, model.y = gee_resi_mari, 
                         treat = 'educational_mobility_residual', mediator = 'marital_status_numeric', data = df_long)
summary(med_mari_resi)

# Downward mobility 
gee_med_downward <- geeglm(marital_status_numeric ~ downward_mobility + age + gender + Race +height +stress + health + warmth + financial + urbanicity,
                           data = df_downward_mobility,
                           id = hhidpn,
                           family = binomial(link = "probit"),
                           corstr = "exchangeable")

gee_pr_mari_downward <- geeglm(depression ~ downward_mobility * marital_status_numeric + age + gender + Race +height +stress + health + warmth + financial + urbanicity,
                               data = df_downward_mobility,
                               id = hhidpn,
                               family = binomial(link = "probit"),
                               corstr = "exchangeable")

med_mari_downward <- mediate(model.m = gee_med_downward, model.y = gee_pr_mari_downward,
                             treat = 'downward_mobility', mediator = 'marital_status_numeric', data = df_downward_mobility)

summary(med_mari_downward)

# Stably middle 
gee_med_stably_middle <- geeglm(marital_status_numeric ~ stably_middle + age + gender + Race +height +stress + health + warmth + financial + urbanicity,
                                data = df_stably_middle,
                                id = hhidpn,
                                family = binomial(link = "probit"),
                                corstr = "exchangeable")

gee_pr_mari_stably_middle <- geeglm(depression ~ stably_middle * marital_status_numeric + age + gender + Race +height +stress + health + warmth + financial + urbanicity,
                                    data = df_stably_middle,
                                    id = hhidpn,
                                    family = binomial(link = "probit"),
                                    corstr = "exchangeable")

med_mari_stably_middle <- mediate(model.m = gee_med_stably_middle, model.y = gee_pr_mari_stably_middle,
                                  treat = 'stably_middle', mediator = 'marital_status_numeric', data = df_stably_middle)

summary(med_mari_stably_middle)

# upward_mobility
gee_med_upward_mobility <- geeglm(marital_status_numeric ~ upward_mobility + age + gender + Race +height +stress + health + warmth + financial + urbanicity,
                                  data = df_upward_mobility,
                                  id = hhidpn,
                                  family = binomial(link = "probit"),
                                  corstr = "exchangeable")

gee_pr_mari_upward_mobility <- geeglm(depression ~ upward_mobility * marital_status_numeric + age + gender + Race +height +stress + health + warmth + financial + urbanicity,
                                      data = df_upward_mobility,
                                      id = hhidpn,
                                      family = binomial(link = "probit"),
                                      corstr = "exchangeable")

med_mari_upward_mobility <- mediate(model.m = gee_med_upward_mobility, model.y = gee_pr_mari_upward_mobility,
                                    treat = 'upward_mobility', mediator = 'marital_status_numeric', data = df_upward_mobility)

summary(med_mari_upward_mobility)

# stably_high
gee_med_stably_high <- geeglm(marital_status_numeric ~ stably_high + age + gender + Race +height +stress + health + warmth + financial + urbanicity,
                              data = df_stably_high,
                              id = hhidpn,
                              family = binomial(link = "probit"),
                              corstr = "exchangeable")

gee_pr_mari_stably_high <- geeglm(depression ~ stably_high * marital_status_numeric + age + gender + Race +height +stress + health + warmth + financial + urbanicity,
                                  data = df_stably_high,
                                  id = hhidpn,
                                  family = binomial(link = "probit"),
                                  corstr = "exchangeable")

med_mari_stably_high <- mediate(model.m = gee_med_stably_high, model.y = gee_pr_mari_stably_high,
                                treat = 'stably_high', mediator = 'marital_status_numeric', data = df_stably_high)

summary(med_mari_stably_high)



# n_chronic_numeric residual
gee_med_resi7<- geeglm(n_chronic_numeric ~ educational_mobility_residual + age + gender + Race +height +stress + health + warmth + financial + urbanicity,
                       data = df_long, 
                       id = hhidpn, 
                       family = binomial(link = "probit"),
                       corstr = "exchangeable")

gee_resi_chronic <- geeglm(depression ~ educational_mobility_residual * n_chronic_numeric + age + gender + Race +height +stress + health + warmth + financial + urbanicity,
                           data = df_long, 
                           id = hhidpn,
                           family = binomial(link = "probit"),
                           corstr = "exchangeable")

med_chronic_resi <- mediate(model.m = gee_med_resi7, model.y = gee_resi_chronic, 
                            treat = 'educational_mobility_residual', mediator = 'n_chronic_numeric', data = df_long)
summary(med_chronic_resi)

# n_chronic_numeric pr
# Downward mobility 
gee_med_downward <- geeglm(n_chronic_numeric ~ downward_mobility + age + gender + Race +height +stress + health + warmth + financial + urbanicity,
                           data = df_downward_mobility,
                           id = hhidpn,
                           family = binomial(link = "probit"),
                           corstr = "exchangeable")

gee_pr_chronic_downward <- geeglm(depression ~ downward_mobility * n_chronic_numeric + age + gender + Race +height +stress + health + warmth + financial + urbanicity,
                                  data = df_downward_mobility,
                                  id = hhidpn,
                                  family = binomial(link = "probit"),
                                  corstr = "exchangeable")

med_chronic_downward <- mediate(model.m = gee_med_downward, model.y = gee_pr_chronic_downward,
                                treat = 'downward_mobility', mediator = 'n_chronic_numeric', data = df_downward_mobility)

summary(med_chronic_downward)

# Stably middle 
gee_med_stably_middle <- geeglm(n_chronic_numeric ~ stably_middle + age + gender + Race +height +stress + health + warmth + financial + urbanicity,
                                data = df_stably_middle,
                                id = hhidpn,
                                family = binomial(link = "probit"),
                                corstr = "exchangeable")

gee_pr_chronic_stably_middle <- geeglm(depression ~ stably_middle * n_chronic_numeric + age + gender + Race +height +stress + health + warmth + financial + urbanicity,
                                       data = df_stably_middle,
                                       id = hhidpn,
                                       family = binomial(link = "probit"),
                                       corstr = "exchangeable")

med_chronic_stably_middle <- mediate(model.m = gee_med_stably_middle, model.y = gee_pr_chronic_stably_middle,
                                     treat = 'stably_middle', mediator = 'n_chronic_numeric', data = df_stably_middle)

summary(med_chronic_stably_middle)

# upward_mobility
gee_med_upward_mobility <- geeglm(n_chronic_numeric ~ upward_mobility + age + gender + Race +height +stress + health + warmth + financial + urbanicity,
                                  data = df_upward_mobility,
                                  id = hhidpn,
                                  family = binomial(link = "probit"),
                                  corstr = "exchangeable")

gee_pr_chronic_upward_mobility <- geeglm(depression ~ upward_mobility * n_chronic_numeric + age + gender + Race +height +stress + health + warmth + financial + urbanicity,
                                         data = df_upward_mobility,
                                         id = hhidpn,
                                         family = binomial(link = "probit"),
                                         corstr = "exchangeable")

med_chronic_upward_mobility <- mediate(model.m = gee_med_upward_mobility, model.y = gee_pr_chronic_upward_mobility,
                                       treat = 'upward_mobility', mediator = 'n_chronic_numeric', data = df_upward_mobility)

summary(med_chronic_upward_mobility)


# stably_high
gee_med_stably_high <- geeglm(n_chronic_numeric ~ stably_high + age + gender + Race +height +stress + health + warmth + financial + urbanicity,
                              data = df_stably_high,
                              id = hhidpn,
                              family = binomial(link = "probit"),
                              corstr = "exchangeable")

gee_pr_chronic_stably_high <- geeglm(depression ~ stably_high * n_chronic_numeric + age + gender + Race +height +stress + health + warmth + financial + urbanicity,
                                     data = df_stably_high,
                                     id = hhidpn,
                                     family = binomial(link = "probit"),
                                     corstr = "exchangeable")

med_chronic_stably_high <- mediate(model.m = gee_med_stably_high, model.y = gee_pr_chronic_stably_high,
                                   treat = 'stably_high', mediator = 'n_chronic_numeric', data = df_stably_high)

summary(med_chronic_stably_high)
