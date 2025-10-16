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


df <- read.csv("klosa_cleaning.csv")

df_long  <- df %>%
  pivot_longer(cols = c("depression2010","depression2012","depression2014","depression2016","depression2018","depression2020"), 
               names_to = "time", 
               values_to = "depression") %>%
  mutate(time = case_when(
    time == "depression2010" ~ 0,
    time == "depression2012" ~ riwmid_w4 - riwmid_w3,
    time == "depression2014" ~ riwmid_w5 - riwmid_w3,
    time == "depression2016" ~ riwmid_w6 - riwmid_w3,
    time == "depression2018" ~ riwmid_w7 - riwmid_w3,
    time == "depression2020" ~ riwmid_w8 - riwmid_w3,
    TRUE ~ NA_real_  
  ))

####longitudinal study####

df_long <- df_long[!(df_long$time == 0 & df_long$depression == 1 &!is.na(df_long$depression)), ]
#df_long <- na.omit(df_long[, c("educational_mobility_residual", "age", "gender", "Race", "urbanicity")])

####GEE####
# unadjusted model
##EM1
gee_model_1 <- geeglm(depression ~ educational_mobility_residual, 
                    data = df_long, 
                    id = ID, 
                    family = binomial(link = "logit"),
                    corstr = "exchangeable")
summary(gee_model_1)

##EM2
gee_model_2 <- geeglm(depression ~ educational_mobility_H , 
                      data = df_long, 
                      id = ID, 
                      family = binomial(link = "logit"), 
                      corstr = "exchangeable")
summary(gee_model_2)

##EM3
gee_model_3 <- geeglm(depression ~ educational_mobility_pr, 
                      data = df_long, 
                      id = ID, 
                      family = binomial(link = "logit"), 
                      corstr = "exchangeable")
summary(gee_model_3)
tab_model(gee_model_1,gee_model_2,gee_model_3)

# adjusted for C1 
##EM4
gee_model_4 <- geeglm(depression ~ educational_mobility_residual + age + gender +  urbanicity, 
                      data = df_long, 
                      id = ID, 
                      family = binomial(link = "logit"),
                      corstr = "exchangeable")
summary(gee_model_4)

##EM5
gee_model_5 <- geeglm(depression ~ educational_mobility_H + age + gender +  urbanicity, 
                      data = df_long, 
                      id = ID, 
                      family = binomial(link = "logit"), 
                      corstr = "exchangeable")
summary(gee_model_5)

##EM6
gee_model_6 <- geeglm(depression ~ educational_mobility_pr + age + gender  + urbanicity, 
                      data = df_long, 
                      id = ID, 
                      family = binomial(link = "logit"), 
                      corstr = "exchangeable")
summary(gee_model_6)
tab_model(gee_model_4,gee_model_5,gee_model_6)

##EM4
gee_model_10 <- geeglm(depression ~ educational_mobility_residual + age + gender+ urbanicity + height, 
                      data = df_long, 
                      id = ID, 
                      family = binomial(link = "logit"),
                      corstr = "exchangeable")
summary(gee_model_10)

##EM5
gee_model_11 <- geeglm(depression ~ educational_mobility_H + age + gender  + urbanicity+ height,
                      data = df_long, 
                      id = ID, 
                      family = binomial(link = "logit"), 
                      corstr = "exchangeable")
summary(gee_model_11)

##EM6
gee_model_12 <- geeglm(depression ~ educational_mobility_pr + age + gender + urbanicity+height,
                      data = df_long, 
                      id = ID, 
                      family = binomial(link = "logit"), 
                      corstr = "exchangeable")
summary(gee_model_12)
tab_model(gee_model_10,gee_model_11,gee_model_12)

# adjusted for C1&C2 * gender
##EM13
gee_model_13 <- geeglm(depression ~ educational_mobility_residual * gender + age  + urbanicity + height, 
                       data = df_long, 
                       id = ID, 
                       family = binomial(link = "logit"),
                       corstr = "exchangeable")
summary(gee_model_13)

##EM14
gee_model_14 <- geeglm(depression ~ educational_mobility_H * gender + age  + urbanicity+ height,
                       data = df_long, 
                       id = ID, 
                       family = binomial(link = "logit"), 
                       corstr = "exchangeable")
summary(gee_model_14)

##EM15
gee_model_15 <- geeglm(depression ~ educational_mobility_pr * gender + age   + urbanicity+ height,
                       data = df_long, 
                       id = ID, 
                       family = binomial(link = "logit"), 
                       corstr = "exchangeable")
summary(gee_model_15)
tab_model(gee_model_13,gee_model_14,gee_model_15)

#common variables
gee_model_16 <- geeglm(depression ~ educational_mobility_residual*gender +age+height,
                       data = df_long, 
                       id = ID, 
                       family = binomial(link = "logit"),
                       corstr = "exchangeable")
tab_model(gee_model_16) 

gee_model_17 <- geeglm(depression ~ educational_mobility_pr + gender +age,
                       data = df_long, 
                       id = ID, 
                       family = binomial(link = "logit"),
                       corstr = "exchangeable")
tab_model(gee_model_17) 

####Moderating effect####
#adjusted for DAGs
##EM resi_drink_now
gee_resi_drink_now <- geeglm(depression ~ educational_mobility_residual*drink_now + age + gender+ urbanicity + height, 
                             data = df_long, 
                             id = ID, 
                             family = binomial(link = "logit"),
                             corstr = "exchangeable")
summary(gee_resi_drink_now)


##EM pr_drink_now
gee_pr_drink_now <- geeglm(depression ~ educational_mobility_pr*drink_now + age + gender+ urbanicity + height,
                           data = df_long, 
                           id = ID, 
                           family = binomial(link = "logit"), 
                           corstr = "exchangeable")
summary(gee_pr_drink_now)
tab_model(gee_resi_drink_now,gee_pr_drink_now)

##EM resi_smoke_now
gee_resi_smoke_now <- geeglm(depression ~ educational_mobility_residual*smoke_now + age + gender+ urbanicity + height, 
                             data = df_long, 
                             id = ID, 
                             family = binomial(link = "logit"),
                             corstr = "exchangeable")
summary(gee_resi_smoke_now)


##EM pr_smoke_now
gee_pr_smoke_now <- geeglm(depression ~ educational_mobility_pr*smoke_now + age + gender+ urbanicity + height,
                           data = df_long, 
                           id = ID, 
                           family = binomial(link = "logit"), 
                           corstr = "exchangeable")
summary(gee_pr_smoke_now)
tab_model(gee_resi_smoke_now,gee_pr_smoke_now)

##EM resi_soc_activity
gee_resi_soc_activity <- geeglm(depression ~ educational_mobility_residual*soc_activity + age + gender+ urbanicity + height, 
                                data = df_long, 
                                id = ID, 
                                family = binomial(link = "logit"),
                                corstr = "exchangeable")
summary(gee_resi_soc_activity)


##EM pr_soc_activity
gee_pr_soc_activity <- geeglm(depression ~ educational_mobility_pr*soc_activity + age + gender+ urbanicity + height,
                              data = df_long, 
                              id = ID, 
                              family = binomial(link = "logit"), 
                              corstr = "exchangeable")
summary(gee_pr_soc_activity)
tab_model(gee_resi_soc_activity,gee_pr_soc_activity)

##EM resi_BMI
gee_resi_BMI <- geeglm(depression ~ educational_mobility_residual*BMI + age + gender+ urbanicity + height, 
                       data = df_long, 
                       id = ID, 
                       family = binomial(link = "logit"),
                       corstr = "exchangeable")
summary(gee_resi_BMI)


##EM pr_BMI
gee_pr_BMI <- geeglm(depression ~ educational_mobility_pr*BMI + age + gender+ urbanicity + height,
                     data = df_long, 
                     id = ID, 
                     family = binomial(link = "logit"), 
                     corstr = "exchangeable")
summary(gee_pr_BMI)
tab_model(gee_resi_BMI,gee_pr_BMI)

##EM resi_marital_status
gee_resi_marital_status <- geeglm(depression ~ educational_mobility_residual*marital_status + age + gender+ urbanicity + height, 
                                  data = df_long, 
                                  id = ID, 
                                  family = binomial(link = "logit"),
                                  corstr = "exchangeable")
summary(gee_resi_marital_status)


##EM pr_marital_status
gee_pr_marital_status <- geeglm(depression ~ educational_mobility_pr*marital_status+ age + gender+ urbanicity + height,
                                data = df_long, 
                                id = ID, 
                                family = binomial(link = "logit"), 
                                corstr = "exchangeable")
summary(gee_pr_marital_status)
tab_model(gee_resi_marital_status,gee_pr_marital_status)

##EM resi_n_chronic_category
gee_resi_n_chronic_category <- geeglm(depression ~ educational_mobility_residual*n_chronic_category + age + gender+ urbanicity + height, 
                                      data = df_long, 
                                      id = ID, 
                                      family = binomial(link = "logit"),
                                      corstr = "exchangeable")
summary(gee_resi_n_chronic_category)


##EM pr_n_chronic_category
gee_pr_n_chronic_category <- geeglm(depression ~ educational_mobility_pr*n_chronic_category + age + gender+ urbanicity + height,
                                    data = df_long, 
                                    id = ID, 
                                    family = binomial(link = "logit"), 
                                    corstr = "exchangeable")
summary(gee_pr_n_chronic_category)
tab_model(gee_resi_n_chronic_category,gee_pr_n_chronic_category)

##EM resi_wealth_quartile
gee_resi_wealth_quartile <- geeglm(depression ~ educational_mobility_residual*wealth_quartile + age + gender+ urbanicity + height, 
                                   data = df_long, 
                                   id = ID, 
                                   family = binomial(link = "logit"),
                                   corstr = "exchangeable")
summary(gee_resi_wealth_quartile)


##EM pr_wealth_quartile
gee_pr_wealth_quartile <- geeglm(depression ~ educational_mobility_pr*wealth_quartile + age + gender+ urbanicity + height,
                                 data = df_long, 
                                 id = ID, 
                                 family = binomial(link = "logit"), 
                                 corstr = "exchangeable")
summary(gee_pr_wealth_quartile)
tab_model(gee_resi_wealth_quartile,gee_pr_wealth_quartile)

####mediation effect analysis####
library(mediation)
df_long$drink_now <- as.numeric(as.character(df_long$drink_now))
df_long$smoke_now <- as.numeric(as.character(df_long$smoke_now))
df_long$soc_activity <- as.numeric(as.character(df_long$soc_activity))
df_long$BMI <- as.numeric(as.character(df_long$BMI))
df_long$marital_status_numeric <- ifelse(df_long$marital_status == "married/partnered", 0, 1)
df_long$n_chronic_numeric <- ifelse(df_long$n_chronic_category == "0 or 1 chronic", 0, 1)
df_long$wealth_numeric <- as.numeric(as.character(df_long$wealth_quartile))- 1

# pr
df_long <- df_long[!is.na(df_long$depression),]

df_long$downward_mobility <- ifelse(df_long$educational_mobility_pr == "Downward mobility", 1, 
                                    ifelse(df_long$educational_mobility_pr == "Stably low", 0, NA))

df_long$stably_middle <- ifelse(df_long$educational_mobility_pr == "Stably middle", 1, 
                                ifelse(df_long$educational_mobility_pr == "Stably low", 0, NA))

df_long$upward_mobility <- ifelse(df_long$educational_mobility_pr == "Upward mobility", 1, 
                                  ifelse(df_long$educational_mobility_pr == "Stably low", 0, NA))

df_long$stably_high <- ifelse(df_long$educational_mobility_pr == "Stably high", 1, 
                              ifelse(df_long$educational_mobility_pr == "Stably low", 0, NA))

df_downward_mobility <- df_long[!is.na(df_long$downward_mobility), ]
df_stably_middle <- df_long[!is.na(df_long$stably_middle), ]
df_upward_mobility <- df_long[!is.na(df_long$upward_mobility), ]
df_stably_high <- df_long[!is.na(df_long$stably_high), ]


# drink now residual
library(geepack)

gee_med_resi <- geeglm(drink_now ~ educational_mobility_residual + age + gender + height , 
                       data = df_long, 
                       id = ID, 
                       family = binomial(link = "probit"),
                       corstr = "exchangeable")

gee_resi_drink <- geeglm(depression ~ educational_mobility_residual * drink_now + age + gender + height + urbanicity, 
                         data = df_long, 
                         id = ID, 
                         family = binomial(link = "probit"),
                         corstr = "exchangeable")

med_drink_resi <- mediate(model.m = gee_med_resi, model.y = gee_resi_drink, 
                          treat = 'educational_mobility_residual', mediator = 'drink_now', data = df_long)
summary(med_drink_resi)

# drink now pr
# Downward mobility 
gee_med_downward <- geeglm(drink_now ~ downward_mobility + age + gender  +height+ urbanicity
                           , 
                           data = df_downward_mobility,
                           id = ID,
                           family = binomial(link = "probit"),
                           corstr = "exchangeable")

gee_pr_drink_downward <- geeglm(depression ~ downward_mobility * drink_now + age + gender  +height+ urbanicity
                                , 
                                data = df_downward_mobility,
                                id = ID,
                                family = binomial(link = "probit"),
                                corstr = "exchangeable")

med_drink_downward <- mediate(model.m = gee_med_downward, model.y = gee_pr_drink_downward,
                              treat = 'downward_mobility', mediator = 'drink_now', data = df_downward_mobility)

summary(med_drink_downward)

# Stably middle 
gee_med_stably_middle <- geeglm(drink_now ~ stably_middle + age + gender  +height+ urbanicity
                                , 
                                data = df_stably_middle,
                                id = ID,
                                family = binomial(link = "probit"),
                                corstr = "exchangeable")

gee_pr_drink_stably_middle <- geeglm(depression ~ stably_middle * drink_now + age + gender  +height+ urbanicity
                                     , 
                                     data = df_stably_middle,
                                     id = ID,
                                     family = binomial(link = "probit"),
                                     corstr = "exchangeable")

med_drink_stably_middle <- mediate(model.m = gee_med_stably_middle, model.y = gee_pr_drink_stably_middle,
                                   treat = 'stably_middle', mediator = 'drink_now', data = df_stably_middle)

summary(med_drink_stably_middle)

# upward_mobility
gee_med_upward_mobility <- geeglm(drink_now ~ upward_mobility + age + gender  +height+ urbanicity
                                  , 
                                  data = df_upward_mobility,
                                  id = ID,
                                  family = binomial(link = "probit"),
                                  corstr = "exchangeable")

gee_pr_drink_upward_mobility <- geeglm(depression ~ upward_mobility * drink_now + age + gender  +height+ urbanicity
                                       , 
                                       data = df_upward_mobility,
                                       id = ID,
                                       family = binomial(link = "probit"),
                                       corstr = "exchangeable")

med_drink_upward_mobility <- mediate(model.m = gee_med_upward_mobility, model.y = gee_pr_drink_upward_mobility,
                                     treat = 'upward_mobility', mediator = 'drink_now', data = df_upward_mobility)

summary(med_drink_upward_mobility)


# stably_high
gee_med_stably_high <- geeglm(drink_now ~ stably_high + age + gender  +height + urbanicity, 
                              data = df_stably_high,
                              id = ID,
                              family = binomial(link = "probit"),
                              corstr = "exchangeable")

gee_pr_drink_stably_high <- geeglm(depression ~ stably_high * drink_now + age + gender  +height + urbanicity,
                                   data = df_stably_high,
                                   id = ID,
                                   family = binomial(link = "probit"),
                                   corstr = "exchangeable")

med_drink_stably_high <- mediate(model.m = gee_med_stably_high, model.y = gee_pr_drink_stably_high,
                                 treat = 'stably_high', mediator = 'drink_now', data = df_stably_high)

summary(med_drink_stably_high)

### smoke now residual ###

gee_med_resi2 <- geeglm(smoke_now ~ educational_mobility_residual + age + gender  +height + urbanicity,
                        data = df_long, 
                        id = ID, 
                        family = binomial(link = "probit"),
                        corstr = "exchangeable")

gee_resi_smoke <- geeglm(depression ~ educational_mobility_residual * smoke_now + age + gender  +height+ urbanicity ,
                         data = df_long, 
                         id =ID,
                         family = binomial(link = "probit"),
                         corstr = "exchangeable")

med_smoke_resi <- mediate(model.m = gee_med_resi2, model.y = gee_resi_smoke, 
                          treat = 'educational_mobility_residual', mediator = 'smoke_now', data = df_long)
summary(med_smoke_resi)

# smoke now pr
# Downward mobility 
gee_med_downward <- geeglm(smoke_now ~ downward_mobility + age + gender  +height + urbanicity,
                           data = df_downward_mobility,
                           id = ID,
                           family = binomial(link = "probit"),
                           corstr = "exchangeable")

gee_pr_smoke_downward <- geeglm(depression ~ downward_mobility * smoke_now + age + gender  +height + urbanicity,
                                data = df_downward_mobility,
                                id = ID,
                                family = binomial(link = "probit"),
                                corstr = "exchangeable")

med_smoke_downward <- mediate(model.m = gee_med_downward, model.y = gee_pr_smoke_downward,
                              treat = 'downward_mobility', mediator = 'smoke_now', data = df_downward_mobility)

summary(med_smoke_downward)

# Stably middle 
gee_med_stably_middle <- geeglm(smoke_now ~ stably_middle + age + gender  +height+ urbanicity ,
                                data = df_stably_middle,
                                id = ID,
                                family = binomial(link = "probit"),
                                corstr = "exchangeable")

gee_pr_smoke_stably_middle <- geeglm(depression ~ stably_middle * smoke_now + age + gender  +height+ urbanicity ,
                                     data = df_stably_middle,
                                     id = ID,
                                     family = binomial(link = "probit"),
                                     corstr = "exchangeable")

med_smoke_stably_middle <- mediate(model.m = gee_med_stably_middle, model.y = gee_pr_smoke_stably_middle,
                                   treat = 'stably_middle', mediator = 'smoke_now', data = df_stably_middle)

summary(med_smoke_stably_middle)

# upward_mobility
gee_med_upward_mobility <- geeglm(smoke_now ~ upward_mobility + age + gender  +height + urbanicity,
                                  data = df_upward_mobility,
                                  id = ID,
                                  family = binomial(link = "probit"),
                                  corstr = "exchangeable")

gee_pr_smoke_upward_mobility <- geeglm(depression ~ upward_mobility * smoke_now + age + gender  +height + urbanicity,
                                       data = df_upward_mobility,
                                       id = ID,
                                       family = binomial(link = "probit"),
                                       corstr = "exchangeable")

med_smoke_upward_mobility <- mediate(model.m = gee_med_upward_mobility, model.y = gee_pr_smoke_upward_mobility,
                                     treat = 'upward_mobility', mediator = 'smoke_now', data = df_upward_mobility)

summary(med_smoke_upward_mobility)


# stably_high
gee_med_stably_high <- geeglm(smoke_now ~ stably_high + age + gender  +height + urbanicity,
                              data = df_stably_high,
                              id = ID,
                              family = binomial(link = "probit"),
                              corstr = "exchangeable")

gee_pr_smoke_stably_high <- geeglm(depression ~ stably_high * smoke_now + age + gender  +height + urbanicity,
                                   data = df_stably_high,
                                   id = ID,
                                   family = binomial(link = "probit"),
                                   corstr = "exchangeable")

med_smoke_stably_high <- mediate(model.m = gee_med_stably_high, model.y = gee_pr_smoke_stably_high,
                                 treat = 'stably_high', mediator = 'smoke_now', data = df_stably_high)

summary(med_smoke_stably_high)

# soc_activity residual
gee_med_resi3 <- geeglm(soc_activity ~ educational_mobility_residual + age + gender  +height + urbanicity,
                        data = df_long, 
                        id = ID, 
                        family = binomial(link = "probit"),
                        corstr = "exchangeable")

gee_resi_activity <- geeglm(depression ~ educational_mobility_residual * soc_activity + age + gender  +height + urbanicity,
                            data = df_long, 
                            id = ID,
                            family = binomial(link = "probit"),
                            corstr = "exchangeable")

med_activity_resi <- mediate(model.m = gee_med_resi3, model.y = gee_resi_activity, 
                             treat = 'educational_mobility_residual', mediator = 'soc_activity', data = df_long)
summary(med_activity_resi)

# soc_activity pr
# Downward mobility 
gee_med_downward <- geeglm(soc_activity ~ downward_mobility + age + gender  +height + urbanicity,
                           data = df_downward_mobility,
                           id = ID,
                           family = binomial(link = "probit"),
                           corstr = "exchangeable")

gee_pr_activity_downward <- geeglm(depression ~ downward_mobility * soc_activity + age + gender  +height + urbanicity,
                                   data = df_downward_mobility,
                                   id = ID,
                                   family = binomial(link = "probit"),
                                   corstr = "exchangeable")

med_activity_downward <- mediate(model.m = gee_med_downward, model.y = gee_pr_activity_downward,
                                 treat = 'downward_mobility', mediator = 'soc_activity', data = df_downward_mobility)

summary(med_activity_downward)

# Stably middle 
gee_med_stably_middle <- geeglm(soc_activity ~ stably_middle + age + gender  +height + urbanicity,
                                data = df_stably_middle,
                                id = ID,
                                family = binomial(link = "probit"),
                                corstr = "exchangeable")

gee_pr_activity_stably_middle <- geeglm(depression ~ stably_middle * soc_activity + age + gender  +height + urbanicity,
                                        data = df_stably_middle,
                                        id = ID,
                                        family = binomial(link = "probit"),
                                        corstr = "exchangeable")

med_activity_stably_middle <- mediate(model.m = gee_med_stably_middle, model.y = gee_pr_activity_stably_middle,
                                      treat = 'stably_middle', mediator = 'soc_activity', data = df_stably_middle)

summary(med_activity_stably_middle)

# upward_mobility
gee_med_upward_mobility <- geeglm(soc_activity ~ upward_mobility + age + gender  +height + urbanicity,
                                  data = df_upward_mobility,
                                  id = ID,
                                  family = binomial(link = "probit"),
                                  corstr = "exchangeable")

gee_pr_activity_upward_mobility <- geeglm(depression ~ upward_mobility * soc_activity + age + gender  +height + urbanicity,
                                          data = df_upward_mobility,
                                          id = ID,
                                          family = binomial(link = "probit"),
                                          corstr = "exchangeable")

med_activity_upward_mobility <- mediate(model.m = gee_med_upward_mobility, model.y = gee_pr_activity_upward_mobility,
                                        treat = 'upward_mobility', mediator = 'soc_activity', data = df_upward_mobility)

summary(med_activity_upward_mobility)


# stably_high
gee_med_stably_high <- geeglm(soc_activity ~ stably_high + age + gender  +height + urbanicity,
                              data = df_stably_high,
                              id = ID,
                              family = binomial(link = "probit"),
                              corstr = "exchangeable")

gee_pr_activity_stably_high <- geeglm(depression ~ stably_high * soc_activity + age + gender  +height + urbanicity,
                                      data = df_stably_high,
                                      id = ID,
                                      family = binomial(link = "probit"),
                                      corstr = "exchangeable")

med_activity_stably_high <- mediate(model.m = gee_med_stably_high, model.y = gee_pr_activity_stably_high,
                                    treat = 'stably_high', mediator = 'soc_activity', data = df_stably_high)

summary(med_activity_stably_high)


# BMI residual
gee_med_resi4<- geeglm(BMI ~ educational_mobility_residual + age + gender  +height + urbanicity,
                       data = df_long, 
                       id = ID, 
                       family = binomial(link = "probit"),
                       corstr = "exchangeable")

gee_resi_BMI <- geeglm(depression ~ educational_mobility_residual * BMI + age + gender  +height + urbanicity,
                       data = df_long, 
                       id = ID,
                       family = binomial(link = "probit"),
                       corstr = "exchangeable")

med_BMI_resi <- mediate(model.m = gee_med_resi4, model.y = gee_resi_BMI, 
                        treat = 'educational_mobility_residual', mediator = 'BMI', data = df_long)
summary(med_BMI_resi)

# BMI pr
# Downward mobility 
gee_med_downward <- geeglm(BMI ~ downward_mobility + age + gender  +height + urbanicity,
                           data = df_downward_mobility,
                           id = ID,
                           family = binomial(link = "probit"),
                           corstr = "exchangeable")

gee_pr_BMI_downward <- geeglm(depression ~ downward_mobility * BMI + age + gender  +height+ urbanicity ,
                              data = df_downward_mobility,
                              id = ID,
                              family = binomial(link = "probit"),
                              corstr = "exchangeable")

med_BMI_downward <- mediate(model.m = gee_med_downward, model.y = gee_pr_BMI_downward,
                            treat = 'downward_mobility', mediator = 'BMI', data = df_downward_mobility)

summary(med_BMI_downward)

# Stably middle 
gee_med_stably_middle <- geeglm(BMI ~ stably_middle + age + gender  +height + urbanicity,
                                data = df_stably_middle,
                                id = ID,
                                family = binomial(link = "probit"),
                                corstr = "exchangeable")

gee_pr_BMI_stably_middle <- geeglm(depression ~ stably_middle * BMI + age + gender  +height + urbanicity,
                                   data = df_stably_middle,
                                   id = ID,
                                   family = binomial(link = "probit"),
                                   corstr = "exchangeable")

med_BMI_stably_middle <- mediate(model.m = gee_med_stably_middle, model.y = gee_pr_BMI_stably_middle,
                                 treat = 'stably_middle', mediator = 'BMI', data = df_stably_middle)

summary(med_BMI_stably_middle)

# upward_mobility
gee_med_upward_mobility <- geeglm(BMI ~ upward_mobility + age + gender  +height + urbanicity,
                                  data = df_upward_mobility,
                                  id = ID,
                                  family = binomial(link = "probit"),
                                  corstr = "exchangeable")

gee_pr_BMI_upward_mobility <- geeglm(depression ~ upward_mobility * BMI + age + gender  +height + urbanicity,
                                     data = df_upward_mobility,
                                     id = ID,
                                     family = binomial(link = "probit"),
                                     corstr = "exchangeable")

med_BMI_upward_mobility <- mediate(model.m = gee_med_upward_mobility, model.y = gee_pr_BMI_upward_mobility,
                                   treat = 'upward_mobility', mediator = 'BMI', data = df_upward_mobility)

summary(med_BMI_upward_mobility)


# stably_high
gee_med_stably_high <- geeglm(BMI ~ stably_high + age + gender  +height + urbanicity,
                              data = df_stably_high,
                              id = ID,
                              family = binomial(link = "probit"),
                              corstr = "exchangeable")

gee_pr_BMI_stably_high <- geeglm(depression ~ stably_high * BMI + age + gender  +height + urbanicity,
                                 data = df_stably_high,
                                 id = ID,
                                 family = binomial(link = "probit"),
                                 corstr = "exchangeable")

med_BMI_stably_high <- mediate(model.m = gee_med_stably_high, model.y = gee_pr_BMI_stably_high,
                               treat = 'stably_high', mediator = 'BMI', data = df_stably_high)

summary(med_BMI_stably_high)

# wealth_numeric residual
gee_med_resi5<- geeglm(wealth_numeric ~ educational_mobility_residual + age + gender  +height + urbanicity,
                       data = df_long, 
                       id = ID, 
                       family = binomial(link = "probit"),
                       corstr = "exchangeable")

gee_resi_wealth <- geeglm(depression ~ educational_mobility_residual * wealth_numeric + age + gender  +height + urbanicity,
                          data = df_long, 
                          id = ID,
                          family = binomial(link = "probit"),
                          corstr = "exchangeable")

med_wealth_resi <- mediate(model.m = gee_med_resi5, model.y = gee_resi_wealth, 
                           treat = 'educational_mobility_residual', mediator = 'wealth_numeric', data = df_long)
summary(med_wealth_resi)

# wealth_numeric pr
# Downward mobility 
gee_med_downward <- geeglm(wealth_numeric ~ downward_mobility + age + gender  +height + urbanicity,
                           data = df_downward_mobility,
                           id = ID,
                           family = binomial(link = "probit"),
                           corstr = "exchangeable")

gee_pr_wealth_downward <- geeglm(depression ~ downward_mobility * wealth_numeric + age + gender  +height + urbanicity,
                                 data = df_downward_mobility,
                                 id = ID,
                                 family = binomial(link = "probit"),
                                 corstr = "exchangeable")

med_wealth_downward <- mediate(model.m = gee_med_downward, model.y = gee_pr_wealth_downward,
                               treat = 'downward_mobility', mediator = 'wealth_numeric', data = df_downward_mobility)

summary(med_wealth_downward)

# Stably middle 
gee_med_stably_middle <- geeglm(wealth_numeric ~ stably_middle + age + gender  +height + urbanicity,
                                data = df_stably_middle,
                                id = ID,
                                family = binomial(link = "probit"),
                                corstr = "exchangeable")

gee_pr_wealth_stably_middle <- geeglm(depression ~ stably_middle * wealth_numeric + age + gender  +height + urbanicity,
                                      data = df_stably_middle,
                                      id = ID,
                                      family = binomial(link = "probit"),
                                      corstr = "exchangeable")

med_wealth_stably_middle <- mediate(model.m = gee_med_stably_middle, model.y = gee_pr_wealth_stably_middle,
                                    treat = 'stably_middle', mediator = 'wealth_numeric', data = df_stably_middle)

summary(med_wealth_stably_middle)

# upward_mobility
gee_med_upward_mobility <- geeglm(wealth_numeric ~ upward_mobility + age + gender  +height + urbanicity,
                                  data = df_upward_mobility,
                                  id = ID,
                                  family = binomial(link = "probit"),
                                  corstr = "exchangeable")

gee_pr_wealth_upward_mobility <- geeglm(depression ~ upward_mobility * wealth_numeric + age + gender  +height + urbanicity,
                                        data = df_upward_mobility,
                                        id = ID,
                                        family = binomial(link = "probit"),
                                        corstr = "exchangeable")

med_wealth_upward_mobility <- mediate(model.m = gee_med_upward_mobility, model.y = gee_pr_wealth_upward_mobility,
                                      treat = 'upward_mobility', mediator = 'wealth_numeric', data = df_upward_mobility)

summary(med_wealth_upward_mobility)


# stably_high
gee_med_stably_high <- geeglm(wealth_numeric ~ stably_high + age + gender  +height + urbanicity,
                              data = df_stably_high,
                              id = ID,
                              family = binomial(link = "probit"),
                              corstr = "exchangeable")

gee_pr_wealth_stably_high <- geeglm(depression ~ stably_high * wealth_numeric + age + gender  +height + urbanicity,
                                    data = df_stably_high,
                                    id = ID,
                                    family = binomial(link = "probit"),
                                    corstr = "exchangeable")

med_wealth_stably_high <- mediate(model.m = gee_med_stably_high, model.y = gee_pr_wealth_stably_high,
                                  treat = 'stably_high', mediator = 'wealth_numeric', data = df_stably_high)

summary(med_wealth_stably_high)




# marital_status residual
gee_med_resi6<- geeglm(marital_status_numeric ~ educational_mobility_residual + age + gender  +height + urbanicity,
                       data = df_long, 
                       id = ID, 
                       family = binomial(link = "probit"),
                       corstr = "exchangeable")

gee_resi_mari <- geeglm(depression ~ educational_mobility_residual * marital_status_numeric + age + gender  +height + urbanicity,
                        data = df_long, 
                        id = ID,
                        family = binomial(link = "probit"),
                        corstr = "exchangeable")

med_mari_resi <- mediate(model.m = gee_med_resi6, model.y = gee_resi_mari, 
                         treat = 'educational_mobility_residual', mediator = 'marital_status_numeric', data = df_long)
summary(med_mari_resi)

# Downward mobility 
gee_med_downward <- geeglm(marital_status_numeric ~ downward_mobility + age + gender  +height + urbanicity,
                           data = df_downward_mobility,
                           id = ID,
                           family = binomial(link = "probit"),
                           corstr = "exchangeable")

gee_pr_mari_downward <- geeglm(depression ~ downward_mobility * marital_status_numeric + age + gender  +height + urbanicity,
                               data = df_downward_mobility,
                               id = ID,
                               family = binomial(link = "probit"),
                               corstr = "exchangeable")

med_mari_downward <- mediate(model.m = gee_med_downward, model.y = gee_pr_mari_downward,
                             treat = 'downward_mobility', mediator = 'marital_status_numeric', data = df_downward_mobility)

summary(med_mari_downward)

# Stably middle 
gee_med_stably_middle <- geeglm(marital_status_numeric ~ stably_middle + age + gender  +height + urbanicity,
                                data = df_stably_middle,
                                id = ID,
                                family = binomial(link = "probit"),
                                corstr = "exchangeable")

gee_pr_mari_stably_middle <- geeglm(depression ~ stably_middle * marital_status_numeric + age + gender  +height + urbanicity,
                                    data = df_stably_middle,
                                    id = ID,
                                    family = binomial(link = "probit"),
                                    corstr = "exchangeable")

med_mari_stably_middle <- mediate(model.m = gee_med_stably_middle, model.y = gee_pr_mari_stably_middle,
                                  treat = 'stably_middle', mediator = 'marital_status_numeric', data = df_stably_middle)

summary(med_mari_stably_middle)

# upward_mobility
gee_med_upward_mobility <- geeglm(marital_status_numeric ~ upward_mobility + age + gender  +height + urbanicity,
                                  data = df_upward_mobility,
                                  id = ID,
                                  family = binomial(link = "probit"),
                                  corstr = "exchangeable")

gee_pr_mari_upward_mobility <- geeglm(depression ~ upward_mobility * marital_status_numeric + age + gender  +height + urbanicity,
                                      data = df_upward_mobility,
                                      id = ID,
                                      family = binomial(link = "probit"),
                                      corstr = "exchangeable")

med_mari_upward_mobility <- mediate(model.m = gee_med_upward_mobility, model.y = gee_pr_mari_upward_mobility,
                                    treat = 'upward_mobility', mediator = 'marital_status_numeric', data = df_upward_mobility)

summary(med_mari_upward_mobility)

# stably_high
gee_med_stably_high <- geeglm(marital_status_numeric ~ stably_high + age + gender  +height + urbanicity,
                              data = df_stably_high,
                              id = ID,
                              family = binomial(link = "probit"),
                              corstr = "exchangeable")

gee_pr_mari_stably_high <- geeglm(depression ~ stably_high * marital_status_numeric + age + gender  +height + urbanicity,
                                  data = df_stably_high,
                                  id = ID,
                                  family = binomial(link = "probit"),
                                  corstr = "exchangeable")

med_mari_stably_high <- mediate(model.m = gee_med_stably_high, model.y = gee_pr_mari_stably_high,
                                treat = 'stably_high', mediator = 'marital_status_numeric', data = df_stably_high)

summary(med_mari_stably_high)



# n_chronic_numeric residual
gee_med_resi7<- geeglm(n_chronic_numeric ~ educational_mobility_residual + age + gender  +height + urbanicity,
                       data = df_long, 
                       id = ID, 
                       family = binomial(link = "probit"),
                       corstr = "exchangeable")

gee_resi_chronic <- geeglm(depression ~ educational_mobility_residual * n_chronic_numeric + age + gender  +height + urbanicity,
                           data = df_long, 
                           id = ID,
                           family = binomial(link = "probit"),
                           corstr = "exchangeable")

med_chronic_resi <- mediate(model.m = gee_med_resi7, model.y = gee_resi_chronic, 
                            treat = 'educational_mobility_residual', mediator = 'n_chronic_numeric', data = df_long)
summary(med_chronic_resi)

# n_chronic_numeric pr
# Downward mobility 
gee_med_downward <- geeglm(n_chronic_numeric ~ downward_mobility + age + gender  +height + urbanicity,
                           data = df_downward_mobility,
                           id = ID,
                           family = binomial(link = "probit"),
                           corstr = "exchangeable")

gee_pr_chronic_downward <- geeglm(depression ~ downward_mobility * n_chronic_numeric + age + gender  +height + urbanicity,
                                  data = df_downward_mobility,
                                  id = ID,
                                  family = binomial(link = "probit"),
                                  corstr = "exchangeable")

med_chronic_downward <- mediate(model.m = gee_med_downward, model.y = gee_pr_chronic_downward,
                                treat = 'downward_mobility', mediator = 'n_chronic_numeric', data = df_downward_mobility)

summary(med_chronic_downward)

# Stably middle 
gee_med_stably_middle <- geeglm(n_chronic_numeric ~ stably_middle + age + gender  +height + urbanicity,
                                data = df_stably_middle,
                                id = ID,
                                family = binomial(link = "probit"),
                                corstr = "exchangeable")

gee_pr_chronic_stably_middle <- geeglm(depression ~ stably_middle * n_chronic_numeric + age + gender  +height + urbanicity,
                                       data = df_stably_middle,
                                       id = ID,
                                       family = binomial(link = "probit"),
                                       corstr = "exchangeable")

med_chronic_stably_middle <- mediate(model.m = gee_med_stably_middle, model.y = gee_pr_chronic_stably_middle,
                                     treat = 'stably_middle', mediator = 'n_chronic_numeric', data = df_stably_middle)

summary(med_chronic_stably_middle)

# upward_mobility
gee_med_upward_mobility <- geeglm(n_chronic_numeric ~ upward_mobility + age + gender  +height + urbanicity,
                                  data = df_upward_mobility,
                                  id = ID,
                                  family = binomial(link = "probit"),
                                  corstr = "exchangeable")

gee_pr_chronic_upward_mobility <- geeglm(depression ~ upward_mobility * n_chronic_numeric + age + gender  +height + urbanicity,
                                         data = df_upward_mobility,
                                         id = ID,
                                         family = binomial(link = "probit"),
                                         corstr = "exchangeable")

med_chronic_upward_mobility <- mediate(model.m = gee_med_upward_mobility, model.y = gee_pr_chronic_upward_mobility,
                                       treat = 'upward_mobility', mediator = 'n_chronic_numeric', data = df_upward_mobility)

summary(med_chronic_upward_mobility)


# stably_high
gee_med_stably_high <- geeglm(n_chronic_numeric ~ stably_high + age + gender  +height + urbanicity,
                              data = df_stably_high,
                              id = ID,
                              family = binomial(link = "probit"),
                              corstr = "exchangeable")

gee_pr_chronic_stably_high <- geeglm(depression ~ stably_high * n_chronic_numeric + age + gender  +height + urbanicity,
                                     data = df_stably_high,
                                     id = ID,
                                     family = binomial(link = "probit"),
                                     corstr = "exchangeable")

med_chronic_stably_high <- mediate(model.m = gee_med_stably_high, model.y = gee_pr_chronic_stably_high,
                                   treat = 'stably_high', mediator = 'n_chronic_numeric', data = df_stably_high)

summary(med_chronic_stably_high)



#1. n_chronic_numeric upward_mobility
ACME <- data.frame(Estimate=rep(NA, 100), CI_lower=rep(NA, 100), CI_upper=rep(NA, 100))
ADE <- data.frame(Estimate=rep(NA, 100), CI_lower=rep(NA, 100), CI_upper=rep(NA, 100))
Prop <- data.frame(Estimate=rep(NA, 100), CI_lower=rep(NA, 100), CI_upper=rep(NA, 100))

success_count <- 0
total_iterations <- 0   

 
categorical_vars <- c("gender", "urbanicity")  

while (success_count < 100) {
  total_iterations <- total_iterations + 1
  cat("now:", total_iterations, "| success:", success_count, "\n")
  
 
  sample_size <- round(nrow(df_upward_mobility) * 0.5)
  sample1 <- sample(nrow(df_upward_mobility), sample_size, replace = FALSE)
  data3 <- df_upward_mobility[sample1, ]
  
 
  if (var(data3$depression, na.rm = TRUE) == 0) {
    cat("skip")
    next
  }
  
  if (var(data3$n_chronic_numeric, na.rm = TRUE) == 0) {
    cat("skip")
    next
  }
  
  category_check <- sapply(data3[categorical_vars], function(x) {
    if (is.factor(x)) {
      nlevels(x)          
    } else {
      length(unique(x))  
    }
  })
  
  if (any(category_check < 2)) {
    failed_vars <- names(category_check)[category_check < 2]
    cat("cate", paste(failed_vars, collapse=", "), "skip")
    next
  }
  
  tryCatch({
    gee_med_upward_mobility <- geeglm(
      n_chronic_numeric ~ upward_mobility + age + gender + urbanicity + height,
      data = data3,
      id = ID,
      family = binomial(link = "probit"),
      corstr = "exchangeable"
    )
    
    gee_pr_wealth_upward_mobility <- geeglm(
      depression ~ upward_mobility * n_chronic_numeric + age + gender + urbanicity + height,
      data = data3,
      id = ID,
      family = binomial(link = "probit"),
      corstr = "exchangeable"
    )
  }, error = function(e) {
    cat("fail:", e$message, "\n")
    return(NULL)
  })

  if (any(is.na(coef(gee_med_upward_mobility))) || 
      any(is.na(coef(gee_pr_wealth_upward_mobility)))) {
    cat("skip")
    next
  }
  
  med_result <- tryCatch({
    mediate(
      model.m = gee_med_upward_mobility, 
      model.y = gee_pr_wealth_upward_mobility,
      treat = "upward_mobility", 
      mediator = "n_chronic_numeric", 
      data = data3
    )
  }, error = function(e) {
    cat("fail:", e$message, "\n")
    return(NULL)
  })
  
  if (is.null(med_result)) next
  
  s <- summary(med_result)
  success_count <- success_count + 1
  ACME[success_count, ] <- c(s$d.avg, s$d.avg.ci)
  ADE[success_count, ] <- c(s$z.avg, s$z.avg.ci)
  Prop[success_count, ] <- c(s$n.avg, s$n.avg.ci)
  
  cat("sucess:", success_count, "/100\n")
}

write.csv(cbind.data.frame(ACME, ADE, Prop),  "Bootstrap_chronic_upward_klosa.csv",
          row.names = F,quote = F)


