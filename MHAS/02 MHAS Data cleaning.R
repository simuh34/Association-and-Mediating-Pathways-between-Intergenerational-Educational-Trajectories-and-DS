library(dplyr)
library(mice)
setwd('D:\\code book\\Educational mobility\\MHAS')
df <- read.csv("MHAS.csv")

####preparing the df####
##define sample size; n = 15723
sum(df$inw3)
df <- df[df$inw3 == 1,]

##exclude age >= 50; n = 14886 
df <- df[which(df$age >= 50), ]

##exclude participants with depression in baseline 2010; n = 9239
##df <- df[which(df$cesd2012 < 5), ]

##exclude NA for outcomes cesd; n = 12539
selected_columns <- c("cesd2015","cesd2018")
df <- df[rowSums(is.na(df[, c("cesd2015","cesd2018")])) < 2, ]

##delete na in education; n = 12485
df <- subset(df, !(is.na(years_edu)))
df <- subset(df, !(is.na(H_edu)))

#### construct the exposure: education mobility ####
## highest education level of parent; n = 11246
## delete rows which education of parents are both NA

df <- subset(df, !(is.na(H_edu_f) & is.na(H_edu_m)))
df$H_highest_education_parent<- ifelse(
  is.na(df$H_edu_f), df$H_edu_m,
  ifelse(
    is.na(df$H_edu_m), df$H_edu_f,
    ifelse(df$H_edu_f> df$H_edu_m, df$H_edu_f, df$H_edu_m)
  )
)

# group the respondent by birth year
df$decade <- ifelse(df$birth_year >= 1913 & df$birth_year <= 1929, 1920,
                    ifelse(df$birth_year>= 1930 & df$birth_year <= 1939, 1930,
                           ifelse(df$birth_year >= 1940 & df$birth_year <= 1949, 1940,
                                  ifelse(df$birth_year >= 1950 & df$birth_year <= 1959, 1950,
                                         1960))))
table(df$decade)

###1. percentile rank of education level
## percentile ranking of respondents' education level
df <- df %>%
  group_by(decade) %>%
  mutate(percentile_rank = rank(years_edu,ties.method = c("max"))/length(years_edu))

## percentile ranking of respondent' parent education level
df <- df %>%
  group_by(decade) %>%
  mutate(percentile_rank_parent =  rank(H_highest_education_parent,ties.method = c("max"))/length(H_highest_education_parent))

summary(df$percentile_rank)
summary(df$percentile_rank_parent)

lm_model <- lm(df$percentile_rank ~ df$percentile_rank_parent)
df$educational_mobility_residual <- resid(lm_model)
summary(df$educational_mobility_residual)

###2. categorized by harmonized education level (harmonized educational mobility)
df <- df %>%
  mutate(H_highest_education_parent = case_when(
    H_highest_education_parent %in% c(1, 2) ~ 1,
    H_highest_education_parent == 3 ~ 2,
    H_highest_education_parent == 4 ~ 3,
    TRUE ~ H_highest_education_parent  
  ))
df$educational_mobility_H <- NA
df$educational_mobility_H[df$H_highest_education_parent == 1 & df$H_edu == 1] <- 1
df$educational_mobility_H[df$H_highest_education_parent == 2 & df$H_edu == 2] <- 2
df$educational_mobility_H[df$H_highest_education_parent == 3 & df$H_edu == 3] <- 3
df$educational_mobility_H[df$H_highest_education_parent > df$H_edu] <- 4
df$educational_mobility_H[df$H_highest_education_parent < df$H_edu] <- 5
df$educational_mobility_H <- factor(df$educational_mobility_H,
                                    levels = c(1, 4, 2, 5, 3),
                                    labels = c("Stably low","Downward mobility", "Stably middle","Upward mobility","Stably high" ))
df$educational_mobility_H <- relevel(df$educational_mobility_H, ref = "Stably low")
table(df$educational_mobility_H)

###3. categorized by percentile ranking of education level
df$education_quan <- cut(
  df$percentile_rank,
  breaks = quantile(df$percentile_rank, probs = c(0, 0.25, 0.75, 1)),
  labels = c(1, 2, 3),
  include.lowest = TRUE
)

df$education_p_quan <- cut(
  df$percentile_rank_parent,
  breaks = quantile(df$percentile_rank_parent, probs = c(0, 0.25, 0.75, 1)),
  labels = c(1, 2, 3),
  include.lowest = TRUE
)

table(df$education_quan)
table(df$education_p_quan)

df$educational_mobility_pr <- NA
df$educational_mobility_pr[df$education_p_quan == 1 & df$education_quan == 1] <- 1
df$educational_mobility_pr[df$education_p_quan == 2 & df$education_quan == 2] <- 2
df$educational_mobility_pr[df$education_p_quan == 3 & df$education_quan == 3] <- 3
df$educational_mobility_pr[as.numeric(df$education_p_quan) > as.numeric(df$education_quan)] <- 4
df$educational_mobility_pr[as.numeric(df$education_p_quan) < as.numeric(df$education_quan)] <- 5
df$educational_mobility_pr <- factor(df$educational_mobility_pr,
                                     levels = c(1, 4, 2, 5, 3),
                                     labels = c("Stably low","Downward mobility", "Stably middle","Upward mobility","Stably high" ))
df$educational_mobility_pr <- relevel(df$educational_mobility_pr, ref = "Stably low")
table(df$educational_mobility_pr)

####construct the outcome: depression####
selected_columns <- c("cesd2012", "cesd2015","cesd2018")

for (col in selected_columns) {
  new_col_name <- paste0(col, "_depression")
  df[[new_col_name]] <- ifelse(is.na(df[[col]]), NA, ifelse(df[[col]] >= 5, 1, 0))
}

table(df$cesd2012_depression)

####covariates####
#recode variabels
#gender: 1 is men; 2 is women
table(df$gender, exclude = NULL)
df$gender <- ifelse(df$gender==1, "men", "women")
df$gender <- factor(df$gender)

#urbanicity
df <- df %>%
  mutate(urbanicity = case_when(
    urbanicity == 0 ~ "urban",
    urbanicity == 1 ~ "rural",
    TRUE ~ NA_character_
  ))
table(df$urbanicity)
df$urbanicity <- factor(df$urbanicity)

#labour force status
table(df$lb_status)
df$lb_status <- ifelse(!is.na(df$lb_status) & df$lb_status == 1, "employed", 
                       ifelse(!is.na(df$lb_status), "other", NA))


#chronic diseases
disease <- subset(df, select = c(r3arthre, #  r ever had arthritis
                                 r3cancre, #  r ever had cancer
                                 r3hibpe,  #  r Ever had high blood pressure
                                 r3diabe,  #  r ever had diabetes
                                 r3lunge,  #  r ever had lung disease
                                 r3hearte, #  r ever had heart problem
                                 r3stroke #  r ever had stroke
))
disease <- data.frame(disease) 
disease$n_chronic <- rowSums(disease) 

disease_numeric <- disease %>%
  mutate(n_chronic_category = case_when(
    n_chronic == 0 | n_chronic == 1~ "0 or 1 chronic",
    n_chronic >= 2 & n_chronic <= 7 ~ "more than 2 chronic",
    TRUE ~ NA_character_  
  ))
df <- cbind(df, n_chronic_category = disease_numeric$n_chronic_category)
#orientation
summary(df$orient)
# df$orient_z <- scale(df$orient)
# summary(df$orient_z)

#word recall
summary(df$r1tr20)
# df$r1tr20_z <- scale(df$r1tr20)

df <- df %>%
  group_by(decade) %>%
  mutate(
    cognitive_impairment = ifelse(
      orient < mean(orient, na.rm = TRUE) - 1.5 * sd(orient, na.rm = TRUE) &
        r1tr20 < mean(r1tr20, na.rm = TRUE) - 1.5 * sd(r1tr20, na.rm = TRUE),
      "Impaired", 
      "Normal"
    )
  ) %>%
  ungroup() 
table(df$cognitive_impairment)

# df <- df %>%
#   mutate(across(all_of(iadl_columns), as.numeric)) %>%
#   mutate(across(all_of(badl_columns), as.numeric))

#mediator
#wealth
#equivalized_wealth
df$people_living_with <- as.numeric(as.character(df$people_living_with))
df$equivalized_wealth <- df$household_wealth/sqrt(df$people_living_with)
df$people_living_with <- as.numeric(as.character(df$people_living_with))

df$equivalized_wealth <- ifelse(is.na(df$people_living_with) | is.na(df$household_wealth), NA, df$household_wealth / sqrt(df$people_living_with))

df$equivalized_wealth <- ifelse(is.na(df$equivalized_wealth), NA,round(df$equivalized_wealth / 1000, 2))

summary(df$equivalized_wealth)
quantile_breaks <- quantile(df$equivalized_wealth, probs = c(0, 0.5, 1), na.rm = TRUE)
df$wealth_quartile <- ifelse(is.na(df$equivalized_wealth), 
                             NA, 
                             cut(df$equivalized_wealth, 
                                 breaks = quantile_breaks, 
                                 include.lowest = TRUE, 
                                 labels = c("1", "2")))

table(df$wealth_quartile, useNA = "ifany")

#marital status: 1.married; 3.partnered; 4.separated; 5.divorced; 7.widowed; 8.never married
df$marital_status<- ifelse(df$marital_status==1|df$marital_status==3, "married/partnered", "other")
table(df$marital_status, exclude = NULL)

#drink 1=Yes, 0=none
table(df$drink_ever, exclude = NULL) 
table(df$num_drink, exclude = NULL)
df$drink_now <- ifelse(df$num_drink == 0, 0, 1)  
table(df$drink_now, exclude = NULL)

#smoke status
table(df$smoke_now, exclude = NULL) #1=Yes, 0=none
table(df$num_smoke, exclude = NULL) #Cont

#socaial activity
table(df$soc_activity, exclude = NULL) #1=Yes, 0=none

#BMI 1=obesity, 0=none
df$BMI <- ifelse(df$BMI >= 30, 1, 0)  
table(df$BMI, exclude = NULL)

####imputation####
variables <- c("age", "wealth_quartile", "lb_status", "marital_status","n_chronic_category","cognitive_impairment","height","urbanicity",
               "drink_ever","drink_now","smoke_now","num_smoke","soc_activity","BMI")
for (var in variables) {
  missing_count <- sum(is.na(df[[var]]))
  missing_percentage <- (missing_count / nrow(df)) * 100
  cat("variable", var, "missing", missing_count, "\n")
}

#covariates imputation
df_converted <- df[, c("age", "wealth_quartile", "lb_status", "marital_status","n_chronic_category","cognitive_impairment","height",
                       "drink_ever","drink_now","smoke_now","num_smoke","soc_activity","BMI")]
classes <- sapply(df_converted, class)
labelled_vars <- names(classes[classes == "labelled"])
df_converted <- df_converted %>%
  mutate_if(names(.) %in% c("wealth_quartile", "lb_status", "marital_status","n_chronic_category","urbanicity","cognitive_impairment",
                            "drink_now","smoke_now","soc_activity","BMI"), as.factor)
set.seed(1005)
mice_mod <- mice(df_converted, method = "cart", m =1, maxit = 5)
imputed_data <- complete(mice_mod)
common_cols <- intersect(names(df), names(imputed_data))
df[common_cols] <- imputed_data[common_cols]

write.csv(df,"mhas_cleaning.csv")
