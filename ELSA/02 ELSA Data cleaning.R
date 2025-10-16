library(dplyr)
library(mice)

df <- read.csv("elsa.csv")

# Data filtering
df <- df %>%
  filter(inw5 == 1, age1 >= 50) %>%
  filter(rowSums(is.na(across(c("cesd2012","cesd2014","cesd2016","cesd2018")))) < 3) %>%
  filter(!is.na(years_edu), !is.na(H_edu)) %>%
  filter(!(is.na(years_edu_f) & is.na(years_edu_m)), !(is.na(H_edu_f) & is.na(H_edu_m)))

# Construct parental education variables
df <- df %>%
  mutate(
    highest_education_parent = ifelse(is.na(years_edu_f), years_edu_m, ifelse(is.na(years_edu_m), years_edu_f, pmax(years_edu_f, years_edu_m))),
    H_edu_f = ifelse(years_edu_f %in% 1:4, 1, ifelse(years_edu_f %in% 5:6, 2, 3)),
    H_edu_m = ifelse(years_edu_m %in% 1:4, 1, ifelse(years_edu_m %in% 5:6, 2, 3)),
    H_highest_education_parent = ifelse(is.na(H_edu_f), H_edu_m, ifelse(is.na(H_edu_m), H_edu_f, pmax(H_edu_f, H_edu_m)))
  )

# Birth decade grouping & educational mobility (percentile + residual method)
df <- df %>%
  mutate(decade = case_when(
    birth_year %in% 1913:1929 ~ 1920,
    birth_year %in% 1930:1939 ~ 1930,
    birth_year %in% 1940:1949 ~ 1940,
    birth_year %in% 1950:1959 ~ 1950,
    TRUE ~ 1960
  )) %>%
  group_by(decade) %>%
  mutate(
    percentile_rank = rank(years_edu, ties.method = "max")/length(years_edu),
    percentile_rank_parent = rank(highest_education_parent, ties.method = "max")/length(highest_education_parent),
    cognitive_impairment = ifelse(orient < mean(orient, na.rm=TRUE)-1.5*sd(orient, na.rm=TRUE) & word_recall < mean(word_recall, na.rm=TRUE)-1.5*sd(word_recall, na.rm=TRUE), "Impaired", "Normal")
  ) %>%
  ungroup() %>%
  mutate(educational_mobility_residual = resid(lm(percentile_rank ~ percentile_rank_parent)))

# Educational mobility (categorical method)
df <- df %>%
  mutate(
    educational_mobility_H = case_when(
      H_highest_education_parent == 1 & H_edu == 1 ~ "Stably low",
      H_highest_education_parent == 2 & H_edu == 2 ~ "Stably middle",
      H_highest_education_parent == 3 & H_edu == 3 ~ "Stably high",
      H_highest_education_parent > H_edu ~ "Downward mobility",
      H_highest_education_parent < H_edu ~ "Upward mobility"
    ) %>% factor(levels = c("Stably low","Downward mobility","Stably middle","Upward mobility","Stably high")) %>% relevel(ref = "Stably low"),
    education_quan = cut(percentile_rank, breaks = quantile(percentile_rank, probs = c(0,0.25,0.75,1)), labels = c(1,2,3), include.lowest = TRUE),
    education_p_quan = cut(percentile_rank_parent, breaks = quantile(percentile_rank_parent, probs = c(0,0.25,0.75,1)), labels = c(1,2,3), include.lowest = TRUE),
    educational_mobility_pr = case_when(
      education_p_quan == 1 & education_quan == 1 ~ "Stably low",
      education_p_quan == 2 & education_quan == 2 ~ "Stably middle",
      education_p_quan == 3 & education_quan == 3 ~ "Stably high",
      as.numeric(education_p_quan) > as.numeric(education_quan) ~ "Downward mobility",
      as.numeric(education_p_quan) < as.numeric(education_quan) ~ "Upward mobility"
    ) %>% factor(levels = c("Stably low","Downward mobility","Stably middle","Upward mobility","Stably high")) %>% relevel(ref = "Stably low")
  )

# Outcome: Depression (dichotomous)
df <- df %>%
  mutate(across(starts_with("cesd"), ~ifelse(is.na(.), NA, ifelse(. >= 3, 1, 0))))



