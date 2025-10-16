library(dplyr)
library(mice)

df <- read.csv("share.csv")

# Data filtering
df <- df %>%
  filter(inw4 == 1, age >= 50, !is.na(years_edu)) %>%
  filter(!(is.na(years_edu_f) & is.na(years_edu_m)), !(is.na(H_edu_f) & is.na(H_edu_m))) %>%
  filter(rowSums(is.na(across(c("depression2010","depression2012","depression2014","depression2016","depression2018")))) < 3) %>%
  filter(depression2010 < 4)

# Construct parental education variables
df <- df %>%
  mutate(
    highest_education_parent = ifelse(is.na(years_edu_f), years_edu_m, ifelse(is.na(years_edu_m), years_edu_f, pmax(years_edu_f, years_edu_m))),
    H_highest_education_parent = ifelse(is.na(H_edu_f), H_edu_m, ifelse(is.na(H_edu_m), H_edu_f, pmax(H_edu_f, H_edu_m)))
  )

# Birth decade & educational mobility (percentile + residual method)
df <- df %>%
  mutate(decade = case_when(
    birth_year %in% 1909:1929 ~ 1920,
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
    # Harmonized education mobility
    educational_mobility_H = case_when(
      H_highest_education_parent == 1 & H_edu == 1 ~ "Stably low",
      H_highest_education_parent == 2 & H_edu == 2 ~ "Stably middle",
      H_highest_education_parent == 3 & H_edu == 3 ~ "Stably high",
      H_highest_education_parent > H_edu ~ "Downward mobility",
      H_highest_education_parent < H_edu ~ "Upward mobility"
    ) %>% factor(levels = c("Stably low","Downward mobility","Stably middle","Upward mobility","Stably high")) %>% relevel(ref = "Stably low"),
    # Percentile-based education mobility
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
  mutate(across(starts_with("depression"), ~ifelse(. >= 3, "Yes", "No")))

# Childhood variables
df <- df %>%
  mutate(
    financial = case_when(
      raccrooms > median(raccrooms, na.rm=TRUE) & raccbath ==1 & raccwaterc ==1 & raccwaterh ==1 & racctoilet ==1 & raccheating ==1 & raccbooks %in% c(4,5) & ramaoccup %in% c(1,2,3) & (is.na(f_occupation)|f_occupation ==1) ~ "better",
      raccrooms < median(raccrooms, na.rm=TRUE) & (raccbath ==0|raccwaterc ==0|raccwaterh ==0|racctoilet ==0|raccheating ==0) & raccbooks ==1 & ramaoccup %in% c(9,11) & (is.na(f_occupation)|f_occupation %in% c(2,4)) ~ "worse",
      TRUE ~ NA_character_
    ),
    health = ifelse(rachshlt %in% 1:3, "healthy", ifelse(rachshlt %in% 4:5, "less_healthy", NA)),
    stress = ifelse(racsevent_s >=1, "stress", ifelse(racsevent_s ==0, "non-stress", NA)),
    warmth = ifelse(rapadrug ==1, "non-warm", ifelse(rapadrug ==0, "warm", NA))
  )

