library(dplyr)
library(mice)
df <- read.csv("klosa.csv")

# Data filtering
df <- df %>%
  filter(inw3 == 1, age >= 50) %>%
  filter(!(is.na(levels_edu_f) & is.na(levels_edu_m)), !(is.na(H_edu_f) & is.na(H_edu_m))) %>%
  filter(rowSums(is.na(across(c("depression2010","depression2012","depression2014","depression2016","depression2018","depression2020")))) < 3) %>%
  filter(depression2010 < 10, !is.na(levels_edu))

# Construct parental education & educational mobility
df <- df %>%
  # Highest parental education (raw & harmonized)
  mutate(
    highest_education_parent = ifelse(is.na(levels_edu_f), levels_edu_m, ifelse(is.na(levels_edu_m), levels_edu_f, pmax(levels_edu_f, levels_edu_m))),
    H_highest_education_parent = ifelse(is.na(H_edu_f), H_edu_m, ifelse(is.na(H_edu_m), H_edu_f, pmax(H_edu_f, H_edu_m)))
  ) %>%
  # Birth decade grouping
  mutate(decade = case_when(
    birth_year %in% 1909:1929 ~ 1920,
    birth_year %in% 1930:1939 ~ 1930,
    birth_year %in% 1940:1949 ~ 1940,
    birth_year %in% 1950:1959 ~ 1950,
    TRUE ~ 1960
  )) %>%
  # Percentile rank & residual mobility
  group_by(decade) %>%
  mutate(
    percentile_rank = rank(levels_edu, ties.method = "max")/length(levels_edu),
    percentile_rank_parent = rank(highest_education_parent, ties.method = "max")/length(highest_education_parent)
  ) %>%
  ungroup() %>%
  mutate(educational_mobility_residual = resid(lm(percentile_rank ~ percentile_rank_parent))) %>%
  # Categorical mobility (harmonized & percentile-based)
  mutate(
    # Harmonized education mobility
    educational_mobility_H = case_when(
      H_highest_education_parent == 1 & H_edu == 1 ~ "Stably low",
      H_highest_education_parent == 2 & H_edu == 2 ~ "Stably middle",
      H_highest_education_parent == 3 & H_edu == 3 ~ "Stably high",
      H_highest_education_parent > H_edu ~ "Downward mobility",
      H_highest_education_parent < H_edu ~ "Upward mobility"
    ) %>% factor(levels = c("Stably low","Downward mobility","Stably middle","Upward mobility","Stably high")) %>% relevel(ref = "Stably low"),
    # Percentile-based mobility
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

# Outcome: Depression (dichotomous, cutoff ≥10)
df <- df %>%
  mutate(across(starts_with("depression"), ~ifelse(. >= 10, "Yes", "No")))