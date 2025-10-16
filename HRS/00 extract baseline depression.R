library(dplyr)
library(mice)
library(haven)
library(stringr)

df <- read.csv("hrs.csv")
childhood <- read_dta("AGGCHLDFH2016A_R.dta")

df <- df %>%
  filter(inw10 == 1, age >= 50, !is.na(years_edu)) %>%
  filter(!(is.na(years_edu_f) & is.na(years_edu_m))) %>%
  filter(rowSums(is.na(across(c("cesd2012","cesd2014","cesd2016","cesd2018","cesd2020")))) < 3, cesd2010 < 3)

df <- df %>%
  mutate(decade = case_when(birth_year %in% 1913:1929 ~ 1920, birth_year %in% 1930:1939 ~ 1930, birth_year %in% 1940:1949 ~ 1940, birth_year %in% 1950:1959 ~ 1950, TRUE ~ 1960)) %>%
  mutate(highest_education_parent = case_when(is.na(years_edu_f) ~ years_edu_m, is.na(years_edu_m) ~ years_edu_f, TRUE ~ pmax(years_edu_f, years_edu_m))) %>%
  group_by(decade) %>%
  mutate(percentile_rank = rank(years_edu, ties.method = "max")/n(), percentile_rank_parent = rank(highest_education_parent, ties.method = "max")/n()) %>%
  ungroup() %>%
  mutate(educational_mobility_residual = resid(lm(percentile_rank ~ percentile_rank_parent))) %>%
  mutate(education_quan = cut(percentile_rank, breaks = quantile(percentile_rank, probs = c(0, 0.25, 0.75, 1), na.rm = TRUE), labels = c(1, 2, 3), include.lowest = TRUE), education_p_quan = cut(percentile_rank_parent, breaks = quantile(percentile_rank_parent, probs = c(0, 0.25, 0.75, 1), na.rm = TRUE), labels = c(1, 2, 3), include.lowest = TRUE)) %>%
  mutate(educational_mobility_pr = case_when(education_p_quan == 1 & education_quan == 1 ~ "Stably low", education_p_quan == 2 & education_quan == 2 ~ "Stably middle", education_p_quan == 3 & education_quan == 3 ~ "Stably high", as.numeric(education_p_quan) > as.numeric(education_quan) ~ "Downward mobility", as.numeric(education_p_quan) < as.numeric(education_quan) ~ "Upward mobility")) %>%
  mutate(educational_mobility_pr = factor(educational_mobility_pr, levels = c("Stably low", "Downward mobility", "Stably middle", "Upward mobility", "Stably high")))

df <- df %>% mutate(across(starts_with("cesd"), ~ifelse(.x >= 3, 1, 0), .names = "{.col}_depression"))

childhood <- childhood %>%
  mutate(hhidpn = as.numeric(str_c(HHID, PN, sep = ""))) %>%
  mutate(financial = case_when(FAMFIN == 5 | MOVFIN == 1 | FMFINH == 1 ~ "YES", FAMFIN %in% c(1, 3) & MOVFIN == 5 & FMFINH == 5 ~ "NO", TRUE ~ NA_character_), stress = case_when(TRPOLICE == 1 | DRKDRUG == 1 | PHYABUSE == 1 | SCHLOVER == 1 ~ "YES", TRPOLICE == 5 & DRKDRUG == 5 & PHYABUSE == 5 & SCHLOVER == 5 ~ "NO", TRUE ~ NA_character_), warmth = case_when(RELWMO == 5 & RELWFA == 5 & (ATTENMO == 1 + EFFMO == 1 + TEACHMO == 1) >= 2 ~ "YES", (RELWMO %in% c(1, 2) | RELWFA %in% c(1, 2)) & (ATTENMO == 4 | EFFMO == 4 | TEACHMO == 4) ~ "NO", RELWMO == 6 | RELWFA == 6 ~ NA_character_, TRUE ~ NA_character_), health = case_when(RTHLTHCH %in% 1:3 ~ "healthy", RTHLTHCH %in% 4:5 ~ "less_healthy", TRUE ~ NA_character_)) %>%
  select(hhidpn, health, stress, financial, warmth)

df <- left_join(df, childhood, by = "hhidpn")

