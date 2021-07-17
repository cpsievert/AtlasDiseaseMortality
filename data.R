library(dplyr)


source("R/AuxFunctions.R", local = TRUE)

DX <- read_table("data-raw/list_dx.txt") %>%
  arrange(chapter, start, level) %>%
  mutate(
    start = as.character(start),
    end = as.character(end),
    desc_EN = as.character(desc_EN),
    chapter_label = paste0(as.roman(chapter), " (", start, "-", end, ")"),
    chapter_label = factor(chapter_label, sort(unique(chapter_label), decreasing = TRUE)),
    description = case_when(
      level == "1" ~ paste0("Chapter ", as.roman(chapter), " (", start, "-", end, "): ", desc_EN),
      level == "3" ~ paste0(" ---> ", start, ": ", desc_EN),
      substr(level, 1, 1) == "2" ~ paste0(" -> ", start, "-", end, ": ", desc_EN),
      TRUE ~ desc_EN
    ),
    dx_median = ifelse(
      is.na(age_dx50), "-", paste0(format_numbers(age_dx50, 1), " (", format_numbers(age_dx25, 1), "-", format_numbers(age_dx75, 1), ")")),
    death_median = ifelse(
      is.na(age_death50), "-", paste0(format_numbers(age_death50, 1), " (", format_numbers(age_death25, 1), "-", format_numbers(age_death75, 1), ")"))
  )

saveRDS(DX, "data/DX.rds")


read_table("data-raw/MRR.txt") %>%
  select(id, sex, cause = cod, est = HR, lower = CI_left, upper = CI_right) %>%
  mutate(ci = format_ci(est, lower, upper, n = 2)) %>%
  left_join(select(DX, id, desc = desc_EN, chapter, chapter_label), by = "id") %>%
  mutate(text = paste0(ci, "\n", desc)) %>%
  saveRDS("data/MRR.rds")



read_table("data-raw/MRRlagged.txt") %>%
  select(id, sex, cause = cod, exposure, est = HR, lower = CI_left, upper = CI_right) %>%
  mutate(ci = format_ci(est, lower, upper, n = 2)) %>%
  left_join(select(DX, id, desc_EN), by = "id") %>%
  saveRDS("data/MRRlagged.rds")


read_table("data-raw/MRRage.txt") %>%
  mutate(
    age_cat = paste0(age_group, "-", age_group + age_categories)
  ) %>%
  select(id, sex, cause = cod, age_cat, est = HR, lower = CI_left, upper = CI_right) %>%
  mutate(ci = format_ci(est, lower, upper, n = 2)) %>%
  left_join(select(DX, id, desc_EN), by = "id") %>%
  saveRDS("data/MRRage.rds")


LYL <- read_table("data-raw/LYL.txt")
bind_rows(
  select(LYL, id, sex, est = LYL_Total, lower = LYL_Total_L, upper = LYL_Total_R) %>%
    mutate(cause = "All"),
  select(LYL, id, sex, est = LYL_Natural, lower = LYL_Natural_L, upper = LYL_Natural_R) %>%
    mutate(cause = "Natural"),
  select(LYL, id, sex, est = LYL_External, lower = LYL_External_L, upper = LYL_External_R) %>%
    mutate(cause = "External")
) %>%
  mutate(ci = format_ci(est, lower, upper, n = 2)) %>%
  left_join(select(DX, id, desc = desc_EN, chapter, chapter_label), by = "id") %>%
  mutate(text = paste0(ci, "\n", desc)) %>%
  saveRDS("data/LYL.rds")


LYLage <- read_table("data-raw/LYLages.txt")
bind_rows(
  select(LYLage, id, sex, age, est = LYL_Total, lower = LYL_Total_L, upper = LYL_Total_R) %>%
    mutate(cause = "All"),
  select(LYLage, id, sex, age, est = LYL_Natural, lower = LYL_Natural_L, upper = LYL_Natural_R) %>%
    mutate(cause = "Natural"),
  select(LYLage, id, sex, age, est = LYL_External, lower = LYL_External_L, upper = LYL_External_R) %>%
    mutate(cause = "External")
) %>%
  mutate(ci = format_ci(est, lower, upper, n = 2)) %>%
  left_join(select(DX, id, desc = desc_EN, chapter, chapter_label), by = "id") %>%
  saveRDS("data/LYLage.rds")


read_table("data-raw/ages.txt") %>%
  saveRDS("data/ages.rds")

read_table("data-raw/incidence.txt") %>%
  saveRDS("data/incidence.rds")
