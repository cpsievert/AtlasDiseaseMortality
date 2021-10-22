library(dplyr)

source("R/utils.R", local = TRUE)

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
      is.na(age_dx50), " - ", paste0(format_numbers(age_dx50, 1), " (", format_numbers(age_dx25, 1), " - ", format_numbers(age_dx75, 1), ")")),
    death_median = ifelse(
      is.na(age_death50), " - ", paste0(format_numbers(age_death50, 1), " (", format_numbers(age_death25, 1), " - ", format_numbers(age_death75, 1), ")"))
  ) %>%
  mutate(sex = ifelse(sex == "All", "persons", ifelse(sex == "Males", "men", "women"))) %>%
  left_join(read_table("data-raw/labels.txt")) %>%
  distinct()

saveRDS(DX, "data/DX.rds")

gmc_map <- list(
  "Circulatory system" = c("Hypertension", "Dislipidemia", "Ischemic heart disease", "Atrial fibrillation", "Heart failure", "Peripheral artery occlusive disease", "Stroke"),
  "Endocrine system" = c("Diabetes Mellitus", "Thyroid disorder", "Gout"),
  "Pulmonary system and allergy" = c("Chronic pulmonary disease", "Allergy"),
  "Gastrointestinal system" = c("Ulcer/chronic gastritis", "Chronic liver disease", "Inflamatory bowel disease", "Diverticular disease of intestine"),
  "Urogenital system" = c("Chronic kidney disease","Prostate disorders"),
  "Musculoskeletal system" = c("Connective tissue disorders", "Osteoporosis"),
  "Hematological system" = c("HIV/AIDS", "Anemias"),
  "Cancers" = "Cancers",
  "Neurological system" = c("Vision problem", "Hearing problem", "Migraine", "Epilepsy", "Parkinson's disease", "Multiple sclerosis", "Neuropathies"),
  "Mental disorders" = "Mental disorders"
)

saveRDS(gmc_map, "data/gmc_map.rds")

gmc_map <- tidyr::unnest(tibble::enframe(gmc_map, "gmc", "desc"))


read_table("data-raw/MRR.txt") %>%
  select(id, sex, cause = cod, est = HR, lower = CI_left, upper = CI_right) %>%
  mutate(
    ci = format_ci(est, lower, upper, n = 2),
    sex = ifelse(sex == "All", "persons", ifelse(sex == "Males", "men", "women"))
  ) %>%
  left_join(select(DX, id, sex, desc = desc_EN, chapter, chapter_label), by = c("id", "sex")) %>%
  left_join(gmc_map, by = "desc") %>%
  mutate(
    text = paste0(scales::wrap_format(40)(desc), "\nMRR = ", ci)
  ) %>%
  saveRDS("data/MRR.rds")

wrap <- scales::wrap_format(60)
exposure_map <- setNames(c("0-6 months", "6-12 months", "1-2 years", "2-5 years", "5-10 years", "10+ years"), 1:6)

read_table("data-raw/MRRlagged.txt") %>%
  select(id, sex, cause = cod, exposure, est = HR, lower = CI_left, upper = CI_right) %>%
  mutate(
    ci = format_ci(est, lower, upper, n = 2),
    sex = ifelse(sex == "All", "persons", ifelse(sex == "Males", "men", "women"))
  ) %>%
  left_join(select(DX, id, sex, desc = desc_EN), by = c("id", "sex")) %>%
  left_join(gmc_map, by = "desc") %>%
  mutate(
    est_display = case_when(
      est < 1 ~ paste0(format_numbers(100 * (1 - est)), "% lower"),
      est < 2 ~ paste0(format_numbers(100 * (est - 1)), "% higher"),
      TRUE ~ paste(format_numbers(est, 1), "times higher")
    ),
    x = recode(exposure, !!!exposure_map),
    text = paste0(x, "<br>MRR = ", ci),
    customdata = glue::glue("{x} after an initial diagnosis, the diagnosed had an average mortality rate <b>{est_display}</b> compared to those of same age and sex without that diagnosis (MRR = {ci})")
  ) %>%
  saveRDS("data/MRRlagged.rds")


read_table("data-raw/MRRage.txt") %>%
  select(id, sex, cause = cod, age_group, est = HR, lower = CI_left, upper = CI_right) %>%
  mutate(
    ci = format_ci(est, lower, upper, n = 2),
    sex = ifelse(sex == "All", "persons", ifelse(sex == "Males", "men", "women"))
  ) %>%
  left_join(select(DX, id, sex, desc = desc_EN), by = c("id", "sex")) %>%
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
  mutate(
    ci = format_ci(est, lower, upper, n = 2),
    sex = ifelse(sex == "All", "persons", ifelse(sex == "Males", "men", "women"))
  ) %>%
  left_join(select(DX, id, sex, desc = desc_EN, chapter, chapter_label), by = c("id", "sex")) %>%
  left_join(gmc_map, by = "desc") %>%
  mutate(text = paste0(scales::wrap_format(40)(desc), "\nLYL = ", ci)) %>%
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
  mutate(sex = ifelse(sex == "All", "persons", ifelse(sex == "Males", "men", "women"))) %>%
  saveRDS("data/LYLage.rds")

read_table("data-raw/LYLages.txt") %>%
  mutate(ci = format_ci(life_exp, life_exp_L, life_exp_R, n = 1)) %>%
  select(id, age, sex, ci, est = life_exp, lower = life_exp_L, upper = life_exp_R) %>%
  mutate(text = ci) %>%
  mutate(sex = ifelse(sex == "All", "persons", ifelse(sex == "Males", "men", "women"))) %>%
  saveRDS("data/lifeExp.rds")

# General population life expectancy by age
read_table("data-raw/LYLages.txt") %>%
  filter(id == 1) %>% # baseline
  select(age, est = life_exp0, sex) %>%
  mutate(
    text = paste0(format_numbers(est, 1), " years remaining<br>at age ", format_numbers(age, 0))
  ) %>%
  mutate(sex = ifelse(sex == "All", "persons", ifelse(sex == "Males", "men", "women"))) %>%
  saveRDS("data/lifeExp0.rds")


read_table("data-raw/cuminc.txt") %>%
  mutate(sex = ifelse(sex == "All", "persons", ifelse(sex == "Males", "men", "women"))) %>%
  saveRDS("data/ages.rds")

read_table("data-raw/incidence.txt") %>%
  mutate(sex = ifelse(sex == "All", "persons", ifelse(sex == "Males", "men", "women"))) %>%
  saveRDS("data/incidence.rds")
