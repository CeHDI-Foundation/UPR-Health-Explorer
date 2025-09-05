# Setup ####
pacman::p_load(
  here,
  dplyr,forcats,ggplot2,magrittr,readr,readxl,stringr,tibble,tidyr,lubridate,
  # plotly,
  janitor,
  sf
)
country_list <- tibble(
  country = readRDS(here("output", "state_geo_enhanced.rds"))$country,
  english_formal = readRDS(here("output", "state_geo_enhanced.rds"))$english_formal
)

# --- GBD data ####
# Global Burden of Disease Collaborative Network. Global Burden of Disease Study 2021 (GBD 2021) Results.
# Seattle, United States: Institute for Health Metrics and Evaluation (IHME), 2022.
# Available from <https://vizhub.healthdata.org/gbd-results/>.

## All causes data ####
# **Query:**  
#   
#   **GBD estimate:** Cause of death of injury  
# **Measure:** Deaths; DALYs  
# **Metric:** Rate  
# **Cause:** All level 3 causes  
# **Location:** All countries and territories  
# **Age:** Age-standardized  
# **Sex:** Both; Female; Male  
# **Year:** Select all  
# 
# This query generated 14 CSV files that I combined and then saved as RDS format in order to save diskspace

# Uncomment below code for first run
# GBD <-
#   # List the filenames of each CSV file in the folder
#   list.files(path = here("data", "GBD"),
#                   pattern = "\\.csv$",
#                   full.names = TRUE) |>
#   # Read them in using data.table::fread()
#   map_df(~data.table::fread(.)) |>
#   # Clean the names
#   janitor::clean_names() |>
#   # Format variables
#   mutate(
#     across(c(measure_name, location_name, sex_name,
#                   cause_name, metric_name), as.factor),
#     date = ymd(paste0(year, "-01-01"))
#     )
# 
# # Split the file by measure
# GBD_DALY <- GBD |> filter(measure_name == "DALYs (Disability-Adjusted Life Years)")
# GBD_deaths <- GBD |> filter(measure_name == "Deaths")
# 
# # Save as RDS
# saveRDS(GBD_DALY, file = here("data", "GBD", "GBD_DALY.rds"))
# saveRDS(GBD_deaths, file = here("data", "GBD", "GBD_deaths.rds"))

# GBD_DALY <- readRDS(here("data", "GBD", "GBD_DALY.rds")) |> 
#   left_join(country_list, join_by(location_name == english_formal)) |> 
#   mutate(country = case_when(location_name == "Global" ~ "Global", 
#                              .default = country)) |> 
#   filter(!is.na(country))
# 
# GBD_deaths <- readRDS(here("data", "GBD", "GBD_deaths.rds"))  |> 
#   left_join(country_list, join_by(location_name == english_formal)) |> 
#   mutate(country = case_when(location_name == "Global" ~ "Global", 
#                              .default = country)) |> 
#   filter(!is.na(country))

## Maternal disorders - Level 4 causes ####
# **Query:**  
#   
#   **GBD estimate:** Cause of death or injury  
# **Measure:** Deaths; DALYs  
# **Metric:** Rate  
# **Cause:** Maternal disorders as well as all associated level 4 causes 
# **Location:** All countries and territories  
# **Age:** Age-standardized; 
# **Sex:** Female  
# **Year:** 2021 
# 
# This query generated a CSV file that I split by Deaths and DALYs and saved in RDS format in order to save diskspace.
# maternal_disorders <-
#   # List the filenames of each CSV file in the folder
#   list.files(path = here("data", "GBD", "maternal_disorders"),
#              pattern = "\\.csv$",
#              full.names = TRUE) |>
#   # Read them in using data.table::fread()
#   map_df(~data.table::fread(.)) |>
#   # Clean the names
#   janitor::clean_names() |>
#   # Format variables
#   mutate(
#     across(c(measure_name, location_name, sex_name,
#              cause_name, metric_name), as.factor),
#     date = ymd(paste0(year, "-01-01"))
#   ) |> 
#   mutate(age_name = fct_relevel(age_name, "Age-standardized")) |> 
#   filter(!age_name %in% c("65-69 years", "60-64 years")) |> 
#   droplevels()
# 
# # Split the file by measure
# maternal_disorders_DALY <- maternal_disorders |> filter(measure_name == "DALYs (Disability-Adjusted Life Years)")
# maternal_disorders_deaths <- maternal_disorders |> filter(measure_name == "Deaths")
# 
# # Save as RDS
# saveRDS(maternal_disorders_DALY, file = here("data", "GBD", "maternal_disorders", "maternal_disorders_DALY.rds"))
# saveRDS(maternal_disorders_deaths, file = here("data", "GBD", "maternal_disorders", "maternal_disorders_deaths.rds"))

# # Maternal disorders: Longitudinal - Level 4 causes ####
# # **Query:**
# # 
# #   **GBD estimate:** Cause of death or injury
# # **Measure:** Deaths
# # **Metric:** Rate
# # **Cause:** Maternal disorders as well as all associated level 4 causes
# # **Location:** All countries and territories
# # **Age:** Age-standardized;
# # **Sex:** Female
# # **Year:** 2005-2021
# 
# # This query generated a CSV file that I split by Deaths and DALYs and saved in RDS format in order to save diskspace.
# maternal_disorders_longitudinal <-
#   # List the filenames of each CSV file in the folder
#   data.table::fread(here("data", "GBD", "maternal_disorders", "IHME-GBD_2021_DATA-2ab7a128-1.csv")) |> 
#   # Clean the names
#   janitor::clean_names() |>
#   # Format variables
#   mutate(
#     across(c(measure_name, location_name, sex_name,
#              cause_name, metric_name), as.factor),
#     date = ymd(paste0(year, "-01-01")))
# droplevels()
# 
# # Save as RDS
# saveRDS(maternal_disorders_longitudinal, file = here("data", "GBD", "maternal_disorders", "maternal_disorders_deaths_longitudinal.rds"))

maternal_disorders_DALY <- readRDS(here("data", "GBD", "maternal_disorders", "maternal_disorders_DALY.rds")) |> 
  left_join(country_list, join_by(location_name == english_formal)) |> 
  mutate(country = case_when(location_name == "Global" ~ "Global", 
                             .default = country)) |> 
  filter(!is.na(country))

maternal_disorders_deaths <- readRDS(here("data", "GBD", "maternal_disorders", "maternal_disorders_deaths.rds")) |> 
  left_join(country_list, join_by(location_name == english_formal)) |> 
  mutate(country = case_when(location_name == "Global" ~ "Global", 
                             .default = country)) |> 
  filter(!is.na(country))

maternal_disorders_deaths_longitudinal <- readRDS(here("data", "GBD", "maternal_disorders", "maternal_disorders_deaths_longitudinal.rds")) |> 
  left_join(country_list, join_by(location_name == country)) |> 
  mutate(country = case_when(location_name == "Global" ~ "Global", 
                             .default = location_name)) |> 
  filter(!is.na(location_name))

# **Maternal haemorrhage** includes both postpartum haemorrhage (defined as blood loss ≥500 ml for vaginal delivery and ≥1000 ml for caesarean delivery) and antepartum haemorrhage (defined as vaginal bleeding from any cause at or beyond 20 weeks of gestation).  
# 
# **Maternal sepsis** is defined as a temperature <36°C or >38°C and clinical signs of shock (systolic blood pressure <90 mmHg and tachycardia >120 bpm). **Other maternal infections** are defined as any maternal infections excluding HIV, STI, or not related to pregnancy. 
# 
# **Maternal hypertensive disorders** include gestational hypertension (onset after 20 weeks gestation), pre-eclampsia, severe preeclampsia, and eclampsia, but exclude chronic hypertension (onset prior to pregnancy or prior to 20 weeks gestation) unless superimposed preeclampsia or eclampsia develop.
# 
# **Maternal obstructed labour and uterine rupture** aggregates obstructed labour (arrest in the first or second stage of active labour despite sufficient contractions), uterine rupture (non-surgical breakdown of uterine wall), and fistula (an abnormal opening between the vagina and the bladder or rectum following childbirth). 
# 
# **Abortion** is defined as elective or medically indicated termination of pregnancy at any gestational age. **Miscarriage** is defined as spontaneous loss of pregnancy before 24 weeks of gestation with complications requiring medical care.
# 
# **Ectopic pregnancy** is defined as pregnancy occurring outside of the uterus.
# 
# **Indirect maternal deaths** are due to existing diseases that are exacerbated by pregnancy. Examples include maternal infections and parasitic diseases complicating pregnancy, childbirth, and the puerperium, and diabetes in pregnancy, childbirth, and the puerperium. 
# 
# **Late maternal deaths** are deaths that occur six weeks to one year after the end of pregnancy, excluding incidental deaths.
# 
# **Maternal deaths aggravated by HIV/AIDS** are deaths occurring in HIV-positive women whose pregnancy has exacerbated their HIV/AIDS, leading to death.
# 
# **Other direct maternal disorders** encompasses a wide range of maternal disorders that do not map to other diseases in the GBD cause list, including other fatal or non-fatal complications occurring during pregnancy, childbirth, and the postpartum period. 
# 
# See also Cresswell et al. for more background on causes of maternal deaths:  
#   Cresswell, Jenny A, Monica Alexander, Michael Y C Chong, Heather M Link, Marija Pejchinovska, Ursula Gazeley, Sahar M A Ahmed, et al. “Global and Regional Causes of Maternal Deaths 2009–20: A WHO Systematic Analysis.” The Lancet Global Health 13, no. 4 (April 2025): e626–34. <https://doi.org/10.1016/S2214-109X(24)00560-6>.

# Remove unused objects ---------------
rm(country_list)