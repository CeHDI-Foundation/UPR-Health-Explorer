# Setup ####
pacman::p_load(
  here,
  tidyverse,
  dplyr,forcats,ggplot2,magrittr,readr,readxl,stringr,tibble,tidyr,lubridate,
  # plotly,
  janitor,
  sf,
  ODataQuery
)

# # Load or install packages from GitHub:
# pacman::p_load_gh(
#   # "DrMattG/SDGsR", # Uses API to get SDGs data
#   "ODataQuery" # More general API use of OData protocol
#   # "aphp/rgho" # Uses API to get data from Global Health Observatory
#   # "PPgp/wpp2024" # United Nations World Population Prospects 2024
#   # "m-muecke/isocountry" # Get ISO codes for countries
# )

state_geo <- readRDS(here("output", "state_geo_enhanced.rds"))

# Load in custom functions
# source(here("utils.R"))

theme_labels <- tribble(
  ~"variable", ~"theme_label",
  "health_systems", "Health systems and services",
  "emergencies", "Health security, emergencies, and disaster relief",
  "ncd", "Non-communicable diseases",
  "communicable", "Communicable diseases",
  "SRHR", "Sexual and reproductive health and rights",
  "mental_health", "Mental health",
  "SOCED", "Social and economic determinants of health",
  "GBV", "Gender-based violence",
  "women", "Women's health",
  "MCAH", "Maternal, child, and adolescent health",
  "essential_medicines", "Essential medicines and health products",
  "disabilities", "Disabilities and health",
  "LGBTI", "Health of LGBTI persons",
  "HIV", "HIV/AIDS and STIs",
  "TB_malaria", "TB and malaria",
  "NTD", "Neglected tropical diseases",
  "TB_malaria_NTD", "TB, malaria, and neglected tropical diseases",
  "vaccinations", "Vaccinations",
  "WASH", "Water and Sanitation",
  "nutrition", "Nutrition",
  "maternal_health", "Maternal health",
  "abortion", "Abortion",
  "incarcerated", "Health of incarcerated persons"
)


# World abortion laws -----------------------------------------------------
# https://reproductiverights.org/maps/worlds-abortion-laws/

gestational_text <- "W5|W8|W10|W12|W13|W14|W16|W17|W18|W20|W22|W24|D90|D120|†|°|º"
world_abortion_laws <- readxl::read_xlsx(here("data", "world_abortion_laws.xlsx")) |> 
  janitor::clean_names() |> 
  mutate(country = str_remove(country, gestational_text)) |> 
  arrange(country) |> 
  mutate(category = factor(case_match(
    category,
    "I" ~ "I. On Request",
    "II" ~ "II. Socioeconomic Grounds",
    "III" ~ "III. To Preserve Health",
    "IV" ~ "IV. To Save the Mother's Life",
    "V" ~ "V. Prohibited Altogether",
    "Varies" ~ "Varies at State level"
  ))) |> 
  mutate(country = case_match(
    country,
    "Antigua & Barbuda" ~ "Antigua and Barbuda",
    "Bolivia" ~ "Bolivia (Plurinational State of)",
    "Bosnia & Herzegovina" ~ "Bosnia and Herzegovina",
    "Cape Verde" ~ "Cabo Verde",
    "Central African Rep." ~ "Central African Republic",
    "Czech Rep." ~ "Czechia",
    "Dem. People’s Rep. of Korea" ~ "Democratic People's Republic of Korea",
    "Dem. Rep. of Congo" ~ "Democratic Republic of the Congo",
    "Eswatini (formerly Swaziland)" ~ "Eswatini",
    "Guinea Bissau" ~ "Guinea-Bissau",
    "Iran" ~ "Iran (Islamic Republic of)" ,
    "Ivory Coast" ~ "C\u00f4te d'Ivoire",
    "Laos" ~ "Lao People's Democratic Republic",
    "Micronesia" ~ "Micronesia (Federated States of)",
    "Moldova" ~ "Republic of Moldova",
    "Dem. People’s Rep. of Korea" ~ "Democratic People's Republic of Korea",
    "Russian Fed." ~ "Russian Federation",
    "Saint Kitts & Nevis" ~ "Saint Kitts and Nevis",
    "Saint Vincent & the Grenadines" ~ "Saint Vincent and the Grenadines",
    "São Tomé & Príncipe" ~ "Sao Tome and Principe",
    "Slovak Rep." ~ "Slovakia",
    "South Korea" ~ "Republic of Korea",
    "Syria" ~ "Syrian Arab Republic",
    "Trinidad & Tobago" ~ "Trinidad and Tobago",
    "Tanzania" ~ "United Republic of Tanzania",
    "Turkey" ~ "T\u00fcrkiye",
    "Great Britain" ~ "United Kingdom of Great Britain and Northern Ireland",
    "Venezuela" ~ "Venezuela (Bolivarian Republic of)",
    "Vietnam" ~ "Viet Nam",
    .default = country
  ))


# WHO ####

live_births <- readxl::read_xlsx(here("data", "Data 2025-08-20 12-18.xlsx"), 
                                 sheet = "Data") |> 
  janitor::clean_names() |> 
  filter(indicator=="Number of births (thousands)") |> 
  select(year:country_iso_3_code, value_numeric) |> 
  mutate(year = as.numeric(year)) |> 
  rename(livebirths = value_numeric)

live_births_wide <- live_births |> 
  pivot_wider(
    names_from = year, values_from = livebirths, names_prefix = "livebirths_"
  )

maternal_deaths <- readxl::read_xlsx(here("data", "Data 2025-08-20 12-18.xlsx"), 
                                 sheet = "Data") |> 
  janitor::clean_names() |> 
  filter(indicator=="Number of maternal deaths") |> 
  select(year:country_iso_3_code, value_numeric) |> 
  mutate(year = as.numeric(year)) |> 
  rename(maternal_deaths = value_numeric)

maternal_deaths_wide <- maternal_deaths |> 
  pivot_wider(
    names_from = year, values_from = maternal_deaths, names_prefix = "maternal_deaths_"
  )

mmr_WHO <- readxl::read_xlsx(here("data", "Data 2025-08-20 12-18.xlsx"), 
                                     sheet = "Data") |> 
  janitor::clean_names() |> 
  filter(indicator=="Maternal mortality ratio (per 100 000 live births) (SDG 3.1.1)") |> 
  select(year:country_iso_3_code, value_numeric) |> 
  mutate(year = as.numeric(year)) |> 
  rename(mmr_WHO = value_numeric)

mmr_WHO_wide <- mmr_WHO |> 
  pivot_wider(
    names_from = year, values_from = mmr_WHO, names_prefix = "livebirths_"
  )

mmr_data_WHO <- mmr_WHO |> left_join(maternal_deaths) |> left_join(live_births) |> 
  mutate(mmr_calc = maternal_deaths/(livebirths/100))

# GHO ####
## API queries ####
gho_api <- ODataQuery::ODataQuery$new("https://ghoapi.azureedge.net/api")

## Metadata ####
# Indicators list
gho_indicators <- gho_api$path("Indicator")$retrieve()$value |> select(-Language)

# Dimensions list
gho_dimensions <- gho_api$path("Dimension")$retrieve()$value
# Countries list
country_codes <- gho_api$path("Dimension", "COUNTRY", "DimensionValues")$retrieve()$value |> 
  select(Code) |> 
  rename(COUNTRY = Code) |> 
  left_join(state_geo |> select(iso3, country) |> sf::st_drop_geometry(), join_by(COUNTRY==iso3)) |> 
  rename(country_name = country)

# Region list
region_codes <- gho_api$path("Dimension", "REGION", "DimensionValues")$retrieve()$value |> 
  select(Code, Title) |> 
  rename(REGION = Code, region_name = Title)
UN_region_codes <- gho_api$path("Dimension", "UNREGION", "DimensionValues")$retrieve()$value |> 
  select(Code, Title) |> 
  rename(UNREGION = Code, UN_region_name = Title)
WB_income_codes <- gho_api$path("Dimension", "WORLDBANKINCOMEGROUP", "DimensionValues")$retrieve()$value |> 
  select(Code, Title) |> 
  rename(WORLDBANKINCOMEGROUP = Code, WB_income_group = Title)


## Search GHO codes ####
search_term <- "reproductive"
search_term_results <- gho_indicators |> filter(str_detect(IndicatorName, regex(search_term, ignore_case = TRUE))|
                           str_detect(IndicatorCode, regex(search_term, ignore_case = TRUE)))

## Indicators ####

### UHC ####
#### Get datasets ####
UHC_AVAILABILITY_SCORE <- gho_api$path("UHC_AVAILABILITY_SCORE")$retrieve()$value |>  tibble()
UHC_INDEX_REPORTED <- gho_api$path("UHC_INDEX_REPORTED")$retrieve()$value|> tibble()
UHC_SCI_CAPACITY <- gho_api$path("UHC_SCI_CAPACITY")$retrieve()$value |> tibble()
UHC_SCI_INFECT <- gho_api$path("UHC_SCI_INFECT")$retrieve()$value |> tibble()
UHC_SCI_NCD <- gho_api$path("UHC_SCI_NCD")$retrieve()$value |> tibble()
UHC_SCI_RMNCH <- gho_api$path("UHC_SCI_RMNCH")$retrieve()$value |> tibble()
#### Combine into one ####
UHC_all <- bind_rows(UHC_INDEX_REPORTED, 
                     UHC_SCI_CAPACITY, UHC_SCI_INFECT,
                     UHC_SCI_NCD, UHC_SCI_RMNCH) |> 
  mutate(
    COUNTRY = case_when(SpatialDimType == "COUNTRY" ~ SpatialDim),
    REGION = case_when(SpatialDimType %in% c("REGION", "GLOBAL")~SpatialDim),
    UNREGION = case_when(SpatialDimType %in% c("UNREGION", "GLOBAL")~SpatialDim),
    WORLDBANKINCOMEGROUP = case_when(SpatialDimType %in% c("WORLDBANKINCOMEGROUP", "GLOBAL")~SpatialDim)
  ) |> 
  left_join(gho_indicators) |> 
  left_join(country_codes) |> 
  left_join(region_codes) |> 
  left_join(UN_region_codes) |> 
  left_join(WB_income_codes) |> 
  rename(YEAR = TimeDim) |> 
  mutate(
    NumericValue = as.numeric(NumericValue),
    year = ymd(paste0(YEAR, "-01-01"))
  )

## Maternal mortality ratio ####
MMR <- gho_api$path("MDG_0000000026")$retrieve()$value |> tibble() |> 
  mutate(
    COUNTRY = case_when(SpatialDimType == "COUNTRY" ~ SpatialDim),
    REGION = case_when(SpatialDimType %in% c("REGION", "GLOBAL")~SpatialDim)
  ) |> 
  left_join(gho_indicators) |> 
  left_join(country_codes) |> 
  left_join(region_codes) |> 
  rename(YEAR = TimeDim) |> 
  mutate(
    # NumericValue = as.numeric(NumericValue),
    across(c(NumericValue:High), ~ as.numeric(.x)),
    year = ymd(paste0(YEAR, "-01-01"))
  ) |> 
  mutate(
    mmr_cat = factor(case_when(
      NumericValue < 10 ~ "<10",
      NumericValue < 20 ~ "10-19",
      NumericValue < 100 ~ "20-99",
      NumericValue < 300 ~ "100-299",
      NumericValue < 500 ~ "300-499",
      NumericValue >= 500 ~ "500+",
      .default = NA), 
      levels = c("<10","10-19", "20-99","100-299", "300-499", "500+" )
    )
  )

## Health check after delivery ####
postnatal_care <- gho_api$path("UNICEF_PNCMOTHER")$retrieve()$value |> tibble() |> 
  mutate(
    COUNTRY = case_when(SpatialDimType == "COUNTRY" ~ SpatialDim),
    REGION = case_when(SpatialDimType %in% c("REGION", "GLOBAL")~SpatialDim)
  ) |> 
  left_join(gho_indicators) |> 
  left_join(country_codes) |> 
  left_join(region_codes) |> 
  rename(YEAR = TimeDim) |> 
  group_by(SpatialDim) |> 
  slice_max(order_by = as.numeric(YEAR), n=1) |> 
  mutate(
    # NumericValue = as.numeric(NumericValue),
    across(c(NumericValue:High), ~ as.numeric(.x)),
    year = ymd(paste0(YEAR, "-01-01"))
  )

postnatal_care_5 <- gho_api$path("pncall5")$retrieve()$value |> tibble() |> 
  mutate(
    COUNTRY = case_when(SpatialDimType == "COUNTRY" ~ SpatialDim),
    REGION = case_when(SpatialDimType %in% c("REGION", "GLOBAL")~SpatialDim)
  ) |> 
  left_join(gho_indicators) |> 
  left_join(country_codes) |> 
  left_join(region_codes) |> 
  rename(YEAR = TimeDim) |> 
  mutate(
    # NumericValue = as.numeric(NumericValue),
    across(c(NumericValue:High), ~ as.numeric(.x)),
    year = ymd(paste0(YEAR, "-01-01"))
  ) |> 
  filter(Dim1 == "SEX_FMLE")

postnatal_care_3 <- gho_api$path("pncall3")$retrieve()$value |> tibble() |> 
  mutate(
    COUNTRY = case_when(SpatialDimType == "COUNTRY" ~ SpatialDim),
    REGION = case_when(SpatialDimType %in% c("REGION", "GLOBAL")~SpatialDim)
  ) |> 
  left_join(gho_indicators) |> 
  left_join(country_codes) |> 
  left_join(region_codes) |> 
  rename(YEAR = TimeDim) |> 
  mutate(
    # NumericValue = as.numeric(NumericValue),
    across(c(NumericValue:High), ~ as.numeric(.x)),
    year = ymd(paste0(YEAR, "-01-01"))
  ) 

# See: https://maternalhealthatlas.org/factsheets

## Births in health facility ####
institutional_birth <- gho_api$path("SRHINSTITUTIONALBIRTH")$retrieve()$value |> tibble() |> 
  mutate(
    COUNTRY = case_when(SpatialDimType == "COUNTRY" ~ SpatialDim),
    REGION = case_when(SpatialDimType %in% c("REGION", "GLOBAL")~SpatialDim)
  ) |> 
  left_join(gho_indicators) |> 
  left_join(country_codes) |> 
  left_join(region_codes) |> 
  rename(YEAR = TimeDim) |> 
  mutate(
    # NumericValue = as.numeric(NumericValue),
    across(c(NumericValue:High), ~ as.numeric(.x)),
    year = ymd(paste0(YEAR, "-01-01"))
  ) |> arrange(YEAR) |> group_by(COUNTRY, YEAR) |> slice_head(n=1)

## HIV death ####
HIV_death <- gho_api$path("HIV_0000000006")$retrieve()$value |> tibble() |> 
  mutate(
    COUNTRY = case_when(SpatialDimType == "COUNTRY" ~ SpatialDim),
    REGION = case_when(SpatialDimType %in% c("REGION", "GLOBAL")~SpatialDim)
  ) |> 
  left_join(gho_indicators) |> 
  left_join(country_codes) |> 
  left_join(region_codes) |> 
  rename(YEAR = TimeDim) |> 
  mutate(
    # NumericValue = as.numeric(NumericValue),
    across(c(NumericValue:High), ~ as.numeric(.x)),
    year = ymd(paste0(YEAR, "-01-01"))
  ) 

## Family planning ####
### Need for family planning met ####
family_planning <- gho_api$path("SDGFPALL")$retrieve()$value |> tibble() |> 
  mutate(
    COUNTRY = case_when(SpatialDimType == "COUNTRY" ~ SpatialDim),
    REGION = case_when(SpatialDimType %in% c("REGION", "GLOBAL")~SpatialDim)
  ) |> 
  left_join(gho_indicators) |> 
  left_join(country_codes) |> 
  left_join(region_codes) |> 
  rename(YEAR = TimeDim) |> 
  mutate(
    # NumericValue = as.numeric(NumericValue),
    across(c(NumericValue:High), ~ as.numeric(.x)),
    year = ymd(paste0(YEAR, "-01-01"))
  ) |> 
  mutate(
    value_cat = factor(case_when(
      NumericValue< 10 ~ "<10",
      NumericValue< 30 ~ "<30",
      NumericValue< 60 ~ "<60",
      NumericValue< 90 ~ "<90",
      NumericValue>= 90 ~ "90+",
      .default = NA), 
      levels = c("10", "<30", "<60", "<90", "90+")
    ))

### Modern contraceptive prevalence ####
contraceptive_prevalence <- gho_api$path("cpmowho")$retrieve()$value |>
  mutate(
    COUNTRY = case_when(SpatialDimType == "COUNTRY" ~ SpatialDim),
    REGION = case_when(SpatialDimType %in% c("REGION", "GLOBAL")~SpatialDim)
  ) |> 
  left_join(gho_indicators) |> 
  left_join(country_codes) |> 
  left_join(region_codes) |> 
  rename(YEAR = TimeDim) |> 
  mutate(
    # NumericValue = as.numeric(NumericValue),
    across(c(NumericValue:High), ~ as.numeric(.x)),
    year = ymd(paste0(YEAR, "-01-01"))
  )

### Unintended pregnancy ####
unintended_pregnancy <- gho_api$path("SRH_PREGNANCY_UNINTENDED_RATE")$retrieve()$value |> tibble() |> 
  mutate(
    COUNTRY = case_when(SpatialDimType == "COUNTRY" ~ SpatialDim),
    REGION = case_when(SpatialDimType %in% c("REGION", "GLOBAL")~SpatialDim)
  ) |> 
  left_join(gho_indicators) |> 
  left_join(country_codes) |> 
  left_join(region_codes) |> 
  rename(YEAR = TimeDim) |> 
  mutate(
    # NumericValue = as.numeric(NumericValue),
    across(c(NumericValue:High), ~ as.numeric(.x)),
    year = ymd(paste0(YEAR, "-01-01"))
  )
### Abortion rate ####
abortion_rate <- gho_api$path("SRH_ABORTION_RATE")$retrieve()$value |> tibble() |> 
  mutate(
    COUNTRY = case_when(SpatialDimType == "COUNTRY" ~ SpatialDim),
    REGION = case_when(SpatialDimType %in% c("REGION", "GLOBAL")~SpatialDim)
  ) |> 
  left_join(gho_indicators) |> 
  left_join(country_codes) |> 
  left_join(region_codes) |> 
  rename(YEAR = TimeDim) |> 
  mutate(
    # NumericValue = as.numeric(NumericValue),
    across(c(NumericValue:High), ~ as.numeric(.x)),
    year = ymd(paste0(YEAR, "-01-01"))
  )

### Adolescent birth rate ####

### Own informed decisions ####
informed_decisions <- gho_api$path("SG_DMK_SRCR_FN_ZS")$retrieve()$value |> tibble() |> 
  mutate(
    COUNTRY = case_when(SpatialDimType == "COUNTRY" ~ SpatialDim),
    REGION = case_when(SpatialDimType %in% c("REGION", "GLOBAL")~SpatialDim),
    UNREGION = case_when(SpatialDimType %in% c("UNREGION", "GLOBAL")~SpatialDim)
  ) |> 
  left_join(gho_indicators) |> 
  left_join(country_codes) |> 
  left_join(region_codes) |> 
  left_join(UN_region_codes) |> 
  rename(YEAR = TimeDim) |> 
  mutate(
    # NumericValue = as.numeric(NumericValue),
    across(c(NumericValue:High), ~ as.numeric(.x)),
    year = ymd(paste0(YEAR, "-01-01"))
  )

## Skilled birth ####
skilled_birth <- gho_api$path("MDG_0000000025")$retrieve()$value |> tibble() |> 
  mutate(
    COUNTRY = case_when(SpatialDimType == "COUNTRY" ~ SpatialDim),
    REGION = case_when(SpatialDimType %in% c("REGION", "GLOBAL")~SpatialDim)
  ) |> 
  left_join(gho_indicators) |> 
  left_join(country_codes) |> 
  left_join(region_codes) |> 
  rename(YEAR = TimeDim) |> 
  mutate(
    # NumericValue = as.numeric(NumericValue),
    across(c(NumericValue:High), ~ as.numeric(.x)),
    year = ymd(paste0(YEAR, "-01-01"))
  ) |>
  mutate(
    value_cat = factor(case_when(
      NumericValue< 60 ~ "<60",
      NumericValue< 70 ~ "60-69",
      NumericValue< 80 ~ "70-79",
      NumericValue< 90 ~ "80-89",
      NumericValue< 98 ~ "90-97",
      NumericValue>= 98 ~ "98+",
      .default = NA), 
      levels = c("<60", "60-69", "70-79", "80-89", "90-97", "98+")
    ))

## Density of nursing and midwifery personnel ####




## HPV ####
### National program ####
HPV_national <- gho_api$path("NCD_CCS_hpv")$retrieve()$value |> tibble() |> 
  mutate(
    COUNTRY = case_when(SpatialDimType == "COUNTRY" ~ SpatialDim),
    REGION = case_when(SpatialDimType %in% c("REGION", "GLOBAL")~SpatialDim)
  ) |> 
  left_join(gho_indicators) |> 
  left_join(country_codes) |> 
  left_join(region_codes) |> 
  rename(YEAR = TimeDim) |> 
  mutate(
    # NumericValue = as.numeric(NumericValue),
    across(c(NumericValue:High), ~ as.numeric(.x)),
    year = ymd(paste0(YEAR, "-01-01"))
  ) 

### Coverage estimates ####
HPV_coverage <- gho_api$path("SDGHPVRECEIVED")$retrieve()$value |> tibble() |> 
  mutate(
    COUNTRY = case_when(SpatialDimType == "COUNTRY" ~ SpatialDim),
    REGION = case_when(SpatialDimType %in% c("REGION", "GLOBAL")~SpatialDim),
    WORLDBANKINCOMEGROUP = case_when(SpatialDimType %in% c("WORLDBANKINCOMEGROUP", "GLOBAL")~SpatialDim)
  ) |> 
  left_join(gho_indicators) |> 
  left_join(country_codes) |> 
  left_join(region_codes) |> 
  left_join(WB_income_codes) |> 
  rename(YEAR = TimeDim) |> 
  mutate(
    # NumericValue = as.numeric(NumericValue),
    across(c(NumericValue:High), ~ as.numeric(.x)),
    year = ymd(paste0(YEAR, "-01-01"))
  )

# NMIRF status -------------------------------
# Data extracted from Annex 02 of URG's report https://www.universal-rights.org/urg-policy-reports/the-emergence-and-evolution-of-national-mechanisms-for-implementation-reporting-and-follow-up/

nmirf_X <- read_csv(here("data", "NMIRF_status.csv")) |> 
  janitor::clean_names() |> 
  rename(iso3 = iso_3166_1, nmirf_classification = state_classification_in_report)

nmirf_Y <- read_csv(here("data", "NMIRF_Y.csv")) |> 
  janitor::clean_names() |> 
  rename(iso3 = iso_3166_1)

NMIRF <- left_join(nmirf_X, nmirf_Y) |> 
  mutate(
    nmirf_classification = factor(
      nmirf_classification,
      levels = c("NMIRF", "Single inter-ministerial", "Ad hoc inter-ministerial", "Single ministerial", "Hybrid")
    ))
rm(nmirf_X, nmirf_Y)

# Remove non-indicator objects -----------------------
rm(
  country_codes, gho_dimensions, gho_indicators, region_codes, 
  state_geo, theme_labels, gestational_text, UN_region_codes, WB_income_codes, 
  search_term, search_term_results
  )

# Save all objects to .rds files
lapply(ls(), function(obj_name) {
  saveRDS(get(obj_name), file = here("data", "API_data", paste0(obj_name, ".rds")))
})