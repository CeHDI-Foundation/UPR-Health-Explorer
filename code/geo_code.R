pacman::p_load(
  here,
  dplyr,forcats,ggplot2,magrittr,readr,readxl,stringr,tibble,tidyr,lubridate,
  # plotly,
  janitor,
  sf,
  necountries
)

# Get member states geometries
state_geo_prep <- necountries::ne_countries |> 
  # filter(type == "main"|country == "Alaska") #|>
  filter(status == "member"|status == "observer"|country == "Alaska"|country == "Greenland"|country=="Somaliland"|country == "Western Sahara") |> 
  select(iso2:sovereign, status, region:polygon)

# Combine US and Alaska
us_alaska <- state_geo_prep |> 
  filter(sovereign == "United States of America") |> 
  st_cast("POLYGON") %>%
  mutate(area = st_area(.)) |> 
  group_by(country) |> 
  slice_max(order_by = area, n = 1) |> 
  ungroup() |> 
  st_union() |> st_sf() |> 
  rename("polygon" = 1) |> 
  mutate(country = "United States of America")

# Combine Somalia and Somaliland
somalia <- state_geo_prep |> 
  filter(sovereign %in% c("Somalia", "Somaliland")) |> 
  st_union() |> st_sf() |> 
  rename("polygon" = 1) |> 
  mutate(country = "Somalia")

# Update geometry for US and Alaska
state_geo_prep <- state_geo_prep |> 
  mutate(polygon = case_when(iso3 == "USA" ~ us_alaska$polygon,
                             country == "Somalia" ~ somalia$polygon,
                             .default = polygon)) |> 
  filter(!country %in% c("Somaliland", "Alaska"))
# Get the centroid of each state and update dataset
point_centroid <- st_centroid(state_geo_prep, of_largest_polygon = TRUE)
state_geo_prep$point_centroid <- point_centroid$polygon
rm(us_alaska, somalia, point_centroid)

# Update state names for compatability with SDG dataset
state_geo_prep <- state_geo_prep |> 
  mutate(country = case_match(
    country,
    "Bolivia" ~ "Bolivia (Plurinational State of)",
    "Brunei" ~ "Brunei Darussalam",
    "D.R. Congo" ~ "Democratic Republic of the Congo",
    "East Timor" ~ "Timor-Leste",
    "Federated States of Micronesia" ~ "Micronesia (Federated States of)",
    "Iran" ~ "Iran (Islamic Republic of)" ,
    "Ivory Coast" ~ "C\u00f4te d'Ivoire",
    "Laos" ~ "Lao People's Democratic Republic",
    "Moldova" ~ "Republic of Moldova",
    "North Korea" ~ "Democratic People's Republic of Korea",
    "Russia" ~ "Russian Federation",
    "South Korea" ~ "Republic of Korea",
    "Syria" ~ "Syrian Arab Republic",
    "Tanzania" ~ "United Republic of Tanzania",
    "Turkey" ~ "T\u00fcrkiye",
    "United Kingdom" ~ "United Kingdom of Great Britain and Northern Ireland",
    "Venezuela" ~ "Venezuela (Bolivarian Republic of)",
    "Vietnam" ~ "Viet Nam",
    "eSwatini" ~ "Eswatini",
    .default = country
  )) |> 
  # st_transform(crs = 3857) |> 
  st_cast("MULTIPOLYGON")  # Recast geometry to all multipolygon rather than a mix, for downstream use with plotly

# UN states listing with offical names
# Downloaded from: https://unterm.un.org/unterm2/en/country
UN_official <- readxl::read_xlsx(here("data", "countries.xlsx")) |> 
  janitor::clean_names() |> 
  select(english_short, english_formal) |> 
  mutate(english_short = trimws(str_remove(english_short, "\\(the\\)|\\(The\\)"))) |> 
  mutate(english_short = case_when(
    english_short == "Netherlands (Kingdom of the)" ~ "Netherlands",
    english_short == "State of Palestine  *" ~ "Palestine",
    .default = english_short
  )) |> 
  mutate(english_formal = str_remove(english_formal, regex("^the\\b\\s+", ignore_case = TRUE)),
         english_formal = case_when(
           english_short == "Palestine" ~ "Palestine",
           english_short == "Guyana" ~ "Republic of Guyana",
           english_formal == "Republic of Türkiye" ~ "Republic of Turkey",
           english_short == "Bahamas" ~ "Commonwealth of the Bahamas",
           english_short == "North Macedonia" ~ "North Macedonia",
           english_short == "Sudan" ~ "Republic of Sudan",
           english_short == "Nepal" ~ "Federal Democratic Republic of Nepal",
           english_short == "Iceland" ~ "Republic of Iceland",
           .default = english_formal
         ))

# WHO regions
who_regions <- read_csv(here("data", "who_regions.csv")) |> select(-country) |> 
  add_row(iso3="GRL", WHO_region ="European Region (EUR)")
  mutate(WHO_region = factor(WHO_region,
                             levels = c("African Region (AFR)", 
                                        "Eastern Mediterranean Region (EMR)",
                                        "South-East Asian Region (SEAR)",
                                        "Western Pacific Region (WPR)",
                                        "Region of the Americas (AMR)",
                                        "European Region (EUR)")))

# Grouping by Fragile/Conflict-affected Situations
# https://thedocs.worldbank.org/en/doc/5c7e4e268baaafa6ef38d924be9279be-0090082025/original/FCSListFY26.pdf
FCS_countries <- tibble("country" = state_geo_prep$country) |> 
  mutate(
    FCS_status = factor(case_when(
      country %in% c(
        "Afghanistan",
        "Burkina Faso",
        "Cameroon",
        "Central African Republic",
        "Democratic Republic of the Congo",
        "Ethiopia",
        "Haiti",
        "Iraq",
        "Lebanon",
        "Mali",
        "Mozambique",
        "Myanmar",
        "Niger",
        "Nigeria",
        "Somalia",
        "South Sudan",
        "Sudan",
        "Syrian Arab Republic",
        "Ukraine",
        "Palestine",
        "Yemen") ~ "Conflict",
      country %in% c(
        "Burundi",
        "Chad",
        "Comoros",
        "Congo",
        "Eritrea",
        "Guinea-Bissau",
        "Kiribati",
        "Libya",
        "Marshall Islands",
        "Micronesia (Federated States of)",
        "Papua New Guinea",
        "Sao Tome and Principe",
        "Solomon Islands",
        "Timor-Leste",
        "Tuvalu",
        "Venezuela (Bolivarian Republic of)",
        "Zimbabwe") ~ "Institutional and social fragility",
      .default = "Other"
    ),
    levels = c("Other", "Institutional and social fragility", "Conflict")))

# Regional partners ----
ecsa_states <- read_csv(here("data", "ecsa_status.csv")) |> select(-country) |> 
  mutate(ECSA_status = factor(case_when(ECSA_status == "Member" ~ "ECSA-HC Member",
                                        ECSA_status == "Non-Member" ~ "ECSA-HC Regional Non-Member")))

CARICOM <- read_csv(here("data", "CARICOM_status.csv")) |> select(-country) |> 
  mutate(status = factor(case_when(status == "Member State" ~ "CARICOM Member State",
                                   status == "Associate Member" ~ "CARICOM Associate Member"))) |> 
  rename(CARICOM_status = status) |> 
  mutate(CARICOM_status=fct_relevel(CARICOM_status, "CARICOM Member State"))

South_Centre <- read_csv(here("data", "SouthCentre_status.csv")) |> select(-country) |> 
  mutate(SC_status = factor("South Centre Member State"))

OACPS <- read_csv(here("data", "OACPS_status.csv")) |> select(-country) |> 
  mutate(OACPS_status = factor("OACPS Member State"),
         region = factor(region)) |> 
  rename(OACPS_region = region)

COMESA <- read_csv(here("data", "COMESA_status.csv")) |> select(-country) |> 
  mutate(COMESA_status = factor("COMESA Member State"))

state_geo <- left_join(state_geo_prep, UN_official, join_by(country == english_short)) |> 
  mutate(english_formal = case_when(country == "Greenland" ~ "Greenland", .default = english_formal)) |> 
  rowid_to_column() |> 
  mutate(income = factor(income, 
                         levels = c("1. High income: OECD", "2. High income: nonOECD", 
                                    "3. Upper middle income", "4. Lower middle income", 
                                    "5. Low income"))) |> 
  left_join(FCS_countries) |> 
  left_join(who_regions) |> 
  arrange(region, WHO_region, subregion) |> 
  mutate(subregion = fct_inorder(subregion),
         across(c(region, wbregion), ~ factor(.x))) |> 
  left_join(ecsa_states) |> #mutate(ECSA_status = fct_na_value_to_level(ECSA_status, "Other")) |> 
  left_join(CARICOM, join_by(iso3==iso3_code)) |> #mutate(CARICOM_status = fct_na_value_to_level(CARICOM_status, "Other")) |> 
  left_join(South_Centre, join_by(iso3==iso3_code)) |> #mutate(SC_status = fct_na_value_to_level(SC_status, "Other")) |> 
  left_join(OACPS, join_by(iso3==iso3_code)) |> #mutate(OACPS_status = fct_na_value_to_level(OACPS_status, "Other")) |> 
  left_join(COMESA, join_by(iso3==iso3_code)) #|> mutate(COMESA_status = fct_na_value_to_level(COMESA_status, "Other"))
  

saveRDS(state_geo, here("output", "state_geo_enhanced.rds"))

state_geo_dist <- state_geo |> 
  # st_set_geometry("point_centroid") |> 
  st_set_geometry("polygon") |> 
  select(country) |> 
  st_distance()

diag(state_geo_dist)<- -1 # Make sure that the distance from itself is always the smallest distance
# give the rows & cols meaningful names
colnames(state_geo_dist) <- state_geo$country
rownames(state_geo_dist) <- state_geo$country
nearest_neighbors_list <- apply(state_geo_dist, 1, function(row_distances) {
  
  # 2. Sort the distances and get the original column indices
  sorted_indices <- order(row_distances)
  
  # 3. Get the names of the closest countries using the indices
  # We skip the first index [1] because it's the country itself (distance = 0).
  # We take the next 10 indices, from [2] to [11].
  closest_country_names <- colnames(state_geo_dist)[sorted_indices[2:30]]
  
  return(closest_country_names)
})
saveRDS(nearest_neighbors_list, here("output", "nearest_neighbors_list.rds"))


# UN states listing with regional groupings
# Downloaded from: https://unstats.un.org/unsd/methodology/m49/overview/

# UNSD <- read_csv2(here("data", "UNSD_Methodology.csv")) |>
#   janitor::clean_names() |>
#   mutate(
#     intermediate_region_name = case_when(
#       is.na(intermediate_region_name) ~ sub_region_name,
#       .default = intermediate_region_name),
#     least_developed_countries_ldc = case_when(
#       least_developed_countries_ldc == "x" ~ TRUE,
#       .default = FALSE)
#   ) |>
#   select(intermediate_region_name, iso_alpha3_code, least_developed_countries_ldc)
