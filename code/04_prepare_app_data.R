# =============================================================================
# 04_prepare_app_data.R
# Builds output/sdg_data_app.rds: the app-ready version of the classified UPR
# recommendations, so app.R does not have to load and reshape the full
# 25 MB file (64 columns, three copies of the recommendation text) at every start.
#
# Run after 03_classify_UHRI_recommendations.qmd, i.e. whenever
# output/UHRI_UPR_classified.rds changes. Then commit output/sdg_data_app.rds.
#
# The full file is kept: report templates 1-3 and the classification
# workflow still read output/UHRI_UPR_classified.rds directly.
# =============================================================================
library(here)
library(dplyr)
library(forcats)

# Columns not used anywhere in the app or the report templates
unused_columns <- c(
  "text1", "text", "themes", "title_2", "title_a", "title_b",
  "affected_persons", "sdgs", "regions_concerned", "recommending_regions",
  "recommending_state_upr_semicolon", "recommending_states_count",
  "ohchr_annotation_id", "document_publication_date", "date_of_publication_on_uhri",
  "date_accessed", "provisional", "manual_upload", "upr_session_number"
)
# Theme columns the app drops at load time (same list as in app.R)
dropped_themes <- c(
  "SRHR", "SRHR_any", "Communicable_combined", "SOCED", "essential_medicines",
  "TB_malaria", "abortion_maternal", "NTD", "vaccinations"
)

sdg_data_app <- readRDS(here("output", "UHRI_UPR_classified.rds")) |>
  # Same steps as app.R used to do at startup
  filter(!is.na(response_upr)) |>          # Japan's response text + Micronesia's pledges
  mutate(response_upr = fct_recode(response_upr, "Noted" = "Partially supported"),
         response_upr = fct_relevel(response_upr, "Noted")) |>
  droplevels() |>
  select(-any_of(c(unused_columns, dropped_themes)))

saveRDS(sdg_data_app, here("output", "sdg_data_app.rds"))
message("Saved output/sdg_data_app.rds: ", nrow(sdg_data_app), " rows x ",
        ncol(sdg_data_app), " columns")