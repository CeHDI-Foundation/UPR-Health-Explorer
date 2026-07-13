pacman::p_load(
  here,
  tidyverse
)

source(here("data", "UPR_WG_docs", "extract_recs_function.R"))

# Run the extract function for each country as needed
extract_upr_recs(
  input = "https://docs.un.org/en/A/HRC/62/14",
  state_under_review = "Georgia", 
  upr_session = 51, 
  document_symbol = "A/HRC/62/14",
  provisional = TRUE, 
  mode = "auto", 
  output_dir = here("data", "UPR_WG_docs", "extracted_recs")
)

# Inspect the extracted file for any issues
recs2 <- readRDS(here("data", "UPR_WG_docs", "extracted_recs", "Central African Republic_5.rds"))


# Uncomment below code for first run
recs_combined <-
  # List the filenames of each rds file in the folder
  list.files(path = here("data", "UPR_WG_docs", "extracted_recs", "dashboard"),
             pattern = "\\.rds$") |> 
  # Read each file into a list
  map(~readRDS(here("data", "UPR_WG_docs", "extracted_recs", "dashboard", .))) |>
  # row-bind each dataframe into a single dataframe
  list_rbind() |> 
  # rename the variables to be consistent with `df_0` object, so that it can easily be appended
  rename(
    text = recommendation, 
    countries_concerned = state_under_review,
    upr_reccomending_states = recommending_states,
    upr_position = position
  ) |> 
  # Format variables as character (to allow row-binding with df_0)
  mutate(
    across(!provisional, as.character)
  ) |> 
  # Add in variables that will be needed for downstream filtering
  mutate(
    upr_reccomending_states = str_replace_all(upr_reccomending_states, ";", " -"), # for consistency with df_0 file
    reccomending_body = "- UPR",
    type = "- Recommendations",
    # Add in an identifier so we know which recommendations were manually added to the dataset
    manual_upload = TRUE
  ) |> 
  select(-c(recommendation_clean, paragraph))

saveRDS(recs_combined, here("data", "UPR_WG_docs", "recs_combined.rds"))
