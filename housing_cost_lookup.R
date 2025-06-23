#scrript created using Gemini

#-----------------------------------------------------------------------------
# R Script to Get Cost-Burdened Household Data for Colorado using tidycensus
#-----------------------------------------------------------------------------
# Description:
# This script retrieves data on housing cost burden for Colorado from the
# most recent American Community Survey (ACS) 5-year estimates using the
# tidycensus package. It calculates the total number and percentage of
# households (both renters and owners) spending 30% or more of their
# household income on housing costs.
#
# Requirements:
# - R installed
# - `tidycensus` and `tidyverse` packages installed:
#   install.packages(c("tidycensus", "tidyverse"))
# - Census API Key: Required to access Census data.
#   Get one here: https://api.census.gov/data/key_signup.html
#-----------------------------------------------------------------------------

# Load necessary libraries
library(tidycensus)
library(tidyverse)
library(duckplyr)
library(duckdb)

#-----------------------------------------------------------------------------
# Configuration
#-----------------------------------------------------------------------------
# Set your Census API Key
# Replace "YOUR_API_KEY" with your actual key.
# You only need to run census_api_key() once per installation,
# or set install = TRUE to store it for future sessions.
# census_api_key("YOUR_API_KEY", install = TRUE)
# Sys.getenv("CENSUS_API_KEY") # Uncomment to check if key is set

# Define the year for the ACS data (e.g., 2022 for 2018-2022 5-year estimates)
# tidycensus defaults to the latest available 5-year ACS if year is omitted.
acs_year <- 2005
acs_survey <- "acs1" # Use 5-year estimates for more reliability

# Define the geography (State of Colorado)
state_fips <- "08" # FIPS code for Colorado

# Get list of variables from tidycensus
acs_variables <- load_variables(acs_year, "acs1")

acs_income_pct_vars <- acs_variables |> 
  filter(str_detect(concept, regex("cost", ignore_case = TRUE)))

housing_costs_income_vars <- acs_variables |>
  filter(str_detect(name, "B25106") | str_detect(name, "B25140")) |>
  pull(name)


#-----------------------------------------------------------------------------
# Fetch Data from Census API
#-----------------------------------------------------------------------------
# Use get_acs to retrieve the data for Colorado
# output = "wide" makes each variable its own column

get_multi_acs <- function(acs_year, acs_survey) {
  # Get list of variables from tidycensus
  acs_variables <- load_variables(acs_year, "acs1")

  housing_costs_income_vars <- acs_variables |>
    filter(str_detect(name, "B25106") | str_detect(name, "B25140")) |>
    pull(name)

  print(paste("Fetching ACS", acs_survey, acs_year, "data for Colorado..."))

  data_place <- get_acs(
    geography = "place",
    variables = housing_costs_income_vars,
    year = acs_year,
    survey = acs_survey,
    cache_table = TRUE
  ) |>
    mutate(geography = "place")

  data_county <- get_acs(
    geography = "county",
    variables = housing_costs_income_vars,
    year = acs_year,
    survey = acs_survey,
    cache_table = TRUE
  ) |>
    mutate(geography = "county")

  # Get state and national data
  state_data <- get_acs(
    geography = "state",
    variables = housing_costs_income_vars,
    year = acs_year,
    survey = acs_survey,
    cache_table = TRUE
  ) |>
    mutate(geography = "state")

  us_data <- get_acs(
    geography = "us",
    variables = housing_costs_income_vars,
    year = acs_year,
    survey = acs_survey,
    cache_table = TRUE
  ) |>
    mutate(geography = "us")

  all_cost_burdened <- bind_rows(
    data_place,
    data_county,
    state_data,
    us_data
  ) |>
    mutate(
      year = acs_year,
      survey = acs_survey
    ) |> 
    left_join(
      acs_variables,
      by = c("variable" = "name")
    )
}

acs_cost_burden_data <- map(c(2005:2019, 2021:2023), \(x) {
  get_multi_acs(acs_year = x, "acs1")
}) |>
  bind_rows()

acs_cost_burden_duckdb <- as_duckdb_tibble(acs_cost_burden_data)

acs_cost_burden_duckdb |> 
  compute_parquet(
    "data/cost_burdened_households_co.parquet"
  )

state_cost_burden <- acs_cost_burden_duckdb |>
  filter(geography == "state" & str_detect(variable, "B25140_")) |>
  mutate(
    label = str_remove(label, "Estimate!!Total:!!")
  ) |> 
  separate_wider_delim(
    label, delim = "!!", names = c("tenure", "cost_burden"),
    too_few = "align_start"
  )
  select(label, estimate, moe) |>
  mutate(
    variable = str_replace(variable, "B25106", "renters"),
    variable = str_replace(variable, "B25140", "owners")
  ) |>
  pivot_wider(
    names_from = variable,
    values_from = c(estimate, moe)
  ) |>
  mutate(
    renters_cost_burdened = estimate_renters / estimate_renters_total * 100,
    owners_cost_burdened = estimate_owners / estimate_owners_total * 100
  )

acs_variables |> 
  filter(str_detect(name, "B25140_"))
View(state_cost_burden)
