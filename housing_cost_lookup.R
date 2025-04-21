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
acs_year <- 2023
acs_survey <- "acs1" # Use 5-year estimates for more reliability

# Define the geography (State of Colorado)
state_fips <- "08" # FIPS code for Colorado

# Get list of variables from tidycensus
acs_variables <- load_variables(acs_year, "acs1")

housing_costs_income_vars <- acs_variables |>
  filter(str_detect(name, "B25106")) |>
  pull(name)


#-----------------------------------------------------------------------------
# Fetch Data from Census API
#-----------------------------------------------------------------------------
# Use get_acs to retrieve the data for Colorado
# output = "wide" makes each variable its own column
print(paste("Fetching ACS", acs_survey, acs_year, "data for Colorado..."))

co_data_place <- get_acs(
  geography = "place",
  state = state_fips,
  variables = housing_costs_income_vars,
  year = acs_year,
  survey = acs_survey,
  cache_table = TRUE
)

co_data_county <- get_acs(
  geography = "place",
  state = state_fips,
  variables = housing_costs_income_vars,
  year = acs_year,
  survey = acs_survey,
  cache_table = TRUE
)

# Get state and national data
state_data <- get_acs(
  geography = "state",
  variables = housing_costs_income_vars,
  year = acs_year,
  survey = acs_survey,
  cache_table = TRUE
)

us_data <- get_acs(
  geography = "us",
  variables = housing_costs_income_vars,
  year = acs_year,
  survey = acs_survey,
  cache_table = TRUE
)

all_cost_burdened <- bind_rows(
  co_data_place,
  co_data_county,
  state_data,
  us_data
) |>
  left_join(
    acs_variables,
    by = c("variable" = "name")
  )

all_cost_burdened_wide <- all_cost_burdened |>
  select(-concept, -variable) |>
  pivot_wider(
    names_from = c(label),
    values_from = c(estimate, moe),
    names_sep = "_"
  )

write_csv(all_cost_burdened, "all_cost_burdened.csv")

# Check if data retrieval was successful
if (is.null(co_cost_burden_data) || nrow(co_cost_burden_data) == 0) {
  stop(
    "Failed to retrieve data. Check API key, internet connection, or variable codes."
  )
} else {
  print("Data fetched successfully.")
}

#-----------------------------------------------------------------------------
# Calculate Cost Burden Metrics
#-----------------------------------------------------------------------------
# Calculate the total number of cost-burdened households and the percentage
co_cost_burden_summary <- co_cost_burden_data %>%
  mutate(
    # Calculate total cost-burdened households (Renters + Owners)
    total_cost_burdened_E = renters_cost_burdenedE + owners_cost_burdenedE,

    # Calculate the percentage of occupied households that are cost-burdened
    # Handle potential division by zero if total_occupied_unitsE is 0
    percent_cost_burdened_E = if_else(
      total_occupied_unitsE > 0,
      (total_cost_burdened_E / total_occupied_unitsE) * 100,
      0 # Assign 0% if there are no occupied units
    )
  ) %>%
  # Select and rename columns for clarity
  select(
    GEOID,
    NAME,
    Total_Occupied_Units = total_occupied_unitsE,
    Renters_Cost_Burdened = renters_cost_burdenedE,
    Owners_Cost_Burdened = owners_cost_burdenedE,
    Total_Cost_Burdened = total_cost_burdened_E,
    Percent_Cost_Burdened = percent_cost_burdened_E
  )

#-----------------------------------------------------------------------------
# Display Results
#-----------------------------------------------------------------------------
print("Cost Burden Summary for Colorado:")
print(co_cost_burden_summary)

# You can also view the results as a tibble
# View(co_cost_burden_summary)

#-----------------------------------------------------------------------------
# Optional: Save Results to CSV
#-----------------------------------------------------------------------------
# write_csv(co_cost_burden_summary, "colorado_cost_burden_summary_acs5_2022.csv")
# print("Results saved to colorado_cost_burden_summary_acs5_2022.csv")
#-----------------------------------------------------------------------------
