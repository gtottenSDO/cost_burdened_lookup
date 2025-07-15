library(duckplyr)
library(tidyverse)

# read parquet data
cost_burdened <- duckplyr::duckplyr_df_from_parquet(
  "data/cost_burdened_households_co.parquet"
)


co_cost_burdened <- cost_burdened |>
  collect() %>%
  filter(
    str_detect(
      GEOID,
      "^08"
    )
  )

co_cost_burdened |>
  write_csv("data/cost_burdened_households_co.csv")
