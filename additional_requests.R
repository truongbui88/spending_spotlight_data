# Load libraries
library(tidyverse)
options(scipen = 999)

# Load data
ss_data <- read_csv("input_data/ss_data.csv")


# Adjust data by "CPI (Average of July to June)"
ss_data_cpi <- ss_data |>
    filter(State != "District of Columbia") |>
    arrange(State, Year) |>
    group_by(State) |>
    mutate(`Total Revenue` = `Total Revenue` * last(`CPI (Average of July to June)`) / `CPI (Average of July to June)`) |>
    mutate(`Total Revenue - Per Pupil` = `Total Revenue - Per Pupil` * last(`CPI (Average of July to June)`) / `CPI (Average of July to June)`) |>
    mutate(`Federal Revenue` = `Federal Revenue` * last(`CPI (Average of July to June)`) / `CPI (Average of July to June)`) |>
    mutate(`Federal Revenue - Per Pupil` = `Federal Revenue - Per Pupil` * last(`CPI (Average of July to June)`) / `CPI (Average of July to June)`) |>
    mutate(`State Revenue` = `State Revenue` * last(`CPI (Average of July to June)`) / `CPI (Average of July to June)`) |>
    mutate(`State Revenue - Per Pupil` = `State Revenue - Per Pupil` * last(`CPI (Average of July to June)`) / `CPI (Average of July to June)`) |>
    mutate(`Local Revenue` = `Local Revenue` * last(`CPI (Average of July to June)`) / `CPI (Average of July to June)`) |>
    mutate(`Local Revenue - Per Pupil` = `Local Revenue - Per Pupil` * last(`CPI (Average of July to June)`) / `CPI (Average of July to June)`) |>
    mutate(`Current Spending` = `Current Spending` * last(`CPI (Average of July to June)`) / `CPI (Average of July to June)`) |>
    mutate(`Current Spending - Per Pupil` = `Current Spending - Per Pupil` * last(`CPI (Average of July to June)`) / `CPI (Average of July to June)`) |>
    mutate(`Total Salary` = `Total Salary` * last(`CPI (Average of July to June)`) / `CPI (Average of July to June)`) |>
    mutate(`Total Salary - Per Pupil` = `Total Salary - Per Pupil` * last(`CPI (Average of July to June)`) / `CPI (Average of July to June)`) |>
    mutate(`Total Benefits` = `Total Benefits` * last(`CPI (Average of July to June)`) / `CPI (Average of July to June)`) |>
    mutate(`Total Benefits - Per Pupil` = `Total Benefits - Per Pupil` * last(`CPI (Average of July to June)`) / `CPI (Average of July to June)`) |>
    mutate(`All Instruction` = `All Instruction` * last(`CPI (Average of July to June)`) / `CPI (Average of July to June)`) |>
    mutate(`All Instruction - Per Pupil` = `All Instruction - Per Pupil` * last(`CPI (Average of July to June)`) / `CPI (Average of July to June)`) |>
    mutate(`Instructional Staff - Salary` = `Instructional Staff - Salary` * last(`CPI (Average of July to June)`) / `CPI (Average of July to June)`) |>
    mutate(`Instructional Staff - Salary - Per Pupil` = `Instructional Staff - Salary - Per Pupil` * last(`CPI (Average of July to June)`) / `CPI (Average of July to June)`) |>
    mutate(`Instructional Staff - Benefits` = `Instructional Staff - Benefits` * last(`CPI (Average of July to June)`) / `CPI (Average of July to June)`) |>
    mutate(`Instructional Staff - Benefits - Per Pupil` = `Instructional Staff - Benefits - Per Pupil` * last(`CPI (Average of July to June)`) / `CPI (Average of July to June)`) |>
    mutate(`All Support Services` = `All Support Services` * last(`CPI (Average of July to June)`) / `CPI (Average of July to June)`) |>
    mutate(`All Support Services - Per Pupil` = `All Support Services - Per Pupil` * last(`CPI (Average of July to June)`) / `CPI (Average of July to June)`) |>
    mutate(`Support Services Staff - Salary` = `Support Services Staff - Salary` * last(`CPI (Average of July to June)`) / `CPI (Average of July to June)`) |>
    mutate(`Support Services Staff - Salary - Per Pupil` = `Support Services Staff - Salary - Per Pupil` * last(`CPI (Average of July to June)`) / `CPI (Average of July to June)`) |>
    mutate(`Support Services Staff - Benefits` = `Support Services Staff - Benefits` * last(`CPI (Average of July to June)`) / `CPI (Average of July to June)`) |>
    mutate(`Support Services Staff - Benefits - Per Pupil` = `Support Services Staff - Benefits - Per Pupil` * last(`CPI (Average of July to June)`) / `CPI (Average of July to June)`) |>
    mutate(`Support Services - Pupil Support Services` = `Support Services - Pupil Support Services` * last(`CPI (Average of July to June)`) / `CPI (Average of July to June)`) |>
    mutate(`Support Services - Pupil Support Services - Per Pupil` = `Support Services - Pupil Support Services - Per Pupil` * last(`CPI (Average of July to June)`) / `CPI (Average of July to June)`) |>
    mutate(`Support Services - Instructional Staff` = `Support Services - Instructional Staff` * last(`CPI (Average of July to June)`) / `CPI (Average of July to June)`) |>
    mutate(`Support Services - Instructional Staff - Per Pupil` = `Support Services - Instructional Staff - Per Pupil` * last(`CPI (Average of July to June)`) / `CPI (Average of July to June)`) |>
    mutate(`Support Services - General Administration` = `Support Services - General Administration` * last(`CPI (Average of July to June)`) / `CPI (Average of July to June)`) |>
    mutate(`Support Services - General Administration - Per Pupil` = `Support Services - General Administration - Per Pupil` * last(`CPI (Average of July to June)`) / `CPI (Average of July to June)`) |>
    mutate(`Support Services - School Administration` = `Support Services - School Administration` * last(`CPI (Average of July to June)`) / `CPI (Average of July to June)`) |>
    mutate(`Support Services - School Administration - Per Pupil` = `Support Services - School Administration - Per Pupil` * last(`CPI (Average of July to June)`) / `CPI (Average of July to June)`) |>
    mutate(`Support Services - Operation & Maintenance` = `Support Services - Operation & Maintenance` * last(`CPI (Average of July to June)`) / `CPI (Average of July to June)`) |>
    mutate(`Support Services - Operation & Maintenance - Per Pupil` = `Support Services - Operation & Maintenance - Per Pupil` * last(`CPI (Average of July to June)`) / `CPI (Average of July to June)`) |>
    mutate(`Support Services - Pupil Transportation` = `Support Services - Pupil Transportation` * last(`CPI (Average of July to June)`) / `CPI (Average of July to June)`) |>
    mutate(`Support Services - Pupil Transportation - Per Pupil` = `Support Services - Pupil Transportation - Per Pupil` * last(`CPI (Average of July to June)`) / `CPI (Average of July to June)`) |>
    mutate(`Support Services - Other` = `Support Services - Other` * last(`CPI (Average of July to June)`) / `CPI (Average of July to June)`) |>
    mutate(`Support Services - Other - Per Pupil` = `Support Services - Other - Per Pupil` * last(`CPI (Average of July to June)`) / `CPI (Average of July to June)`) |>
    mutate(`Total Capital Outlays` = `Total Capital Outlays` * last(`CPI (Average of July to June)`) / `CPI (Average of July to June)`) |>
    mutate(`Total Capital Outlays - Per Pupil` = `Total Capital Outlays - Per Pupil` * last(`CPI (Average of July to June)`) / `CPI (Average of July to June)`) |>
    mutate(`Short-Term Debt` = `Short-Term Debt` * last(`CPI (Average of July to June)`) / `CPI (Average of July to June)`) |>
    mutate(`Short-Term Debt - Per Pupil` = `Short-Term Debt - Per Pupil` * last(`CPI (Average of July to June)`) / `CPI (Average of July to June)`) |>
    mutate(`Long-Term Debt` = `Long-Term Debt` * last(`CPI (Average of July to June)`) / `CPI (Average of July to June)`) |>
    mutate(`Long-Term Debt - Per Pupil` = `Long-Term Debt - Per Pupil` * last(`CPI (Average of July to June)`) / `CPI (Average of July to June)`) |>
    mutate(`Total Debt` = `Short-Term Debt` + `Long-Term Debt`) |>
    mutate(`Total Debt - Per Pupil` = `Short-Term Debt - Per Pupil` + `Long-Term Debt - Per Pupil`)



# Create a data frame with the change in `Total Revenue - Per Pupil` for each state between 2002 and 2020
rev_change_03 <- ss_data_cpi |>
    select(Year, State, `Total Revenue - Per Pupil`) |>
    filter(Year == 2003 | Year == 2019) |>
    pivot_wider(
        names_from = Year,
        values_from = `Total Revenue - Per Pupil`
    ) |>
    mutate(
        `Total Revenue - Per Pupil Percent Change` = `2019` / `2003` - 1
    ) |>
    rename(
        `Total Revenue - Per Pupil 2019` = `2019`
    ) |>
    select(State, `Total Revenue - Per Pupil Percent Change`, `Total Revenue - Per Pupil 2019`)

names(rev_change_03)[1:2] <- c("State", "Percent Change Since 2003")


rev_change_02 <- ss_data_cpi |>
  select(Year, State, `Total Revenue - Per Pupil`) |>
  filter(Year == 2002 | Year == 2020) |>
  pivot_wider(
    names_from = Year,
    values_from = `Total Revenue - Per Pupil`
  ) |>
  mutate(
    `Total Revenue - Per Pupil Percent Change` = `2020` / `2002` - 1
  ) |>
  rename(
    `Total Revenue - Per Pupil 2020` = `2020`
  ) |>
  select(State, `Total Revenue - Per Pupil Percent Change`, `Total Revenue - Per Pupil 2020`)

names(rev_change_02)[1:2] <- c("State", "Percent Change Since 2002")


rev_change <- left_join(rev_change_02, rev_change_03)


# Write csv
write_csv(rev_change, "output_data/additional_requests/rev_change.csv")