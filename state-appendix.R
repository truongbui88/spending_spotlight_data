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



# Revenue data, with Federal Revenue, State Revenue, and Local Revenue as the columns.
# Pivot wider and make a column called Format which identifies whether the data is in Total or Per Pupil format.

# Total Revenue
total_rev <- ss_data_cpi |>
    select(
        Year, State,
        `Total Revenue`, `Total Revenue - Per Pupil`
    ) |>
    mutate(
        `Total - Inflation Adjusted` = `Total Revenue`,
        `Per Student - Inflation Adjusted` = `Total Revenue - Per Pupil`
    ) |>
    select(Year, State, `Total - Inflation Adjusted`, `Per Student - Inflation Adjusted`) |>
    group_by(Year, State) |>
    pivot_longer(
        !c(Year, State),
        names_to = "Format",
        values_to = "Revenue"
    )



# Federal Revenue
fed_rev <- ss_data_cpi |>
    select(
        Year, State,
        `Federal Revenue`, `Federal Revenue - Per Pupil`
    ) |>
    mutate(
        `Total - Inflation Adjusted` = `Federal Revenue`,
        `Per Student - Inflation Adjusted` = `Federal Revenue - Per Pupil`
    ) |>
    select(Year, State, `Total - Inflation Adjusted`, `Per Student - Inflation Adjusted`) |>
    group_by(Year, State) |>
    pivot_longer(
        !c(Year, State),
        names_to = "Format",
        values_to = "Federal Revenue"
    )

# State Revenue
state_rev <- ss_data_cpi |>
    select(
        Year, State,
        `State Revenue`, `State Revenue - Per Pupil`
    ) |>
    mutate(
        `Total - Inflation Adjusted` = `State Revenue`,
        `Per Student - Inflation Adjusted` = `State Revenue - Per Pupil`
    ) |>
    select(Year, State, `Total - Inflation Adjusted`, `Per Student - Inflation Adjusted`) |>
    group_by(Year, State) |>
    pivot_longer(
        !c(Year, State),
        names_to = "Format",
        values_to = "State Revenue"
    )

# Local Revenue
local_rev <- ss_data_cpi |>
    select(
        Year, State,
        `Local Revenue`, `Local Revenue - Per Pupil`
    ) |>
    mutate(
        `Total - Inflation Adjusted` = `Local Revenue`,
        `Per Student - Inflation Adjusted` = `Local Revenue - Per Pupil`
    ) |>
    select(Year, State, `Total - Inflation Adjusted`, `Per Student - Inflation Adjusted`) |>
    group_by(Year, State) |>
    pivot_longer(
        !c(Year, State),
        names_to = "Format",
        values_to = "Local Revenue"
    )

# Combine all revenue data into one data frame
rev_data <- left_join(total_rev, fed_rev, by = c("Year", "State", "Format")) |>
    left_join(state_rev, by = c("Year", "State", "Format")) |>
    left_join(local_rev, by = c("Year", "State", "Format"))


# Save data
write_csv(rev_data, "output_data/appendix/rev_data_total.csv")
