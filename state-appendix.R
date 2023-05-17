# Load libraries
library(tidyverse)
library(jsonlite)
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

# Write this data to json
rev_data_total_json <- toJSON(rev_data, pretty = TRUE)
write(rev_data_total_json, "output_data/appendix/rev_data_total.json")




# Create a table with `Total Revenue - Per Pupil` and Enrollment change for each state between 2002 and 2020

# Create a data frame with the change in `Total Revenue - Per Pupil` for each state between 2002 and 2020
rev_change <- ss_data_cpi |>
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

# Create a data frame with the change in Enrollment for each state between 2002 and 2020
enroll_change <- ss_data_cpi |>
    select(Year, State, Enrollment) |>
    filter(Year == 2002 | Year == 2020) |>
    pivot_wider(
        names_from = Year,
        values_from = Enrollment
    ) |>
    mutate(
        `Enrollment Percent Change` = `2020` / `2002` - 1
    ) |>
    rename(
        `Enrollment (2020)` = `2020`
    ) |>
    select(State, `Enrollment Percent Change`, `Enrollment (2020)`)

# Combine the two data frames into one
rev_enroll_change <- left_join(rev_change, enroll_change, by = "State")

# Save data
write_csv(rev_enroll_change, "output_data/appendix/rev_enrollment_table.csv")


# Salary & Benefits

ss_data_cpi_ii <- ss_data_cpi |>
    mutate(
        `Total Salary + Benefits - Per Pupil` = `Total Salary - Per Pupil` + `Total Benefits - Per Pupil`,
        `Total Salary - Per Pupil Percent` = `Total Salary - Per Pupil` / `Total Salary + Benefits - Per Pupil`,
        `Total Benefits - Per Pupil Percent` = `Total Benefits - Per Pupil` / `Total Salary + Benefits - Per Pupil`
    )


# ss_data_cpi with Total Salary as the column.
# Pivot wider and make a column called Format which identifies whether the data is in Total, Per Pupil, or Percent format.

ss_data_cpi_salary <- ss_data_cpi_ii |>
    select(
        Year, State,
        `Total Salary`, `Total Salary - Per Pupil`, `Total Salary - Per Pupil Percent`
    ) |>
    mutate(
        `Total - Inflation Adjusted` = `Total Salary`,
        `Per Student - Inflation Adjusted` = `Total Salary - Per Pupil`,
        `Percent` = `Total Salary - Per Pupil Percent`
    ) |>
    select(Year, State, `Total - Inflation Adjusted`, `Per Student - Inflation Adjusted`, `Percent`) |>
    group_by(Year, State) |>
    pivot_longer(
        !c(Year, State),
        names_to = "Format",
        values_to = "Total Salary"
    )

# ss_data_cpi with Total Benefits as the column.
# Pivot wider and make a column called Format which identifies whether the data is in Total, Per Pupil, or Percent format.

ss_data_cpi_benefits <- ss_data_cpi_ii |>
    select(
        Year, State,
        `Total Benefits`, `Total Benefits - Per Pupil`, `Total Benefits - Per Pupil Percent`
    ) |>
    mutate(
        `Total - Inflation Adjusted` = `Total Benefits`,
        `Per Student - Inflation Adjusted` = `Total Benefits - Per Pupil`,
        `Percent` = `Total Benefits - Per Pupil Percent`
    ) |>
    select(Year, State, `Total - Inflation Adjusted`, `Per Student - Inflation Adjusted`, `Percent`) |>
    group_by(Year, State) |>
    pivot_longer(
        !c(Year, State),
        names_to = "Format",
        values_to = "Total Benefits"
    )

# Combine ss_data_cpi_salary and ss_data_cpi_benefits into one data frame
ss_data_cpi_salary_benefits <- left_join(ss_data_cpi_salary, ss_data_cpi_benefits, by = c("Year", "State", "Format"))


# Save data
write_csv(ss_data_cpi_salary_benefits, "output_data/appendix/salary_benefits.csv")



# Salary & Benefits Percent Change

ss_data_cpi_iii <- ss_data_cpi |>
    mutate(`Total Salary` = `Total Salary - Per Pupil`) |>
    mutate(`Total Benefits` = `Total Benefits - Per Pupil`)

ss_data_cpi_iii <- ss_data_cpi_iii |>
    # filter(Year != 2002) |>
    left_join(
        ss_data_cpi_iii |>
            filter(Year == 2002) |>
            select(State, `Total Salary`, `Total Benefits`),
        by = "State"
    ) |>
    mutate(
        `Total Salary - Per Pupil Percent Change` = `Total Salary.x` / `Total Salary.y` - 1,
        `Total Benefits - Per Pupil Percent Change` = `Total Benefits.x` / `Total Benefits.y` - 1
    ) |>
    select(Year, State, `Total Salary - Per Pupil Percent Change`, `Total Benefits - Per Pupil Percent Change`, `Total Salary.x`, `Total Benefits.x`) |>
    mutate(`Total Salary` = `Total Salary.x`) |>
    mutate(`Total Benefits` = `Total Benefits.x`) |>
    select(-`Total Salary.x`, -`Total Benefits.x`)


write_csv(ss_data_cpi_iii, "output_data/appendix/salary_benefits_pct.csv")
