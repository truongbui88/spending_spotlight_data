# Load libraries
library(tidyverse)
library(jsonlite)
library(zoo)
library(janitor)
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


#Truong's comment on ss_data_cpi: the code chunk above can be significantly simplified as shown below.
#The CPI data looks good and can be reproduced by the "inflation data.R" script. 
ss_data_cpi2 <- ss_data |> 
  clean_names() |> 
  filter(state != "District of Columbia") |> 
  arrange(state, year) |> 
  group_by(state) |> 
  mutate(across(c(total_revenue:long_term_debt_per_pupil), ~.x * last(cpi_average_of_july_to_june) / cpi_average_of_july_to_june)) |> 
  ungroup() |> 
  mutate(total_debt = short_term_debt + long_term_debt,
         total_debt_per_pupil = short_term_debt_per_pupil + long_term_debt_per_pupil)


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


fed_rev_per_student <- fed_rev |>
  filter(Format == "Per Student - Inflation Adjusted") |>
  select(State, Year, `Federal Revenue`)

state_rev_per_student <- state_rev |>
  filter(Format == "Per Student - Inflation Adjusted") |>
  select(State, Year, `State Revenue`)

local_rev_per_student <- local_rev |>
  filter(Format == "Per Student - Inflation Adjusted") |>
  select(State, Year, `Local Revenue`)

basic_rev_data <- fed_rev_per_student |>
  left_join(state_rev_per_student, by = c("State", "Year")) |>
  left_join(local_rev_per_student, by = c("State", "Year"))

basic_rev_data <- basic_rev_data |>
  mutate(Year = paste0(Year, "-01-01"))

# Save data as JSON
basic_rev_data_json <- toJSON(basic_rev_data, pretty = TRUE)

write(basic_rev_data_json, "output_data/appendix/basic_rev_data.json")


#Truong's comment on basic_rev_data: more efficient transformation can be done as shown below:
basic_rev_data2 <- ss_data_cpi2 |> 
  select(year, state, federal_revenue_per_pupil, state_revenue_per_pupil, local_revenue_per_pupil) |> 
  mutate(year = paste0(year, "-01-01"))


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

# Pivote ss_data_cpi_salary_benefits wide
ss_data_cpi_salary_benefits_wide <- ss_data_cpi_salary_benefits |>
  pivot_wider(
    names_from = Format,
    values_from = c(`Total Salary`, `Total Benefits`)
  ) |>
  mutate(Year = paste0(Year, "-01-01"))

# Write to JSON
ss_data_cpi_salary_benefits_json <- toJSON(ss_data_cpi_salary_benefits_wide, pretty = TRUE)

write(ss_data_cpi_salary_benefits_json, "output_data/appendix/salary_benefits.json")


#Truong's comment on ss_data_cpi_salary_benefits: the code needs to be cleaned up as we don't need that many transformations to produce the same result.


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

# Save data to JSON
rev_enroll_change_json <- toJSON(rev_enroll_change, pretty = TRUE)

write(rev_enroll_change_json, "output_data/appendix/rev_enrollment_table.json")


#Truong's comment on revenue and enrollment change calculation: can be done more efficiently as shown below:
rev_change2 <- ss_data_cpi2 |> 
  select(year, state, total_revenue_per_pupil, enrollment) |> 
  group_by(state) |> 
  summarise(total_revenue_per_pupil_percent_change = total_revenue_per_pupil[year == 2020] / total_revenue_per_pupil[year == 2002] - 1,
            total_revenue_per_pupil_2020 = total_revenue_per_pupil[year == 2020],
            enrollment_percent_change = enrollment[year == 2020] / enrollment[year == 2002] - 1,
            enrollment_2020 = enrollment[year == 2020])


# Enrollment and Staffing Trends

# Get enrollment data from ss_data_cpi
enroll_data <- ss_data_cpi |>
  select(Year, State, Enrollment)

#  Get staffing data from 'input_data/teachers_non_teachers.csv'
staff_data <- read_csv("input_data/teachers_non_teachers.csv") |>
  select(Year, State, `All staff`, Teachers) |>
  # create a column for non-teachers
  mutate(`Non-Teachers` = `All staff` - Teachers) |>
  select(Year, State, Teachers, `Non-Teachers`)


# Combine enrollment and staffing data into one data frame
enroll_staff_data <- left_join(enroll_data, staff_data, by = c("Year", "State"))

# For each state calculate the percentage change from 2002 for enrollment, teachers, and non-teachers
enroll_staff_data <- enroll_staff_data |>
  mutate(
    `Enrollment Percent Change` = Enrollment / Enrollment[Year == 2002] - 1,
    `Teachers Percent Change` = Teachers / Teachers[Year == 2002] - 1,
    `Non-Teachers Percent Change` = `Non-Teachers` / `Non-Teachers`[Year == 2002] - 1
  ) |>
  mutate(Year = paste0(Year, "-01-01"))

# Write the data to json
enroll_staff_data_json <- toJSON(enroll_staff_data, pretty = TRUE)

write(enroll_staff_data_json, "output_data/appendix/enroll_staff_data.json")


# Teacher Salary Growth vs Revenue Per Student Growth

# Get revenue data from ss_data_cpi
rev_data <- ss_data_cpi |>
  select(Year, State, `Total Revenue - Per Pupil`)

# Get teacher salary data from input_data/salary_data.csv
salary_data <- read_csv("input_data/salary_data.csv") |>
  select(Year, State, `Salary Adj`)

# Combine revenue and salary data into one data frame
rev_salary_data <- left_join(rev_data, salary_data, by = c("Year", "State"))

# Calculate the percentage change from 2002 for revenue and teacher salary
rev_salary_data <- rev_salary_data |>
  mutate(
    `Total Revenue - Per Pupil Percent Change` = `Total Revenue - Per Pupil` / `Total Revenue - Per Pupil`[Year == 2002] - 1,
    `Salary Adj Percent Change` = `Salary Adj` / `Salary Adj`[Year == 2002] - 1
  ) |>
  mutate(Year = paste0(Year, "-01-01"))

# Write the data to json
rev_salary_data_json <- toJSON(rev_salary_data, pretty = TRUE)

write(rev_salary_data_json, "output_data/appendix/rev_salary_data.json")


# NAEP

# Load 4th and 8th grade NAEP data
naep_math_8 <- read_csv("input_data/raw_naep/naep_8_math_ts.csv") |> arrange(Year, State)
naep_math_4 <- read_csv("input_data/raw_naep/naep_4_math_ts.csv") |> arrange(Year, State)
naep_reading_8 <- read_csv("input_data/raw_naep/naep_8_reading_ts.csv") |> arrange(Year, State)
naep_reading_4 <- read_csv("input_data/raw_naep/naep_4_reading_ts.csv") |> arrange(Year, State)


# Combine rev_data and naep data into one data frame

# 8th grade math
naep_math_8_rev <- naep_math_8 |>
  filter(State != "District of Columbia") |>
  filter(State != "DoDEA") |>
  filter(State != "Puerto Rico") |>
  filter(Year != 2022) |>
  right_join(rev_data, by = c("Year", "State")) |>
  filter(Year != 2002) |>
  filter(Year != 2020) |>
  arrange(State, Year) |>
  rename(naep_math_8 = NAEP) |>
  group_by(State) |> # You need to group by state as each state will have its own timeline
  mutate(naep_math_8 = na.approx(naep_math_8)) |> # This does the linear interpolation
  mutate(
    `Total Revenue - Per Pupil Percent Change` = `Total Revenue - Per Pupil` / `Total Revenue - Per Pupil`[Year == 2003] - 1,
    naep_math_8_change = naep_math_8 / naep_math_8[Year == 2003] - 1
  ) |>
  ungroup()

# 4th grade math
naep_math_4_rev <- naep_math_4 |>
  filter(State != "District of Columbia") |>
  filter(State != "DoDEA") |>
  filter(State != "Puerto Rico") |>
  filter(Year != 2022) |>
  right_join(rev_data, by = c("Year", "State")) |>
  filter(Year != 2002) |>
  filter(Year != 2020) |>
  arrange(State, Year) |>
  rename(naep_math_4 = NAEP) |>
  group_by(State) |> # You need to group by state as each state will have its own timeline
  mutate(naep_math_4 = na.approx(naep_math_4)) |> # This does the linear interpolation
  mutate(
    `Total Revenue - Per Pupil Percent Change` = `Total Revenue - Per Pupil` / `Total Revenue - Per Pupil`[Year == 2003] - 1,
    naep_math_4_change = naep_math_4 / naep_math_4[Year == 2003] - 1
  ) |>
  ungroup()

# 8th grade reading
naep_reading_8_rev <- naep_reading_8 |>
  filter(State != "District of Columbia") |>
  filter(State != "DoDEA") |>
  filter(State != "Puerto Rico") |>
  filter(Year != 2022) |>
  right_join(rev_data, by = c("Year", "State")) |>
  filter(Year != 2002) |>
  filter(Year != 2020) |>
  arrange(State, Year) |>
  rename(naep_reading_8 = NAEP) |>
  group_by(State) |> # You need to group by state as each state will have its own timeline
  mutate(naep_reading_8 = na.approx(naep_reading_8)) |> # This does the linear interpolation
  mutate(
    `Total Revenue - Per Pupil Percent Change` = `Total Revenue - Per Pupil` / `Total Revenue - Per Pupil`[Year == 2003] - 1,
    naep_reading_8_change = naep_reading_8 / naep_reading_8[Year == 2003] - 1
  ) |>
  ungroup()


# 4th grade reading
naep_reading_4_rev <- naep_reading_4 |>
  filter(State != "District of Columbia") |>
  filter(State != "DoDEA") |>
  filter(State != "Puerto Rico") |>
  filter(Year != 2022) |>
  right_join(rev_data, by = c("Year", "State")) |>
  filter(Year != 2002) |>
  filter(Year != 2020) |>
  arrange(State, Year) |>
  rename(naep_reading_4 = NAEP) |>
  group_by(State) |> # You need to group by state as each state will have its own timeline
  mutate(naep_reading_4 = na.approx(naep_reading_4)) |> # This does the linear interpolation
  mutate(
    `Total Revenue - Per Pupil Percent Change` = `Total Revenue - Per Pupil` / `Total Revenue - Per Pupil`[Year == 2003] - 1,
    naep_reading_4_change = naep_reading_4 / naep_reading_4[Year == 2003] - 1
  ) |>
  ungroup()


# Pair down the variables
naep_math_8_rev <- naep_math_8_rev |>
  select(Year, State, `Total Revenue - Per Pupil`, `Total Revenue - Per Pupil Percent Change`, `naep_math_8`, `naep_math_8_change`)

naep_math_4_rev <- naep_math_4_rev |>
  select(Year, State, `naep_math_4`, `naep_math_4_change`)

naep_reading_8_rev <- naep_reading_8_rev |>
  select(Year, State, `naep_reading_8`, `naep_reading_8_change`)

naep_reading_4_rev <- naep_reading_4_rev |>
  select(Year, State, `naep_reading_4`, `naep_reading_4_change`)


# Combine the data frames into one
naep_rev_data <- naep_math_8_rev |>
  left_join(naep_math_4_rev, by = c("Year", "State")) |>
  left_join(naep_reading_8_rev, by = c("Year", "State")) |>
  left_join(naep_reading_4_rev, by = c("Year", "State")) |>
  mutate(Year = paste0(Year, "-01-01"))

# Write to json
naep_rev_data_json <- toJSON(naep_rev_data, pretty = TRUE)

write(naep_rev_data_json, "output_data/appendix/naep_rev_data.json")


#Truong's comment on NAEP data processing: the data can be processed more efficiently as below:
naep_math_8 <- naep_math_8 |> clean_names() |> 
  rename(naep_math_8 = naep) |> 
  select(1:3)

naep_math_4 <- naep_math_4 |> clean_names() |> 
  rename(naep_math_4 = naep) |> 
  select(1:3)

naep_reading_8 <- naep_reading_8 |> clean_names() |> 
  rename(naep_reading_8 = naep) |> 
  select(1:3)

naep_reading_4 <- naep_reading_4 |> clean_names() |> 
  rename(naep_reading_4 = naep) |> 
  select(1:3)

rev_data2 <- rev_data |> clean_names()

naep_rev_data2 <- naep_math_8 |> 
  left_join(naep_math_4) |> 
  left_join(naep_reading_8) |> 
  left_join(naep_reading_4) |> 
  right_join(rev_data2) |> 
  arrange(state, year) |> 
  filter(!year %in% c(2002, 2020)) |> 
  group_by(state) |> 
  mutate(across(contains("naep"), na.approx),   #linear interpolation
         across(naep_math_8:total_revenue_per_pupil, ~ .x / .x[year == 2003] - 1, .names = "{.col}_change"),  #cumulative growth calculation (baseline year is 2003)
         year = paste0(year, "-01-01")) |> 
  ungroup() |> 
  #rearrange the columns for readability
  select(year, state, total_revenue_per_pupil, total_revenue_per_pupil_change,
         naep_rev_data2 |> select(contains("naep")) |> names() |> sort())


# Low Income NAEP

# Load 4th and 8th grade NAEP data
li_naep_math_8 <- read_csv("input_data/raw_naep/naep_8_math_low_income_ts.csv") |> arrange(Year, State)
li_naep_math_4 <- read_csv("input_data/raw_naep/naep_4_math_low_income_ts.csv") |> arrange(Year, State)
li_naep_reading_8 <- read_csv("input_data/raw_naep/naep_8_reading_low_income_ts.csv") |> arrange(Year, State)
li_naep_reading_4 <- read_csv("input_data/raw_naep/naep_4_reading_low_income_ts.csv") |> arrange(Year, State)


# Combine rev_data and naep data into one data frame

# 8th grade math
li_naep_math_8_rev <- li_naep_math_8 |>
  filter(State != "District of Columbia") |>
  filter(State != "DoDEA") |>
  filter(State != "Puerto Rico") |>
  filter(Year != 2022) |>
  right_join(rev_data, by = c("Year", "State")) |>
  filter(Year != 2002) |>
  filter(Year != 2020) |>
  arrange(State, Year) |>
  rename(li_naep_math_8 = NAEP) |>
  group_by(State) |> # You need to group by state as each state will have its own timeline
  mutate(li_naep_math_8 = na.approx(li_naep_math_8)) |> # This does the linear interpolation
  mutate(
    `Total Revenue - Per Pupil Percent Change` = `Total Revenue - Per Pupil` / `Total Revenue - Per Pupil`[Year == 2003] - 1,
    li_naep_math_8_change = li_naep_math_8 / li_naep_math_8[Year == 2003] - 1
  ) |>
  ungroup()

# 4th grade math
li_naep_math_4_rev <- li_naep_math_4 |>
  filter(State != "District of Columbia") |>
  filter(State != "DoDEA") |>
  filter(State != "Puerto Rico") |>
  filter(Year != 2022) |>
  right_join(rev_data, by = c("Year", "State")) |>
  filter(Year != 2002) |>
  filter(Year != 2020) |>
  arrange(State, Year) |>
  rename(li_naep_math_4 = NAEP) |>
  group_by(State) |> # You need to group by state as each state will have its own timeline
  mutate(li_naep_math_4 = na.approx(li_naep_math_4)) |> # This does the linear interpolation
  mutate(
    `Total Revenue - Per Pupil Percent Change` = `Total Revenue - Per Pupil` / `Total Revenue - Per Pupil`[Year == 2003] - 1,
    li_naep_math_4_change = li_naep_math_4 / li_naep_math_4[Year == 2003] - 1
  ) |>
  ungroup()

# 8th grade reading
li_naep_reading_8_rev <- li_naep_reading_8 |>
  filter(State != "District of Columbia") |>
  filter(State != "DoDEA") |>
  filter(State != "Puerto Rico") |>
  filter(Year != 2022) |>
  right_join(rev_data, by = c("Year", "State")) |>
  filter(Year != 2002) |>
  filter(Year != 2020) |>
  arrange(State, Year) |>
  rename(li_naep_reading_8 = NAEP) |>
  group_by(State) |> # You need to group by state as each state will have its own timeline
  mutate(li_naep_reading_8 = na.approx(li_naep_reading_8)) |> # This does the linear interpolation
  mutate(
    `Total Revenue - Per Pupil Percent Change` = `Total Revenue - Per Pupil` / `Total Revenue - Per Pupil`[Year == 2003] - 1,
    li_naep_reading_8_change = li_naep_reading_8 / li_naep_reading_8[Year == 2003] - 1
  ) |>
  ungroup()


# 4th grade reading
li_naep_reading_4_rev <- li_naep_reading_4 |>
  filter(State != "District of Columbia") |>
  filter(State != "DoDEA") |>
  filter(State != "Puerto Rico") |>
  filter(Year != 2022) |>
  right_join(rev_data, by = c("Year", "State")) |>
  filter(Year != 2002) |>
  filter(Year != 2020) |>
  arrange(State, Year) |>
  rename(li_naep_reading_4 = NAEP) |>
  group_by(State) |> # You need to group by state as each state will have its own timeline
  mutate(li_naep_reading_4 = na.approx(li_naep_reading_4)) |> # This does the linear interpolation
  mutate(
    `Total Revenue - Per Pupil Percent Change` = `Total Revenue - Per Pupil` / `Total Revenue - Per Pupil`[Year == 2003] - 1,
    li_naep_reading_4_change = li_naep_reading_4 / li_naep_reading_4[Year == 2003] - 1
  ) |>
  ungroup()


# Pair down the variables
li_naep_math_8_rev <- li_naep_math_8_rev |>
  select(Year, State, `Total Revenue - Per Pupil`, `Total Revenue - Per Pupil Percent Change`, `li_naep_math_8`, `li_naep_math_8_change`)

li_naep_math_4_rev <- li_naep_math_4_rev |>
  select(Year, State, `li_naep_math_4`, `li_naep_math_4_change`)

li_naep_reading_8_rev <- li_naep_reading_8_rev |>
  select(Year, State, `li_naep_reading_8`, `li_naep_reading_8_change`)

li_naep_reading_4_rev <- li_naep_reading_4_rev |>
  select(Year, State, `li_naep_reading_4`, `li_naep_reading_4_change`)


# Combine the data frames into one
li_naep_rev_data <- li_naep_math_8_rev |>
  left_join(li_naep_math_4_rev, by = c("Year", "State")) |>
  left_join(li_naep_reading_8_rev, by = c("Year", "State")) |>
  left_join(li_naep_reading_4_rev, by = c("Year", "State")) |>
  mutate(Year = paste0(Year, "-01-01"))

# Write to json
li_naep_rev_data_json <- toJSON(li_naep_rev_data, pretty = TRUE)

write(li_naep_rev_data_json, "output_data/appendix/li_naep_rev_data.json")


#Truong's comment: similar to the non-low-income NAEP data, the low-income NAEP data can be processed more efficiently using the same method above.