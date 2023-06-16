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







# Table 6: NAEP Score Growth for the Five Highest Spending States

naep_math_4 <- read_csv("input_data/raw_naep/naep_4_math_ts.csv")
naep_math_8 <- read_csv("input_data/raw_naep/naep_8_math_ts.csv")
naep_reading_4 <- read_csv("input_data/raw_naep/naep_4_reading_ts.csv")
naep_reading_8 <- read_csv("input_data/raw_naep/naep_8_reading_ts.csv")


naep_reading_4_table <- naep_reading_4 |>
  filter(Year == 2019) |>
  filter(State != "District of Columbia") |>
  filter(State != "DoDEA") |>
  filter(State != "Puerto Rico") |>
  select(State, `NAEP`) |>
  left_join(
    naep_reading_4 |>
      filter(Year == 2003) |>
      select(State, NAEP),
    by = "State"
  ) |>
  mutate(`Reading 4 Score Pct` = `NAEP.x` / `NAEP.y` - 1) |>
  mutate(`Reading 4 Score Diff` = `NAEP.x` - `NAEP.y`) |>
  select(State, `Reading 4 Score Diff`) |>
  # mutate(`Reading 4 Score Pct` = round(`Reading 4 Score Pct`, 3)) |>
  mutate(`Reading 4 Score Diff` = round(`Reading 4 Score Diff`, 0))


naep_reading_8_table <- naep_reading_8 |>
  filter(Year == 2019) |>
  filter(State != "District of Columbia") |>
  filter(State != "DoDEA") |>
  filter(State != "Puerto Rico") |>
  select(State, `NAEP`) |>
  left_join(
    naep_reading_8 |>
      filter(Year == 2003) |>
      select(State, NAEP),
    by = "State"
  ) |>
  mutate(`Reading 8 Score Pct` = `NAEP.x` / `NAEP.y` - 1) |>
  mutate(`Reading 8 Score Diff` = `NAEP.x` - `NAEP.y`) |>
  select(State, `Reading 8 Score Diff`) |>
  # mutate(`Reading 8 Score Pct` = round(`Reading 8 Score Pct`, 3)) |>
  mutate(`Reading 8 Score Diff` = round(`Reading 8 Score Diff`, 0))


naep_math_4_table <- naep_math_4 |>
  filter(Year == 2019) |>
  filter(State != "District of Columbia") |>
  filter(State != "DoDEA") |>
  filter(State != "Puerto Rico") |>
  select(State, `NAEP`) |>
  left_join(
    naep_math_4 |>
      filter(Year == 2003) |>
      select(State, NAEP),
    by = "State"
  ) |>
  mutate(`Math 4 Score Pct` = `NAEP.x` / `NAEP.y` - 1) |>
  mutate(`Math 4 Score Diff` = `NAEP.x` - `NAEP.y`) |>
  select(State, `Math 4 Score Diff`) |>
  # mutate(`Math 4 Score Pct` = round(`Math 4 Score Pct`, 3)) |>
  mutate(`Math 4 Score Diff` = round(`Math 4 Score Diff`, 0))


naep_math_8_table <- naep_math_8 |>
  filter(Year == 2019) |>
  filter(State != "District of Columbia") |>
  filter(State != "DoDEA") |>
  filter(State != "Puerto Rico") |>
  select(State, `NAEP`) |>
  left_join(
    naep_math_8 |>
      filter(Year == 2003) |>
      select(State, NAEP),
    by = "State"
  ) |>
  mutate(`Math 8 Score Pct` = `NAEP.x` / `NAEP.y` - 1) |>
  mutate(`Math 8 Score Diff` = `NAEP.x` - `NAEP.y`) |>
  select(State, `Math 8 Score Diff`) |>
  # mutate(`Math 8 Score Pct` = round(`Math 8 Score Pct`, 3)) |>
  mutate(`Math 8 Score Diff` = round(`Math 8 Score Diff`, 0))





# Low income NAEP

low_income_naep_math_4 <- read_csv("input_data/raw_naep/naep_4_math_low_income_ts.csv")
low_income_naep_reading_4 <- read_csv("input_data/raw_naep/naep_4_reading_low_income_ts.csv")
low_income_naep_math_8 <- read_csv("input_data/raw_naep/naep_8_math_low_income_ts.csv")
low_income_naep_reading_8 <- read_csv("input_data/raw_naep/naep_8_reading_low_income_ts.csv")


low_income_naep_reading_4 <- low_income_naep_reading_4 |>
  filter(State != "DoDEA") |>
  mutate(Eligible = as.numeric(NAEP))

low_income_naep_reading_4_table <- low_income_naep_reading_4 |>
  filter(Year == 2019) |>
  filter(State != "District of Columbia") |>
  filter(State != "DoDEA") |>
  filter(State != "Puerto Rico") |>
  select(State, `Eligible`) |>
  left_join(
    low_income_naep_reading_4 |>
      filter(Year == 2003) |>
      select(State, Eligible),
    by = "State"
  ) |>
  mutate(`Reading Score Diff` = `Eligible.x` - `Eligible.y`) |>
  select(State, `Reading Score Diff`) |>
  mutate(`Reading Score Diff` = round(`Reading Score Diff`, 0)) |>
  rename(`Low Income Reading 4 Score` = `Reading Score Diff`)


low_income_naep_reading_8 <- low_income_naep_reading_8 |>
  filter(State != "DoDEA") |>
  mutate(Eligible = as.numeric(NAEP))

low_income_naep_reading_8_table <- low_income_naep_reading_8 |>
  filter(Year == 2019) |>
  filter(State != "District of Columbia") |>
  filter(State != "DoDEA") |>
  filter(State != "Puerto Rico") |>
  select(State, `Eligible`) |>
  left_join(
    low_income_naep_reading_8 |>
      filter(Year == 2003) |>
      select(State, Eligible),
    by = "State"
  ) |>
  mutate(`Reading Score Diff` = `Eligible.x` - `Eligible.y`) |>
  select(State, `Reading Score Diff`) |>
  mutate(`Reading Score Diff` = round(`Reading Score Diff`, 0)) |>
  rename(`Low Income Reading 8 Score` = `Reading Score Diff`)


low_income_naep_math_4 <- low_income_naep_math_4 |>
  filter(State != "DoDEA") |>
  mutate(Eligible = as.numeric(NAEP))

low_income_naep_math_4_table <- low_income_naep_math_4 |>
  filter(Year == 2019) |>
  filter(State != "District of Columbia") |>
  filter(State != "DoDEA") |>
  filter(State != "Puerto Rico") |>
  select(State, `Eligible`) |>
  left_join(
    low_income_naep_math_4 |>
      filter(Year == 2003) |>
      select(State, Eligible),
    by = "State"
  ) |>
  mutate(`Math Score Diff` = `Eligible.x` - `Eligible.y`) |>
  select(State, `Math Score Diff`) |>
  mutate(`Math Score Diff` = round(`Math Score Diff`, 0)) |>
  rename(`Low Income Math 4 Score` = `Math Score Diff`)


low_income_naep_math_8 <- low_income_naep_math_8 |>
  filter(State != "DoDEA") |>
  mutate(Eligible = as.numeric(NAEP))

low_income_naep_math_8_table <- low_income_naep_math_8 |>
  filter(Year == 2019) |>
  filter(State != "District of Columbia") |>
  filter(State != "DoDEA") |>
  filter(State != "Puerto Rico") |>
  select(State, `Eligible`) |>
  left_join(
    low_income_naep_math_8 |>
      filter(Year == 2003) |>
      select(State, Eligible),
    by = "State"
  ) |>
  mutate(`Math Score Diff` = `Eligible.x` - `Eligible.y`) |>
  select(State, `Math Score Diff`) |>
  mutate(`Math Score Diff` = round(`Math Score Diff`, 0)) |>
  rename(`Low Income Math 8 Score` = `Math Score Diff`)


# Create a table of the top 5 and bottom 5 states for `Total Revenue Per Pupil`

intro_table_6 <- ss_data_cpi |>
  filter(Year == 2019) |>
  filter(State != "District of Columbia") |>
  select(State, `Total Revenue - Per Pupil`) |>
  left_join(
    ss_data_cpi |>
      filter(Year == 2003) |>
      select(State, `Total Revenue - Per Pupil`),
    by = "State"
  ) |>
  mutate(`Total Revenue Per Pupil Diff` = `Total Revenue - Per Pupil.x` / `Total Revenue - Per Pupil.y` - 1) |>
  arrange(desc(`Total Revenue Per Pupil Diff`)) |>
  mutate(`Total Revenue Per Pupil Diff` = round(`Total Revenue Per Pupil Diff`, 3))

intro_table_6_top <- intro_table_6 |>
  head(3)

intro_table_6_bottom <- intro_table_6 |>
  tail(3)

# Row bind the top and bottom tables
intro_table_6 <- rbind(intro_table_6_top, intro_table_6_bottom)


# Left join NAEP tables to intro table

intro_table_6_join <- intro_table_6 |>
  left_join(
    naep_math_4_table,
    by = "State"
  ) |>
  left_join(
    naep_math_8_table,
    by = "State"
  ) |>
  left_join(
    naep_reading_4_table,
    by = "State"
  ) |>
  left_join(
    naep_reading_8_table,
    by = "State"
  ) |>
  left_join(
    low_income_naep_math_4_table,
    by = "State"
  ) |>
  left_join(
    low_income_naep_math_8_table,
    by = "State"
  ) |>
  left_join(
    low_income_naep_reading_4_table,
    by = "State"
  ) |>
  left_join(
    low_income_naep_reading_8_table,
    by = "State"
  )

intro_table_6 <- intro_table_6_join |>
  mutate(`Revenue 2019` = `Total Revenue - Per Pupil.x`) |>
  mutate(`Revenue 2003` = `Total Revenue - Per Pupil.y`) |>
  select(State, `Revenue 2019`, `Revenue 2003`, `Total Revenue Per Pupil Diff`, `Reading 4 Score Diff`, `Math 4 Score Diff`, `Reading 8 Score Diff`, `Math 8 Score Diff`, `Low Income Reading 4 Score`, `Low Income Math 4 Score`, `Low Income Reading 8 Score`, `Low Income Math 8 Score`)


write_csv(intro_table_6, "output_data/additional_requests/intro_table_6_revised.csv")

