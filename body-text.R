# RUN AFTER body-data-processing.R

##################################################
############### TEXT FOR THE BODY ################
##################################################

# Pull "United States" and get the 2020 `Total Revenue - Per Pupil` and 2002 `Total Revenue - Per Pupil`
us_2020 <- ss_data_cpi |>
    filter(State == "United States") |>
    filter(Year == 2020) |>
    select(`Total Revenue - Per Pupil`)

us_2002 <- ss_data_cpi |>
    filter(State == "United States") |>
    filter(Year == 2002) |>
    select(`Total Revenue - Per Pupil`)

us_2020
us_2002

# Pull "United States" and get the 2020 `Total Benefits - Per Pupil` and 2002 `Total Benefits - Per Pupil`
us_2020_benefits <- ss_data_cpi |>
    filter(State == "United States") |>
    filter(Year == 2020) |>
    select(`Total Benefits - Per Pupil`)

us_2002_benefits <- ss_data_cpi |>
    filter(State == "United States") |>
    filter(Year == 2002) |>
    select(`Total Benefits - Per Pupil`)

# Calculate the difference between us_2020_benefits and us_2002_benefits
us_2020_benefits[1, 2] - us_2002_benefits[1, 2]
us_2020_benefits[1, 2] / us_2002_benefits[1, 2] - 1

# Enrollment

# Pull "United States" and get the 2020 `Enrollment` and 2002 `Enrollment`
us_2020_enrollment <- ss_data_cpi |>
    filter(State == "United States") |>
    filter(Year == 2020) |>
    select(`Enrollment`)

us_2002_enrollment <- ss_data_cpi |>
    filter(State == "United States") |>
    filter(Year == 2002) |>
    select(`Enrollment`)

# Calculate the difference between us_2020_enrollment and us_2002_enrollment
us_2020_enrollment[1, 2] - us_2002_enrollment[1, 2]

# Calculate the percent change between us_2020_enrollment and us_2002_enrollment
us_2020_enrollment[1, 2] / us_2002_enrollment[1, 2] - 1

# Get `All staff` for 2020 and 2002 from teachers_non_teachers
teachers_non_teachers_2020 <- staffing |>
    filter(State == "United States") |>
    filter(Year == 2020) |>
    select(`All staff`)

teachers_non_teachers_2002 <- staffing |>
    filter(State == "United States") |>
    filter(Year == 2002) |>
    select(`All staff`)

# Calculate the difference between teachers_non_teachers_2020 and teachers_non_teachers_2002
teachers_non_teachers_2020[1, 2] - teachers_non_teachers_2002[1, 2]

# Calculate the percent change between teachers_non_teachers_2020 and teachers_non_teachers_2002
teachers_non_teachers_2020[1, 2] / teachers_non_teachers_2002[1, 2] - 1

# Get Non-Teachers from `All staff` - `Teachers` for 2020 and 2002 from staffing
non_teachers_2020 <- staffing |>
    filter(State == "United States") |>
    filter(Year == 2020) |>
    select(`All staff`, `Teachers`) |>
    mutate(`Non-Teachers` = `All staff` - `Teachers`)

non_teachers_2002 <- staffing |>
    filter(State == "United States") |>
    filter(Year == 2002) |>
    select(`All staff`, `Teachers`) |>
    mutate(`Non-Teachers` = `All staff` - `Teachers`)

# Calculate the difference between non_teachers_2020 and non_teachers_2002
non_teachers_2020[1, 3] - non_teachers_2002[1, 3]

(non_teachers_2020[1, 3] - non_teachers_2002[1, 3]) / (teachers_non_teachers_2020[1, 2] - teachers_non_teachers_2002[1, 2])


#  Get average salary for teachers in the "United States" for 2020 and 2002
teachers_salary_2020 <- salary_pct |>
    filter(State == "United States")




# Key Trend #1

# Count the number of rows in rev_table that are greater than 0
rev_table |>
    filter(State != "United States") |>
    filter(Growth > 0) |>
    count()

rev_table |>
    filter(State != "United States") |>
    filter(Growth > 0.5) |>
    count()

rev_table |>
    filter(State != "United States") |>
    filter(Growth > 0.5)


# Get the `Federal Revenue - Per Pupil`, `State Revenue - Per Pupil`, `Local Revenue - Per Pupil`, `Total Revenue - Per Pupil` for 2020 and 2002 for the "United States"
us_2020_rev <- ss_data_cpi |>
    filter(State == "United States") |>
    filter(Year == 2020) |>
    select(`Federal Revenue - Per Pupil`, `State Revenue - Per Pupil`, `Local Revenue - Per Pupil`, `Total Revenue - Per Pupil`)

us_2002_rev <- ss_data_cpi |>
    filter(State == "United States") |>
    filter(Year == 2002) |>
    select(`Federal Revenue - Per Pupil`, `State Revenue - Per Pupil`, `Local Revenue - Per Pupil`, `Total Revenue - Per Pupil`)

# Calculate the difference between us_2020_rev$`Federal Revenue - Per Pupil` and us_2002_rev$`Federal Revenue - Per Pupil`
us_2020_rev$`Federal Revenue - Per Pupil` - us_2002_rev$`Federal Revenue - Per Pupil`

# Calculate the percent change between us_2020_rev$`Federal Revenue - Per Pupil` and us_2002_rev$`Federal Revenue - Per Pupil`
us_2020_rev$`Federal Revenue - Per Pupil` / us_2002_rev$`Federal Revenue - Per Pupil` - 1

# Calculate the difference between us_2020_rev$`State Revenue - Per Pupil` and us_2002_rev$`State Revenue - Per Pupil`
us_2020_rev$`State Revenue - Per Pupil` - us_2002_rev$`State Revenue - Per Pupil`

# Calculate the percent change between us_2020_rev$`State Revenue - Per Pupil` and us_2002_rev$`State Revenue - Per Pupil`
us_2020_rev$`State Revenue - Per Pupil` / us_2002_rev$`State Revenue - Per Pupil` - 1

# Calculate the difference between us_2020_rev$`Local Revenue - Per Pupil` and us_2002_rev$`Local Revenue - Per Pupil`
us_2020_rev$`Local Revenue - Per Pupil` - us_2002_rev$`Local Revenue - Per Pupil`

# Calculate the percent change between us_2020_rev$`Local Revenue - Per Pupil` and us_2002_rev$`Local Revenue - Per Pupil`
us_2020_rev$`Local Revenue - Per Pupil` / us_2002_rev$`Local Revenue - Per Pupil` - 1


rev_table |>
    filter(State != "United States") |>
    filter(`2020` > 20e3)



# Key Trend #2

# Get the `Total Revenue - Per Pupil` for 2020 and 2002 for the "United States"

us_2020_rev <- ss_data_cpi |>
    filter(State == "United States") |>
    filter(Year == 2020) |>
    select(`Total Revenue - Per Pupil`)

us_2002_rev <- ss_data_cpi |>
    filter(State == "United States") |>
    filter(Year == 2002) |>
    select(`Total Revenue - Per Pupil`)

# Calculate the percent difference between us_2020_rev$`Total Revenue - Per Pupil` and us_2002_rev$`Total Revenue - Per Pupil`
us_2020_rev$`Total Revenue - Per Pupil` / us_2002_rev$`Total Revenue - Per Pupil` - 1


# Get `Salary Adj` for 2020 and 2002 for the "United States" from salary_pct
us_2020_salary <- salary_pct |>
    filter(State == "United States") |>
    filter(Year == 2020) |>
    select(`Salary Adj`)

us_2002_salary <- salary_pct |>
    filter(State == "United States") |>
    filter(Year == 2002) |>
    select(`Salary Adj`)

# Calculate the percent difference between us_2020_salary$`Salary Adj` and us_2002_salary$`Salary Adj`
us_2020_salary$`Salary Adj` / us_2002_salary$`Salary Adj` - 1


# Pull "United States" and get the 2020 `Enrollment` and 2002 `Enrollment`
us_2020_enrollment <- ss_data_cpi |>
    filter(State == "United States") |>
    filter(Year == 2020) |>
    select(`Enrollment`)

us_2002_enrollment <- ss_data_cpi |>
    filter(State == "United States") |>
    filter(Year == 2002) |>
    select(`Enrollment`)


# Calculate the percent change between us_2020_enrollment and us_2002_enrollment
us_2020_enrollment[1, 2] / us_2002_enrollment[1, 2] - 1



# Key Trend #3

# Staffing / Enrollment Changes

staff_greater_enrollment <- enrollment_trends |>
    left_join(staffing_trends, by = "State") |>
    arrange(`Enrollment Growth`) |>
    select(State, `Enrollment 2020`, `Enrollment 2002`, `Enrollment Growth`, `Total Staff Growth`, `Non-Teachers Growth`, `Teachers Growth`) |>
    mutate_at(vars(`Enrollment Growth`, `Teachers Growth`, `Non-Teachers Growth`, `Total Staff Growth`), round, 3) |>
    select(State, `Enrollment 2002`, `Enrollment 2020`, `Enrollment Growth`, `Total Staff Growth`, `Non-Teachers Growth`, `Teachers Growth`) |>
    filter(State != "United States") |>
    mutate(`Staff_Greater_Enrollment` = ifelse(`Total Staff Growth` > `Enrollment Growth`, 1, 0)) |>
    # Get the sum of `Staff_Greater_Enrollment`
    as.data.frame() |>
    summarise(`Staff_Greater_Enrollment` = sum(`Staff_Greater_Enrollment`))

staff_greater_enrollment


non_teachers_growth <- enrollment_trends |>
    left_join(staffing_trends, by = "State") |>
    arrange(`Enrollment Growth`) |>
    select(State, `Enrollment 2020`, `Enrollment 2002`, `Enrollment Growth`, `Total Staff Growth`, `Non-Teachers Growth`, `Teachers Growth`) |>
    mutate_at(vars(`Enrollment Growth`, `Teachers Growth`, `Non-Teachers Growth`, `Total Staff Growth`), round, 3) |>
    select(State, `Enrollment 2002`, `Enrollment 2020`, `Enrollment Growth`, `Total Staff Growth`, `Non-Teachers Growth`, `Teachers Growth`) |>
    filter(State != "United States")



# Load Complete US Staffing Data

complete_staffing <- read_csv("input_data/complete_staffing.csv")

# Calculate `Other support services staff ` as a percent of non-teachers
complete_staffing_total <- complete_staffing |>
    as.data.frame() |>
    filter(Category != Teachers) |>
    summarise(
        `2002` = sum(`2002`),
        `2020` = sum(`2020`)
    )


# Get `Other support services staff` from Category

other_support_services <- complete_staffing |>
    filter(Category == "Other support services staff") |>
    select(`2002`, `2020`)

# Get `Instructional aids` from Category
instructional_aids <- complete_staffing |>
    filter(Category == "Instructional aides") |>
    select(`2002`, `2020`)

# Calculate the percent of `Other support services staff` from  `Total Non-Teachers`

(other_support_services$`2020` + instructional_aids$`2020`) / complete_staffing_total$`2020`


complete_staffing_table <- complete_staffing |>
    mutate(growth = `2020` / `2002` - 1) |>
    mutate(`2020 pct` = `2020` / complete_staffing_total$`2020`)




# Enrollment, Staffing Trends

enrollment_staffing_trends |>
    as.data.frame() |>
    mutate(`Total Staff Growth Count` = ifelse(`Total Staff Growth` > 0, 1, 0)) |>
    summarise(
        `Total Staff Growth Count` = sum(`Total Staff Growth Count`)
    )




# Key Trend #4


employee_benefits_table <- ss_data_cpi |>
    filter(Year == 2020) |>
    select(State, `Total Benefits - Per Pupil`) |>
    left_join(
        ss_data_cpi |>
            filter(Year == 2002) |>
            select(State, `Total Benefits - Per Pupil`),
        by = "State"
    ) |>
    mutate(`Total Benefits - Per Pupil 2020` = `Total Benefits - Per Pupil.x`) |>
    mutate(`Total Benefits - Per Pupil 2002` = `Total Benefits - Per Pupil.y`) |>
    mutate(
        `Percent Change` = `Total Benefits - Per Pupil 2020` / `Total Benefits - Per Pupil 2002` - 1
    ) |>
    select(State, `Total Benefits - Per Pupil 2020`, `Total Benefits - Per Pupil 2002`, `Percent Change`) |>
    arrange(desc(`Percent Change`))


# Remove the United States row, add a rank column, then add back the United States row to the top of the table

employee_benefits_table_states <- employee_benefits_table |>
    as.data.frame() |>
    filter(State != "United States") |>
    arrange(desc(`Percent Change`)) |>
    mutate(`Growth Rank` = row_number()) |>
    arrange(desc(`Total Benefits - Per Pupil 2020`)) |>
    mutate(`2020 Rank` = row_number()) |>
    rename(
        `2020` = `Total Benefits - Per Pupil 2020`,
        `2002` = `Total Benefits - Per Pupil 2002`,
        `Growth` = `Percent Change`
    ) |>
    select(`Growth Rank`, `2020 Rank`, State, `2020`, `2002`, `Growth`) |>
    arrange(desc(`Growth`))


employee_benefits_table_us <- employee_benefits_table |>
    as.data.frame() |>
    filter(State == "United States") |>
    mutate(
        `Growth Rank` = " ",
        `2020 Rank` = " "
    ) |>
    rename(
        `2020` = `Total Benefits - Per Pupil 2020`,
        `2002` = `Total Benefits - Per Pupil 2002`,
        `Growth` = `Percent Change`
    ) |>
    select(`Growth Rank`, `2020 Rank`, State, `2020`, `2002`, `Growth`)


employee_benefits_table <- rbind(employee_benefits_table_us, employee_benefits_table_states)


employee_benefits_table <- employee_benefits_table |>
    # Round values
    mutate(`Growth` = round(`Growth`, 3))


employee_benefits_table |>
    filter(State == "United States")

# Calculate the the difference between 2020 and 2002

employee_benefits_table$`2020` - employee_benefits_table$`2002`



# Key Trend #5

intro_table_6


rev_table_ii <- ss_data_cpi |>
    filter(Year == 2020) |>
    select(State, `Total Revenue - Per Pupil`) |>
    left_join(
        ss_data_cpi |>
            filter(Year == 2002) |>
            select(State, `Total Revenue - Per Pupil`) |>
            mutate(
                `Total Revenue - Per Pupil` = `Total Revenue - Per Pupil`
            ),
        by = "State"
    ) |>
    mutate(`Total Revenue - Per Pupil 2020` = `Total Revenue - Per Pupil.x`) |>
    mutate(`Total Revenue - Per Pupil 2002` = `Total Revenue - Per Pupil.y`) |>
    mutate(
        `Percent Change` = `Total Revenue - Per Pupil 2020` / `Total Revenue - Per Pupil 2002` - 1
    ) |>
    select(State, `Total Revenue - Per Pupil 2020`, `Total Revenue - Per Pupil 2002`, `Percent Change`) |>
    filter(State == "United States")

rev_table_ii$`Total Revenue - Per Pupil 2020` - rev_table_ii$`Total Revenue - Per Pupil 2002`

rev_table_count <- ss_data_cpi |>
    filter(Year == 2020) |>
    select(State, `Total Revenue - Per Pupil`) |>
    left_join(
        ss_data_cpi |>
            filter(Year == 2002) |>
            select(State, `Total Revenue - Per Pupil`) |>
            mutate(
                `Total Revenue - Per Pupil` = `Total Revenue - Per Pupil`
            ),
        by = "State"
    ) |>
    mutate(`Total Revenue - Per Pupil 2020` = `Total Revenue - Per Pupil.x`) |>
    mutate(`Total Revenue - Per Pupil 2002` = `Total Revenue - Per Pupil.y`) |>
    mutate(
        `Percent Change` = `Total Revenue - Per Pupil 2020` / `Total Revenue - Per Pupil 2002` - 1
    ) |>
    select(State, `Total Revenue - Per Pupil 2020`, `Total Revenue - Per Pupil 2002`, `Percent Change`) |>
    mutate(greater_than_10pct = ifelse(`Percent Change` >= 0.1, 1, 0)) |>
    as.data.frame() |>
    summarise(
        `Number of States` = sum(greater_than_10pct)
    )


ss_data_cpi |>
    filter(Year == 2020) |>
    select(State, `Total Revenue - Per Pupil`) |>
    left_join(
        ss_data_cpi |>
            filter(Year == 2002) |>
            select(State, `Total Revenue - Per Pupil`) |>
            mutate(
                `Total Revenue - Per Pupil` = `Total Revenue - Per Pupil`
            ),
        by = "State"
    ) |>
    mutate(`Total Revenue - Per Pupil 2020` = `Total Revenue - Per Pupil.x`) |>
    mutate(`Total Revenue - Per Pupil 2002` = `Total Revenue - Per Pupil.y`) |>
    mutate(
        `Percent Change` = `Total Revenue - Per Pupil 2020` / `Total Revenue - Per Pupil 2002` - 1
    ) |>
    select(State, `Total Revenue - Per Pupil 2020`, `Total Revenue - Per Pupil 2002`, `Percent Change`) |>
    mutate(greater_than_50pct = ifelse(`Percent Change` > 0.5, 1, 0)) |>
    filter(greater_than_50pct == 1)


ss_data_cpi |>
    filter(Year == 2020) |>
    select(State, `Total Revenue - Per Pupil`) |>
    left_join(
        ss_data_cpi |>
            filter(Year == 2002) |>
            select(State, `Total Revenue - Per Pupil`) |>
            mutate(
                `Total Revenue - Per Pupil` = `Total Revenue - Per Pupil`
            ),
        by = "State"
    ) |>
    mutate(`Total Revenue - Per Pupil 2020` = `Total Revenue - Per Pupil.x`) |>
    mutate(`Total Revenue - Per Pupil 2002` = `Total Revenue - Per Pupil.y`) |>
    mutate(
        `Percent Change` = `Total Revenue - Per Pupil 2020` / `Total Revenue - Per Pupil 2002` - 1
    ) |>
    select(State, `Total Revenue - Per Pupil 2020`, `Total Revenue - Per Pupil 2002`, `Percent Change`) |>
    filter(`Total Revenue - Per Pupil 2020` > 20e3) |>
    arrange(desc(`Total Revenue - Per Pupil 2020`))


ss_data_cpi |>
    filter(Year == 2020) |>
    select(State, `Total Revenue - Per Pupil`) |>
    left_join(
        ss_data_cpi |>
            filter(Year == 2002) |>
            select(State, `Total Revenue - Per Pupil`) |>
            mutate(
                `Total Revenue - Per Pupil` = `Total Revenue - Per Pupil`
            ),
        by = "State"
    ) |>
    mutate(`Total Revenue - Per Pupil 2020` = `Total Revenue - Per Pupil.x`) |>
    mutate(`Total Revenue - Per Pupil 2002` = `Total Revenue - Per Pupil.y`) |>
    mutate(
        `Percent Change` = `Total Revenue - Per Pupil 2020` / `Total Revenue - Per Pupil 2002` - 1
    ) |>
    select(State, `Total Revenue - Per Pupil 2020`, `Total Revenue - Per Pupil 2002`, `Percent Change`) |>
    arrange(`Total Revenue - Per Pupil 2020`)



#   Support Services Expenditures
# Get `Support Services - Pupil Support Services - Per Pupil`,
# `Support Services - Instructional Staff - Per Pupil`,
# `Support Services - General Administration - Per Pupil`,
# `Support Services - School Administration - Per Pupil`,
# `Support Services - Operation & Maintenance - Per Pupil`,
# `Support Services - Pupil Transportation - Per Pupil`, and
# `Support Services - Other - Per Pupil` for 2002 and 2020
# for the United States from ss_data_cpi


support_services_us <- ss_data_cpi |>
    filter(Year == 2020) |>
    filter(State == "United States") |>
    select(
        State,
        `Support Services - Pupil Support Services - Per Pupil`,
        `Support Services - Instructional Staff - Per Pupil`,
        `Support Services - General Administration - Per Pupil`,
        `Support Services - School Administration - Per Pupil`,
        `Support Services - Operation & Maintenance - Per Pupil`,
        `Support Services - Pupil Transportation - Per Pupil`,
        `Support Services - Other - Per Pupil`
    ) |>
    mutate(`Total` = `Support Services - Pupil Support Services - Per Pupil` +
        `Support Services - Instructional Staff - Per Pupil` +
        `Support Services - General Administration - Per Pupil` +
        `Support Services - School Administration - Per Pupil` +
        `Support Services - Operation & Maintenance - Per Pupil` +
        `Support Services - Pupil Transportation - Per Pupil` +
        `Support Services - Other - Per Pupil`) |>
    left_join(
        ss_data_cpi |>
            filter(Year == 2002) |>
            filter(State == "United States") |>
            select(
                State,
                `Support Services - Pupil Support Services - Per Pupil`,
                `Support Services - Instructional Staff - Per Pupil`,
                `Support Services - General Administration - Per Pupil`,
                `Support Services - School Administration - Per Pupil`,
                `Support Services - Operation & Maintenance - Per Pupil`,
                `Support Services - Pupil Transportation - Per Pupil`,
                `Support Services - Other - Per Pupil`
            ) |>
            mutate(`Total` = `Support Services - Pupil Support Services - Per Pupil` +
                `Support Services - Instructional Staff - Per Pupil` +
                `Support Services - General Administration - Per Pupil` +
                `Support Services - School Administration - Per Pupil` +
                `Support Services - Operation & Maintenance - Per Pupil` +
                `Support Services - Pupil Transportation - Per Pupil` +
                `Support Services - Other - Per Pupil`),
        by = "State"
    )


support_services_us_pct <- support_services_us |>
    mutate(`Total Growth` = `Total.x` / `Total.y` - 1) |>
    mutate(`Pupil Support Services Growth` = `Support Services - Pupil Support Services - Per Pupil.x` / `Support Services - Pupil Support Services - Per Pupil.y` - 1) |>
    mutate(`Instructional Staff Growth` = `Support Services - Instructional Staff - Per Pupil.x` / `Support Services - Instructional Staff - Per Pupil.y` - 1) |>
    mutate(`General Administration Growth` = `Support Services - General Administration - Per Pupil.x` / `Support Services - General Administration - Per Pupil.y` - 1) |>
    mutate(`School Administration Growth` = `Support Services - School Administration - Per Pupil.x` / `Support Services - School Administration - Per Pupil.y` - 1) |>
    mutate(`Operation & Maintenance Growth` = `Support Services - Operation & Maintenance - Per Pupil.x` / `Support Services - Operation & Maintenance - Per Pupil.y` - 1) |>
    mutate(`Pupil Transportation Growth` = `Support Services - Pupil Transportation - Per Pupil.x` / `Support Services - Pupil Transportation - Per Pupil.y` - 1) |>
    mutate(`Other Growth` = `Support Services - Other - Per Pupil.x` / `Support Services - Other - Per Pupil.y` - 1) |>
    select(
        State,
        `Total Growth`,
        `Pupil Support Services Growth`,
        `Instructional Staff Growth`,
        `General Administration Growth`,
        `School Administration Growth`,
        `Operation & Maintenance Growth`,
        `Pupil Transportation Growth`,
        `Other Growth`
    )


support_serivces_us_diff <- support_services_us |>
    mutate(`Total Diff` = `Total.x` - `Total.y`) |>
    mutate(`Pupil Support Services Diff` = `Support Services - Pupil Support Services - Per Pupil.x` - `Support Services - Pupil Support Services - Per Pupil.y`) |>
    mutate(`Instructional Staff Diff` = `Support Services - Instructional Staff - Per Pupil.x` - `Support Services - Instructional Staff - Per Pupil.y`) |>
    mutate(`General Administration Diff` = `Support Services - General Administration - Per Pupil.x` - `Support Services - General Administration - Per Pupil.y`) |>
    mutate(`School Administration Diff` = `Support Services - School Administration - Per Pupil.x` - `Support Services - School Administration - Per Pupil.y`) |>
    mutate(`Operation & Maintenance Diff` = `Support Services - Operation & Maintenance - Per Pupil.x` - `Support Services - Operation & Maintenance - Per Pupil.y`) |>
    mutate(`Pupil Transportation Diff` = `Support Services - Pupil Transportation - Per Pupil.x` - `Support Services - Pupil Transportation - Per Pupil.y`) |>
    mutate(`Other Diff` = `Support Services - Other - Per Pupil.x` - `Support Services - Other - Per Pupil.y`) |>
    select(
        State,
        `Total Diff`,
        `Pupil Support Services Diff`,
        `Instructional Staff Diff`,
        `General Administration Diff`,
        `School Administration Diff`,
        `Operation & Maintenance Diff`,
        `Pupil Transportation Diff`,
        `Other Diff`
    )


round(0.2535084, 3)

support_services_us_pct |>
  t()

ss_data_cpi |>
    filter(Year == 2020) |>
    select(
        State,
        `Support Services - Pupil Support Services - Per Pupil`,
        `Support Services - Instructional Staff - Per Pupil`,
        `Support Services - General Administration - Per Pupil`,
        `Support Services - School Administration - Per Pupil`,
        `Support Services - Operation & Maintenance - Per Pupil`,
        `Support Services - Pupil Transportation - Per Pupil`,
        `Support Services - Other - Per Pupil`
    ) |>
    mutate(`Total` = `Support Services - Pupil Support Services - Per Pupil` +
        `Support Services - Instructional Staff - Per Pupil` +
        `Support Services - General Administration - Per Pupil` +
        `Support Services - School Administration - Per Pupil` +
        `Support Services - Operation & Maintenance - Per Pupil` +
        `Support Services - Pupil Transportation - Per Pupil` +
        `Support Services - Other - Per Pupil`) |>
    left_join(
        ss_data_cpi |>
            filter(Year == 2002) |>
            select(
                State,
                `Support Services - Pupil Support Services - Per Pupil`,
                `Support Services - Instructional Staff - Per Pupil`,
                `Support Services - General Administration - Per Pupil`,
                `Support Services - School Administration - Per Pupil`,
                `Support Services - Operation & Maintenance - Per Pupil`,
                `Support Services - Pupil Transportation - Per Pupil`,
                `Support Services - Other - Per Pupil`
            ) |>
            mutate(`Total` = `Support Services - Pupil Support Services - Per Pupil` +
                `Support Services - Instructional Staff - Per Pupil` +
                `Support Services - General Administration - Per Pupil` +
                `Support Services - School Administration - Per Pupil` +
                `Support Services - Operation & Maintenance - Per Pupil` +
                `Support Services - Pupil Transportation - Per Pupil` +
                `Support Services - Other - Per Pupil`),
        by = "State"
    ) |>
    mutate(`Total Growth` = `Total.x` / `Total.y` - 1) |>
    filter(`Total Growth` >= 0.5)


ss_data_cpi |>
    filter(Year == 2020) |>
    select(
        State,
        `Support Services - Pupil Support Services - Per Pupil`,
        `Support Services - Instructional Staff - Per Pupil`,
        `Support Services - General Administration - Per Pupil`,
        `Support Services - School Administration - Per Pupil`,
        `Support Services - Operation & Maintenance - Per Pupil`,
        `Support Services - Pupil Transportation - Per Pupil`,
        `Support Services - Other - Per Pupil`
    ) |>
    mutate(`Total` = `Support Services - Pupil Support Services - Per Pupil` +
        `Support Services - Instructional Staff - Per Pupil` +
        `Support Services - General Administration - Per Pupil` +
        `Support Services - School Administration - Per Pupil` +
        `Support Services - Operation & Maintenance - Per Pupil` +
        `Support Services - Pupil Transportation - Per Pupil` +
        `Support Services - Other - Per Pupil`) |>
    left_join(
        ss_data_cpi |>
            filter(Year == 2002) |>
            select(
                State,
                `Support Services - Pupil Support Services - Per Pupil`,
                `Support Services - Instructional Staff - Per Pupil`,
                `Support Services - General Administration - Per Pupil`,
                `Support Services - School Administration - Per Pupil`,
                `Support Services - Operation & Maintenance - Per Pupil`,
                `Support Services - Pupil Transportation - Per Pupil`,
                `Support Services - Other - Per Pupil`
            ) |>
            mutate(`Total` = `Support Services - Pupil Support Services - Per Pupil` +
                `Support Services - Instructional Staff - Per Pupil` +
                `Support Services - General Administration - Per Pupil` +
                `Support Services - School Administration - Per Pupil` +
                `Support Services - Operation & Maintenance - Per Pupil` +
                `Support Services - Pupil Transportation - Per Pupil` +
                `Support Services - Other - Per Pupil`),
        by = "State"
    ) |>
    mutate(`Total Growth` = `Total.x` / `Total.y` - 1) |>
    filter(`Total Growth` >= 0.6)


ss_data_cpi |>
    filter(Year == 2020) |>
    select(
        State,
        `Support Services - Pupil Support Services - Per Pupil`,
        `Support Services - Instructional Staff - Per Pupil`,
        `Support Services - General Administration - Per Pupil`,
        `Support Services - School Administration - Per Pupil`,
        `Support Services - Operation & Maintenance - Per Pupil`,
        `Support Services - Pupil Transportation - Per Pupil`,
        `Support Services - Other - Per Pupil`
    ) |>
    mutate(`Total` = `Support Services - Pupil Support Services - Per Pupil` +
        `Support Services - Instructional Staff - Per Pupil` +
        `Support Services - General Administration - Per Pupil` +
        `Support Services - School Administration - Per Pupil` +
        `Support Services - Operation & Maintenance - Per Pupil` +
        `Support Services - Pupil Transportation - Per Pupil` +
        `Support Services - Other - Per Pupil`) |>
    left_join(
        ss_data_cpi |>
            filter(Year == 2002) |>
            select(
                State,
                `Support Services - Pupil Support Services - Per Pupil`,
                `Support Services - Instructional Staff - Per Pupil`,
                `Support Services - General Administration - Per Pupil`,
                `Support Services - School Administration - Per Pupil`,
                `Support Services - Operation & Maintenance - Per Pupil`,
                `Support Services - Pupil Transportation - Per Pupil`,
                `Support Services - Other - Per Pupil`
            ) |>
            mutate(`Total` = `Support Services - Pupil Support Services - Per Pupil` +
                `Support Services - Instructional Staff - Per Pupil` +
                `Support Services - General Administration - Per Pupil` +
                `Support Services - School Administration - Per Pupil` +
                `Support Services - Operation & Maintenance - Per Pupil` +
                `Support Services - Pupil Transportation - Per Pupil` +
                `Support Services - Other - Per Pupil`),
        by = "State"
    ) |>
    mutate(`Total Growth` = `Total.x` / `Total.y` - 1) |>
    filter(`Total Growth` <= 0.1)




ss_data_cpi |>
    filter(Year == 2020) |>
    select(
        State,
        `Support Services - Pupil Support Services - Per Pupil`,
        `Support Services - Instructional Staff - Per Pupil`,
        `Support Services - General Administration - Per Pupil`,
        `Support Services - School Administration - Per Pupil`,
        `Support Services - Operation & Maintenance - Per Pupil`,
        `Support Services - Pupil Transportation - Per Pupil`,
        `Support Services - Other - Per Pupil`
    ) |>
    mutate(`Total` = `Support Services - Pupil Support Services - Per Pupil` +
        `Support Services - Instructional Staff - Per Pupil` +
        `Support Services - General Administration - Per Pupil` +
        `Support Services - School Administration - Per Pupil` +
        `Support Services - Operation & Maintenance - Per Pupil` +
        `Support Services - Pupil Transportation - Per Pupil` +
        `Support Services - Other - Per Pupil`) |>
    left_join(
        ss_data_cpi |>
            filter(Year == 2002) |>
            select(
                State,
                `Support Services - Pupil Support Services - Per Pupil`,
                `Support Services - Instructional Staff - Per Pupil`,
                `Support Services - General Administration - Per Pupil`,
                `Support Services - School Administration - Per Pupil`,
                `Support Services - Operation & Maintenance - Per Pupil`,
                `Support Services - Pupil Transportation - Per Pupil`,
                `Support Services - Other - Per Pupil`
            ) |>
            mutate(`Total` = `Support Services - Pupil Support Services - Per Pupil` +
                `Support Services - Instructional Staff - Per Pupil` +
                `Support Services - General Administration - Per Pupil` +
                `Support Services - School Administration - Per Pupil` +
                `Support Services - Operation & Maintenance - Per Pupil` +
                `Support Services - Pupil Transportation - Per Pupil` +
                `Support Services - Other - Per Pupil`),
        by = "State"
    ) |>
    mutate(`Total Growth` = `Total.x` / `Total.y` - 1) |>
    filter(`Total Growth` <= 0)



support_services_us_pct |>
    t()


support_services_states <- ss_data_cpi |>
    filter(Year == 2020) |>
    select(
        State,
        `Support Services - Pupil Support Services - Per Pupil`,
        `Support Services - Instructional Staff - Per Pupil`,
        `Support Services - General Administration - Per Pupil`,
        `Support Services - School Administration - Per Pupil`,
        `Support Services - Operation & Maintenance - Per Pupil`,
        `Support Services - Pupil Transportation - Per Pupil`,
        `Support Services - Other - Per Pupil`
    ) |>
    mutate(`Total` = `Support Services - Pupil Support Services - Per Pupil` +
        `Support Services - Instructional Staff - Per Pupil` +
        `Support Services - General Administration - Per Pupil` +
        `Support Services - School Administration - Per Pupil` +
        `Support Services - Operation & Maintenance - Per Pupil` +
        `Support Services - Pupil Transportation - Per Pupil` +
        `Support Services - Other - Per Pupil`) |>
    left_join(
        ss_data_cpi |>
            filter(Year == 2002) |>
            select(
                State,
                `Support Services - Pupil Support Services - Per Pupil`,
                `Support Services - Instructional Staff - Per Pupil`,
                `Support Services - General Administration - Per Pupil`,
                `Support Services - School Administration - Per Pupil`,
                `Support Services - Operation & Maintenance - Per Pupil`,
                `Support Services - Pupil Transportation - Per Pupil`,
                `Support Services - Other - Per Pupil`
            ) |>
            mutate(`Total` = `Support Services - Pupil Support Services - Per Pupil` +
                `Support Services - Instructional Staff - Per Pupil` +
                `Support Services - General Administration - Per Pupil` +
                `Support Services - School Administration - Per Pupil` +
                `Support Services - Operation & Maintenance - Per Pupil` +
                `Support Services - Pupil Transportation - Per Pupil` +
                `Support Services - Other - Per Pupil`),
        by = "State"
    ) |>
    mutate(`Total Growth` = `Total.x` / `Total.y` - 1) |>
    mutate(`Pupil Support Services Growth` = `Support Services - Pupil Support Services - Per Pupil.x` / `Support Services - Pupil Support Services - Per Pupil.y` - 1) |>
    mutate(`Instructional Staff Growth` = `Support Services - Instructional Staff - Per Pupil.x` / `Support Services - Instructional Staff - Per Pupil.y` - 1) |>
    mutate(`General Administration Growth` = `Support Services - General Administration - Per Pupil.x` / `Support Services - General Administration - Per Pupil.y` - 1) |>
    mutate(`School Administration Growth` = `Support Services - School Administration - Per Pupil.x` / `Support Services - School Administration - Per Pupil.y` - 1) |>
    mutate(`Operation & Maintenance Growth` = `Support Services - Operation & Maintenance - Per Pupil.x` / `Support Services - Operation & Maintenance - Per Pupil.y` - 1) |>
    mutate(`Pupil Transportation Growth` = `Support Services - Pupil Transportation - Per Pupil.x` / `Support Services - Pupil Transportation - Per Pupil.y` - 1) |>
    mutate(`Other Growth` = `Support Services - Other - Per Pupil.x` / `Support Services - Other - Per Pupil.y` - 1) |>
    mutate(`Total Diff` = `Total.x` - `Total.y`) |>
    mutate(`Pupil Support Services Diff` = `Support Services - Pupil Support Services - Per Pupil.x` - `Support Services - Pupil Support Services - Per Pupil.y`) |>
    mutate(`Instructional Staff Diff` = `Support Services - Instructional Staff - Per Pupil.x` - `Support Services - Instructional Staff - Per Pupil.y`) |>
    mutate(`General Administration Diff` = `Support Services - General Administration - Per Pupil.x` - `Support Services - General Administration - Per Pupil.y`) |>
    mutate(`School Administration Diff` = `Support Services - School Administration - Per Pupil.x` - `Support Services - School Administration - Per Pupil.y`) |>
    mutate(`Operation & Maintenance Diff` = `Support Services - Operation & Maintenance - Per Pupil.x` - `Support Services - Operation & Maintenance - Per Pupil.y`) |>
    mutate(`Pupil Transportation Diff` = `Support Services - Pupil Transportation - Per Pupil.x` - `Support Services - Pupil Transportation - Per Pupil.y`) |>
    mutate(`Other Diff` = `Support Services - Other - Per Pupil.x` - `Support Services - Other - Per Pupil.y`) |>
    # 2020 values
    mutate(`Total 2020` = `Total.x`) |>
    mutate(`Pupil Support Services 2020` = `Support Services - Pupil Support Services - Per Pupil.x`) |>
    mutate(`Instructional Staff 2020` = `Support Services - Instructional Staff - Per Pupil.x`) |>
    mutate(`General Administration 2020` = `Support Services - General Administration - Per Pupil.x`) |>
    mutate(`School Administration 2020` = `Support Services - School Administration - Per Pupil.x`) |>
    mutate(`Operation & Maintenance 2020` = `Support Services - Operation & Maintenance - Per Pupil.x`) |>
    mutate(`Pupil Transportation 2020` = `Support Services - Pupil Transportation - Per Pupil.x`) |>
    mutate(`Other 2020` = `Support Services - Other - Per Pupil.x`) |>
    select(
        State,
        `Total 2020`,
        `Pupil Support Services 2020`,
        `Instructional Staff 2020`,
        `General Administration 2020`,
        `School Administration 2020`,
        `Operation & Maintenance 2020`,
        `Pupil Transportation 2020`,
        `Other 2020`,
        `Total Growth`,
        `Pupil Support Services Growth`,
        `Instructional Staff Growth`,
        `General Administration Growth`,
        `School Administration Growth`,
        `Operation & Maintenance Growth`,
        `Pupil Transportation Growth`,
        `Other Growth`,
        `Total Diff`,
        `Pupil Support Services Diff`,
        `Instructional Staff Diff`,
        `General Administration Diff`,
        `School Administration Diff`,
        `Operation & Maintenance Diff`,
        `Pupil Transportation Diff`,
        `Other Diff`
    )


support_services_pupil_support_services <- support_services_states |>
    arrange(desc(`Pupil Support Services 2020`))



# Get `All Instruction - Per Pupil` data from ss_data_cpi for 2002 and 2020

instructional_states <- ss_data_cpi |>
    filter(Year == 2020) |>
    select(
        State,
        `All Instruction - Per Pupil`
    ) |>
    left_join(
        ss_data_cpi |>
            filter(Year == 2002) |>
            select(
                State,
                `All Instruction - Per Pupil`
            ),
        by = "State"
    ) |>
    rename(`Instructional 2020` = `All Instruction - Per Pupil.x`) |>
    rename(`Instructional 2002` = `All Instruction - Per Pupil.y`) |>
    mutate(`Instructional Growth` = `Instructional 2020` / `Instructional 2002` - 1)


# Get `Instructional Growth` data from instructional_states for 2002 and 2020

instructional_states_us <- instructional_states |>
    filter(State == "United States")



# Employee Benefits

employee_benefits_table_us <- employee_benefits_table |>
    filter(State == "United States")


employee_benefits_table_us$`2020` - employee_benefits_table_us$`2002`

employee_benefits_table_us


employee_benefits_table |>
    filter(State != "United States") |>
    filter(Growth > 1)


employee_benefits_table |>
    filter(Growth < 0.2)


# Capital Outlay

capital_outlay_table_us <- capital_outlay_table |>
    filter(State == "United States")

capital_outlay_table_us$`2020` - capital_outlay_table_us$`2002`

capital_outlay_table_us$Growth


# Total Debt

total_debt_table_us <- total_debt_table |>
    filter(State == "United States")

total_debt_table_us$`2020` - total_debt_table_us$`2002`

total_debt_table_us$Growth

total_debt_table |>
    filter(Growth > 2)



# Enrollment

enrollment_table_us <- enrollment_table |>
    filter(State == "United States")

declining_enrollment <- enrollment_table |>
    filter(State != "United States") |>
    filter(Growth < 0)

# Sum 2020 enrollment for California, Florida, and Texas

declining_enrollment |>
    filter(State == "California" | State == "Florida" | State == "Texas") |>
    group_by(State) |>
    summarise(`2020 Enrollment` = sum(`2020`))




# Complete Staffing

complete_staffing_list <- complete_staffing |>
    as.data.frame() |>
    # pivot wider
    mutate(
        `Growth` = round(`2020` / `2002` - 1, 3)
    )


# Staffing

staffing_trends_states <- staffing_trends |>
    select(State, `Non-Teachers Growth`)

staffing_trends_states_25pct <- staffing_trends_states |>
    mutate(`Non-Teachers Growth Count` = ifelse(`Non-Teachers Growth` > 0.25, 1, 0))

staffing_trends_states_25pct |>
    summarise(
        `Non-Teachers Growth Count` = sum(`Non-Teachers Growth Count`)
    )


staffing_trends |>
    as.data.frame() |>
    filter(State != "United States") |>
    summarise(
        `Non-Teachers 2020` = sum(`Non-Teachers 2020`)
    )


staffing_trends |>
    as.data.frame() |>
    filter(State == "United States")


staffing_trends |>
    filter(State == "United States") |>
    select(State, `Teachers 2020`, `Teachers Growth`)


teacher_reductions <- staffing_trends |>
    filter(State != "United States") |>
    filter(`Teachers Growth` < 0) |>
    filter(`Non-Teachers Growth` < 0)




# Get the percentage of non-teachers from `All staff` from the United States in 2020

staffing_trends |>
    filter(State == "United States") |>
    select(`Non-Teachers 2020`, `All staff 2020`) |>
    mutate(`Non-Teachers %` = `Non-Teachers 2020` / `All staff 2020` * 100) |>
    select(`Non-Teachers %`)



# Average Teacher Salary Growth

salary_table_us <- salary_table |>
    filter(State == "United States")


salary_delcine_states <- salary_table |>
    filter(State != "United States") |>
    filter(`Percent Change` < 0) |>
    arrange(`Percent Change`)


salary_table |>
    arrange(desc(`Percent Change`))



# 4th Grade Reading NAEP Scores
naep_reading_4_states <- naep_reading_4 |>
    filter(Year == 2019) |>
    # filter(State != "United States") |>
    select(State, `NAEP`) |>
    left_join(
        naep_reading_4 |>
            filter(Year == 2003) |>
            select(State, NAEP),
        by = "State"
    ) |>
    mutate(`Reading 4 Score Pct` = `NAEP.x` / `NAEP.y` - 1) |>
    mutate(`Reading 4 Score Diff` = `NAEP.x` - `NAEP.y`) |>
    select(State, `NAEP.x`, `NAEP.y`, `Reading 4 Score Pct`, `Reading 4 Score Diff`) |>
    rename(`Reading Score 2019` = `NAEP.x`, `Reading Score 2003` = `NAEP.y`)

# 4th Grade Math NAEP Scores
naep_math_4_states <- naep_math_4 |>
    filter(Year == 2019) |>
    # filter(State != "United States") |>
    select(State, `NAEP`) |>
    left_join(
        naep_math_4 |>
            filter(Year == 2003) |>
            select(State, NAEP),
        by = "State"
    ) |>
    mutate(`Math 4 Score Pct` = `NAEP.x` / `NAEP.y` - 1) |>
    mutate(`Math 4 Score Diff` = `NAEP.x` - `NAEP.y`) |>
    select(State, `NAEP.x`, `NAEP.y`, `Math 4 Score Pct`, `Math 4 Score Diff`) |>
    rename(`Math Score 2019` = `NAEP.x`, `Math Score 2003` = `NAEP.y`)


# 8th Grade Reading NAEP Scores
naep_reading_8_states <- naep_reading_8 |>
    filter(Year == 2019) |>
    # filter(State != "United States") |>
    select(State, `NAEP`) |>
    left_join(
        naep_reading_8 |>
            filter(Year == 2003) |>
            select(State, NAEP),
        by = "State"
    ) |>
    mutate(`Reading 8 Score Pct` = `NAEP.x` / `NAEP.y` - 1) |>
    mutate(`Reading 8 Score Diff` = `NAEP.x` - `NAEP.y`) |>
    select(State, `NAEP.x`, `NAEP.y`, `Reading 8 Score Pct`, `Reading 8 Score Diff`) |>
    rename(`Reading Score 2019` = `NAEP.x`, `Reading Score 2003` = `NAEP.y`)

# 8th Grade Math NAEP Scores
naep_math_8_states <- naep_math_8 |>
    filter(Year == 2019) |>
    # filter(State != "United States") |>
    select(State, `NAEP`) |>
    left_join(
        naep_math_8 |>
            filter(Year == 2003) |>
            select(State, NAEP),
        by = "State"
    ) |>
    mutate(`Math 8 Score Pct` = `NAEP.x` / `NAEP.y` - 1) |>
    mutate(`Math 8 Score Diff` = `NAEP.x` - `NAEP.y`) |>
    select(State, `NAEP.x`, `NAEP.y`, `Math 8 Score Pct`, `Math 8 Score Diff`) |>
    rename(`Math Score 2019` = `NAEP.x`, `Math Score 2003` = `NAEP.y`)



# 4th Grade Low Income Reading NAEP Scores
li_naep_reading_4_states <- low_income_naep_reading_4 |>
    filter(Year == 2019) |>
    filter(State != "District of Columbia") |>
    filter(State != "Puerto Rico") |>
    select(State, `Eligible`) |>
    left_join(
        low_income_naep_reading_4 |>
            filter(Year == 2003) |>
            select(State, Eligible),
        by = "State"
    ) |>
    mutate(`Reading Score Diff` = `Eligible.x` - `Eligible.y`) |>
    select(State, `Eligible.x`, `Eligible.y`, `Reading Score Diff`) |>
    rename(`Reading Score 2019` = `Eligible.x`, `Reading Score 2003` = `Eligible.y`) |>
    arrange(desc(`Reading Score Diff`))



# 4th Grade Low Inocme Math NAEP Scores
li_naep_math_4_states <- low_income_naep_math_4 |>
    filter(Year == 2019) |>
    filter(State != "District of Columbia") |>
    filter(State != "Puerto Rico") |>
    select(State, `Eligible`) |>
    left_join(
        low_income_naep_math_4 |>
            filter(Year == 2003) |>
            select(State, Eligible),
        by = "State"
    ) |>
    mutate(`Math Score Diff` = `Eligible.x` - `Eligible.y`) |>
    select(State, `Eligible.x`, `Eligible.y`, `Math Score Diff`) |>
    rename(`Math Score 2019` = `Eligible.x`, `Math Score 2003` = `Eligible.y`) |>
    arrange(desc(`Math Score Diff`))



# 8th Grade Low Income Reading NAEP Scores
li_naep_reading_8_states <- low_income_naep_reading_8 |>
    filter(Year == 2019) |>
    filter(State != "District of Columbia") |>
    filter(State != "Puerto Rico") |>
    select(State, `Eligible`) |>
    left_join(
        low_income_naep_reading_8 |>
            filter(Year == 2003) |>
            select(State, Eligible),
        by = "State"
    ) |>
    mutate(`Reading Score Diff` = `Eligible.x` - `Eligible.y`) |>
    select(State, `Eligible.x`, `Eligible.y`, `Reading Score Diff`) |>
    rename(`Reading Score 2019` = `Eligible.x`, `Reading Score 2003` = `Eligible.y`) |>
    arrange(desc(`Reading Score Diff`))


# 8th Grade Low Income Math NAEP Scores
li_naep_math_8_states <- low_income_naep_math_8 |>
    filter(Year == 2019) |>
    filter(State != "District of Columbia") |>
    filter(State != "Puerto Rico") |>
    select(State, `Eligible`) |>
    left_join(
        low_income_naep_math_8 |>
            filter(Year == 2003) |>
            select(State, Eligible),
        by = "State"
    ) |>
    mutate(`Math Score Diff` = `Eligible.x` - `Eligible.y`) |>
    select(State, `Eligible.x`, `Eligible.y`, `Math Score Diff`) |>
    rename(`Math Score 2019` = `Eligible.x`, `Math Score 2003` = `Eligible.y`) |>
    arrange(desc(`Math Score Diff`))
