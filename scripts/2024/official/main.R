cli_rule(left = "Generating tabulation for {area}")
ts <- list()
area_name_overall <- toupper(area_overall)


# 1.1
cli_alert_info("Generating PWD Living Alone")
ts$ts_pwd_living_alone <- data$cbms_person_record |>
  left_join(
    data$cbms_household_record |> select(uuid, hh_size),
    by = "uuid"
  ) |>
  group_by(barangay, uuid) |>
  summarise(
    people_in_hh = n(),
    has_pwd = any(b10_pwd == 1, na.rm = TRUE),
    is_pwd_alone = any(b10_pwd == 1 & hh_size == 1, na.rm = TRUE),
    .groups = "drop"
  ) |>
  group_by(barangay) |>
  summarise(
    total_population = sum(people_in_hh),
    pwd_alone_count = sum(is_pwd_alone, na.rm = TRUE),
    total_households = n(),
    hh_with_pwd = sum(has_pwd, na.rm = TRUE)
  ) |>
  mutate(
    proportion_pct_alone = round((pwd_alone_count / total_population) * 100, 2),
    pct_hh_with_pwd = round((hh_with_pwd / total_households) * 100, 2),
    pct_hh_without_pwd = round(100 - pct_hh_with_pwd, 2)
  ) |>

  rename(
    `Barangay` = barangay,
    `Total Population` = total_population,
    `PWDs Living Alone` = pwd_alone_count,
    `Total Households` = total_households,
    `Households with PWD` = hh_with_pwd,
    `PWD Alone (%)` = proportion_pct_alone,
    `HH with PWD (%)` = pct_hh_with_pwd,
    `HH without PWD (%)` = pct_hh_without_pwd
  ) |>
  arrange(desc(`Households with PWD`))



# 1.2. Create the base analysis dataframe
cli_alert_info("Generating Proportion of households with access to basic drinking water services")
ts$ts_sdg_1_4_1_5_p1 <- data$cbms_household_record |>
  # 1. Categorize the Service Level
  mutate(
    water_access_status = case_when(
      n03_service_level_drinking_water %in% c(1, 2) ~ "Basic/Safely Managed",
      n03_service_level_drinking_water %in% c(3, 4, 5) ~ "Limited/Unimproved",
      TRUE ~ NA_character_
    )
  ) |>

  filter(!is.na(water_access_status)) |>


  group_by(barangay) |>
  summarise(
    total_hh = n(),
    hh_with_access = sum(water_access_status == "Basic/Safely Managed"),
    hh_without_access = sum(water_access_status == "Limited/Unimproved"),
    .groups = "drop"
  ) |>


  mutate(
    access_Gap = round((hh_without_access / total_hh) * 100, 2)
  ) |>

  rename(
    `Barangay` = barangay,
    `Total Households` = total_hh,
    `Access: Basic/Safely Managed` = hh_with_access,
    `No Basic Access (Vulnerable)` = hh_without_access,
    `Access Gap (%)` = access_Gap
  ) |>

  arrange(desc(`Access Gap (%)`))



# 2.1
cli_alert_info("Generating Barangay senior stats")
ts$ts_unregistered_seniors <- data$cbms_person_record |>

  filter(a05_age >= 60) |>


  mutate(
    no_senior_id = (b07_senior_citizen_id == 2),
    male_no_id   = (no_senior_id & a03_sex == 1),
    female_no_id = (no_senior_id & a03_sex == 2)
  ) |>

  # 3. Group and Summarise by Barangay
  group_by(barangay) |>
  summarise(
    total_seniors = n(),
    seniors_without_id = sum(no_senior_id, na.rm = TRUE),
    male_no_id_count = sum(male_no_id, na.rm = TRUE),
    female_no_id_count = sum(female_no_id, na.rm = TRUE),
    .groups = "drop"
  ) |>


  mutate(
    exclusion_rate = round((seniors_without_id / total_seniors) * 100, 2),
    registration_rate = 100 - exclusion_rate # Forces the math to 100.00%
  ) |>

  rename(
    `Barangay` = barangay,
    `Total Seniors (60+)` = total_seniors,
    `Unregistered (No ID)` = seniors_without_id,
    `Male (No ID)` = male_no_id_count,
    `Female (No ID)` = female_no_id_count,
    `ID Coverage Gap (%)` = exclusion_rate,
    `Registration Rate (%)` = registration_rate
  ) |>

  # Sort by highest number of unregistered seniors
  arrange(desc(`Unregistered (No ID)`))


# print(ts$ts_unregistered_seniors)

# 2.2
cli_alert_info("Generating Registration demographics")
ts$ts_phil_id_demographics <- data$cbms_person_record |>

  mutate(
    has_phil_id = if_else(b03_phil_id == 1, 1L, 0L),
    sex_label = case_when(
      a03_sex == 1 ~ "Male",
      a03_sex == 2 ~ "Female",
      TRUE ~ "Unknown"
    ),

    custom_age_group = case_when(
      a05_age <= 4  ~ "1 0-4 (Early Childhood)",
      a05_age <= 14 ~ "2 5-14 (School Age)",
      a05_age <= 24 ~ "3 15-24 (Youth)",
      a05_age <= 59 ~ "4 25-59 (Working Age)",
      a05_age >= 60 ~ "5 60+ (Senior Citizen)",
      TRUE ~ NA_character_
    )
  ) |>
  filter(!is.na(custom_age_group), sex_label != "Unknown") |>


  group_by(custom_age_group, sex_label) |>
  summarise(
    total_persons = n(),
    registered_with_id = sum(has_phil_id, na.rm = TRUE),
    .groups = "drop"
  ) |>


  mutate(
    penetration_rate_pct = round((registered_with_id / total_persons) * 100, 2)
  ) |>

  pivot_wider(
    names_from = sex_label,
    values_from = c(total_persons, registered_with_id, penetration_rate_pct),
    names_glue = "{sex_label}_{.value}"
  ) |>

  # Renaming and Selection of columns
  select(
    `Age Group` = custom_age_group,

    # Male Columns
    `Male: Total` = Male_total_persons,
    `Male: Registered` = Male_registered_with_id,
    `Male: Reg. Rate (%)` = Male_penetration_rate_pct,

    # Female Columns
    `Female: Total` = Female_total_persons,
    `Female: Registered` = Female_registered_with_id,
    `Female: Reg. Rate (%)` = Female_penetration_rate_pct
  ) |>

  # Sort by Age Group
  arrange(`Age Group`)


# print(ts$ts_phil_id_demographics)



# 3
cli_alert_info("Generating Assessment of 4Ps Program Coverage")
ts$ts_4ps_exclusion_analysis <- data$cbms_person_record |>

  distinct(barangay, uuid) |>


  left_join(
    data$cbms_household_record |>
      select(uuid, overcrowding_status, m05_a_4ps),
      by = "uuid"
  ) |>

  mutate(
    # Check if household is overcrowded (1 means Yes/Overcrowded)
    is_poor = if_else(overcrowding_status == 1, TRUE, FALSE),

    # Check if household is in 4Ps (1 means Yes)
    is_4ps = if_else(m05_a_4ps == 1, TRUE, FALSE),

    # THE CRITICAL METRICS:
    # 1. Covered Poor: They are overcrowded AND receiving 4Ps
    covered_poor = if_else(is_poor == TRUE & is_4ps == TRUE, 1L, 0L),

    # 2. Left Behind: They are overcrowded but NOT receiving 4Ps
    left_behind_poor = if_else(is_poor == TRUE & is_4ps == FALSE, 1L, 0L)
  ) |>

  # 4. Group by Barangay and Tabulate
  group_by(barangay) |>
  summarise(
    total_households = n(),
    total_poor_hh = sum(is_poor, na.rm = TRUE),
    hh_poor_and_4ps = sum(covered_poor, na.rm = TRUE),
    hh_poor_no_4ps = sum(left_behind_poor, na.rm = TRUE)
  ) |>

  # 5. Calculate the "Exclusion Rate"
  mutate(
    # What percentage of the poor in this barangay are NOT getting 4Ps?
    exclusion_rate_pct = if_else(
      total_poor_hh > 0,
      round((hh_poor_no_4ps / total_poor_hh) * 100, 2),
      0
    )
  ) |>

  rename(
    `Barangay` = barangay,
    `Total HH` = total_households,
    `Vulnerable HH (Overcrowded)` = total_poor_hh,
    `4Ps Beneficiaries` = hh_poor_and_4ps,
    `Uncovered Vulnerable HH` = hh_poor_no_4ps,
    `Exclusion Gap (%)` = exclusion_rate_pct,
  ) |>
  # 6. Sort to show the barangays with the most "Left Behind" households at the top
  arrange(desc(`Uncovered Vulnerable HH`))


# print(ts$ts_4ps_exclusion_analysis)

#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#
#Don't changes the codes after this comment
#Don't changes the codes after this comment
#Don't changes the codes after this comment
#Don't changes the codes after this comment


meta <- get_rcdf_metadata(path_input_data[1], name = 'meta')
ts <- lapply(ts, add_source_note, source_note = meta$source_note)


ts_filename_2 <- file.path(
  path_output,
  glue::glue('{config$cbms_round} CBMS Tabulation - {area_overall} ({Sys.Date()}).xlsx')
)

cli_rule(left = "Generating output files")


table_refs <- read.xlsx("references/2024-cbms-tabulation-specs2.xlsx")

generate_output(
  data = ts,
  path = ts_filename_2,
  include_table_list = TRUE,
  table_list_reference = table_refs,
  format = "xlsx"
)

cli_text("Suc♥️cess")
