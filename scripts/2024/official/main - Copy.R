cli_rule(left = "Generating tabulation for {area}")
ts <- list()
area_name_overall <- toupper(area_overall)

#Put your codes after this comments
#Put your codes after this comments
#Put your codes after this comments
#Put your codes after this comments
#Put your codes after this comments

#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#
# Summary stats
cli_alert_info("Generating summary statistics")
ts$ts_summary_stats <- data$cbms_household_record |>
  summarise(
    barangay = area_name_overall,
    total_households = n_distinct(uuid),
    average_household_size = sum(hh_size, na.rm = T) / n(),
    average_number_of_nuclear_families = sum(number_of_nuclear_families, na.rm = T) / n_distinct(uuid),
    total_number_of_males = sum(number_of_males, na.rm = T),
    total_number_of_females = sum(number_of_females, na.rm = T),
    total_number_of_household_members = sum(hh_size, na.rm = T),
    .groups = 'drop'
  ) |>
  dplyr::bind_rows(
    data$cbms_household_record |>
      group_by(barangay) |>
      summarise(
        total_households = n_distinct(uuid),
        average_household_size = sum(hh_size, na.rm = T) / n(),
        average_number_of_nuclear_families = sum(number_of_nuclear_families, na.rm = T) / n_distinct(uuid),
        total_number_of_males = sum(number_of_males, na.rm = T),
        total_number_of_females = sum(number_of_females, na.rm = T),
        total_number_of_household_members = sum(hh_size, na.rm = T),
        .groups = 'drop'
      )
  ) |>
  mutate(sex_ratio = paste0(
    round((total_number_of_males / total_number_of_females) * 100),
    ":100")
  ) |>
  rename_label(
    barangay = config$aggregation$label,
    total_households = "Total households",
    average_household_size = "Average household size",
    average_number_of_nuclear_families = "Average number of nuclear families",
    total_number_of_household_members = "Total number of household members",
    total_number_of_males = "Total number of males",
    total_number_of_females = "Total number of females",
    sex_ratio = "Sex ratio (male:female)"
  )



#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#
cli_alert_info("Generating distribution of households by size")
# Covered households by size
ts$ts_hh_size_distribution <- data$cbms_household_record |>
  generate_crosstab(
    barangay,
    hh_size,
    label_total_row = area_name_overall,
    position_total = "top"
  )



#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#
cli_alert_info("Generating distribution of households by Access to Internet")
# Distribution of households by Access to Internet
ts$ts_internet_access <- data$cbms_household_record |>
  mutate(
    internet_access = factor(
      case_when(
        k01_internet_access == 1 & k02_internet_at_home == 1 ~ 1L,
        k01_internet_access == 1 & k02_internet_at_home == 2 ~ 2L,
        k01_internet_access == 2 ~ 3L
      ),
      levels = c(1, 2, 3),
      labels = c(
        "With access to internet and with own internet access at home",
        "With access to internet but does not own internet at home",
        "Without access to internet"
      )
    )
  ) |>
  generate_crosstab(
    barangay,
    internet_access,
    label_total_row = area_name_overall,
    position_total = "top"
  )



#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#
# distribution of households by respondent's perception on safety
cli_alert_info("Generating distribution of households by respondent's perception on safety")
ts$ts_perception_on_safety <- data$cbms_household_record |>
  generate_crosstab(
    barangay,
    l01_safe_walking_alone,
    label_total_row = area_name_overall,
    position_total = "top"
  )


#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#
# distribution of households by main source of water supply
cli_alert_info("Generating distribution of households by main source of water supply")
ts$ts_main_source_of_water <- data$cbms_household_record |>
  generate_crosstab(
    barangay,
    n01_main_water,
    label_total_row = area_name_overall,
    position_total = "top"
  )



#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#
# distribution of households by main source of drinking water
cli_alert_info("Generating distribution of households by main source of drinking water")
ts$ts_main_source_of_drinking_water <- data$cbms_household_record |>
  generate_crosstab(
    barangay,
    n02_drinking_water,
    label_total_row = area_name_overall,
    position_total = "top"
  )



#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#
# distribution of households by service level of drinking water
cli_alert_info("Generating distribution of households by service level of drinking water")
ts$ts_service_level_drinking_water <- data$cbms_household_record |>
  generate_crosstab(
    barangay,
    n03_service_level_drinking_water,
    label_total_row = area_name_overall,
    position_total = "top"
  )



#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#
# distribution of households by type of toilet facility
cli_alert_info("Generating distribution of households by type of toilet facility")
ts$ts_type_of_toilet_facility <- data$cbms_household_record |>
  generate_crosstab(
    barangay,
    n08_toilet_facility,
    label_total_row = area_name_overall,
    position_total = "top"
  )


#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#
# distribution of households by service level of toilet facility
cli_alert_info("Generating distribution of households by service level of toilet facility")
ts$ts_service_level_toilet_facility <- data$cbms_household_record |>
  generate_crosstab(
    barangay,
    n08_service_level_toilet_facility,
    label_total_row = area_name_overall,
    position_total = "top"
  )



#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#
# distribution of households by service level of handwashing facility
cli_alert_info("Generating distribution of households by service level of handwashing facility")
ts$ts_service_level_handwashing_facility <- data$cbms_household_record |>
  generate_crosstab(
    barangay,
    n13_service_level_handwashing_facility,
    label_total_row = area_name_overall,
    position_total = "top"
  )



#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#
# distribution of households by the type of building they occupy
cli_alert_info("Generating distribution of households by type of building")
ts$ts_building_type <- data$cbms_household_record |>
  generate_crosstab(
    barangay,
    n13_service_level_handwashing_facility,
    label_total_row = area_name_overall,
    position_total = "top"
  )



#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#
# distribution of households by materials used in the roof of the building they occupy
cli_alert_info("Generating distribution of households by materials used in the roof")
ts$ts_roof_materials <-  data$cbms_household_record |>
  generate_crosstab(
    barangay,
    o03_roof,
    label_total_row = area_name_overall,
    position_total = "top"
  )



#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#
# distribution of households by materials used in the outer walls of the housing unit they occupy
cli_alert_info("Generating distribution of households by materials used in the outer walls")
ts$ts_outer_walls <- data$cbms_household_record |>
  generate_crosstab(
    barangay,
    o04_outer_walls,
    label_total_row = area_name_overall,
    position_total = "top"
  )



#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#
# distribution of households by materials used in the floor of the housing unit they occupy
cli_alert_info("Generating distribution of households by materials used in the floor")
ts$ts_floor_materials <- data$cbms_household_record |>
  generate_crosstab(
    barangay,
    o06_floor,
    label_total_row = area_name_overall,
    position_total = "top"
  )



#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#
# distribution of households by tenure status of the housing unit and lot they occupy
cli_alert_info("Generating distribution of households by tenure status")
ts$ts_tenure_status <- data$cbms_household_record |>
  generate_crosstab(
    barangay,
    o09_tenure,
    label_total_row = area_name_overall,
    position_total = "top"
  )



#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#
# distribution of households by presence of electricity in place of dwelling
cli_alert_info("Generating distribution of households by access to electricity")
ts$ts_access_to_electricity <- data$cbms_household_record |>
  filter(o01_building_type %in% c(1:7, 9)) |>
  generate_crosstab(
    barangay,
    o11_electricity,
    label_total_row = area_name_overall,
    position_total = "top"
  )



#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#
# distribution of households by the fuel they use most of the time for cooking
cli_alert_info("Generating distribution of households by fuel for cooking")
ts$ts_fuel_for_cooking <- data$cbms_household_record |>
  filter(o01_building_type %in% c(1:7, 9)) |>
  generate_crosstab(
    barangay,
    o13_fuel_for_cooking,
    label_total_row = area_name_overall,
    position_total = "top"
  )


#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#
# distribution of households by Access to Secure Tenure
cli_alert_info("Generating distribution of households by Access to Secure Tenure")
ts$ts_settlement_status <- data$cbms_household_record |>
  mutate(
    informal_settlers = factor(
      case_when(
        o09_tenure %in% c(4, 7) ~ 1L,
        o09_tenure %in% c(1:3, 5, 6) ~ 2L
      ),
      levels = c(1, 2),
      labels = c(
        "Informal settlers", "Not informal settlers"
      )
    )
  ) |>
  generate_crosstab(
    barangay,
    informal_settlers,
    label_total_row = area_name_overall,
    position_total = "top"
  )


#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#
# distribution of households by Overcrowding Status
cli_alert_info("Generating distribution of households by Access to Secure Tenure")
ts$ts_overcrowding_status <- data$cbms_household_record |>
  mutate(floor_area_per_capita = o07_floor_area / hh_size) |>
  mutate(
    overcrowding_status = factor(
      case_when(
        floor_area_per_capita < 4 ~ 1L,
        floor_area_per_capita >= 4 ~ 2L
      ),
      levels = c(1, 2),
      labels = c(
        "Overcrowded", "Not overcrowded"
      )
    )
  ) |>
  generate_crosstab(
    barangay,
    overcrowding_status,
    label_total_row = area_name_overall,
    position_total = "top"
  )



#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#
# distribution of households by Reliance on Clean Fuels/Technology
cli_alert_info("Generating distribution of households by Reliance on Clean Fuels/Technology")
ts$ts_clean_fuel_status <- data$cbms_household_record |>
  filter(o01_building_type %in% c(1:7, 9)) |>
  mutate(
    clean_fuel_status = factor(
      case_when(
        o13_fuel_for_cooking %in% c(1, 3) ~ 1L,
        o13_fuel_for_cooking %in% c(2, 4:6, 9) ~ 2L
      ),
      levels = c(1, 2),
      labels = c(
        "Using clean fuels or technology", "Not using clean fuels"
      )
    )
  ) |>
  generate_crosstab(
    barangay,
    clean_fuel_status,
    label_total_row = area_name_overall,
    position_total = "top"
  )



#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#
# Number of Households with Member/s who got Ill/Sick/Injured but did not Avail Medical Treatment by Main Reason
cli_alert_info("Generating Number of Households with Member/s who got Ill/Sick/Injured but did not Avail Medical Treatment by Main Reason")
ts$ts_reason_not_availed_medical_treatment <- data$cbms_household_record |>
  filter(
    f01_with_member_who_has_been_sick == 1,
    f02_availed_medical_treatment == 2
  ) |>
  generate_crosstab(
    barangay,
    f04_reason_not_availed_treatment,
    label_total_row = area_name_overall,
    position_total = "top"
  )


#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#
# distribution of Covered Population by Sex and Age Group
cli_alert_info("Generating distribution of Covered Population by Sex and Age Group")
ts$ts_sex_and_age_group <- data$cbms_person_record |>
  group_by(barangay) |>
  generate_crosstab(
    a05_age_group_five_years,
    a03_sex,
    label_total_row = area_name_overall,
    position_total = "top",
    label_total_column = "Both Sexes"
  )


#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#
# distribution of Covered Population by Sex
cli_alert_info("Generating distribution of Covered Population by Sex")
ts$ts_sex <- data$cbms_person_record |>
  generate_crosstab(
    barangay,
    a03_sex,
    label_total_row = area_name_overall,
    position_total = "top",
    label_total_column = "Both Sexes"
  )



#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#
# distribution of Covered Population by Age Group
cli_alert_info("Generating distribution of Covered Population by Age Group")
ts$ts_five_year_age_group <- data$cbms_person_record |>
  generate_crosstab(
    barangay,
    a05_age_group_five_years,
    label_total_row = area_name_overall,
    position_total = "top"
  )



#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#
# distribution of Covered Population 16 to 21 Years Old who are not Attending School by Sex and Reason
cli_alert_info("Generating distribution of Covered Population 16 to 21 Years Old who are not Attending School by Sex and Reason")
ts$ts_reason_non_attending_school_sex <- data$cbms_person_record |>
  filter(a05_age >= 16 & a05_age <= 21, d01_currently_attending_school == 2) |>
  group_by(barangay) |>
  generate_crosstab(
    d06_reason_not_attending_school,
    a03_sex,
    label_total_row = area_name_overall,
    position_total = "top",
    label_total_column = "Both Sexes"
  )



#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#
# distribution of Covered Population by Ethnicity
cli_alert_info("Generating distribution of Covered Population by Ethnicity")
ts$ts_ethnicity <- data$cbms_person_record |>
  generate_crosstab(
    barangay,
    a09_ethnicity,
    label_total_row = area_name_overall,
    position_total = "top"
  )



#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#
# distribution of Covered Population 60 Years Old and Over by Ownership of Senior Citizen ID
cli_alert_info("Generating distribution of Covered Population 60 Years Old and Over by Ownership of Senior Citizen ID")
ts$ts_senior_citizen_id <- data$cbms_person_record |>
  filter(a05_age >= 60) |>
  mutate(
    senior_citizen_id = factor(
      case_when(
        b07_senior_citizen_id == 1 ~ 1L,
        b07_senior_citizen_id == 2 ~ 2L,
        b07_senior_citizen_id == 8 ~ 8L,
      ),
      levels = c(1, 2, 8),
      labels = c(
        "With Senior Citizen ID",
        "Without Senior Citizen ID",
        "Not reported"
      )
    ))|>
  generate_crosstab(
    barangay,
    senior_citizen_id,
    label_total_row = area_name_overall,
    position_total = "top"
  )



#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#
# Proportion of Farmers and Farm Workers among Covered Population 15 Years Old and Over Excluding Overseas Filipino Workers
cli_alert_info("Generating proportion of Farmers and Farm Workers among Covered Population 15 Years Old and Over Excluding Overseas Filipino Workers")
ts$ts_farmer <- data$cbms_person_record |>
  filter(a05_age >= 15, c02_ofi %in% c(3, 4, 6)) |>
  mutate(
    farmer = factor(
      e17_farmer,
      levels = c(1, 2),
      labels = c("Farmer/farm laborer","Not farmer/farm laborer")))|>
  generate_crosstab(
    barangay,
    farmer,
    label_total_row = area_name_overall,
    position_total = "top")



#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#
# Proportion of Fisherfolk and Fish Workers among Covered Population 15 Years Old and Over Excluding Overseas Filipino Workers
cli_alert_info("Generating proportion of Fisherfolk and Fish Workers among Covered Population 15 Years Old and Over Excluding Overseas Filipino Workers")
ts$ts_fisherfolk <- data$cbms_person_record |>
  filter(a05_age >= 15, c02_ofi %in% c(3, 4, 6)) |>
  mutate(
    fisherfolk = factor(
      e18_fisherfolk,
      levels = c(1, 2),
      labels = c("Fisherfolk/farm worker", "Not fisherfolk/farm worker")))|>
  generate_crosstab(
    barangay,
    fisherfolk,
    label_total_row = area_name_overall,
    position_total = "top")



#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#
# distribution of Covered Population 3 to 24 Years Old by Schooling Status
cli_alert_info("Generating distribution of Covered Population 3 to 24 Years Old by Schooling Status")
ts$ts_schooling_status <- data$cbms_person_record |>
  filter(a05_age >= 3 & a05_age <= 24) |>
  mutate(
    currently_attending_school = factor(
      d01_currently_attending_school,
      levels = c(1, 2),
      labels = c("Currently attending school", "Not Currently not attending school")))|>
  generate_crosstab(
    barangay,
    currently_attending_school,
    label_total_row = area_name_overall,
    position_total = "top")



#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#
# distribution of Covered Population 3 to 24 Years Old by Schooling Status and Sex
cli_alert_info("Generating distribution of Covered Population 3 to 24 Years Old by Schooling Status and Sex")
ts$ts_schooling_status_by_sex <- data$cbms_person_record |>
  filter(a05_age >= 3 & a05_age <= 24) |>
  mutate(
    currently_attending_school = factor(
      d01_currently_attending_school,
      levels = c(1, 2),
      labels = c("Currently attending school","Not currently attending school"))
  ) |>
  group_by(barangay) |>
  generate_crosstab(
    a03_sex,
    currently_attending_school,
    position_total = "top",
    label_total_row = "Both Sexes"
  )



#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#
# Labor Force Participation Rate among Covered Population 15 Years Old and Over Excluding Overseas Filipino Workers
cli_alert_info("Generating Labor Force Participation Rate among Covered Population 15 Years Old and Over Excluding Overseas Filipino Workers")
ts$ts_lfp_area <- data$cbms_person_record |>
  filter(a05_age >= 15, c02_ofi %in% c(3, 4, 6)) |>
  mutate(
    labor_force_participation = factor(
      e01_labor_force_participation,
      levels = c(1, 2),
      labels = c("In the labor force", "Not in the labor force")
    ))|>
  generate_crosstab(
    barangay,
    labor_force_participation,
    label_total_row = area_name_overall,
    position_total = "top")



#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#
# Labor Force Participation Rate among Covered Population 15 Years Old and Over Excluding Overseas Filipino Workers by Sex
cli_alert_info("Generating Labor Force Participation Rate among Covered Population 15 Years Old and Over Excluding Overseas Filipino Workers by Sex")
ts$ts_lfp_sex <- data$cbms_person_record |>
  filter(a05_age >= 15, c02_ofi %in% c(3, 4, 6)) |>
  mutate(
    labor_force_participation = factor(
      e01_labor_force_participation,
      levels = c(1, 2),
      labels = c("In the labor force", "Not in the labor force")))|>
  group_by(barangay) |>
  generate_crosstab(
    a03_sex,
    labor_force_participation,
    label_total_row = "Both Sexes",
    position_total = "top")



#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#
# Labor Force Participation Rate among Covered Population 15 Years Old and Over Excluding Overseas Filipino Workers by Age Group
cli_alert_info("Generating Labor Force Participation Rate among Covered Population 15 Years Old and Over Excluding Overseas Filipino Workers by Age Group")
ts$ts_lfp_age_group <- data$cbms_person_record |>
  filter(a05_age >= 15, c02_ofi %in% c(3, 4, 6)) |>
  mutate(
    labor_force_participation = factor(
      e01_labor_force_participation,
      levels = c(1, 2),
      labels = c("In the labor force", "Not in the labor force")))|>
  group_by(barangay) |>
  generate_crosstab(
    a05_age_group_five_years,
    labor_force_participation,
    label_total_row = area_name_overall,
    position_total = "top")



#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#
# Key employment measure
cli_alert_info("Generating key employment measure")
ts$ts_key_employment_measure <- data$cbms_person_record |>
  filter(a05_age >= 15 & c02_ofi %in% c(3, 4, 6), e01_labor_force_participation == 1) |>
  group_by(barangay, a03_sex) |>
  summarise(
    f1_Employed = sum(e01_employment_status == 1, na.rm = T),
    f2_Unemployed = sum(e01_employment_status == 2, na.rm = T),
    f3_Underemployed = sum(e01_underemployment_status == 1, na.rm = T),
    .groups = 'drop'
  ) |>
  tidyr::pivot_wider(
    names_from = a03_sex,
    values_from = c(
      f1_Employed,
      f2_Unemployed,
      f3_Underemployed
    )
  ) |>
  add_row_total() |>
  mutate(
    f1_Employed_0 = f1_Employed_1 + f1_Employed_2, # Total Employed
    f2_Unemployed_0 = f2_Unemployed_1 + f2_Unemployed_2, # Total Unemployed

    f0_0 = f1_Employed_0 + f2_Unemployed_0, # Total LFP
    f0_1 = f1_Employed_1 + f2_Unemployed_1, # Total LFP Male
    f0_2 = f1_Employed_2 + f2_Unemployed_2, # Total LFP Female

    f3_Underemployed_0 = f3_Underemployed_1 + f3_Underemployed_2, # Total Underemployed

    p1_Employed_0 = 100 * (f1_Employed_0 / f0_0),
    p2_Unemployed_0 = 100 * (f2_Unemployed_0 / f0_0),
    p3_Underemployed_0 = 100 * (f3_Underemployed_0 / f1_Employed_0),

    p1_Employed_1 = 100 * (f1_Employed_1 / f0_1),
    p2_Unemployed_1 = 100 * (f2_Unemployed_1 / f0_1),
    p3_Underemployed_1 = 100 * (f3_Underemployed_1 / f1_Employed_1),

    p1_Employed_2 = 100 * (f1_Employed_2 / f0_2),
    p2_Unemployed_2 = 100 * (f2_Unemployed_2 / f0_2),
    p3_Underemployed_2 = 100 * (f3_Underemployed_2 / f1_Employed_2)

  ) |>
  select(
    barangay,
    matches("f0_"),
    matches("f1_"),
    matches("f2_"),
    matches("f3_"),
    matches("p1_"),
    matches("p2_"),
    matches("p3_")
  ) |>
  rename_all(~ str_replace_all(., '^f[1-3]_', 'Frequency__')) |>
  rename_all(~ str_replace_all(., '^p[1-3]_', 'Percent__')) |>
  rename_all(~ str_replace_all(., '_1$', '__Male')) |>
  rename_all(~ str_replace_all(., '_2$', '__Female')) |>
  rename_all(~ str_replace_all(., '_0$', '__Both sexes')) |>
  rename_all(~ str_replace_all(., '^f0', 'Frequency__In the labor force')) |>
  rename_label(barangay = "Area name")




#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#
# distribution of Working Children aged 5 to 17 Years Old by Occupation Group
cli_alert_info("Generating distribution of Working Children aged 5 to 17 Years Old by Occupation Group")
ts$ts_working_children_occupation <- data$cbms_person_record |>
  filter(
    a05_age >= 5 & a05_age <= 17,
    e01_work_past_week == 1 | e02_job_or_business_past_week == 1
  ) |>
  group_by(barangay) |>
  generate_crosstab(
    a05_age,
    e05_occupation_group,
    label_total_row = area_name_overall,
    position_total = "top"
  )



#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#
# distribution of Employed Persons in Managerial Positions
cli_alert_info("Generating distribution of Employed Persons in Managerial Positions")
ts$ts_employed_managers <- data$cbms_person_record |>
  filter(a05_age >= 15, e01_employment_status == 1, e05_occupation_group == 1) |>
  generate_crosstab(
    barangay,
    a03_sex,
    label_total_row = "Both sexes",
    position_total = "top"
  )




#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#
# distribution of Covered Population 15 Years Old and Over Excluding Overseas Filipino Workers who are Employed by Class of Worker
cli_alert_info("Generating distribution of Covered Population 15 Years Old and Over Excluding Overseas Filipino Workers who are Employed by Class of Worker")
ts$ts_employed_class_of_workers <- data$cbms_person_record |>
  filter(a05_age >= 15, e01_employment_status == 1) |>
  generate_crosstab(
    barangay,
    e08_class_of_worker,
    label_total_row = area_name_overall,
    position_total = "top"
  )



#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#
# distribution of Covered Population 15 Years Old and Over Excluding Overseas Filipino Workers who are Employed by Industry Group
cli_alert_info("Generating distribution of Covered Population 15 Years Old and Over Excluding Overseas Filipino Workers who are Employed by Industry Group")
ts$ts_employed_industry_group <- data$cbms_person_record |>
  filter(a05_age >= 15, e01_employment_status == 1) |>
  generate_crosstab(
    barangay,
    e06_industry_group,
    label_total_row = area_name_overall,
    position_total = "top"
  )



#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#
# distribution of Covered Population 15 Years Old and Over Excluding Overseas Filipino Workers who are Employed by Industry Group
cli_alert_info("Generating distribution of Covered Population 15 Years Old and Over Excluding Overseas Filipino Workers who are Employed by Industry Group")
ts$ts_youth_engagement <- data$cbms_person_record |>
  filter(a05_age >= 15 & a05_age <= 24) |>
  mutate(
    neet = factor(case_when(
      d01_currently_attending_school != 1 & e01_employment_status != 1 & d08_tvet_currently_attending != 1 ~ 1L,
      d01_currently_attending_school == 1 | e01_employment_status == 1 | d08_tvet_currently_attending == 1 ~ 2L

    ),
    levels = c(1, 2),
    labels = c(
      "Not in education, employment or training", "In education, employment or training"
    ))) |>
  generate_crosstab(
    barangay,
    neet,
    label_total_row = area_name_overall,
    position_total = "top"
  )



#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#
# distribution of Senior High School Graduate not Attending School
cli_alert_info("Generating distribution of Senior High School Graduate not Attending School")
ts$ts_shs_grad_not_in_school_by_employment <- data$cbms_person_record |>
  filter(
    a05_age >= 3 & a05_age <= 24,
    d01_currently_attending_school == 2,
    a11_hgc %in% c(
      '34000130',
      '36000012',
      '34001130',
      '34002130',
      '34003130',
      '35000130',
      '34001131',
      '34001132',
      '34001133',
      '34001134',
      '34001135',
      '35000131',
      '35000132',
      '35000133',
      '35000134',
      '35000135'
    )
  ) |>
  mutate(
    employment_ind = if_else(
      e01_work_past_week == 1 | e02_job_or_business_past_week == 1,
      "Working SHS graduate",
      "Not working SHS graduate"
    )
  ) |>
  generate_crosstab(
    barangay,
    employment_ind,
    label_total_row = area_name_overall,
    position_total = "top"
  )



#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#
# distribution of Covered Population 15 Years Old and Over who are Technical and Vocational Education and Training Graduates not Attending School by Employment Status
cli_alert_info("Generating distribution of Covered Population 15 Years Old and Over who are Technical and Vocational Education and Training Graduates not Attending School by Employment Status")
ts$ts_tvet_grad_not_sch_employment <- data$cbms_person_record |>
  filter(
    a05_age >= 15,
    d01_currently_attending_school ==  2,
    d07_tvet_graduate == 1
  ) |>
  mutate(
    employment_ind = if_else(
      e01_employment_status == 1,
      "Working TVET graduate",
      "Not working TVET graduate"
    )
  ) |>
  generate_crosstab(
    barangay,
    employment_ind,
    label_total_row = area_name_overall,
    position_total = "top"
  )



#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#
# distribution of Population in Urban Barangay who are Informal Settler
cli_alert_info("Generating distribution of Population in Urban Barangay who are Informal Settler")
ts$ts_manufacturing_industry <- data$cbms_person_record |>
  filter(a05_age >= 15, e01_employment_status == 1) |>
  mutate(
    industry_ind = if_else(
      e06_industry_group == 3,
      "In manufacturing industry",
      "Not in manufacturing industry"
    )
  ) |>
  generate_crosstab(
    barangay,
    industry_ind,
    label_total_row = area_name_overall,
    position_total = "top"
  )



#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#
# distribution of Households with Owned/Owner-like Possession of Housing Unit
cli_alert_info("Generating distribution of Households with Owned/Owner-like Possession of Housing Unit")
ts$ts_owner_like_house <- data$cbms_household_record |>
  filter(o01_building_type %in% c(1:7)) |>
  mutate(
    owner_like = if_else(o09_tenure %in% c(1, 2, 3), "With owned or owner-like possession of the housing unit", "Without possession of the housing unit"),
  ) |>
  generate_crosstab(
    barangay,
    owner_like,
    label_total_row = area_name_overall,
    position_total = "top"
  )



#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#
# distribution of Employed Person who are Self-Employed and Unpaid Family Workers
cli_alert_info("Generating distribution of Employed Person who are Self-Employed and Unpaid Family Workers")
ts$ts_informal_employment <- data$cbms_person_record |>
  filter(a05_age >= 15, e01_employment_status == 1) |>
  mutate(
    informal =
      if_else(e08_class_of_worker %in% c(3, 6), "Informal employment", "Formal employment")
  ) |>
  generate_crosstab(
    barangay,
    informal,
    label_total_row = area_name_overall,
    position_total = "top"
  )



#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#
#percentage of Barangays that Have Available Garbage Collection Services
cli_alert_info("Generating percentage of Barangays that Have Available Garbage Collection Services")
ts$ts_waste_collection_status <- data$cbms_barangay_record |>
  generate_crosstab(
    barangay,
    d02_garbage_collection,
    label_total_row = area_name_overall,
    position_total = "top"
  )

#percentage of Barangays that have Available Cellphone Network Signal
cli_alert_info("Generating percentage of Barangays that have Available Cellphone Network Signal")
ts$ts_network_availability <- data$cbms_barangay_record |>
  generate_crosstab(
    barangay,
    e01_availability_network,
    label_total_row = area_name_overall,
    position_total = "top"
  )


#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#
#percentage of Barangays by Disaster Risk Reduction and Management Measures
cli_alert_info("Generating percentage of Barangays by Disaster Risk Reduction and Management Measures")
ts$ts_drrm <- data$cbms_barangay_record |>
  mutate(
    disaster_plan = factor(
      f01_disaster_plan,
      levels = c(1, 2),
      labels = c("with Written DRRM Plan", "without Written DRRM Plan")
    ),
    disaster_response_team = factor(
      f03_disaster_response,
      levels = c(1, 2),
      labels = c("with Disaster/Emergency Response Team", "without Disaster/Emergency Response Team")
    ),
    disaster_hotline = factor(
      f04_disaster_hotline_presence,
      levels = c(1, 2),
      labels = c("with Disaster/Emergency Hotline", "without Disaster/Emergency Hotline")
    )
  ) |>
  generate_crosstab(
    barangay,
    starts_with("disaster_"),
    label_total_row = area_name_overall,
    position_total = "top"
  )









#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#
#Don't changes the codes after this comment
#Don't changes the codes after this comment
#Don't changes the codes after this comment
#Don't changes the codes after this comment


meta <- get_rcdf_metadata(path_input_data[1], name = 'meta')
ts <- lapply(ts, add_source_note, source_note = meta$source_note)

ts_filename <- file.path(
  path_output,
  glue::glue('{config$cbms_round} CBMS Tabulation - {area_overall} ({Sys.Date()}) - no reference file.xlsx')
)

ts_filename_2 <- file.path(
  path_output,
  glue::glue('{config$cbms_round} CBMS Tabulation - {area_overall} ({Sys.Date()}).xlsx')
)

cli_rule(left = "Generating output files")

generate_output(
  data = ts,
  path = ts_filename,
  include_table_list = TRUE,
  format = "xlsx"
)

table_refs <- read.xlsx("references/2024-cbms-tabulation-specs.xlsx")

generate_output(
  data = ts,
  path = ts_filename_2,
  include_table_list = TRUE,
  table_list_reference = table_refs,
  format = "xlsx"
)

cli_text("Suc♥️cess")
