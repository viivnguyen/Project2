## code to prepare `DATASET` dataset goes here

dataset <- read.csv("https://data.austintexas.gov/api/views/hc5t-p62z/rows.csv?accessType=DOWNLOAD")


# Selecting Columns of Interest
asian_american <- dataset |>
  janitor::clean_names() |>
  dplyr::rename(education = "education_completed",
                siblings = "brother_sister",
                ideal_life = "satisfied_with_life_1",
                life_satisfaction = "satisfied_with_life_2"
                ) |>
  dplyr::select (
    # Demographic Information
    survey_id, age, gender, ethnicity, marital_status, education,
    # House Hold
    household_size, no_one, spouse, children, grand_children, parent, grandparent,
    siblings, other_relative, friends, other,
    # Employment/Salary
    full_time_employment,part_time_employment, self_employed_full_time,
    self_employed_part_time, student, homemaker, disabled, unemployed, retired,
    other_employement, income, achieving_ends_meet,

    # Immigration and Acculturation
    us_born, duration_of_residency, primary_language, english_speaking, english_difficulties,
    familiarity_with_america, familiarity_with_ethnic_origin, identify_ethnically,
    belonging, discrimination,

    # Emotional Well-Being
    quality_of_life, ideal_life, life_satisfaction, weakness, shame, disappointment,
    disclosure, antidepressants, danger, recovery, treatment, counseling, preference,

    # Social and Community Resources
    see_family, close_family, helpful_family, see_friends, close_friends, helpful_friends,
    family_respect, similar_values, successful_family, trust, loyalty, family_pride, expression,
    spend_time_together, feel_close, togetherness, close_knit_community, community_trust
    )

asian_american[asian_american == ""] <- NA

usethis::use_data(asian_american, overwrite = TRUE)
