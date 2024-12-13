# Script to prepare mouse trial data
library(readxl)
library(dplyr)

# Read all three sheets from the Excel file
mouse_birth <- read_excel("data-raw/mousedata.xlsx", sheet = "Birth")
mouse_weight <- read_excel("data-raw/mousedata.xlsx", sheet = "Body Weight")
mouse_outcome <- read_excel("data-raw/mousedata.xlsx", sheet = "Outcome")

# Clean birth data
mouse_birth <- mouse_birth %>%
  arrange(ID) %>%
  mutate(Treatment = as.factor(Treatment))

# Clean weight data
mouse_weight <- mouse_weight %>%
  rename(
    "Date_Weight_1" = "Date Body Weight 1",
    "Date_Weight_2" = "Date Body Weight 2...5",
    "Date_Weight_3" = "Date Body Weight 2...7"
  ) %>%
  mutate(
    # Convert all weights to numeric
    `Body Weight 1` = as.numeric(`Body Weight 1`),
    `Body Weight 2` = as.numeric(`Body Weight 2`),
    `Body Weight 3` = as.numeric(`Body Weight 3`)
  ) %>%
  arrange(ID)

# Clean outcome data
mouse_outcome <- mouse_outcome %>%
  rename(
    "ID" = "Subject_ID",
    "Date_Outcome_1" = "Date Ourcome 1",
    "Date_Outcome_2" = "Date Outcome 2",
    "Date_Outcome_3" = "Date Outcome 3"
  ) %>%
  arrange(ID)

# Print cleaned structure
cat("\nCleaned data structures:\n")
cat("\nBirth data:\n")
str(mouse_birth)
cat("\nWeight data:\n")
str(mouse_weight)
cat("\nOutcome data:\n")
str(mouse_outcome)

# Save all datasets to data/ folder using base R
save(mouse_birth, mouse_weight, mouse_outcome, 
     file = "data/mousedata.rda")