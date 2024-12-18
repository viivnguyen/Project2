
<!-- README.md is generated from README.Rmd. Please edit that file -->

# asianamsurvey

## Overview

The `asianamsurvey` package is designed to streamline the analysis of
survey data, with a specific focus on demographic and self-reported
response surveys. It includes tools for data cleaning, transformation,
visualization, and summarization, making it easier for researchers and
statisticians to extract meaningful insights from complex survey
datasets.

## Why Use This Package?

- Create customizable scatter plots for survey data
- Generate proportion tables for categorical variables
- Handle different types of data structures efficiently
- Convert survey responses to numeric scales systematically
- Process categorical variables with custom numeric mappings
- Built-in support for survey data analysis
- Comprehensive error handling and input validation
- Consistent and well-documented interface

## Installation

You can install this package from GitHub:

``` r
# install.packages("devtools")
devtools::install_github("yourusername/yourpackagename")
```

## Usage

Here are some quick examples of what you can do with this package:

### Scatter Plots

``` r
# Create a scatter plot of survey data
scatter(asian_american,
        x = "age",
        y = "income",
        color = "education",
        title = "Income by Age and Education Level",
        add_trendline = TRUE)
```

### Proportion Tables

``` r
# Create a proportion table for survey responses
proptable(asian_american,
         x = education,
         y = english_speaking,
         include_total = TRUE,
         digits = 3)
```

### Survey Levels

``` r
# Convert ordinal responses to numeric values
data <- data.frame(proficiency = c("Well", "Not at all", NA, "Not well", "Very well"))
response_scale <- c("Not at all", "Not well", "Well", "Very well")
data <- surveylevels(data, 
                    column = "proficiency", 
                    ordered_levels = response_scale)
```

### Make Numeric

``` r
# Convert categorical responses with custom mapping
data <- data.frame(spouse = c("Living with spouse", "Not living with spouse", NA))
data <- makenumeric(data, 
                    column = "spouse",
                    true_values = "Living with spouse",
                    true_number = 1,
                    false_value = 0,
                    na_value = -1)
```

## Features

- **Scatter Plot Function**: Create customizable scatter plots with
  options for:
  - Trend lines
  - Threshold lines
  - Faceting
  - Custom themes
  - Multiple color schemes
  - Error handling for invalid inputs
- **Proportion Table Function**: Generate summary tables with:
  - Proportions or counts
  - Optional row totals
  - Customizable decimal places
  - Flexible grouping options
  - Precise calculations with tolerance handling
- **Survey Levels Function**: Process survey response levels with:
  - Ordered level conversion
  - Automatic numeric mapping
  - NA handling
  - Level validation
  - Transparent mapping display
  - Robust error checking for invalid levels
- **Make Numeric Function**: Convert categorical variables with:
  - Custom true/false value mapping
  - Flexible NA handling
  - Multiple true value support
  - Input validation
  - Automatic type conversion
  - Comprehensive error handling

## Dataset: asian_american

The package includes one example datasets: Asian American survey data
with demographic and socioeconomic variables \### Description: This
dataset contains survey responses from the “Asian American Quality of
Life” study conducted in Austin, capturing demographic, household, and
quality-of-life metrics to assess the needs and preferences of Asian
American communities. We have wrangled it so as to only include
information regarding their reflections on their immigration and
acculturation experiences, emotional well-being, and access to social
and community resources.

- **Overview**:
  - Entries: 2,609
  - Columns: 231
  - Purpose: Analyze and improve quality of life for Asian American
    populations.
- **Key Information**:
  - Demographics: Age, Gender, Ethnicity, Marital Status, Education
    Level
  - Immigration and Acculturation: English Proficiency, Cultural Warning
  - Emotional Well-Being: Thoughts on Depression and Related Aspects
  - Social and Community Resources: Closeness to Family and Friends

### Source:

Data is from the “Asian American Quality of Life” study by the City of
Austin.

## Getting Help

For more detailed information about any function, use the R help system:

``` r
?scatter
?proptable
?surveylevels
?makenumeric
```

## Testing

All functions come with comprehensive test coverage: - Input validation
tests - Edge case handling - Error message verification - Calculation
accuracy checks - Object type validation

Run tests using:

``` r
devtools::test()
```

\`\`\`

</rewritten_file>
