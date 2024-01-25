#### Preamble ####
# Purpose: Runs tests on the cleaned datasets obtained from "02-data_cleaning.R"
# to check the validity of the dataset entries.
# Author: Sima Shmuylovich
# Date: 25 January 2024
# Contact: sima.shmuylovich@mail.utoronto.ca
# License: MIT
# Pre-requisites: Run "import_packages.R", "01-download_data.R", and
# "02-data_cleaning.R"

#### Workspace setup ####
library(tidyverse)

#### Test data ####
cleaned_automobile_thefts_data_test <- read_csv("outputs/data/cleaned_automobile_thefts_data.csv")
cleaned_all_crime_data_test <- read_csv("outputs/data/cleaned_all_crime_data.csv")

# Dataset 1: Data Validation/Tests
# 1. "year" does not contain years before <starting_year> or after <ending_year>
cleaned_automobile_thefts_data_test$year %>% min() == starting_year
cleaned_automobile_thefts_data_test$year %>% max() == ending_year

# 2. <num_automobile_thefts> is greater than or equal to 0.
cleaned_automobile_thefts_data_test$num_automobile_thefts %>% min() >= 0

# Dataset 3: Data Validation/Tests
# 1. "year" does not contain years before <starting_year> or after <ending_year>
cleaned_all_crime_data_test$year %>% min() == starting_year
cleaned_all_crime_data_test$year %>% max() == ending_year

# 2. We have <num_crime_types> types of crime: "Assault", "Automobile-Theft", 
# "Bike-Theft", "Break-and-Enter", "Homicide", "Robbery", "Shooting", 
# "Theft-from-Vehicle"
cleaned_all_crime_data_test$crime_type %>%
  unique() %>%
  length() == num_crime_types

cleaned_all_crime_data_test$crime_type %>%
  unique() %in% c("Assault", "Automobile-Theft", "Bike-Theft", "Break-and-Enter", 
                  "Homicide", "Robbery", "Shooting", "Theft-from-Vehicle")

# 3. Number of crime occurrences is equal to or greater than 0
cleaned_all_crime_data_test$num_crime_occurrences %>% min() >= 0
