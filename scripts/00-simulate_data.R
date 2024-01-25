#### Preamble ####
# Purpose: Simulates datasets regarding neighbourhood crime in Toronto
# Author: Sima Shmuylovich
# Date: 25 January 2024
# Contact: sima.shmuylovich@mail.utoronto.ca
# License: MIT
# Pre-requisites: Run "import_packages.R"

#### Loading Packages ####
library(tidyverse)

#### Simulate data ####
set.seed(483)

### Global Variables ###
# Simulating data between 2014 and 2023
starting_year <- 2014 
ending_year <- 2023
num_years <- ending_year - starting_year + 1 # Number of years observed
num_crime_types <- 8 # Number of crime types

## Dataset 1: Simulate year and number of automobile thefts. 
# Expected Columns: year | num_automobile_thefts
automobile_thefts_data_simulation <-
  tibble(
    # Sequence of years between <starting_year> and <ending_year> inclusive
    year = c(starting_year:ending_year),
    # Sequence of <num_years> Random variables representing the number of 
    # automobile thefts. 
    # Use Uniform Distribution
    # Round Random variables to whole numbers
    num_automobile_thefts = round(runif(n=num_years, min = 0, max = 15000))
  )
# Create table for the simulated data
automobile_thefts_data_simulation

## Dataset 3: Simulate year, crime type, and number of occurrences. 
# Expected Columns: year | crime_type | num_crime_occurrences 
all_crime_data_simulation <-
  tibble(
    # Sequence of years between <starting_year> and <ending_year> inclusive
    # Each year repeats <num_crime_types> 
    # i.e (2014, 2014, 2015, 2015, 2016, 2016, 2017, 2017)
    year = rep(c(starting_year:ending_year), each=num_crime_types),
    # Sequence of all crime types repeating <num_years> times
    crime_type = rep(c("Assault", "Automobile-Theft", "Bike-Theft", "Break-and-Enter", 
                       "Homicide", "Robbery", "Shooting", "Theft-from-Vehicle"
                       ), num_years),
    # Sequence of <num_years> * <num_crime_types> Random variables
    # Use Uniform Distribution
    # Round Random variables to whole numbers
    num_crime_occurrences = round(runif(n=num_years * num_crime_types, min = 0, max = 30000))
  )
# Create table for the simulated data
all_crime_data_simulation

# Dataset 1: Data Validation/Tests
# 1. "year" does not contain years before <starting_year> or after <ending_year>
automobile_thefts_data_simulation$year %>% min() == starting_year
automobile_thefts_data_simulation$year %>% max() == ending_year

# 2. <num_automobile_thefts> is greater than or equal to 0.
automobile_thefts_data_simulation$num_automobile_thefts %>% min() >= 0

# Dataset 3: Data Validation/Tests
# 1. "year" does not contain years before <starting_year> or after <ending_year>
all_crime_data_simulation$year %>% min() == starting_year
all_crime_data_simulation$year %>% max() == ending_year

# 2. We have <num_crime_types> types of crime: "Assault", "Automobile-Theft", 
# "Bike-Theft", "Break-and-Enter", "Homicide", "Robbery", "Shooting", 
# "Theft-from-Vehicle"
all_crime_data_simulation$crime_type %>%
  unique() %>%
  length() == num_crime_types

all_crime_data_simulation$crime_type %>%
  unique() %in% c("Assault", "Automobile-Theft", "Bike-Theft", "Break-and-Enter", 
                  "Homicide", "Robbery", "Shooting", "Theft-from-Vehicle")

# 3. Number of crime occurrences is equal to or greater than 0
all_crime_data_simulation$num_crime_occurrences %>% min() >= 0




