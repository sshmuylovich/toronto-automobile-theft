#### Preamble ####
# Purpose: Cleans the raw Toronto Neighbourhood Crim Rates dataset obtained from
# "01-download_data.R".
# Author: Sima Shmuylovich
# Date: 25 January 2024
# Contact: sima.shmuylovich@mail.utoronto.ca
# License: MIT
# Pre-requisites: Run "import_packages.R" and "01-download_data.R"

#### Workspace setup ####
library(tidyverse)
library(janitor)
library(sf)

#### Clean data ####
raw_csv_data <- read_csv("inputs/data/unedited_data.csv")
raw_shapefile_data <- readRDS("inputs/data/unedited_data.rds")

cleaned_raw_csv_data <-
  raw_csv_data %>%
  # Only select attributes of interest
  select('_id', 'AREA_NAME', 
         'ASSAULT_2014', 'ASSAULT_2015', 'ASSAULT_2016', 'ASSAULT_2017', 
         'ASSAULT_2018', 'ASSAULT_2019', 'ASSAULT_2020', 'ASSAULT_2021', 
         'ASSAULT_2022', 'ASSAULT_2023', 
         'AUTOTHEFT_2014', 'AUTOTHEFT_2015', 'AUTOTHEFT_2016', 'AUTOTHEFT_2017',
         'AUTOTHEFT_2018', 'AUTOTHEFT_2019', 'AUTOTHEFT_2020', 'AUTOTHEFT_2021',
         'AUTOTHEFT_2022', 'AUTOTHEFT_2023', 
         'BIKETHEFT_2014', 'BIKETHEFT_2015', 'BIKETHEFT_2016', 'BIKETHEFT_2017',
         'BIKETHEFT_2018', 'BIKETHEFT_2019', 'BIKETHEFT_2020', 'BIKETHEFT_2021',
         'BIKETHEFT_2022', 'BIKETHEFT_2023',
         'BREAKENTER_2014', 'BREAKENTER_2015', 'BREAKENTER_2016', 
         'BREAKENTER_2017', 'BREAKENTER_2018', 'BREAKENTER_2019', 'BREAKENTER_2020',
         'BREAKENTER_2021', 'BREAKENTER_2022', 'BREAKENTER_2023', 
         'HOMICIDE_2014', 'HOMICIDE_2015', 'HOMICIDE_2016', 'HOMICIDE_2017',
         'HOMICIDE_2018', 'HOMICIDE_2019', 'HOMICIDE_2020', 'HOMICIDE_2021',
         'HOMICIDE_2022', 'HOMICIDE_2023',
         'ROBBERY_2014', 'ROBBERY_2015', 'ROBBERY_2016', 'ROBBERY_2017', 
         'ROBBERY_2018', 'ROBBERY_2019', 'ROBBERY_2020', 'ROBBERY_2021',
         'ROBBERY_2022', 'ROBBERY_2023', 
         'SHOOTING_2014', 'SHOOTING_2015', 'SHOOTING_2016', 'SHOOTING_2017',
         'SHOOTING_2018', 'SHOOTING_2019', 'SHOOTING_2020', 'SHOOTING_2021',
         'SHOOTING_2022', 'SHOOTING_2023', 
         'THEFTFROMMV_2014', 'THEFTFROMMV_2015', 'THEFTFROMMV_2016', 
         'THEFTFROMMV_2017', 'THEFTFROMMV_2018', 'THEFTFROMMV_2019', 
         'THEFTFROMMV_2020', 'THEFTFROMMV_2021', 'THEFTFROMMV_2022',
         'THEFTFROMMV_2023',
         'geometry'
  ) %>%
  # Rename attributes to match convention
  rename(ID = '_id', GEOJSON_COORDINATES = 'geometry')

cleaned_automobile_thefts_data <- cleaned_raw_csv_data %>%
  summarise(
    AUTOTHEFT_2014 = sum(AUTOTHEFT_2014, na.rm = TRUE),
    AUTOTHEFT_2015 = sum(AUTOTHEFT_2015, na.rm = TRUE),
    AUTOTHEFT_2016 = sum(AUTOTHEFT_2016, na.rm = TRUE),
    AUTOTHEFT_2017 = sum(AUTOTHEFT_2017, na.rm = TRUE),
    AUTOTHEFT_2018 = sum(AUTOTHEFT_2018, na.rm = TRUE),
    AUTOTHEFT_2019 = sum(AUTOTHEFT_2019, na.rm = TRUE),
    AUTOTHEFT_2020 = sum(AUTOTHEFT_2020, na.rm = TRUE),
    AUTOTHEFT_2021 = sum(AUTOTHEFT_2021, na.rm = TRUE),
    AUTOTHEFT_2022 = sum(AUTOTHEFT_2022, na.rm = TRUE),
    AUTOTHEFT_2023 = sum(AUTOTHEFT_2023, na.rm = TRUE)
  ) %>%
  gather(key = "year", value = "num_automobile_thefts") %>%
  mutate(year = as.numeric(sub("AUTOTHEFT_", "", year)))

cleaned_map_data <- cleaned_raw_csv_data %>%
  select(AREA_NAME, GEOJSON_COORDINATES, 
         AUTOTHEFT_2014, AUTOTHEFT_2015, AUTOTHEFT_2016, AUTOTHEFT_2017,
         AUTOTHEFT_2018, AUTOTHEFT_2019, AUTOTHEFT_2020, AUTOTHEFT_2021,
         AUTOTHEFT_2022, AUTOTHEFT_2023) %>%
  gather(key = "year", value = "num_automobile_thefts", 
         AUTOTHEFT_2014:AUTOTHEFT_2023) %>%
  mutate(year = sub("AUTOTHEFT_", "", year)) %>%
  rename(neighbourhood = 'AREA_NAME', geojson_coordinates = 'GEOJSON_COORDINATES')

cleaned_all_crime_data <- cleaned_raw_csv_data %>%
  summarise(
    across(starts_with("ASSAULT"), ~sum(., na.rm = TRUE)),
    across(starts_with("AUTOTHEFT"), ~sum(., na.rm = TRUE)),
    across(starts_with("BIKETHEFT"), ~sum(., na.rm = TRUE)),
    across(starts_with("BREAKENTER"), ~sum(., na.rm = TRUE)),
    across(starts_with("HOMICIDE"), ~sum(., na.rm = TRUE)),
    across(starts_with("ROBBERY"), ~sum(., na.rm = TRUE)),
    across(starts_with("SHOOTING"), ~sum(., na.rm = TRUE)),
    across(starts_with("THEFTFROMMV"), ~sum(., na.rm = TRUE))
  ) %>%
  pivot_longer(
    cols = everything(),
    names_to = "crime_year",
    values_to = "num_crime_occurrences"
  ) %>%
  separate(crime_year, into = c("crime_type", "year"), sep = "_") %>%
  mutate(
    year = as.numeric(year),
    crime_type = case_when(
      str_detect(crime_type, "ASSAULT") ~ "Assault",
      str_detect(crime_type, "AUTOTHEFT") ~ "Automobile-Theft",
      str_detect(crime_type, "BIKETHEFT") ~ "Bike-Theft",
      str_detect(crime_type, "BREAKENTER") ~ "Break-and-Enter",
      str_detect(crime_type, "HOMICIDE") ~ "Homicide",
      str_detect(crime_type, "ROBBERY") ~ "Robbery",
      str_detect(crime_type, "SHOOTING") ~ "Shooting",
      str_detect(crime_type, "THEFTFROMMV") ~ "Theft-from-Vehicle"
    )
  ) %>%
  select(year, crime_type, num_crime_occurrences) %>%
  arrange(year)

#### Save data ####
write_csv(cleaned_raw_csv_data, "outputs/data/cleaned_raw_data.csv")
write_csv(cleaned_automobile_thefts_data, "outputs/data/cleaned_automobile_thefts_data.csv")
write_csv(cleaned_map_data, "outputs/data/cleaned_map_data.csv")
write_csv(cleaned_all_crime_data, "outputs/data/cleaned_all_crime_data.csv")
saveRDS(raw_shapefile_data, file = "outputs/data/cleaned_map_data.rds")


