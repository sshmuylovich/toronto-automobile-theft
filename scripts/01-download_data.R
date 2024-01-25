#### Preamble ####
# Purpose: Downloads and saves the data from the opendatatoronto package.
# Author: Sima Shmuylovich
# Date: 25 January 2024
# Contact: sima.shmuylovich@mail.utoronto.ca
# License: MIT
# Pre-requisites: Run "import_packages.R"

#### Workspace setup ####
library(opendatatoronto)
library(tidyverse)
library(sf)

#### Download data ####
csv_data <-
  search_packages("Neighbourhood Crime Rates") %>%
  list_package_resources() %>%
  filter(name == "neighbourhood-crime-rates") %>%
  get_resource()

shapefile_data <- get_resource("c6606c1a-b7bd-4ce1-ac88-bf5f0b2fb98b")

#### Saving the Datasets ####
write_csv(csv_data, "inputs/data/unedited_data.csv")
saveRDS(shapefile_data, "inputs/data/unedited_data.rds")







         
