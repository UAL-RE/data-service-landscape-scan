# Update services present
# Jeffrey C. Oliver
# jcoliver@email.arizona.edu
# 2021-01-05

rm(list = ls())

################################################################################
# Following review by Data Cooperative folks, what services are confirmed to 
# be present? From this list, create the converse: a list of services *not* 
# present at each institution's library. This latter list will be used to 
# confirm that the service is not provided by the institution's library

library(dplyr)
library(readr)

qc_files <- list.files(path = "data/services-qc", 
                       pattern = "services-present-*",
                       full.names = TRUE)

# Load each QC file into memory. Update the "Present" column to be logical, 
# rather than character (start by making "Present" column all lower case via
# tolower)

# Data frame to hold results
services <- NULL
for (qc_file in qc_files) {
  qc_check <- read_csv(file = qc_file, col_types = cols())
  qc_check$Present <- tolower(qc_check$Present)
  qc_check <- qc_check %>%
    mutate(Present_logical = if_else(condition = Present == "y",
                                     true = TRUE,
                                     false = FALSE))

  if (is.null(services)) {
    services <- qc_check
  } else {
    services <- bind_rows(services, qc_check)
  }
}

# Only consider present if BOTH reviewers found it. First filter to only those
# services that were found by at least one reviewer
services_present <- services %>%
  filter(Present_logical == TRUE) %>%
  select(-Present, -Notes) %>%
  group_by(Institution, Service, URL) %>%
  summarise(times_found = n()) %>%
  filter(times_found == 2) %>%
  select(-times_found)

# Previous implementation only required one reviewer to have found
# Filter those determined to be present and remove duplicates
# services_present <- services %>%
#   filter(Present_logical == TRUE) %>%
#   distinct()

# Write to updated services present file
write_csv(x = services_present, 
          file = "data/services-present-confirmed.csv")

################################################################################
# Use the confirmed present with initial data files (services-present.csv and 
# services-absent.csv) to create list of all those service/institution 
# combinations which do not offer service

services_present_initial <- read_csv(file = "data/services-present.csv", 
                                     col_types = cols())
services_present_initial <- services_present_initial %>%
  select(-Times_found)
services_absent_initial <- read_csv(file = "data/services-absent.csv",
                                    col_types = cols())
# Combine these initial absent and present lists to get a complete matrix of 
# Institution/Service
services_inital <- bind_rows(services_present_initial, services_absent_initial) %>%
  select(Institution, Service) %>%
  distinct()

# Will only need two columns for set difference
services_present <- services_present %>%
  select(Institution, Service)

# Do set difference to see which Institution/Service combinations are in 
# services_initial (I) but not in services_present (P). A = I \ P
services_absent <- dplyr::setdiff(services_inital, services_present) %>%
  distinct() %>%
  arrange(Institution, Service)

write_csv(x = services_absent,
          file = "data/services-absent-to-confirm.csv")
