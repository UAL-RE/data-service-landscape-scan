# Create data files for QA/QC of initial survey
# Jeffrey C. Oliver
# jcoliver@email.arizona.edu
# 2020-07-29

rm(list = ls())

################################################################################
library(dplyr)
library(tidyr)
library(readr)
library(stringr)

responses <- readr::read_csv(file = "data/survey-form-responses-clean.csv")

# Drop Timestamp and Reviewer columns
# Convert to longer
# After conversion, drop any NA values in URL column
# De-duplicate rows
responses_long <- responses %>%
  select(-Timestamp, -Reviewer) %>%
  pivot_longer(cols = -Institution, 
                      names_to = "Service", 
                      values_to = "URL")

# Create data frame with only data that have values in URL field
services_present <- responses_long %>%
  filter(!is.na(URL)) %>%
  arrange(Institution, Service)

# Create data frame with unique URL for each service (in some cases, there 
# might be two unique URLs for a service at an institution, if the reviewers 
# found the resource at different places). Will use this to check to see if 
# service is really present at the indicated URL
services_present <- services_present[!duplicated(services_present), ]

write_csv(x = services_present, 
          path = "data/services-present.csv")

# Create data frame of those rows with NA in URL field; will want to double-
# check these to make sure service is _not_ described on website
services_absent <- responses_long %>%
  filter(is.na(URL)) %>%
  arrange(Institution, Service)

# For absent services, need to be sure neither reviewer entered a URL in the 
# field. We know this is the case if there are two rows for an Institution/
# Service combination.
# Not interested in "Other_service_*"
# Also, drop the URL column, as it is now full of NAs
services_absent <- services_absent %>%
  group_by(Institution, Service) %>%
  filter(n() > 1) %>%
  filter(str_sub(Service, start = 1, end = 13) != "Other_service")
  select(-URL)

# Drop any duplicates from absent services data frame
services_absent <- services_absent[!duplicated(services_absent), ]
services_absent

write_csv(x = services_absent, 
          path = "data/services-absent.csv")

# library(ggplot2)
# services_counts <- services_present %>%
#   select(-URL) %>%
#   distinct() %>%
#   filter(str_sub(Service, start = 1, end = 13) != "Other_service") %>%
#   group_by(Service) %>%
#   summarize(Counts = n())
# 
# ggplot(data = services_counts, mapping = aes(x = Service, y = Counts)) +
#   geom_point()
