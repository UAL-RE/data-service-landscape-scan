# Assemble single data file with data on services / institutions
# Jeff Oliver
# jcoliver@arizona.edu
# 2021-04-01

# These data are:
# + The suite of services confirmed as present following review by Data 
#   Cooperative team; data/services-present-confirmed.csv
# + Any additional services initially categorized as absent; 
#   data/services-additional.csv

library(dplyr)
library(readr)

confirmed <- read_csv(file = "data/services-present-confirmed.csv")
additional <- read_csv(file = "data/services-additional.csv")

# Some of the services are duplicated (http vs. https is one) in confirmed, 
# restrict to first instance
confirmed_aggregate <- confirmed %>%
  group_by(Institution, Service) %>%
  summarize(URL = paste(URL, collapse = "; ")) %>%
  ungroup()

# Drop the "Notes" column from the additional data
additional <- additional %>%
  select(-Notes)

# Bind the two data sets together
if (all(colnames(confirmed_aggregate) == colnames(additional))) {
  services <- bind_rows(confirmed_aggregate, additional)
} else {
  message("Column mismatch between two data sets. Forgoing bind.")
}

if (any(table(services$Institution, services$Service) != 1)) {
  message("Did not find exactly 1 row for each Institution / Service combo.")
}

if (nrow(services) != (29 * 25)) {
  message("Number of rows not 29 * 25")
}

write.csv(x = services, 
          file = "data/services-final.csv", 
          row.names = FALSE)
