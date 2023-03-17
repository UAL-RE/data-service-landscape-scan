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
library(stringr)

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

# For some of the services, we have the delivery mode included, too 
# (instruction, consult, web) as part of the Service name. Want to separate 
# that information into a separate column, "Mode"
services <- services %>%
  mutate(Mode = case_when(str_detect(string = Service, 
                                    pattern = "_instruction$") ~ "Instruction",
                          str_detect(string = Service,
                                    pattern = "_consult$") ~ "Consult",
                          str_detect(string = Service,
                                    pattern = "_web") ~ "Web",
                          TRUE ~ NA_character_)) %>%
  # no need to include mode in service at this point, so remove from Service
  mutate(Service = str_replace(string = Service, 
                               pattern = "_instruction$|_consult$|_web$",
                               replacement = "")) %>%
  # Rearrange columns, so URL is at end
  select(Institution, Service, Mode, URL)
  
write.csv(x = services, 
          file = "data/services-final.csv", 
          row.names = FALSE)
