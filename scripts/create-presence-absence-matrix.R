# Create presence absence matrix
# Jeff Oliver
# jcoliver@arizona.edu
# 2021-05-12

library(dplyr)
library(tidyr)
library(readr)

# Create a matrix of services, with one row for each institution and a column 
# for each service indicating absence (0) or presence (0)

services <- read_csv(file = "data/services-final.csv")

# Incoming data use the presence of URL to indicate presence, while a missing 
# URL (NA) indicates absent
# Some services were separated into different modes (consult, instruction, web),
# so we need to collapse across these modes and count the service as "present" 
# if the institution offers the service in any mode
services_pa <- services %>%
  select(-Mode) %>%
  mutate(pa = if_else(condition = is.na(URL), 
                      true = 0,
                      false = 1)) %>%
  group_by(Institution, Service) %>%
  summarize(pa_sum = sum(pa)) %>%
  mutate(PA = if_else(condition = pa_sum == 0,
                      true = 0,
                      false = 1)) %>%
  select(-pa_sum) %>%
  ungroup() %>%
  pivot_wider(id_cols = Institution, names_from = Service, values_from = PA)

write_csv(x = services_pa, file = "data/services-final-pa.csv")
