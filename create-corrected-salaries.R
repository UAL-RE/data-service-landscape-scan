# Create salary data corrected for county employment cost
# Jeff Oliver
# jcoliver@arizona.edu
# 2022-08-09

library(dplyr)

# Output should be:
# Institution salaries_corr total_corr
# where the latter two come from data/salaries-ipeds.csv, but are divided by 
# the annual average wages per employee, which is in the column called
# avg_annual_pay. Use the values where own_code = 0 (which is for all types of 
# employers, public and private companies)

# TODO: There are non-county level entries in the bls-wages data set, might 
# be worth looking to see if we can use those...

potential_metro_rows <- c()
for (ci in institutions$City) {
  grepped <- grep(pattern = ci, x = area_codes$area_title)
  potential_metro_rows <- c(potential_metro_rows, grepped)
}
potential_metros <- area_codes[potential_metro_rows, ]

city_states <- unique(paste0(institutions$City, ", ", institutions$State))
city_rows <- c()
for (cs in city_states) {
  grepped <- grep(pattern = cs, x = area_codes$area_title)
  city_rows <- c(city_rows, grepped)
}
area_codes[city_rows, ]

institutions <- read.csv(file = "data/institutions.csv")
ipeds <- read.csv(file = "data/salaries-ipeds.csv")
wages <- read.csv(file = "data/bls-wages-2019.csv")
area_codes <- read.csv(file = "data/bls-area-titles.csv")

# Only want info for *all* employer types (public, private, govt)
wages <- wages %>%
  filter(own_code == 0)

# Need to make sure area code strings have leading zero when <10,000 (as they 
# are in the wages object)

# Join in area name from area_codes to wages object
