# Create salary data corrected for county employment cost
# Jeff Oliver
# jcoliver@arizona.edu
# 2022-08-09

library(dplyr)

# Uses salary data from IPEDS and attempts to control for the cost of labor by 
# dividing by the average salary for the city/county/region where the campus
# is located
# annual average wages per employee is in the column called avg_annual_pay. 
# Use only the values where own_code = 0 (which is for all types of employers, 
# public and private companies)

institutions <- read.csv(file = "data/institutions.csv")
ipeds <- read.csv(file = "data/salaries-ipeds.csv")
wages <- read.csv(file = "eda/bls-wages-2019.csv")

# Create a data frame with wages and BLS area identifier
corr_salaries <- ipeds %>%
  left_join(institutions, by = c("institution" = "Institution")) %>%
  select(c(institution, salaries_wages, total_expenditures, BLS_area))

# Reduce wages data frame to only those where own_code = 0 (which is for all 
# types of employers, public and private companies)
wages <- wages %>%
  filter(own_code == 0)

# Join the BLS wage information to the new df based on the BLS area identifier
corr_salaries <- corr_salaries %>%
  left_join(wages, by = c("BLS_area" = "area_fips")) %>%
  select(c(institution, salaries_wages, total_expenditures, avg_annual_pay))

# Write this to a file
write.csv(x = corr_salaries,
          file = "eda/salaries-ipeds-CoL.csv",
          row.names = FALSE)
