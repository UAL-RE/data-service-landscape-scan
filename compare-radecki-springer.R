# Some minor comparisons with Radecki & Springer 2020
# Jeff Oliver
# jcoliver@arizona.edu
# 2022-09-07

pa <- read.csv(file = "data/services-final-pa.csv")

# Vector of the institutions that overlap between this work and R&S 2020
common_institutions <- c(3, 8, 9, 10, 11, 15, 25)

# Our categories of geospatial services
geo_services <- c("Aerial_imagery", "Geospatial", "Geospatial_software")

# See which ones offered any geo services
any_geo <- rowSums(x = pa[common_institutions, geo_services]) > 0

# See which ones offered statistical consulting
any_stats <- pa$Statistical_consulting[common_institutions]

# Collate results
compare <- data.frame(Institution = pa$Institution[common_institutions], 
                      Geospatial = any_geo, 
                      Statistics = any_stats > 0)
