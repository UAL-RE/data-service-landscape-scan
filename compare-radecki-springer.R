# Some minor comparisons with Radecki & Springer 2020
# Jeff Oliver
# jcoliver@arizona.edu
# 2022-09-07

########################################
# Start by calculating numbers of services for those institutions that were 
# present in both studies
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

########################################
# Also calculate the % of institutions with consult/training for stats and 
# geospatial for *all* institutions in R&S 2020
rs <- read.csv(file = "data/Radecki-Springer-R1-raw.csv")
geo_consult <- length(grep(pattern = "Geospatial",
                           x = rs$Library..Consulting.))
geo_training <- length(grep(pattern = "Geospatial",
                            x = rs$Library..Training.Events.))
stat_consult <- length(grep(pattern = "Statistical",
                           x = rs$Library..Consulting.))
stat_training <- length(grep(pattern = "Statistical",
                            x = rs$Library..Training.Events.))
rs_summary <- data.frame(service = c("Geospatial", "Geospatial", "Statistical", "Statistical"),
                         modality = c("Consult", "Training", "Consult", "Training"),
                         counts = c(geo_consult, geo_training, stat_consult, stat_training))
rs_summary$perc <- rs_summary$counts/nrow(rs)
