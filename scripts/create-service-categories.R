# Create file linking data services to broad categories
# Jeff Oliver
# jcoliver@arizona.edu
# 2022-08-16

library(dplyr)

services <- read.csv(file = "data/services-final.csv")

service_cats <- data.frame(Service = unique(services$Service),
                           Category = NA)

# Create vectors of the three different service categories
geospatial <- c("Aerial_imagery", "Geospatial", "Geospatial_software")
data_management <- c("Data_management_plans", "Data_management", 
                     "Data_repository", "DMPTool")
data_science <- c("Data_analysis", "Data_visualization_software", 
                  "Text_data_mining", "Statistical_consulting")

service_cats <- service_cats %>%
  mutate(Category = case_when(Service %in% geospatial ~ "Geospatial",
                              Service %in% data_management ~ "Data Management",
                              Service %in% data_science ~ "Data Science"))

write.csv(x = service_cats,
          file = "data/services-categories.csv",
          row.names = FALSE)
