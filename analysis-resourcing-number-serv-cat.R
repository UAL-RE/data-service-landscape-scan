# Test how well salaries predict number of services in three categories
# Jeff Oliver
# jcoliver@arizona.edu
# 2021-06-02

library(dplyr)
library(tidyr)
library(ggplot2)

# 1. Test to see if salaries predict number of services
# 2. Test to see if total expenditures predict number of services

services <- read.csv(file = "data/services-final.csv")
serv_categories <- read.csv(file = "data/services-categories.csv")
resources <- read.csv(file = "data/salaries-ipeds.csv")

# Ensure Category is leveled consistently with elsewhere
serv_categories$Category <- factor(x = serv_categories$Category,
                                   levels = c("Data Management",
                                              "Geospatial",
                                              "Data Science"))

# Join categories to services and drop any that could not be assigned to a 
# category; remove duplicate entries (i.e. services offered in multiple modes)
services_cats <- services %>%
  filter(!is.na(URL)) %>%
  left_join(serv_categories, by = c("Service" = "Service")) %>%
  filter(!is.na(Category)) %>%
  select(c(Institution, Service, Category)) %>%
  distinct()

# Count how many in each category for each institution
services_counts <- services_cats %>%
  group_by(Institution, Category) %>%
  summarize(Category_count = n()) %>%
  ungroup() %>% # Just in case an institution had 0 for a category
  complete(Institution, Category, fill = list(Category_count = 0))

# Join the salary data
# Easier to interpret if salaries are in millions of dollars
services_dist <- services_counts %>%
  left_join(resources, by = c("Institution" = "institution")) %>%
  select(-c(unitid, ipeds_institution, year)) %>%
  mutate(salaries_wages = salaries_wages/1e6,
         total_expenditures = total_expenditures/1e6)

# 1. Test to see if salaries predict number of services

# Plot the data to see how they look
salaries_plot <- ggplot(data = services_dist, 
                        mapping = aes(x = salaries_wages, 
                                      y = Category_count,
                                      color = Category)) +
  geom_point() +
  geom_smooth(method = "glm",
              method.args = list(family = "poisson"),
              se = FALSE) +
  scale_color_brewer(type = "qualitative", palette = "Set2", direction = 1) +
  xlab(label = "Total salaries/wages ($M)") +
  ylab(label = "Number of services offered") +
  theme_minimal()
print(salaries_plot)
ggsave(filename = "output/salaries-services-categories.png",
       plot = salaries_plot)

# Run glm, because Category_count is count, and should be Poisson modeled, 
# We want to know if the Category of service influences the effect of 
# resourcing, so we include an interaction term
salaries_glm <- glm(Category_count ~ salaries_wages * Category,
                    data = services_dist,
                    family = "poisson")
salaries_summary <- summary(salaries_glm)
# Coefficients:
#                                         Estimate Std. Error z value Pr(>|z|)    
#   (Intercept)                          1.3110817  0.2319880   5.652 1.59e-08 ***
#   salaries_wages                      -0.0004611  0.0117098  -0.039   0.9686    
#   CategoryGeospatial                  -0.4702112  0.3621726  -1.298   0.1942    
#   CategoryData Science                -0.7797417  0.3616633  -2.156   0.0311 *  
#   salaries_wages:CategoryGeospatial    0.0051134  0.0180116   0.284   0.7765    
#   salaries_wages:CategoryData Science  0.0198734  0.0172192   1.154   0.2484 

# dfbetas, which look at influence of each point on each coefficient estimate
salaries_dfbeta <- dfbeta(salaries_glm)
dfbeta_thresh <- 2/sqrt(nrow(services_dist))

# See if any are beyond the threshold
any(abs(salaries_dfbeta) > dfbeta_thresh)
# FALSE
# Nope, we're good

################################################################################
# 2. Test to see if total expenditures predict number of services

# Plot the data to see how they look
expenditures_plot <- ggplot(data = services_dist, 
                        mapping = aes(x = total_expenditures, 
                                      y = Category_count,
                                      color = Category)) +
  geom_point() +
  geom_smooth(method = "glm",
              method.args = list(family = "poisson"),
              se = FALSE) +
  scale_color_brewer(type = "qualitative", palette = "Set2", direction = 1) +
  xlab(label = "Total expenditures ($M)") +
  ylab(label = "Number of services offered") +
  theme_minimal()
print(expenditures_plot)

# Run glm, because Category_count is count, and should be Poisson modeled, 
# We want to know if the Category of service influences the effect of 
# resourcing, so we include an interaction term
expenditures_glm <- glm(Category_count ~ total_expenditures * Category,
                    data = services_dist,
                    family = "poisson")
expenditures_summary <- summary(expenditures_glm)
# Coefficients:
#                                             Estimate Std. Error z value Pr(>|z|)    
#   (Intercept)                              1.3364138  0.2481721   5.385 7.24e-08 ***
#   total_expenditures                      -0.0007516  0.0050710  -0.148   0.8822    
#   CategoryGeospatial                      -0.4838745  0.3869001  -1.251   0.2111    
#   CategoryData Science                    -0.7822507  0.3851579  -2.031   0.0423 *  
#   total_expenditures:CategoryGeospatial    0.0023425  0.0077910   0.301   0.7637    
#   total_expenditures:CategoryData Science  0.0080279  0.0074784   1.073   0.2831

# dfbetas, for assessing undue influence
expenditures_dfbeta <- dfbeta(expenditures_glm)
expenditures_thresh <- 2/sqrt(nrow(services_dist))

# See if any are beyond the threshold
any(abs(expenditures_dfbeta) > expenditures_thresh)
# Yes, some influential points
influential <- which(rowSums(abs(expenditures_dfbeta) > expenditures_thresh) > 0)
# Print out rows that are influential
services_dist %>%
  slice(influential) %>%
  select(Institution, Category)
# 1 Washington State University Geospatial

# Run analysis without that row
services_dist_subset <- services_dist %>%
  slice(-influential)

# Run glm on the reduced data set
expenditures_glm_subset <- glm(Category_count ~ total_expenditures * Category,
                        data = services_dist_subset,
                        family = "poisson")
expenditures_summary_subset <- summary(expenditures_glm_subset)
# Coefficients
#                                             Estimate Std. Error z value Pr(>|z|)    
#   (Intercept)                              1.3364138  0.2481721   5.385 7.24e-08 ***
#   total_expenditures                      -0.0007516  0.0050710  -0.148   0.8822    
#   CategoryGeospatial                      -0.3135534  0.3999533  -0.784   0.4331    
#   CategoryData Science                    -0.7822507  0.3851579  -2.031   0.0423 *  
#   total_expenditures:CategoryGeospatial   -0.0005129  0.0081040  -0.063   0.9495    
#   total_expenditures:CategoryData Science  0.0080279  0.0074784   1.073   0.2831  

# Check for influence
# dfbetas, for assessing undue influence
expenditures_dfbeta_subset <- dfbeta(expenditures_glm_subset)
expenditures_thresh_subset <- 2/sqrt(nrow(services_dist_subset))

# See if any are beyond the threshold
any(abs(expenditures_dfbeta_subset) > expenditures_thresh_subset)
# Nope, stop here