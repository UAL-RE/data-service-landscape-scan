# Test how well resourcing predicts number of services in three categories
# Jeff Oliver
# jcoliver@arizona.edu
# 2022-08-02

library(dplyr)
library(tidyr)
library(ggplot2)
library(lme4)

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
       plot = salaries_plot,
       width = 6.5, height = 2.5, units = "in")

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

# Another outlier method is Cook's distance
salaries_cooks <- cooks.distance(salaries_glm)
cooks_cutoff <- 4 / (nrow(services_dist) - length(salaries_glm$coefficients) - 2)
influential <- which(salaries_cooks > cooks_cutoff)
services_dist$Institution[influential]
# Here Stanford *is* influential. Try GLM without it

# GLM sans Stanford
salaries_ss_glm <- glm(Category_count ~ salaries_wages * Category,
                    data = services_dist %>% filter(Institution != "Stanford University"),
                    family = "poisson")
salaries_ss_summary <- summary(salaries_ss_glm)
# Coefficients:
#                                        Estimate Std. Error z value Pr(>|z|)    
#   (Intercept)                          1.353257   0.280667   4.822 1.42e-06 ***
#   salaries_wages                      -0.003283   0.015866  -0.207   0.8361    
#   CategoryGeospatial                  -0.654145   0.441148  -1.483   0.1381    
#   CategoryData Science                -0.861274   0.451932  -1.906   0.0567 .  
#   salaries_wages:CategoryGeospatial    0.017163   0.024271   0.707   0.4795    
#   salaries_wages:CategoryData Science  0.025185   0.024526   1.027   0.3045  

# dfbetas
salaries_ss_dfbeta <- dfbeta(salaries_ss_glm)
dfbeta_ss_thresh <- 2/sqrt(nrow(services_dist))

# See if any are beyond the threshold
any(abs(salaries_ss_dfbeta) > dfbeta_ss_thresh)
# TRUE, need to see which ones and exclude them
salaries_influential <- which(rowSums(abs(salaries_ss_dfbeta) > dfbeta_ss_thresh) > 0)

# Print out rows that are influential
services_dist %>%
  filter(Institution != "Stanford University") %>%
  slice(salaries_influential) %>%
  select(Institution, Category)
# 1 Washington State University Geospatial  
# 2 Washington State University Data Science

# Run analysis without those rows (exclude Stanford, too)
services_dist_so <- services_dist %>%
  filter(Institution != "Stanford University") %>%
  slice(-salaries_influential)
  
salaries_so_glm <- glm(Category_count ~ salaries_wages * Category,
                               data = services_dist_so,
                               family = "poisson")
salaries_so_summary <- summary(salaries_so_glm)
# Coefficients:
#                                        Estimate Std. Error z value Pr(>|z|)    
#   (Intercept)                          1.353257   0.280667   4.822 1.42e-06 ***
#   salaries_wages                      -0.003283   0.015866  -0.207    0.836    
#   CategoryGeospatial                  -0.458742   0.453020  -1.013    0.311    
#   CategoryData Science                -0.680126   0.463495  -1.467    0.142    
#   salaries_wages:CategoryGeospatial    0.008022   0.025002   0.321    0.748    
#   salaries_wages:CategoryData Science  0.016864   0.025215   0.669    0.504    

# dfbetas
salaries_so_dfbeta <- dfbeta(salaries_so_glm)
dfbeta_so_thresh <- 2/sqrt(nrow(services_dist_so))

# See if any are beyond the threshold
any(abs(salaries_so_dfbeta) > dfbeta_so_thresh)
# FALSE

################################################################################
# Aside: Since we have multiple observations per institution, there is an issue 
# of non-independence among the observation. Probably want to include 
# Institution as a random effect

# Will send a warning: 
# boundary (singular) fit: see help('isSingular')
# Mostly likely due to random effects variance estimate = 0; only real concern 
# is that it reduces power of test
salaries_glmer <- glmer(Category_count ~ salaries_wages * Category + (1|Institution),
                        data = services_dist,
                        family = "poisson")
salaries_glmer_summary <- summary(salaries_glmer)
# Fixed effects:
#                                         Estimate Std. Error z value Pr(>|z|)    
#   (Intercept)                          1.3110817  0.2319923   5.651 1.59e-08 ***
#   salaries_wages                      -0.0004611  0.0117101  -0.039   0.9686    
#   CategoryGeospatial                  -0.4702112  0.3621777  -1.298   0.1942    
#   CategoryData Science                -0.7797417  0.3616707  -2.156   0.0311 *  
#   salaries_wages:CategoryGeospatial    0.0051134  0.0180119   0.284   0.7765    
#   salaries_wages:CategoryData Science  0.0198734  0.0172195   1.154   0.2484

salaries_glmer_influ <- influence(salaries_glmer)
salaries_glmer_dfbeta <- dfbeta(salaries_glmer_influ)
dfbeta_glmer_thresh <- 2/sqrt(nrow(services_dist))

# See if any are beyond the threshold
any(abs(salaries_glmer_dfbeta) > dfbeta_glmer_thresh)
# FALSE

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