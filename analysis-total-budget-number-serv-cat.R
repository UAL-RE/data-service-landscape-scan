# Test how well total expenditures predict number of services in three categories
# Jeff Oliver
# jcoliver@arizona.edu
# 2021-06-02

library(dplyr)
library(tidyr)
library(ggplot2)

# 1. Test to see if total expenditures predict number of services
# 2. Test to see if total expenditures corrected for cost of labor predict 
# number of services

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

# Join the salary data (for uncorrected salaries)
# Easier to interpret if salaries are in millions of dollars
services_dist <- services_counts %>%
  left_join(resources, by = c("Institution" = "institution")) %>%
  select(-c(unitid, ipeds_institution, year)) %>%
  mutate(total_expenditures = total_expenditures/1e6)

# 1. Test to see if salaries predict number of services

# Plot the data to see how they look
expend_plot <- ggplot(data = services_dist, 
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
print(expend_plot)

# Run glm, because Category_count is count, and should be Poisson modeled, 
# We want to know if the Category of service influences the effect of 
# resourcing, so we include an interaction term
expend_glm <- glm(Category_count ~ total_expenditures * Category,
                    data = services_dist,
                    family = "poisson")
expend_summary <- summary(expend_glm)
# Coefficients:
#                                             Estimate Std. Error z value Pr(>|z|)    
#   (Intercept)                              1.3364138  0.2481721   5.385 7.24e-08 ***
#   total_expenditures                      -0.0007516  0.0050710  -0.148   0.8822    
#   CategoryGeospatial                      -0.4838745  0.3869001  -1.251   0.2111    
#   CategoryData Science                    -0.7822507  0.3851579  -2.031   0.0423 *  
#   total_expenditures:CategoryGeospatial    0.0023425  0.0077910   0.301   0.7637    
#   total_expenditures:CategoryData Science  0.0080279  0.0074784   1.073   0.2831  

# dfbetas, which look at influence of each point on each coefficient estimate
expend_dfbeta <- dfbeta(expend_glm)
dfbeta_thresh <- 2/sqrt(nrow(services_dist))

# See if any are beyond the threshold
any(abs(expend_dfbeta) > dfbeta_thresh)
# TRUE Gotta look closer

# Create data frame and plot for quick investigation
expend_dfbeta_df <- as.data.frame(expend_dfbeta)
expend_dfbeta_long <- expend_dfbeta_df %>%
  mutate(ID = rownames(expend_dfbeta_df)) %>%
  pivot_longer(cols = -c(ID),
               names_to = "Coefficient",
               values_to = "Delta")
expend_dfbeta_plot <- ggplot(data = expend_dfbeta_long,
                             mapping = aes(x = ID, y = Delta)) +
  geom_point() +
  geom_hline(yintercept = dfbeta_thresh) +
  geom_hline(yintercept = -dfbeta_thresh) +
  theme_minimal() +
  facet_wrap(~ Coefficient)
print(expend_dfbeta_plot)
# One point: CategoryGeospatial
# Which row of our data do these correspond to?
influential <- which(rowSums(abs(expend_dfbeta) > dfbeta_thresh) > 0)
# print out the institution / category
services_dist %>%
  select(c(Institution, Category)) %>%
  slice(influential)
# Try analyses without those influential points
services_sans_outliers <- services_dist[-influential, ]

# Run GLM
expend_so_glm <- glm(Category_count ~ total_expenditures * Category,
                           data = services_sans_outliers,
                           family = "poisson")
expend_so_glm_summary <- summary(expend_so_glm)
# Coefficients:
#                                             Estimate Std. Error z value Pr(>|z|)    
#   (Intercept)                              1.3364138  0.2481721   5.385 7.24e-08 ***
#   total_expenditures                      -0.0007516  0.0050710  -0.148   0.8822    
#   CategoryGeospatial                      -0.3135534  0.3999533  -0.784   0.4331    
#   CategoryData Science                    -0.7822507  0.3851579  -2.031   0.0423 *  
#   total_expenditures:CategoryGeospatial   -0.0005129  0.0081040  -0.063   0.9495    
#   total_expenditures:CategoryData Science  0.0080279  0.0074784   1.073   0.2831  

# dfbetas again
expend_so_dfbeta <- dfbeta(expend_so_glm)
dfbeta_so_thresh <- 2/sqrt(nrow(services_sans_outliers))

# See if any are beyond the threshold
any(abs(expend_so_dfbeta) > dfbeta_so_thresh)
# FALSE
# Nope, we're good

################################################################################
# Now see what analyses look like when expenditures are corrected for variation 
# in cost of labor among cities
cor_resources <- read.csv(file = "data/salaries-ipeds-CoL.csv")

# Join the salary data (for corrected salaries)
services_dist_cor <- services_counts %>%
  left_join(cor_resources, by = c("Institution" = "institution")) %>%
  mutate(corrected_expenditures = total_expenditures / avg_annual_pay)

# Plot the data to see how they look
expend_cor_plot <- ggplot(data = services_dist_cor, 
                        mapping = aes(x = corrected_expenditures, 
                                      y = Category_count,
                                      color = Category)) +
  geom_point() +
  geom_smooth(method = "glm",
              method.args = list(family = "poisson"),
              se = FALSE) +
  scale_color_brewer(type = "qualitative", palette = "Set2", direction = 1) +
  xlab(label = "Total expenditures corrected for cost of labor") +
  ylab(label = "Number of services offered") +
  theme_minimal()
print(expend_cor_plot)

# Run GLM
expend_cor_glm <- glm(Category_count ~ corrected_expenditures * Category,
                    data = services_dist_cor,
                    family = "poisson")
expend_cor_summary <- summary(expend_cor_glm)
# Coefficients:
#                                                 Estimate Std. Error z value Pr(>|z|)    
#   (Intercept)                                  1.3788290  0.3174956   4.343 1.41e-05 ***
#   corrected_expenditures                      -0.0001049  0.0004161  -0.252    0.801    
#   CategoryGeospatial                          -0.6926699  0.5054770  -1.370    0.171    
#   CategoryData Science                        -0.6609313  0.5088563  -1.299    0.194    
#   corrected_expenditures:CategoryGeospatial    0.0004277  0.0006492   0.659    0.510    
#   corrected_expenditures:CategoryData Science  0.0003418  0.0006561   0.521    0.602  

# dfbetas, which look at influence of each point on each coefficient estimate
expend_cor_dfbeta <- dfbeta(expend_cor_glm)
dfbeta_cor_thresh <- 2/sqrt(nrow(services_dist_cor))

# See if any are beyond the threshold
any(abs(expend_cor_dfbeta) > dfbeta_cor_thresh)
# TRUE

# Create data frame and plot for quick investigation
expend_cor_dfbeta_df <- as.data.frame(expend_cor_dfbeta)
expend_cor_dfbeta_long <- expend_cor_dfbeta_df %>%
  mutate(ID = rownames(expend_cor_dfbeta_df)) %>%
  pivot_longer(cols = -c(ID),
               names_to = "Coefficient",
               values_to = "Delta")
expend_cor_dfbeta_plot <- ggplot(data = expend_cor_dfbeta_long,
                          mapping = aes(x = ID, y = Delta)) +
  geom_point() +
  geom_hline(yintercept = dfbeta_cor_thresh) +
  geom_hline(yintercept = -dfbeta_cor_thresh) +
  theme_minimal() +
  facet_wrap(~ Coefficient)
print(expend_cor_dfbeta_plot)
# Three points, two CategoryData Science, one CategoryGeospatial
# Which rows of our data do these correspond to?
influential <- which(rowSums(abs(expend_cor_dfbeta) > dfbeta_cor_thresh) > 0)
# print out the institution / category
services_dist_cor %>%
  select(c(Institution, Category)) %>%
  slice(influential)

# Try analyses without those influential points
services_sans_outliers <- services_dist_cor[-influential, ]

# Run GLM
expend_cor_so_glm <- glm(Category_count ~ corrected_expenditures * Category,
                        data = services_sans_outliers,
                        family = "poisson")
expend_cor_so_summary <- summary(expend_cor_so_glm)
# Coefficients:
#                                                 Estimate Std. Error z value Pr(>|z|)    
#   (Intercept)                                  1.3788290  0.3174956   4.343 1.41e-05 ***
#   corrected_expenditures                      -0.0001049  0.0004161  -0.252    0.801    
#   CategoryGeospatial                          -0.4519216  0.5188625  -0.871    0.384    
#   CategoryData Science                        -0.5906870  0.5305195  -1.113    0.266    
#   corrected_expenditures:CategoryGeospatial    0.0001560  0.0006678   0.234    0.815    
#   corrected_expenditures:CategoryData Science  0.0003595  0.0006865   0.524    0.601 

# dfbetas again
expend_cor_so_dfbeta <- dfbeta(expend_cor_so_glm)
dfbeta_cor_so_thresh <- 2/sqrt(nrow(services_sans_outliers))

# See if any are beyond the threshold
any(abs(expend_cor_so_dfbeta) > dfbeta_cor_so_thresh)
# FALSE, so we can stop here
