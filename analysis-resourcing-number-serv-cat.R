# Test how well resourcing predicts number of services in three categories
# Jeff Oliver
# jcoliver@arizona.edu
# 2021-06-02

library(dplyr)
library(tidyr)
library(ggplot2)
library(car)     # For outlier detection

# 1. Test to see if salaries predict number of services
# 2. Test to see if total expenditures predict number of services

services <- read.csv(file = "data/services-final.csv")
resources <- read.csv(file = "data/salaries-ipeds.csv")
serv_categories <- read.csv(file = "data/services-categories.csv")

# Ensure Category is leveled consistently with elsewhere
serv_categories$Category <- factor(x = serv_categories$Category,
                                   levels = c("Data Management",
                                              "Geospatial",
                                              "Data Science"))

# Join categories to services and drop any that could not be assigned to a 
# category
services_dist <- services %>%
  filter(!is.na(URL)) %>%
  left_join(serv_categories, by = c("Service" = "Service")) %>%
  filter(!is.na(Category)) %>%
  select(c(Institution, Category))

# Count how many in each category for each institution
services_dist <- services_dist %>%
  group_by(Institution, Category) %>%
  summarize(Category_count = n()) %>%
  ungroup() %>% # Just in case an institution had 0 for a category
  complete(Institution, Category, fill = list(Category_count = 0))

# Join the salary data
services_dist <- services_dist %>%
  left_join(resources, by = c("Institution" = "institution")) %>%
  select(-c(unitid, ipeds_institution, year))

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
  xlab(label = "Total salaries/wages ($)") +
  ylab(label = "Number of services offered") +
  theme_minimal()
print(salaries_plot)
ggsave(filename = "output/salaries-services-categories.png",
       plot = salaries_plot)

# Run glm, because Category_count is count, and should be Poisson modeled, 
# We want to know if the Category of service influences the effect of 
# resourcing, so we include an interaction term
salaries_glm <- glm(Category_count ~ salaries_wages + salaries_wages:Category,
                    data = services_dist,
                    family = "poisson")
salaries_summary <- summary(salaries_glm)
# Coefficients:
#                                         Estimate Std. Error z value Pr(>|z|)    
#   (Intercept)                          1.407e+00  1.149e-01  12.238  < 2e-16 ***
#   salaries_wages                       1.859e-08  6.019e-09   3.089  0.00201 ** 
#   salaries_wages:CategoryGeospatial   -9.623e-09  5.820e-09  -1.654  0.09821 .  
#   salaries_wages:CategoryData Science -1.979e-08  6.362e-09  -3.111  0.00187 ** 

# Yeah, but dang Stanford is way out there on the plot
# Looks like there might be some influential points (from plot), calculate 
# Cook's distance and see if any need to be excluded
salaries_cooks <- cooks.distance(salaries_glm)
cooks_cutoff <- 4 / (nrow(services_dist) - length(salaries_glm$coefficients) - 2)
influential <- which(salaries_cooks > cooks_cutoff)
services_dist$Institution[influential]

# Yeah, Stanford is pretty influential; try GLM without it
salaries_glm_ss <- glm(Category_count ~ salaries_wages + salaries_wages:Category,
                    data = services_dist %>% filter(Institution != "Stanford University"),
                    family = "poisson")
salaries_summary_ss <- summary(salaries_glm_ss)
# Coefficients:
#                                         Estimate Std. Error z value Pr(>|z|)    
#   (Intercept)                          1.416e+00  1.430e-01   9.900  < 2e-16 ***
#   salaries_wages                       1.988e-08  8.361e-09   2.378 0.017430 *  
#   salaries_wages:CategoryGeospatial   -1.110e-08  6.821e-09  -1.628 0.103615    
#   salaries_wages:CategoryData Science -2.501e-08  7.488e-09  -3.340 0.000839 ***

# Plot that graph without Stanford
salaries_plot_ss <- ggplot(data = services_dist %>% filter(Institution != "Stanford University"), 
                        mapping = aes(x = salaries_wages, 
                                      y = Category_count,
                                      color = Category)) +
  geom_point() +
  geom_smooth(method = "glm",
              method.args = list(family = "poisson"),
              se = FALSE) +
  scale_color_brewer(type = "qualitative", palette = "Set2", direction = 1) +
  xlab(label = "Total salaries/wages ($)") +
  ylab(label = "Number of services offered") +
  theme_minimal()
print(salaries_plot_ss)
ggsave(filename = "output/salaries-services-categories-ss.png",
       plot = salaries_plot_ss)

# Interpreting the Poisson model, delta y = y(e^B - 1)
# i.e. it's not linear - effect of x on y will vary based on value of y
