# Test how well resourcing predicts number of services
# Jeff Oliver
# jcoliver@arizona.edu
# 2021-06-02

library(dplyr)
library(tidyr)
library(ggplot2)
library(car)     # For outlier detection

# 1. Test to see if salaries predict number of services
# 2. Test to see if total expenditures predict number of services

services <- read.csv(file = "data/services-final-pa.csv")
resources <- read.csv(file = "data/salaries-ipeds.csv")

# Count number of institutions that offer each service and join with salary data
services_dist <- services %>% 
  rowwise(Institution) %>% 
  mutate(Service_count = sum(c_across(Aerial_imagery:Web_scraping))) %>%
  select(Institution, Service_count) %>%
  left_join(resources, by = c("Institution" = "institution"))

# May want to see which point is UArizona, so add column with that information
services_dist <- services_dist %>%
  mutate(UArizona = if_else(condition = Institution == "University of Arizona",
                            true = TRUE,
                            false = FALSE))

# 1. Test to see if salaries predict number of services

# Plot the data to see how they look
salaries_plot <- ggplot(data = services_dist, 
                        mapping = aes(x = salaries_wages, 
                                      y = Service_count)) +
  geom_point() +
  # geom_smooth(method = "lm", se = FALSE) +
  geom_smooth(method = "glm", 
              method.args = list(family = "poisson"), 
              se = FALSE) +
  xlab(label = "Total salaries/wages ($)") +
  ylab(label = "Number of services offered") +
  theme_minimal()
print(salaries_plot)
ggsave(filename = "output/salaries-services.png",
       plot = salaries_plot)

# Run glm, because Service_count is count, and should be Poisson modeled
salaries_glm <- glm(Service_count ~ salaries_wages,
                    data = services_dist,
                    family = "poisson")
salaries_summary <- summary(salaries_glm)
# Coefficients:
#                   Estimate Std. Error z value Pr(>|z|)    
#   (Intercept)    2.134e+00  1.339e-01  15.942   <2e-16 ***
#   salaries_wages 1.261e-08  6.321e-09   1.995   0.0461 *  

# Interpreting the Poisson model, delta y = y(e^B - 1)
# i.e. it's not linear - effect of x on y will vary based on value of y

# Looks like there might be some influential points (from plot), calculate 
# Cook's distance and see if any need to be excluded
salaries_cooks <- cooks.distance(salaries_glm)
cooks_cutoff <- 4 / (nrow(services_dist) - length(salaries_glm$coefficients) - 2)
influential <- which(salaries_cooks > cooks_cutoff)
services_dist$Institution[influential]
# None. We're good. Make a copy of the plot showing where UA is.

salaries_plot_az <- ggplot(data = services_dist %>% arrange(UArizona),
                           mapping = aes(x = salaries_wages/1e6, 
                                         y = Service_count)) +
  geom_point(size = 2.0, mapping = aes(color = UArizona, shape = UArizona)) +
  scale_shape_manual(values = c(16, 17)) + # circle, triangle
  scale_color_manual(values = c("#555555", "#FF0000")) +
  geom_smooth(method = "glm", 
              method.args = list(family = "poisson"), 
              se = FALSE) +
  xlab(label = "Total salaries/wages (Million $)") +
  ylab(label = "Number of services offered") +
  theme_minimal() +
  theme(legend.position = "none")
print(salaries_plot_az)
ggsave(file = "output/salaries-services-az.png",
       plot = salaries_plot_az,
       width = 6.5, height = 2.5, units = "in")

# 2. Test to see if total expenditures predict number of services

# Start with plot to eyeball linearity
expenditures_plot <- ggplot(data = services_dist, 
                            mapping = aes(x = total_expenditures/1e6, 
                                          y = Service_count)) +
  geom_point() +
  geom_smooth(method = "glm", 
              method.args = list(family = "poisson"),
              se = FALSE) +
  xlab(label = "Total expenditures (Million $)") +
  ylab(label = "Number of services offered") +
  theme_bw()
print(expenditures_plot)

# Run glm
expenditures_glm <- glm(Service_count ~ total_expenditures, 
                        data = services_dist,
                        family = "poisson")
expenditures_summary <- summary(expenditures_glm)
# Coefficients:
#                       Estimate Std. Error z value Pr(>|z|)    
#   (Intercept)        2.161e+00  1.427e-01   15.14   <2e-16 ***
#   total_expenditures 4.460e-09  2.753e-09    1.62    0.105 

# Not significant, stop here