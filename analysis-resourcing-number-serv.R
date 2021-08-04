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
  geom_smooth(method = "lm", se = FALSE) +
  xlab(label = "Total salaries/wages ($)") +
  ylab(label = "Number of services offered") +
  theme_minimal()
print(salaries_plot)

# Run linear regression
salaries_lm <- lm(Service_count ~ salaries_wages, data = services_dist)
salaries_summary <- summary(salaries_lm)
# Coefficients:
#                   Estimate Std. Error t value Pr(>|t|)    
#   (Intercept)    8.053e+00  1.046e+00   7.696 8.28e-08 ***
#   salaries_wages 1.459e-07  5.270e-08   2.768   0.0109 *  
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 2.353 on 23 degrees of freedom
# Multiple R-squared:  0.2499,	Adjusted R-squared:  0.2173 
# F-statistic: 7.663 on 1 and 23 DF,  p-value: 0.01094


# Some potential influential points (from plot), calculate Cook's distance and 
# see if any need to be excluded
salaries_cooks <- cooks.distance(salaries_lm)
# Subtracting two for cutoff because coefficient length includes intercept
cooks_cutoff <- 4 / (nrow(services_dist) - length(salaries_lm$coefficients) - 2)
influential <- which(salaries_cooks > cooks_cutoff)
services_dist$Institution[influential]
# [1] "Washington State University"

# Try linear regression without Washington state
sans_wsu <- services_dist %>%
  filter(!(Institution == "Washington State University"))

# Run linear regression
sans_wsu_lm <- lm(Service_count ~ salaries_wages, data = sans_wsu)
sans_wsu_summary <- summary(sans_wsu_lm)
# Coefficients:
#                   Estimate Std. Error t value Pr(>|t|)    
#   (Intercept)    8.845e+00  1.001e+00   8.840 1.08e-08 ***
#   salaries_wages 1.137e-07  4.947e-08   2.299   0.0314 *  
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 2.13 on 22 degrees of freedom
# Multiple R-squared:  0.1937,	Adjusted R-squared:  0.157 
# F-statistic: 5.284 on 1 and 22 DF,  p-value: 0.03139

# Look for influential points again
sans_wsu_cooks <- cooks.distance(sans_wsu_lm)
sans_wsu_cutoff <- 4 / (nrow(sans_wsu) - length(sans_wsu_lm$coefficients) - 2)
sans_wsu_infl <- which(sans_wsu_cooks > sans_wsu_cutoff)
services_dist$Institution[sans_wsu_infl]
# None. Can stand with 24 institutions.
sans_wsu_plot <- ggplot(data = sans_wsu,
                        mapping = aes(x = salaries_wages/1e6, 
                                      y = Service_count)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  xlab(label = "Total salaries/wages (Million $)") +
  ylab(label = "Number of services offered") +
  theme_minimal()
print(sans_wsu_plot)

ggsave(filename = "output/salaries-services-sans-wsu.png",
       plot = sans_wsu_plot)

# Because we are pedantic, try without Stanford anyway
sans_two <- sans_wsu %>%
  filter(!(Institution == "Stanford University"))

# Run linear regression
sans_two_lm <- lm(Service_count ~ salaries_wages, data = sans_two)
sans_two_summary <- summary(sans_two_lm)
# Coefficients:
#                 Estimate Std. Error t value Pr(>|t|)    
# (Intercept)    9.226e+00  1.254e+00   7.360 3.05e-07 ***
#   salaries_wages 8.918e-08  6.893e-08   1.294     0.21    
# ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 2.167 on 21 degrees of freedom
# Multiple R-squared:  0.07382,	Adjusted R-squared:  0.02972 
# F-statistic: 1.674 on 1 and 21 DF,  p-value: 0.2098

# Look for influential points
sans_two_cooks <- cooks.distance(sans_two_lm)
sans_two_cutoff <- 4 / (nrow(sans_two) - length(sans_two_lm$coefficients) - 2)
sans_two_infl <- which(sans_two_cooks > sans_two_cutoff)
services_dist$Institution[sans_two_infl]
# No additional influential points, so use this model
sans_two_plot <- ggplot(data = services_dist,
                        mapping = aes(x = salaries_wages, 
                                      y = Service_count)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, lty = 2, lwd = 0.5) +
  geom_smooth(data = sans_two,
              method = "lm", se = FALSE) +
  xlab(label = "Total salaries/wages ($)") +
  ylab(label = "Number of services offered") +
  theme_minimal()
print(sans_two_plot)
ggsave(file = "output/salaries-services-sans-two.png",
       plot = sans_two_plot)

# Same plot as sans two above, but color UArizona red
sans_two_plot_az <- ggplot(data = services_dist %>% arrange(UArizona),
                        mapping = aes(x = salaries_wages/1e6, 
                                      y = Service_count)) +
  geom_point(size = 2.0, mapping = aes(color = UArizona)) +
  scale_color_manual(values = c("#555555", "#FF0000")) +
  geom_smooth(method = "lm", se = FALSE, lty = 2, lwd = 0.5) +
  geom_smooth(data = sans_two,
              method = "lm", se = FALSE) +
  xlab(label = "Total salaries/wages (Million $)") +
  ylab(label = "Number of services offered") +
  theme_minimal() +
  theme(legend.position = "none")
print(sans_two_plot_az)
ggsave(file = "output/salaries-services-az.png",
       plot = sans_two_plot_az,
       width = 6.5, height = 2.5, units = "in")

# 2. Test to see if total expenditures predict number of services

# Start with plot to eyeball linearity
expenditures_plot <- ggplot(data = services_dist, 
                        mapping = aes(x = total_expenditures/1e6, 
                                      y = Service_count)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  xlab(label = "Total expenditures (Million $)") +
  ylab(label = "Number of services offered") +
  theme_bw()
print(expenditures_plot)

# Run linear regression
expenditures_lm <- lm(Service_count ~ total_expenditures, data = services_dist)
expenditures_summary <- summary(expenditures_lm)
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept)        8.365e+00  1.178e+00   7.100 3.12e-07 ***
#   total_expenditures 5.085e-08  2.389e-08   2.129   0.0442 *  
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 2.483 on 23 degrees of freedom
# Multiple R-squared:  0.1646,	Adjusted R-squared:  0.1283 
# F-statistic: 4.532 on 1 and 23 DF,  p-value: 0.0442

# See if we need to remove any outliers
expenditures_cooks <- cooks.distance(expenditures_lm)
expenditures_cutoff <- 4 / (nrow(services_dist) - length(expenditures_lm$coefficients) - 2)
expenditures_infl <- which(expenditures_cooks > expenditures_cutoff)
services_dist$Institution[expenditures_infl]
# [1] "Washington State University"

# Try again without Washington State
sans_wsu_exp_lm <- lm(Service_count ~ total_expenditures, data = sans_wsu)
sans_wsu_exp_summary <- summary(sans_wsu_exp_lm)
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept)        9.307e+00  1.137e+00   8.188    4e-08 ***
#   total_expenditures 3.504e-08  2.262e-08   1.549    0.136    
# ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 2.253 on 22 degrees of freedom
# Multiple R-squared:  0.09832,	Adjusted R-squared:  0.05734 
# F-statistic: 2.399 on 1 and 22 DF,  p-value: 0.1357

sans_wsu_exp_plot <- ggplot(data = sans_wsu, 
                            mapping = aes(x = total_expenditures/1e6, 
                                          y = Service_count)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  xlab(label = "Total expenditures (Million $)") +
  ylab(label = "Number of services offered") +
  theme_bw()
print(sans_wsu_exp_plot)

# Not significant, stop here