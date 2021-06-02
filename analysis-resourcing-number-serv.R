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

# 1. Test to see if salaries predict number of services

# Plot the data to see how they look
salaries_plot <- ggplot(data = services_dist, 
                        mapping = aes(x = salaries_wages, 
                                      y = Service_count)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  xlab(label = "Total salaries/wages ($)") +
  ylab(label = "Number of services offered") +
  theme_bw()
print(salaries_plot)

# Run linear regression
salaries_lm <- lm(Service_count ~ salaries_wages, data = services_dist)
salaries_summary <- summary(salaries_lm)
# Coefficients:
#                   Estimate Std. Error t value Pr(>|t|)    
#   (Intercept)    8.152e+00  1.069e+00   7.625 9.67e-08 ***
#   salaries_wages 1.425e-07  5.384e-08   2.647   0.0144 *  
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 2.404 on 23 degrees of freedom
# Multiple R-squared:  0.2335,	Adjusted R-squared:  0.2002 
# F-statistic: 7.006 on 1 and 23 DF,  p-value: 0.01441


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
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept)    8.957e+00  1.024e+00   8.747  1.3e-08 ***
#   salaries_wages 1.098e-07  5.063e-08   2.170   0.0411 *  
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 2.18 on 22 degrees of freedom
# Multiple R-squared:  0.1763,	Adjusted R-squared:  0.1388 
# F-statistic: 4.707 on 1 and 22 DF,  p-value: 0.04111

# Look for influential points again
sans_wsu_cooks <- cooks.distance(sans_wsu_lm)
sans_wsu_cutoff <- 4 / (nrow(sans_wsu) - length(sans_wsu_lm$coefficients) - 2)
sans_wsu_infl <- which(sans_wsu_cooks > sans_wsu_cutoff)
services_dist$Institution[sans_wsu_infl]
# None. Can stand with 24 institutions.
sans_wsu_plot <- ggplot(data = sans_wsu,
                        mapping = aes(x = salaries_wages, 
                                      y = Service_count)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  xlab(label = "Total salaries/wages ($)") +
  ylab(label = "Number of services offered") +
  theme_bw()
print(sans_wsu_plot)

ggsave(filename = "output/salaries-services.pdf",
       plot = sans_wsu_plot)

# Because we are pedantic, try without Stanford anyway
sans_two <- sans_wsu %>%
  filter(!(Institution == "Stanford University"))

# Run linear regression
sans_two_lm <- lm(Service_count ~ salaries_wages, data = sans_two)
sans_two_summary <- summary(sans_two_lm)

# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept)    9.372e+00  1.282e+00   7.311 3.38e-07 ***
#   salaries_wages 8.318e-08  7.049e-08   1.180    0.251    
# ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 2.216 on 21 degrees of freedom
# Multiple R-squared:  0.06219,	Adjusted R-squared:  0.01753 
# F-statistic: 1.393 on 1 and 21 DF,  p-value: 0.2512

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
  theme_bw()
print(sans_two_plot)
ggsave(file = "output/salaries-services.pdf",
       plot = sans_two_plot)

# 2. Test to see if total expenditures predict number of services

# Start with plot to eyeball linearity
expenditures_plot <- ggplot(data = services_dist, 
                        mapping = aes(x = total_expenditures, 
                                      y = Service_count)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  xlab(label = "Total expenditures ($)") +
  ylab(label = "Number of services offered") +
  theme_bw()
print(expenditures_plot)

# Run linear regression
expenditures_lm <- lm(Service_count ~ total_expenditures, data = services_dist)
expenditures_summary <- summary(expenditures_lm)

# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept)        8.445e+00  1.197e+00   7.053 3.47e-07 ***
#   total_expenditures 4.996e-08  2.427e-08   2.058   0.0511 .  
# ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 2.524 on 23 degrees of freedom
# Multiple R-squared:  0.1555,	Adjusted R-squared:  0.1188 
# F-statistic: 4.235 on 1 and 23 DF,  p-value: 0.0511

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
# (Intercept)        9.399e+00  1.156e+00   8.129 4.52e-08 ***
#   total_expenditures 3.394e-08  2.301e-08   1.475    0.154    
# ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 2.292 on 22 degrees of freedom
# Multiple R-squared:  0.09001,	Adjusted R-squared:  0.04864 
# F-statistic: 2.176 on 1 and 22 DF,  p-value: 0.1543

