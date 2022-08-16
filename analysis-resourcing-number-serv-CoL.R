# Test how well resourcing, correcting for cost of labor, predicts number of services
# Jeff Oliver
# jcoliver@arizona.edu
# 2022-08-16

library(dplyr)
library(tidyr)
library(ggplot2)

# 1. Test to see if salaries predict number of services
# 2. Test to see if total expenditures predict number of services

services <- read.csv(file = "data/services-final-pa.csv")
resources <- read.csv(file = "data/salaries-ipeds-CoL.csv")

# Correct salaries and total expentitures by labor cost
resources <- resources %>%
  mutate(salaries_wages = salaries_wages/avg_annual_pay) %>%
  mutate(total_expenditures = total_expenditures/avg_annual_pay)

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
  geom_smooth(method = "glm", 
              method.args = list(family = "poisson"), 
              se = FALSE) +
  xlab(label = "Total salaries/wages ($)") +
  ylab(label = "Number of services offered") +
  theme_minimal()
print(salaries_plot)
ggsave(filename = "output/salaries-services-CoL.png",
       plot = salaries_plot)

# Run glm, because Service_count is count, and should be Poisson modeled
salaries_glm <- glm(Service_count ~ salaries_wages,
                    data = services_dist,
                    family = "poisson")
salaries_summary <- summary(salaries_glm)
# Coefficients:
#                   Estimate Std. Error z value Pr(>|z|)    
#   (Intercept)    2.1747261  0.1812441  11.999   <2e-16 ***
#   salaries_wages 0.0006568  0.0005826   1.127     0.26  

# Interpreting the Poisson model, delta y = y(e^B - 1)
# i.e. it's not linear - effect of x on y will vary based on value of y

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
#   (Intercept)        2.2878341  0.1889583  12.108   <2e-16 ***
#   total_expenditures 0.0001051  0.0002435   0.432    0.666    