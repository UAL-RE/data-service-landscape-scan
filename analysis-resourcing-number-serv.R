# Test how well resourcing predicts number of services
# Jeff Oliver
# jcoliver@arizona.edu
# 2021-06-02

library(dplyr)
library(tidyr)
library(ggplot2)

# 1. Test to see if salaries predict number of services
# 2. Test to see if total expenditures predict number of services

services <- read.csv(file = "data/services-final-pa.csv")
resources <- read.csv(file = "data/salaries-ipeds.csv")

# Count number of institutions that offer each service and join with salary data
# Easier to interpret if salaries are in millions of dollars
services_dist <- services %>% 
  rowwise(Institution) %>% 
  mutate(Service_count = sum(c_across(Aerial_imagery:Web_scraping))) %>%
  dplyr::select(c(Institution, Service_count)) %>%
  left_join(resources, by = c("Institution" = "institution")) %>%
  mutate(salaries_wages = salaries_wages/1e6,
         total_expenditures = total_expenditures/1e6)

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
              se = FALSE,
              size = 0.5) +
  xlab(label = "Total salaries/wages ($M)") +
  ylab(label = "Number of services offered") +
  theme_minimal()
print(salaries_plot)
ggsave(filename = "output/salaries-services.png",
       plot = salaries_plot,
       width = 6.5, height = 2.5, units = "in")

salaries_plot_2lines <- salaries_plot +
  geom_smooth(data = services_dist %>% filter(Institution != "Stanford University"),
              method = "glm",
              method.args = list(family = "poisson"),
              se = FALSE, 
              linetype = 2,
              color = "black",
              size = 0.5)
ggsave(filename = "output/figure-3.png",
       plot = salaries_plot_2lines,
       width = 6.5, height = 2.5, units = "in")

# Run glm, because Service_count is count, and should be Poisson modeled
salaries_glm <- glm(Service_count ~ salaries_wages,
                    data = services_dist,
                    family = "poisson")
salaries_summary <- summary(salaries_glm)
# Coefficients:
#                  Estimate Std. Error z value Pr(>|z|)    
#   (Intercept)    2.134294   0.133876  15.942   <2e-16 ***
#   salaries_wages 0.012609   0.006321   1.995   0.0461 *  

# dfbetas, which look at influence of each point on each coefficient estimate
salaries_dfbeta <- dfbeta(salaries_glm)
salaries_dfbeta_thresh <- 2/sqrt(nrow(services_dist))

# See if any are beyond the threshold
any(abs(salaries_dfbeta) > salaries_dfbeta_thresh)
# Nope

# Another outlier method is Cook's distance
salaries_cooks <- cooks.distance(salaries_glm)
cooks_cutoff <- 4 / (nrow(services_dist) - length(salaries_glm$coefficients) - 2)
influential <- which(salaries_cooks > cooks_cutoff)
services_dist$Institution[influential]
# None. We're good. 

# No outliers, but would be good to have the p-value if we exclude Stanford
# University...
salaries_notree_glm <- glm(Service_count ~ salaries_wages,
                    data = services_dist %>% filter(Institution != "Stanford University"),
                    family = "poisson")
salaries_notree_summary <- summary(salaries_notree_glm)
# Coefficients:
#         Estimate Std. Error z value Pr(>|z|)    
#   (Intercept)    2.123119   0.167722  12.659   <2e-16 ***
#   salaries_wages 0.013330   0.009065   1.471    0.141 

# Make a copy of the plot showing where UA is.
salaries_plot_az <- ggplot(data = services_dist %>% arrange(UArizona),
                           mapping = aes(x = salaries_wages, 
                                         y = Service_count)) +
  geom_point(size = 2.0, mapping = aes(color = UArizona, shape = UArizona)) +
  scale_shape_manual(values = c(16, 17)) + # circle, triangle
  scale_color_manual(values = c("#555555", "#FF0000")) +
  geom_smooth(method = "glm", 
              method.args = list(family = "poisson"), 
              se = FALSE) +
  xlab(label = "Total salaries/wages ($M)") +
  ylab(label = "Number of services offered") +
  theme_minimal() +
  theme(legend.position = "none")
print(salaries_plot_az)
ggsave(file = "output/salaries-services-az.png",
       plot = salaries_plot_az,
       width = 6.5, height = 2.5, units = "in")

########################################
# 2. Test to see if total expenditures predict number of services

# Start with plot to eyeball linearity
expenditures_plot <- ggplot(data = services_dist, 
                            mapping = aes(x = total_expenditures, 
                                          y = Service_count)) +
  geom_point() +
  geom_smooth(method = "glm", 
              method.args = list(family = "poisson"),
              se = FALSE) +
  xlab(label = "Total expenditures ($M)") +
  ylab(label = "Number of services offered") +
  theme_bw()
print(expenditures_plot)

# Run glm
expenditures_glm <- glm(Service_count ~ total_expenditures, 
                        data = services_dist,
                        family = "poisson")
expenditures_summary <- summary(expenditures_glm)
# Coefficients:
#                      Estimate Std. Error z value Pr(>|z|)    
#   (Intercept)        2.160613   0.142674   15.14   <2e-16 ***
#   total_expenditures 0.004460   0.002753    1.62    0.105 

# Look to see if any points had undue influence
expenditures_dfbeta <- dfbeta(expenditures_glm)
expenditures_dfbeta_thresh <- 2/sqrt(nrow(services_dist))

# See if any are beyond the threshold
any(abs(expenditures_dfbeta) > expenditures_dfbeta_thresh)
# None beyond threshold