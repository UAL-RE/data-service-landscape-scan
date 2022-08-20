# Test how well salaries predict number of services in three categories
# Jeff Oliver
# jcoliver@arizona.edu
# 2021-06-02

library(dplyr)
library(tidyr)
library(ggplot2)
library(car)     # For outlier detection

# 1. Test to see if salaries predict number of services
# 2. Test to see if salaries corrected for cost of labor predict number of 
# services

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
  mutate(salaries_wages = salaries_wages/1e6)

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
# Now see what analyses look like when salaries are corrected for variation in 
# cost of labor among cities
cor_resources <- read.csv(file = "data/salaries-ipeds-CoL.csv")

# Join the salary data (for corrected salaries)
services_dist_cor <- services_counts %>%
  left_join(cor_resources, by = c("Institution" = "institution")) %>%
  mutate(corrected_salaries = salaries_wages / avg_annual_pay)

# Plot the data to see how they look
salaries_cor_plot <- ggplot(data = services_dist_cor, 
                        mapping = aes(x = corrected_salaries, 
                                      y = Category_count,
                                      color = Category)) +
  geom_point() +
  geom_smooth(method = "glm",
              method.args = list(family = "poisson"),
              se = FALSE) +
  scale_color_brewer(type = "qualitative", palette = "Set2", direction = 1) +
  xlab(label = "Salaries/wages corrected for cost of labor") +
  ylab(label = "Number of services offered") +
  theme_minimal()
print(salaries_cor_plot)

# Run GLM
salaries_cor_glm <- glm(Category_count ~ corrected_salaries * Category,
                    data = services_dist_cor,
                    family = "poisson")
salaries_cor_summary <- summary(salaries_cor_glm)
# Coefficients:
#   Estimate Std. Error z value Pr(>|z|)    
# (Intercept)                              1.3373418  0.3046533   4.390 1.13e-05 ***
#   corrected_salaries                      -0.0001208  0.0010071  -0.120    0.904    
# CategoryGeospatial                      -0.6753069  0.4823009  -1.400    0.161    
# CategoryData Science                    -0.7764947  0.4881857  -1.591    0.112    
# corrected_salaries:CategoryGeospatial    0.0010236  0.0015600   0.656    0.512    
# corrected_salaries:CategoryData Science  0.0012558  0.0015708   0.799    0.424  

# dfbetas, which look at influence of each point on each coefficient estimate
salaries_cor_dfbeta <- dfbeta(salaries_cor_glm)
dfbeta_cor_thresh <- 2/sqrt(nrow(services_dist_cor))

# See if any are beyond the threshold
any(abs(salaries_cor_dfbeta) > dfbeta_cor_thresh)
# TRUE
# Aha. Gotta look closer

# Create data frame and plot for quick investigation
sal_cor_dfbeta <- as.data.frame(salaries_cor_dfbeta)
sal_cor_dfbeta_long <- sal_cor_dfbeta %>%
  mutate(ID = rownames(sal_cor_dfbeta)) %>%
  pivot_longer(cols = -c(ID),
               names_to = "Coefficient",
               values_to = "Delta")
sal_cor_dfbeta_plot <- ggplot(data = sal_cor_dfbeta_long,
                          mapping = aes(x = ID, y = Delta)) +
  geom_point() +
  geom_hline(yintercept = dfbeta_cor_thresh) +
  geom_hline(yintercept = -dfbeta_cor_thresh) +
  theme_minimal() +
  facet_wrap(~ Coefficient)
print(sal_cor_dfbeta_plot)
# Two points, one CategoryData Science, one CategoryGeospatial
# Which rows of our data do these correspond to?
influential <- which(rowSums(abs(salaries_cor_dfbeta) > dfbeta_cor_thresh) > 0)
# print out the institution / category
services_dist_cor %>%
  select(c(Institution, Category)) %>%
  slice(influential)

# Try analyses without those influential points
services_sans_outliers <- services_dist_cor[-influential, ]

# Run GLM
salaries_cor_so_glm <- glm(Category_count ~ corrected_salaries * Category,
                        data = services_sans_outliers,
                        family = "poisson")
salaries_cor_so_summary <- summary(salaries_cor_so_glm)
# Coefficients:
#                                             Estimate Std. Error z value Pr(>|z|)    
#   (Intercept)                              1.861e+00  2.333e-01   7.976 1.51e-15 ***
#   corrected_salaries                      -8.312e-05  7.703e-04  -0.108  0.91407    
#   CategoryGeospatial                      -3.792e-01  3.691e-01  -1.027  0.30423    
#   CategoryData Science                    -1.200e+00  4.438e-01  -2.704  0.00684 ** 
#   corrected_salaries:CategoryGeospatial    4.307e-04  1.196e-03   0.360  0.71869    
#   corrected_salaries:CategoryData Science  2.134e-03  1.358e-03   1.571  0.11624 

# dfbetas again
salaries_cor_so_dfbeta <- dfbeta(salaries_cor_so_glm)
dfbeta_cor_so_thresh <- 2/sqrt(nrow(services_sans_outliers))

# See if any are beyond the threshold
any(abs(salaries_cor_so_dfbeta) > dfbeta_cor_so_thresh)
# FALSE, so we can stop here

