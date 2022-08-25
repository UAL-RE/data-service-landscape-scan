# Test how well resourcing predicts individual services
# Jeff Oliver
# jcoliver@arizona.edu
# 2021-07-12

library(dplyr)

# 1. Test to see if salaries predict probabilities of individual services
# 2. Test to see if total expenditures predict probabilities of individual services

services <- read.csv(file = "data/services-final-pa.csv")
resources <- read.csv(file = "data/salaries-ipeds.csv")
services <- services %>%
  left_join(resources, by = c("Institution" = "institution"))

# May want to see which point is UArizona, so add column with that information
services <- services %>%
  mutate(UArizona = if_else(condition = Institution == "University of Arizona",
                            true = TRUE,
                            false = FALSE))

# Drop invariant services
column_sums <- colSums(services %>% select(Aerial_imagery:Web_scraping))
invar <- which(column_sums %in% c(0, nrow(services)))
invar_columns <- colnames(services %>% select(Aerial_imagery:Web_scraping))[invar]
services <- services %>%
  select(-all_of(invar_columns))

# Test to see if salaries or total expenditures predict individual services
service_names <- colnames(services %>% select(Aerial_imagery:Web_scraping))

# Going to run logistic regression on each service
logit_results <- data.frame(service = service_names,
                            salary_coeff = NA,
                            salary_p_value = NA,
                            expend_coeff = NA,
                            expend_p_value = NA)

for (i in 1:nrow(logit_results)) {
  service_name <- logit_results$service[i]
  service_data <- services %>%
    select(salaries_wages, total_expenditures, all_of(service_name), 
           UArizona, Institution) %>%
    rename(present = all_of(service_name)) # rename for easier formula building

  # Tests using salary as predictor
  salary_logit <- glm(present ~ salaries_wages, 
                      data = service_data,
                      family = "binomial")

  salary_summary <- summary(salary_logit)
  logit_results$salary_coeff[i] <- salary_summary$coefficients[2, "Estimate"]
  logit_results$salary_p_value[i] <- salary_summary$coefficients[2, "Pr(>|z|)"]
  
  # Tests using total expenditures as predictor
  expend_logit <- glm(present ~ total_expenditures, 
                      data = service_data,
                      family = "binomial")
  expend_summary <- summary(expend_logit)
  logit_results$expend_coeff[i] <- expend_summary$coefficients[2, "Estimate"]
  logit_results$expend_p_value[i] <- expend_summary$coefficients[2, "Pr(>|z|)"]
}
logit_results

