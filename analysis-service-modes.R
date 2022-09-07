# Plot modality frequency for each service
# Jeff Oliver
# jcoliver@arizona.edu
# 2021-08-20

library(dplyr)
library(tidyr)
library(ggplot2)
library(multcomp)

services <- read.csv(file = "data/services-final.csv")

# Drop any rows missing modality information, then any remaining that have 
# missing URL (indicating service not offered in that modality)
services <- services %>%
  filter(!is.na(Mode)) %>%
  filter(!is.na(URL)) %>%
  dplyr::select(-URL)

# Let's try a plot of this, similar to what we see in analysis-ranked-services

# Want to count number of institutions offering each service, regardless of mode
service_counts <- services %>%
  dplyr::select(Institution, Service) %>%
  distinct() %>%
  group_by(Service) %>%
  summarize(Inst_count = n())
  
# Now collapse across universities to get counts of each combo of Service X Mode
mode_counts <- services %>%
  group_by(Service, Mode) %>%
  summarize(Count = n()) %>%
  ungroup()

# Join mode_counts with service_counts and calculate relative frequency of each 
# mode for each service
mode_props <- mode_counts %>%
  left_join(service_counts) %>%
  mutate(Percent = (Count/Inst_count) * 100) %>%
  dplyr::select(-Inst_count) %>% 
  mutate(Service = gsub(pattern = "_", 
                        replacement = " ", 
                        x = Service))

# Need to re-level Services so data management, geospatial, and data science 
# cluster appropriately

mode_props$Service_graph <- gsub(pattern = " ",
                                 replacement = "\n",
                                 x = mode_props$Service)

mode_props$Service_graph <- factor(x = mode_props$Service_graph,
                                   levels = c("Data\nmanagement",
                                              "Data\nmanagement\nplans",
                                              "Geospatial",
                                              "Geospatial\nsoftware",
                                              "Data\nanalysis",
                                              "Data\nvisualization\nsoftware"))

mode_rank_plot <- ggplot(data = mode_props,
                         mapping = aes(x = Service_graph, y = Percent, fill = Mode)) +
  geom_col(position = position_dodge2(reverse = FALSE)) +
  scale_fill_brewer(type = "qualitative", palette = "Set2", direction = 1) +
  xlab(label = element_blank()) +
  ylab(label = "Percent offered in Mode") +
  # coord_flip() +
  theme_bw()
print(mode_rank_plot)
ggsave(file = "output/service-modes.png",
       plot = mode_rank_plot,
       width = 6.5, height = 2.5, units = "in")

# What about doing a logistic regression, where we do a very rough 
# categorization of geo vs. management vs. data science and use those groups
# to test to see if each modality was offered. Is this three analyses? 

# Or is this one analysis with Mode + Area + Mode * Area?


# Fine, but a little tough to make comparisons because different areas have 
# differing levels of support. i.e. data management supported everywhere, data 
# analysis not so much.

# How many services are offered by a specific institution? 
# Of those institutions data management plans, what proportion are consult, 
# instruction, or web?


# Want to get counts for each institution, service, and mode of delivery
modes_wide <- services %>%
  group_by(Institution, Service, Mode) %>%
  mutate(Counts = n()) %>% # Create dummy column for values_from
  pivot_wider(id_cols = c(Institution, Service), 
              names_from = Mode,
              values_from = Counts) %>%
  # Replace missing values with 0; there is probably a more elegant across-y way
  mutate(Instruction = if_else(is.na(Instruction), 0, 1),
         Consult = if_else(is.na(Consult), 0, 1),
         Web = if_else(is.na(Web), 0, 1)) %>%
  ungroup()

# Now we transform back to long and categorize each service
modes_long <- modes_wide %>%
  dplyr::select(-Institution) %>%
  pivot_longer(cols = -Service, 
               names_to = "Mode", 
               values_to = "Offered") %>%
  mutate(Category = if_else(Service %in% c("Data_analysis", "Data_visualization_software"),
                            "Data_science",
                            if_else(Service %in% c("Geospatial", "Geospatial_software"),
                                    "Geospatial", "Data_management")))

# Because we will be using pedantic functions for our post-hoc tests, need to 
# explicitly cast Category and Mode as factors
modes_long$Mode <- as.factor(x = modes_long$Mode)
modes_long$Category <- as.factor(x = modes_long$Category)

# Do three separate analyses. One for each Mode
web_logit <- glm(Offered ~ Category, 
                 data = modes_long[modes_long$Mode == "Web", ],
                 family = binomial())
summary(multcomp::glht(web_logit, multcomp::mcp(Category = "Tukey")))
#                                     Estimate Std. Error z value Pr(>|z|)
# Data_science - Data_management == 0  -2.1691     0.5054  -4.291  < 1e-04 ***
# Geospatial - Data_management == 0    -0.4345     0.5275  -0.824  0.68806
# Geospatial - Data_science == 0        1.7346     0.4859   3.570  0.00102 **

consult_logit <- glm(Offered ~ Category, 
                     data = modes_long[modes_long$Mode == "Consult", ],
                     family = binomial())
summary(multcomp::glht(consult_logit, multcomp::mcp(Category = "Tukey")))
#                                     Estimate Std. Error z value Pr(>|z|)
# Data_science - Data_management == 0  -1.7918     0.5713  -3.136  0.00478 **
# Geospatial - Data_management == 0    -1.2164     0.5803  -2.096  0.08973 . 
# Geospatial - Data_science == 0        0.5754     0.4677   1.230  0.43311

instruction_logit <- glm(Offered ~ Category, 
                     data = modes_long[modes_long$Mode == "Instruction", ],
                     family = binomial())
summary(multcomp::glht(instruction_logit, multcomp::mcp(Category = "Tukey")))
#                                     Estimate Std. Error z value Pr(>|z|)
# Data_science - Data_management == 0   0.8575     0.4631   1.851    0.153
# Geospatial - Data_management == 0     0.7397     0.4424   1.672    0.215
# Geospatial - Data_science == 0       -0.1178     0.4979  -0.237    0.970

# Same general approach, but now faceting data on Category
# Maybe _easier_ to explain
data_sci_logit <- glm(Offered ~ Mode,
                      data = modes_long[modes_long$Category == "Data_science", ],
                      family = binomial())
summary(multcomp::glht(data_sci_logit, multcomp::mcp(Mode = "Tukey")))
#                            Estimate Std. Error z value Pr(>|z|)   
# Instruction - Consult == 0   0.6931     0.4873   1.422  0.32913   
# Web - Consult == 0          -0.9163     0.4592  -1.996  0.11311   
# Web - Instruction == 0      -1.6094     0.4899  -3.285  0.00296 **

data_management_logit <- glm(Offered ~ Mode,
                      data = modes_long[modes_long$Category == "Data_management", ],
                      family = binomial())
summary(multcomp::glht(data_management_logit, multcomp::mcp(Mode = "Tukey")))
#                            Estimate Std. Error z value Pr(>|z|)   
# Instruction - Consult == 0  -1.9561     0.5508  -3.551  0.00110 **
# Web - Consult == 0          -0.5390     0.6091  -0.885  0.64739   
# Web - Instruction == 0       1.4171     0.4796   2.955  0.00865 **

geospatial_logit <- glm(Offered ~ Mode,
                             data = modes_long[modes_long$Category == "Geospatial", ],
                             family = binomial())
summary(multcomp::glht(geospatial_logit, multcomp::mcp(Mode = "Tukey")))
#                              Estimate Std. Error z value Pr(>|z|)
# Instruction - Consult == 0 -1.488e-15  4.787e-01   0.000    1.000
# Web - Consult == 0          2.429e-01  4.940e-01   0.492    0.875
# Web - Instruction == 0      2.429e-01  4.940e-01   0.492    0.875