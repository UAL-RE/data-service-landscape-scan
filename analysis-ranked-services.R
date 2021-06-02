# Rank services across institutions
# Jeff Oliver
# jcoliver@arizona.edu
# 2021-05-27

# 1. Count how many institutions offer each service and create plot
# 2. Plot distribution of # services per institution

library(dplyr)
library(tidyr)
library(ggplot2)

services <- read.csv(file = "data/services-final-pa.csv")

# 1. Count number of institutions that offer each service

services_counts <- services %>%
  pivot_longer(names_to = "Service",
               values_to = "PA",
               -Institution) %>%
  filter(PA == 1) %>%
  group_by(Service) %>%
  summarize(Count = n()) %>%
  arrange(desc(Count))

# Remove the underscores from service names for easier plotting
services_counts$Service <- gsub(pattern = "_",
                                replacement = " ",
                                x = services_counts$Service)

# Reorder levels so plot is easier to read
services_counts$Service <- factor(x = services_counts$Service,
                                  levels = rev(services_counts$Service))

# Plot the results
service_rank_plot <- ggplot(data = services_counts, 
                            mapping = aes(x = Service, y = Count)) +
  geom_bar(stat = "identity") +
  ylab(label = "# Institutions") +
  xlab(label = element_blank()) +
  coord_flip() +
  theme_bw()
print(service_rank_plot)
ggsave(filename = "output/service-ranks.pdf", 
       plot = service_rank_plot)

# 2. Plot distribution of # services per institution
services_dist <- services %>% 
  rowwise(Institution) %>% 
  mutate(Service_count = sum(c_across(Aerial_imagery:Web_scraping))) %>%
  select(Institution, Service_count)

mean_num_svc <- mean(services_dist$Service_count)

service_histogram <- ggplot(data = services_dist,
                            mapping = aes(x = Service_count)) +
  geom_histogram(bins = 10) +
  geom_vline(xintercept = mean_num_svc, lty = 2, lwd = 0.25) +
  scale_x_continuous(breaks = seq(4, 16, 2)) +
  xlab("# Services") +
  ylab("# Institutions") +
  theme_bw()
print(service_histogram)
ggsave(filename = "output/service-distribution.pdf",
       plot = service_histogram)
