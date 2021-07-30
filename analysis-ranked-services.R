# Rank services across institutions
# Jeff Oliver
# jcoliver@arizona.edu
# 2021-05-27

# 1. Count how many institutions offer each service and create plot
# 2. Plot distribution of # services per institution

library(dplyr)
library(tidyr)
library(ggplot2)
library(ggpubr)  # for multi-panel plot

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
ggsave(filename = "output/service-ranks.png", 
       plot = service_rank_plot)

# Same plot as above, but highlighting ones at UArizona
# Start by seeing which ones are at UArizona
# Am I lazy, or just over-enamored with pivot?
ua_services <- services %>%
  filter(Institution == "University of Arizona") %>%
  pivot_longer(cols = -Institution, names_to = "Service", values_to = "Present") %>%
  filter(Present == 1) %>%
  select(-Institution) %>%
  # Need to update by replacing underscore with space in service name
  mutate(Service = gsub(pattern = "_",
                        replacement = " ",
                        x = Service))

# Add a column to services_counts indicating which ones are offered at UArizona
services_counts <- services_counts %>%
  mutate(UArizona = if_else(condition = Service %in% ua_services$Service,
                            true = TRUE,
                            false = FALSE))

service_rank_plot_az <- ggplot(data = services_counts, 
                            mapping = aes(x = Service, y = Count)) +
  geom_bar(stat = "identity", mapping = aes(fill = UArizona)) +
  scale_fill_manual(values = c("#777777", "#FF9999")) +
  ylab(label = "# Institutions") +
  xlab(label = element_blank()) +
  coord_flip() +
  theme_minimal() +
  theme(legend.position = "none",
        axis.text = element_text(size = 8))
print(service_rank_plot_az)
ggsave(filename = "output/service-ranks-az.png", 
       plot = service_rank_plot_az, 
       width = 3.25, height = 3.25, units = "in")

# 2. Plot distribution of # services per institution
services_dist <- services %>% 
  rowwise(Institution) %>% 
  mutate(Service_count = sum(c_across(Aerial_imagery:Web_scraping))) %>%
  select(Institution, Service_count)

mean_num_svc <- mean(services_dist$Service_count)

service_histogram <- ggplot(data = services_dist,
                            mapping = aes(x = Service_count)) +
  geom_histogram(bins = 10, fill = "#777777") +
  geom_vline(xintercept = mean_num_svc, lty = 2, lwd = 0.25) +
  scale_x_continuous(breaks = seq(4, 16, 2)) +
  xlab("# Services") +
  ylab("# Institutions") +
  theme_minimal()
print(service_histogram)
ggsave(filename = "output/service-distribution.png",
       plot = service_histogram)

# Make histogram again, with red line for UA value
service_histogram_az <- ggplot(data = services_dist,
                            mapping = aes(x = Service_count)) +
  geom_histogram(bins = 10, fill = "#777777") +
  # scale_fill_manual(values = c("#00FF00")) +
  geom_vline(xintercept = mean_num_svc, lty = 2, lwd = 0.25) +
  geom_vline(xintercept = services_dist$Service_count[services_dist$Institution == "University of Arizona"],
             lty = 1, lwd = 0.5, color = "#FF0000") +
  scale_x_continuous(breaks = seq(4, 16, 2)) +
  xlab("# Services") +
  ylab("# Institutions") +
  theme_minimal() +
  theme(axis.text = element_text(size = 8))
print(service_histogram_az)
ggsave(filename = "output/service-distribution-az.png",
       plot = service_histogram_az,
       width = 3.25, height = 3.25, units = "in")

# Make a single plot with AZ-focused graphs
two_plots <- ggarrange(service_rank_plot_az, 
                       service_histogram_az,
                       ncol = 2,
                       labels = c("A", "B"))
print(two_plots)  
ggsave(filename = "output/service-dist-ranks-az.png",
       width = 6.5, height = 3, units = "in")
