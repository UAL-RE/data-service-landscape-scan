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
  dplyr::summarize(Count = n()) %>% # collision possible with Hmisc::summarize
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
ggsave(filename = "output/figure-1.png",
       plot = service_rank_plot,
       width = 6.5, height = 3.25, units = "in")

# A version of service_rank_plot that separates top from bottom half
services_counts <- services_counts %>%
  mutate(Half = Count > max(Count)/2)

ubiquitous_services <- ggplot(data = services_counts %>% filter(Half == TRUE),
                                  mapping = aes(x = Service, y = Count)) +
  geom_bar(stat = "identity") +
  ylab(label = "# Institutions") +
  xlab(label = element_blank()) +
  scale_y_continuous(limits = c(0, max(services_counts$Count))) +
  coord_flip() +
  theme_bw() +
  theme(axis.text.y = element_text(size = 12))
print(ubiquitous_services)
ggsave(filename = "output/service-ranks-ubiquitous.png", 
       plot = ubiquitous_services)

uncommon_services <- ggplot(data = services_counts %>% filter(Half != TRUE),
                              mapping = aes(x = Service, y = Count)) +
  geom_bar(stat = "identity") +
  ylab(label = "# Institutions") +
  xlab(label = element_blank()) +
  scale_y_continuous(limits = c(0, max(services_counts$Count))) +
  coord_flip() +
  theme_bw() +
  theme(axis.text.y = element_text(size = 12))
print(uncommon_services)
ggsave(filename = "output/service-ranks-uncommon.png", 
       plot = uncommon_services)

# Same plot as above, but highlighting ones at UArizona
# Start by seeing which ones are at UArizona
# Am I lazy, or just over-enamored with pivot?
ua_services <- services %>%
  filter(Institution == "University of Arizona") %>%
  pivot_longer(cols = -Institution, names_to = "Service", values_to = "Present") %>%
  filter(Present == 1) %>%
  dplyr::select(-Institution) %>%
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
  dplyr::select(Institution, Service_count)

mean_num_svc <- mean(services_dist$Service_count)
median_num_svc <- median(services_dist$Service_count)
se_num_svc <- sd(services_dist$Service_count)/sqrt(nrow(services_dist))

service_histogram <- ggplot(data = services_dist,
                            mapping = aes(x = Service_count)) +
  geom_histogram(bins = 10, fill = "#777777") +
  geom_vline(xintercept = mean_num_svc, lty = 2, lwd = 0.25) +
  geom_vline(xintercept = median_num_svc, lwd = 0.25) +
  scale_x_continuous(breaks = seq(4, 16, 2)) +
  xlab("# Services") +
  ylab("# Institutions") +
  theme_bw()
print(service_histogram)
ggsave(filename = "output/service-distribution.png",
       plot = service_histogram)
ggsave(filename = "output/figure-2.png",
       plot = service_histogram,
       width = 6.5, height = 3.25, units = "in")

# Combine ranks and histogram into a single figure and save
figure_1 <- ggarrange(service_rank_plot, 
                      service_histogram,
                      ncol = 2,
                      labels = c("A", "B"))
print(figure_1)
ggsave(filename = "output/figure-1_2.png",
       width = 6.5, height = 6, units = "in", dpi = 1200)

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
