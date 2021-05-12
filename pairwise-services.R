# Perform pairwise correlation on services to test co-occurrence
# Jeff Oliver
# jcoliver@arizona.edu
# 2021-05-12

library(Hmisc)
library(readr)
library(dplyr)
library(corrplot)

# Much drawn from http://www.sthda.com/english/wiki/correlation-matrix-a-quick-start-guide-to-analyze-format-and-visualize-a-correlation-matrix-using-r-software

services_pa <- read_csv(file = "data/services-final-pa.csv")

# Don't need Institution and we will need a numeric matrix
pa_matrix <- services_pa %>%
  select(-Institution) %>%
  as.matrix()

# Services that are invariant (i.e. present at all institutions or absent from 
# all institutions) need to be removed before test
total_present <- colSums(pa_matrix)
ubiquitous_services <- which(total_present == (nrow(services_pa)))
absent_services <- which(total_present == 0) # probably none of these
pa_matrix_var <- pa_matrix[, -c(ubiquitous_services, absent_services)]

# Calculate correlation coefficients and p-values
corr_matrix <- rcorr(pa_matrix_var)

# Diagonal correlations are uninformative, set to 0
diag(corr_matrix$r) <- 0

# For multiple comparisons adjust p-values per Holm-Bonferroni
corr_matrix$p_adj <- matrix(data = p.adjust(corr_matrix$P, method = "holm"),
                            nrow = nrow(corr_matrix$P),
                            byrow = TRUE)

# Need to name rows & columns to plot correctly
rownames(corr_matrix$p_adj) <- colnames(corr_matrix$p_adj) <- colnames(corr_matrix$P)

# Plot only those correlations that are significant
corrplot(corr = corr_matrix$r, 
         type = "upper", 
         order = "alphabet", 
         p.mat = corr_matrix$p_adj,
         sig.level = 0.05, 
         insig = "blank")

# Extract only those correlations that remain significant after p-value 
# adjustment, start by converting correlation & adjusted-p matrices to a data 
# frame
upper_corr <- upper.tri(corr_matrix$r)
corr_df <- data.frame(svc_1 = rownames(corr_matrix$r)[row(corr_matrix$r)[upper_corr]],
                      svc_2 = rownames(corr_matrix$r)[col(corr_matrix$r)[upper_corr]],
                      corr = corr_matrix$r[upper_corr],
                      p_adj = corr_matrix$p_adj[upper_corr])

# Subset only those rows with adjusted p-values less than 0.05
sig_corr <- corr_df[corr_df$p_adj < 0.05, ]
#                          svc_1      svc_2      corr        p_adj
# 26 Data_visualization_software Geospatial 0.7985494 0.0003600954