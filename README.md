# Data service landscape scan

R code and data for a landscape scan of data services at academic libraries

## Summary

This work generally focuses on four questions:

1. Which research data services does an academic library provide?
2. For a subset of those services, what form does the support come in? i.e.
consulting, instruction, or web resources?
3. Are there differences in support between three categories of services: data 
management, geospatial, and data science?
3. How does library resourcing (i.e. salaries) affect the number of research 
data services?

## Approach

Using direct survey of web resources, we investigated the services offered at 
25 Research 1 universities in the United States of America.

## Dependencies

The following R packages are used in this work:

+ dplyr
+ ggplot2
+ ggpubr
+ lme4
+ multcomp
+ readr
+ stringr
+ tidyr

## Directory structure

+ data: data used in this work; see data/README.md for more information
+ eda: preliminary exploratory data analyses; see eda/README.md for more 
information
+ output: destination for outputs like plots; files are not under version 
control
+ scripts: R scripts with bulk of analysis and data visualization for this
project; see scripts/README.md for more information
