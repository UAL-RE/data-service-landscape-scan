# R scripts for data service landscape scan

+ analysis-ranked-services.R: Count of each service across institutions and 
plots showing number of institutions with each service.
+ analysis-resourcing-number-serv-cat.R: Test how well resourcing (salaries; 
total library expenditures) predicts number of services in three categories; an 
extension of tests in analysis-resourcing-number-serv.R that include an 
additional predictor variable.
+ analysis-resourcing-number-serv.R: Test how well resourcing (salaries; 
total library expenditures) predicts number of services at an institution's 
library.
+ analysis-service-modes.R: Visualization and analysis of different modes of 
delivery (workshop, consultation, web) for three broad categories of services 
(Data Management, Geospatial, Data Science).
+ clean-initial-responses.R: Creation of data files for QA/QC of initial survey
+ compare-radecki-springer.R: Some minor comparisons with 
[Radecki & Springer 2020](https://doi.org/10.18665/sr.314397)
+ create-presence-absence-matrix.R: Creation of a matrix of services, with one 
row for each institution and a column for each service indicating absence (0) 
or presence (0).
+ create-service-categories.R: Creation of the file categorizing services into 
three broad categories (Data Management, Geospatial, Data Science) (resulting 
file is data/services-categories.csv).
+ create-services-analysis-data.R: Creation of a single data file with data on 
services and institutions (resulting file is data/services-final.csv).
+ services-present-qc.R: Creation of a list of services marked as "absent" from 
institutions' libraries; used in quality control process to confirm absence.