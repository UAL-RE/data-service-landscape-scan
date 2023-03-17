# Data for data service landscape scan

+ institutions.csv: List of institutions considered peers of the University of 
Arizona as well as any institutions in the PAC-12 that are not peers of the 
University of Arizona. For each institution, includes location (state, city), 
public/private status, land grant status, Association of American Universities 
membership, and whether or not institution includes a medical school. Retrieved 
from [https://uair.arizona.edu/content/ua-peers](https://uair.arizona.edu/content/ua-peers) 
on 2020-05-21.
+ Radecki-Springer-R1-raw.csv: Data for R1 universities from the the work of 
[Radecki & Springer 2020](https://doi.org/10.18665/sr.314397).
+ salaries-ipeds.csv: Library total expenditures and salary/wage expenditures 
for 2019. Retrieved from [Integrated Postsecondary Education Data System](https://nces.ed.gov/ipeds/use-the-data)
(IPEDS) survey data on 2021-05-17.
+ services-absent-to-confirm.csv: Following review of services initially 
categorized as present, the institution / service combinations that remain 
absent and need final check by Data Cooperative personnel to ensure they are 
absent.
+ services-absent.csv: List of institution / service combinations that were not 
identified by either reviewer.
+ services-additional.csv: Institution / service with additional services 
information; includes all institution / service combinations in 
services-absent-to-confirm.csv with information indicating whether service 
actually present (a URL in the URL column) or absent (an `NA` in the URL 
column).
+ services-categories: Categorization of different services into three broad 
categories: Data Management, Geospatial, and Data Science.
+ services-final.csv: List of institution / service / mode combinations 
that are offered at an institution. Values in URL field indicate source 
describing the service; multiple URLs are separated by a semicolon. NA values 
in the URL column indicate that service / mode was not found at the 
institution's library.
**This is the final version of data that is used for most subsequent analyses.**
+ services-final-pa.csv: Matrix of institution /service combinations indicating 
presence / absence of services. 
+ services-present-confirmed.csv: Services that are confirmed as actually being 
present. All services listed in services-present.csv were reviewed by two Data 
Cooperative personnel.
+ services-present-initial.csv: List of URLs with information about specific 
services provided at institutions, includes column indicating how many reviewers 
(1 or 2) identified this URL as listing the service for the respective 
institution.
+ services-qc: Initial quality checks by individual team members.
+ survey-form-dictionary.csv: Key to column name change from 
survey-form-responses.csv to survey-form-responses-clean.csv.
+ survey-form-responses.csv: Google sheet output from form responses. Includes 
URL describing services in question; survey performed independently by two UAL 
staff, indicated by "Your initials" column.
+ survey-form-responses-clean.csv: Copy of data from survey-form-responses.csv 
with more compute-friendly column names.