# Data for data service landscape scan

+ institutions.csv: List of institutions considered peers of the University of Arizona as well as any institutions in the PAC-12 that are not peers of the University of Arizona. For each institution, includes location (state, city), public/private status, land grant status, Association of American Universities membership, and whether or not institution includes a medical school. Retrieved from [https://uair.arizona.edu/content/ua-peers](https://uair.arizona.edu/content/ua-peers) on 2020-05-21.
+ services-absent.csv: List of institution / service combinations that were not identified by either reviewer
+ services-present.csv: List of URLs with information about specific services provided at institutions, includes column indicating how many reviewers (1 or 2) identified this URL as listing the service for the respective institution
+ survey-form-dictionary.csv: Key to column name change from survey-form-responses.csv to survey-form-responses-clean.csv
+ survey-form-responses.csv: Google sheet output from form responses. Includes URL describing services in question; survey performed independently by two UAL staff, indicated by "Your initials" column
+ survey-form-responses-clean.csv: Copy of data from survey-form-responses.csv with more compute-friendly column names