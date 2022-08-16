# Data for data service landscape scan

+ bls-area-titles.csv: Area titles from U.S. Bureau of Labor Statistics, 
downloaded from [https://www.bls.gov/cew/classifications/areas/qcew-area-titles.htm](https://www.bls.gov/cew/classifications/areas/qcew-area-titles.htm) on 2022-08-09. 
For use with bls-wages-2019.csv (area_fips column of bls-wages-2019.csv is 
_almost_ equivalent to area_fips column of bls-area-titles.csv; the former has 
a leading zero for all values below 10000 (e.g. "01001" for Autauga County, 
Alabama) while the latter lacks the leading zero for those values below 10000 
(e.g. "1001" for Autauga County, Alabama).
+ bls-wages-2019.csv: County wages from U.S. Bureau of Labor Statistics, 
available from [https://data.bls.gov/cew/data/api/2019/a/industry/10.csv](https://data.bls.gov/cew/data/api/2019/a/industry/10.csv). Downloaded on 2022-08-09 through 
the portal at [https://data.bls.gov/cew/apps/table_maker/v4/table_maker.htm#type=1&year=2019&qtr=A&own=0&ind=10&supp=0](https://data.bls.gov/cew/apps/table_maker/v4/table_maker.htm#type=1&year=2019&qtr=A&own=0&ind=10&supp=0). Data are 2019 annual averages 
for all establishment sizes, all industries. For total (across all ownership 
types, use values in rows where own_code is 0).
+ institutions.csv: List of institutions considered peers of the University of 
Arizona as well as any institutions in the PAC-12 that are not peers of the 
University of Arizona. For each institution, includes location (state, city), 
public/private status, land grant status, Association of American Universities 
membership, and whether or not institution includes a medical school. Retrieved 
from [https://uair.arizona.edu/content/ua-peers](https://uair.arizona.edu/content/ua-peers) 
on 2020-05-21.
+ salaries-ipeds.csv: Library total expenditures and salary/wage expenditures 
for 2019. Retrieved from [Integrated Postsecondary Education Data System](https://nces.ed.gov/ipeds/use-the-data)
(IPEDS) survey data on 2021-05-17.
+ salaries-ipeds-CoL.csv: Library total expenditures and salary/wage 
expenditures and cost of labor for 2019. Salary data come from 
salaries-ipeds.csv and cost of labor information are drawn from the U.S. Bureau
of Labor Statistics. See the script create-salaries-CoL.R
+ services-absent-to-confirm.csv: Following review of services initially 
categorized as present, the institution / service combinations that remain 
absent and need final check by Data Cooperative personnel to ensure they are 
absent.
+ services-absent.csv: List of institution / service combinations that were not 
identified by either reviewer
+ services-additional.csv: Institution / service with additional services 
information; includes all institution / service combinations in 
services-absent-to-confirm.csv with information indicating whether service 
actually present (a URL in the URL column) or absent (an `NA` in the URL 
column).
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
institution
+ survey-form-dictionary.csv: Key to column name change from 
survey-form-responses.csv to survey-form-responses-clean.csv
+ survey-form-responses.csv: Google sheet output from form responses. Includes 
URL describing services in question; survey performed independently by two UAL 
staff, indicated by "Your initials" column
+ survey-form-responses-clean.csv: Copy of data from survey-form-responses.csv 
with more compute-friendly column names