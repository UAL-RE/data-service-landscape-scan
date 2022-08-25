# Exploratory data analysis scripts

The scripts in this directory were used for a variety of exploratory analyses, 
but are not part of the workflow in the published work. They are kept here for 
posterity, but may be out of date and refer to assests that no longer exist. 
Use at your own risk.

The scripts with a *CoL.R suffix in the file name attempt to control for cost 
of labor based on average annual salaries for the city/county/region in which 
the campus is located. On further discussion with economists, this is probably 
not a good approach, given that we have only have a single observation from 
each city/county/region, and cannot unequivocally control for resourcing this 
way.

## Data files:

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
+ salaries-ipeds-CoL.csv: Library total expenditures and salary/wage 
expenditures and cost of labor for 2019. Salary data come from 
salaries-ipeds.csv and cost of labor information are drawn from the U.S. Bureau
of Labor Statistics. See the script eda/create-salaries-CoL.R
