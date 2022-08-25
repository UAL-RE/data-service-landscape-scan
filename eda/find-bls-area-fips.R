# Find Bureau of Labor Statistics area identifiers for each institution
# Jeff Oliver
# jcoliver@arizona.edu
# 2022-08-09

# Note: this is a helper script to investigate BLS data; it is not part of the 
# data preparation or analysis workflow

institutions <- read.csv(file = "data/institutions.csv")
area_codes <- read.csv(file = "eda/bls-area-titles.csv")

# Look for (near) exact matches to city, state abbreviation
city_states <- unique(paste0(institutions$City, ", ", institutions$State))
city_rows <- c()
for (cs in city_states) {
  grepped <- grep(pattern = cs, x = area_codes$area_title)
  city_rows <- c(city_rows, grepped)
}
potential_cities <- area_codes[city_rows, ]
# Print those out and copy appropriate codes
potential_cities

# Look for potential matches on city names
potential_metro_rows <- c()
for (ci in institutions$City) {
  grepped <- grep(pattern = ci, x = area_codes$area_title)
  potential_metro_rows <- c(potential_metro_rows, grepped)
}
# Remove any of the city rows identified above
potential_metro_rows <- setdiff(potential_metro_rows, city_rows)
potential_metros <- area_codes[potential_metro_rows, ]

# Look for specific entries
potential_metros[grep(pattern = "Tempe", x = potential_metros$area_title), ]
potential_metros[grep(pattern = "Phoenix", x = potential_metros$area_title), ]
potential_metros[grep(pattern = "Palo Alto", x = potential_metros$area_title), ]
potential_metros[grep(pattern = "College Station", x = potential_metros$area_title), ]
potential_metros[grep(pattern = "Berkeley", x = potential_metros$area_title), ]
potential_metros[grep(pattern = "Davis", x = potential_metros$area_title), ]
potential_metros[grep(pattern = "Los Angeles", x = potential_metros$area_title), ]
potential_metros[grep(pattern = "College Park", x = potential_metros$area_title), ]
potential_metros[grep(pattern = "Minneapolis", x = potential_metros$area_title), ]
potential_metros[grep(pattern = "Austin", x = potential_metros$area_title), ]
potential_metros[grep(pattern = "Seattle", x = potential_metros$area_title), ]

# Still remaining: ASU, Stanford, Cal, Davis, UMD
# REMEMBER: Some codes will appear as four-digit codes, e.g. 4013 for Maricopa 
# County, AZ. The wages data (area_fips column) has a leading zero for any 
# integers less than 10000, so 4013 should actually be 04013
area_codes[grep(x = area_codes$area_title, pattern = "Phoenix"), ]
area_codes[grep(pattern = institutions$County[institutions$Institution == "Arizona State University"],
                x = area_codes$area_title), ]
area_codes[grep(x = area_codes$area_title, pattern = "Stanford"), ]
area_codes[grep(pattern = institutions$County[institutions$Institution == "Stanford University"],
                x = area_codes$area_title), ]
area_codes[grep(x = area_codes$area_title, pattern = "Oakland"), ]
area_codes[grep(pattern = institutions$County[institutions$Institution == "University of California, Berkeley"],
                x = area_codes$area_title), ]
area_codes[grep(x = area_codes$area_title, pattern = "Sacramento"), ]
area_codes[grep(pattern = institutions$County[institutions$Institution == "University of California, Davis"],
                x = area_codes$area_title), ]
area_codes[grep(pattern = institutions$County[institutions$Institution == "University of Maryland, College Park"],
                x = area_codes$area_title), ]
