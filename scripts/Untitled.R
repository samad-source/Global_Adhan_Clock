# Check whether the country information contains the USA and Canada
country_lookup[
  country_lookup$country_code %in% c("US", "CA", "MX"),
]

# Check the original country codes in the city dataset
unique(
  cities$country_code[
    cities$country_code %in% c("US", "CA", "MX")
  ]
)

# Inspect a few unmatched records
head(
  cities[
    is.na(cities$continent),
    c("city", "country_code", "latitude", "longitude")
  ],
  15
)

# Check the structure of the country lookup
head(country_lookup, 10)