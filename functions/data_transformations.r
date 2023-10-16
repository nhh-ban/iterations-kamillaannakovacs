# transform_metadata_to_df.
# The function transform_metadata_to_df should complete the transformation of stations_metadata to a data frame that 
# looks similar to the example rows below:

# data_transformations.r

library(dplyr)
library(lubridate)

transform_metadata_to_df <- function(stations_metadata) {
  stations_df <- lapply(stations_metadata, function(station) {
    data.frame(
      id = station$id, 
      name = station$name, 
      latestData = as_datetime(station$latestData, tz = "UTC"), 
      lat = station$lat, 
      lon = station$lon,
      stringsAsFactors = FALSE
    )
  }) %>% bind_rows()

  return(stations_df)
}


# Problem 4a

library(anytime)

# 'to_iso8601' converts a date-time object to ISO 8601 format, applying an offset in days.
# 
# @param datetime A date-time object.
# @param offset_days The number of days to add to 'datetime' (can be negative for subtraction).
# @return A string with the date-time in ISO 8601 format.
to_iso8601 <- function(datetime, offset_days) {
  # Apply the offset
  offset_datetime <- datetime + days(offset_days)
  
  # Convert to ISO 8601 format with "Z" (Zulu time zone, i.e., UTC)
  iso_datetime <- anytime::iso8601(offset_datetime, tz = "UTC")
  
  return(iso_datetime)
}


source('functions/data_transformations.r')

print(to_iso8601(as_datetime("2016-09-01 10:11:12"), 0))
# Should print: [1] "2016-09-01T10:11:12Z"

print(to_iso8601(as_datetime("2016-09-01 10:11:12"), -4))
# Should print: [1] "2016-08-28T10:11:12Z"
