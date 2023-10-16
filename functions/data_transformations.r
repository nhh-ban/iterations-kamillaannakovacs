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
