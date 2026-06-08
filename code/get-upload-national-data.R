#!/usr/bin/env Rscript
library(sf)
library(tidyverse)
library(spanishoddata)
library(arrow)

# Set data directory
spod_set_data_dir("data")

# Get zones
message("Fetching zones...")
zones <- spod_get_zones(zones = "distr", ver = 2)

# Get valid dates and identify the latest 7 days
valid_dates <- spod_get_valid_dates(ver = 2)
recent_dates <- tail(valid_dates, 7)
message("Latest 7 days available: ", paste(recent_dates, collapse = ", "))

# Define geographic bounding box for the Northern Spain study area
bbox_north <- st_bbox(c(xmin = -3.1, ymin = 42.8, xmax = -1.0, ymax = 43.4), crs = st_crs(4326))
bbox_sf <- st_as_sfc(bbox_north)

# Filter zones that intersect the bounding box
sf::sf_use_s2(FALSE)
zones_wgs84 <- st_transform(zones, 4326)
zones_north <- zones_wgs84 |>
  st_filter(bbox_sf, .predicate = st_intersects)

# Prepare locations data (centroids)
locations_north <- zones_north |>
  st_centroid() |>
  st_coordinates() |>
  as.data.frame() |>
  mutate(id = zones_north$id) |>
  rename(lon = X, lat = Y)

# Get OD data for the latest 7 days
message("Downloading OD data for 7 days (this may take a few minutes)...")
flows <- spod_get(
  type = "origin-destination",
  zones = "districts",
  dates = recent_dates
)

dim(flows)
flows_national_raw <- flows |> collect()
dim(flows_national_raw)
# Save as parquet and rds and upload
system.time({
  arrow::write_parquet(flows_national_raw, "flows_national_raw.parquet")
})

system.time({
  saveRDS(flows_national_raw, "flows_national_raw.rds")
})

# Upload to github release
system.time({
system("gh release upload v1 flows_national_raw.parquet")
})

# Filter and aggregate the OD data by hour
message("Filtering and aggregating data...")
od_data_time <- flows |>
  mutate(time = as.POSIXct(paste0(date, "T", hour, ":00:00"))) |>
  group_by(origin = id_origin, dest = id_destination, time) |>
  summarise(count = sum(n_trips, na.rm = TRUE), .groups = "drop") |>
  collect()

flows_north_time <- od_data_time |>
  filter(origin %in% zones_north$id & dest %in% zones_north$id)

# Save output files locally in Parquet, Arrow (Feather), and RDS formats
message("Saving generated files...")
arrow::write_parquet(flows_north_time, "flows_north_time.parquet")
arrow::write_parquet(locations_north, "locations_north.parquet")

arrow::write_feather(flows_north_time, "flows_north_time.arrow")
arrow::write_feather(locations_north, "locations_north.arrow")

saveRDS(flows_north_time, "flows_north_time.rds")
saveRDS(locations_north, "locations_north.rds")

# Save national OD files
message("Saving national files...")
arrow::write_parquet(od_data_time, "od-data-week-national.parquet")
saveRDS(od_data_time, "od-data-week-national.rds")

# Save national zones
message("Saving national zones...")
sf::st_write(zones, "zones-national.gpkg", delete_dsn = TRUE)
saveRDS(zones, "zones-national.rds")

# Upload the files to the latest GitHub release (v1) using the gh CLI
message("Uploading files to GitHub Release v1...")
system("gh release upload v1 flows_north_time.parquet locations_north.parquet flows_north_time.arrow locations_north.arrow flows_north_time.rds locations_north.rds od-data-week-national.parquet od-data-week-national.rds zones-national.gpkg zones-national.rds --clobber")

message("Data update and upload complete!")

