pak::pak("tdscience/tartu26")

library(spanishoddata)
library(flowmapblue)
library(mapgl)
library(tidyverse)
library(sf)
library(tmap)
tmap_mode("view")

spod_set_data_dir('data')
# Get zones
zones <- spod_get_zones(zones = "distr", ver = 2)
valid_dates <- spod_get_valid_dates(ver = 2)
recent_dates = tail(valid_dates, 3)

# Define a geographic bounding box for the Northern Spain study area
# Covers from the coast to south of Pamplona, and from the French border
# westward to include the Pamplona--Donostia corridor
bbox_north <- st_bbox(c(xmin = -3.1, ymin = 42.8, xmax = -1.0, ymax = 43.4), crs = st_crs(4326))
bbox_sf <- st_as_sfc(bbox_north)

# Select zones that intersect the bounding box
sf::sf_use_s2(FALSE)
zones_wgs84 <- st_transform(zones, 4326)
zones_north <- zones_wgs84 |>
  st_filter(bbox_sf, .predicate = st_intersects)

# Visualise the selected zones:
# plot(st_geometry(zones_north)) # basic plot
qtm(zones_north) + qtm(bbox_sf)

# Prepare location data (centroids) for the flow map
locations_north <- zones_north |>
  st_centroid() |>
  st_coordinates() |>
  as.data.frame() |>
  mutate(id = zones_north$id) |>
  rename(lon = X, lat = Y)

system.time({
flows <- spod_get(
  type = "origin-destination",
  zones = "districts",
  dates = recent_dates
)

od_data_time <- flows |>
  mutate(time = as.POSIXct(paste0(date, "T", hour, ":00:00"))) |>
  group_by(origin = id_origin, dest = id_destination, time) |>
  summarise(count = sum(n_trips, na.rm = TRUE), .groups = "drop") |>
  collect()

flows_north_time <- od_data_time |>
  filter(origin %in% zones_north$id & dest %in% zones_north$id)
})
# Save as arrow:
arrow::write_parquet(flows_north_time, "flows_north_time.parquet")
arrow::write_parquet(locations_north, "locations_north.parquet")

# check dataset sizes in memory:
object.size(flows) |> print(units = "Mb")
object.size(od_data_time) |> print(units = "Mb")
object.size(flows_north_time) |> print(units = "Mb")
# We'll also save the datasets as .rds files for people who don't have arrow:
saveRDS(flows_north_time, "flows_north_time.rds")
saveRDS(locations_north, "locations_north.rds")
system("gh release list")
system("gh release upload v1 flows_north_time.parquet locations_north.parquet flows_north_time.rds locations_north.rds --clobber")

message("Precomputed Northern Spain data not found.")
# Download from GitHub release assets:
# system("gh release download v1 --pattern 'flows_north_time.rds'")
release_url <- "https://github.com/tdscience/tartu26/releases/download/v1/"
for(f in c("flows_north_time.rds", "locations_north.rds")) {
  if (!file.exists(f)) download.file(paste0(release_url, f), f, mode = "wb")
}

locations_north <- readRDS("locations_north.rds")
flows_north_time <- readRDS("flows_north_time.rds")
flowmap_north_interactive <- flowmapblue(
  locations = locations_north,
  flows = flows_north_time,
  mapboxAccessToken = Sys.getenv("MAPBOX_TOKEN"),
  darkMode = TRUE,
  animation = FALSE,
  clustering = TRUE
)

# Display the map
flowmap_north_interactive



# Note: this code block requires a version of the mapgl package from the e-kotov R-universe repository, which includes the add_time_control() function for spatio-temporal flow maps.
install.packages('mapgl', repos = c('https://e-kotov.r-universe.dev', 'https://cloud.r-project.org'))
library(mapgl)

# Create the MapLibre map centered on the Northern Spain corridor
north_mapgl <- maplibre(
  style = carto_style("dark-matter"),
  center = c(-1.75, 43.05),
  zoom = 8.5,
  projection = "mercator"
) |>
  add_flowmap(
    id = "north-flows",
    locations = locations_north,
    flows = flows_north_time,
    flow_time_column = "time",
    flow_color_scheme = "Teal",
    flow_dark_mode = TRUE
  ) |>
  add_time_control(
    data = flows_north_time,
    time_column = "time",
    time_interval = "hour",
    title = "Northern Spain OD Flows"
  )

# Display the map
north_mapgl


# Save the map as an HTML file
htmlwidgets::saveWidget(flowmap_north_interactive, "north_spain_flowmap.html")


# ── 1. Load zones and classify by country ──────────────────────────────
sf::sf_use_s2(FALSE)
zones <- spod_get_zones(zones = "distr", ver = 2)

# Recreate larger area covering all the border between Spain and France, then select zones that intersect it:
bbox_north <- st_bbox(c(xmin = -3.1, ymin = 42.0, xmax = 4.0, ymax = 43.4), crs = st_crs(4326))
bbox_sf <- st_as_sfc(bbox_north)
zones_wgs84 <- st_transform(zones, 4326)
zones_north <- zones_wgs84 |>
  st_filter(bbox_sf, .predicate = st_intersects)

# Tag each zone by country
zones_north <- zones_north |>
  mutate(country = ifelse(grepl("^FR", id), "FR", "ES"))

cat("Zones in study area:", nrow(zones_north), "\n")
cat("  Spanish:", sum(zones_north$country == "ES"), "\n")
cat("  French: ", sum(zones_north$country == "FR"), "\n")
mapview::mapview(zones_north, zcol = "country", legend = TRUE) + mapview::mapview(st_geometry(bbox_sf), color = "red", alpha.regions = 0, lwd = 2)

# ── 2. Get OD data and classify each pair ───────────────────────────
valid_dates <- spod_get_valid_dates(2)
recent_dates <- tail(valid_dates, 3)

flows <- spod_get(
  type = "origin-destination",
  zones = "districts",
  dates = recent_dates
)

# Build a lookup table: zone ID → country
zone_country <- zones_north |>
  st_drop_geometry() |>
  select(id, country)

# Filter to study-area zones and tag each OD pair
od_north <- flows |>
  filter(
    id_origin %in% zones_north$id,
    id_destination %in% zones_north$id
  ) |>
  collect() |>
  left_join(zone_country, by = c("id_origin" = "id")) |>
  rename(country_origin = country) |>
  left_join(zone_country, by = c("id_destination" = "id")) |>
  rename(country_dest = country) |>
  mutate(
    border_crossing = case_when(
      country_origin != country_dest ~ "cross-border",
      country_origin == "ES"         ~ "within-Spain",
      country_origin == "FR"         ~ "within-France"
    )
  )

# ── 3. Temporal patterns by border status ──────────────────────────
hourly_by_border <- od_north |>
  group_by(hour, border_crossing) |>
  summarise(
    total_trips = sum(n_trips, na.rm = TRUE),
    .groups = "drop"
  ) |>
  collect()

# Plot: trip volumes by hour, faceted by border status
ggplot(hourly_by_border, aes(x = hour, y = total_trips, colour = border_crossing)) +
  geom_line(linewidth = 1) +
  scale_y_log10(labels = scales::comma) +
  labs(
    title = "Hourly trip volumes by border-crossing status",
    subtitle = paste("Northern Spain study area,", 
                     paste(recent_dates, collapse = ", ")),
    x = "Hour of day",
    y = "Total trips (log scale)",
    colour = "Flow type"
  ) +
  theme_minimal()
ggsave("images/hourly_border_flows.png", width = 8, height = 5)
# ── 4. Distance-decay comparison ───────────────────────────────────
# Compare within-Spain vs cross-border flows at similar distances
dist_comparison <- od_north |>
  group_by(distance, border_crossing) |>
  summarise(
    total_trips = sum(n_trips, na.rm = TRUE),
    n_pairs     = n(),
    .groups     = "drop"
  ) |>
  collect()

if (!file.exists("spain_france_elevation.tif")) {
  release_url <- "https://github.com/tdscience/tartu26/releases/download/v1/spain_france_elevation.tif"
  download.file(release_url, "spain_france_elevation.tif", mode = "wb")
}

library(sf)
library(terra)
library(tidyverse)
library(spanishoddata)
library(tmap)

elev <- rast("spain_france_elevation.tif")
qtm(elev) + tm_layout(main.title = "Elevation raster for Northern Spain study area")

# Create the transect line in WGS84
line <- st_linestring(matrix(c(-1.8, 1.8, 43.3, 42.3), ncol = 2)) |>
  st_sfc(crs = 4326) |>
  st_sf(geometry = _)

# Project to UTM 30N for equal-length segmentation
line_proj <- st_transform(line, 25830)
pts <- st_line_sample(line_proj, n = 101) |> st_cast("POINT")

# Construct 100 individual line segments (chunks)
segments <- list()
for (i in 1:100) {
  segments[[i]] <- st_linestring(rbind(st_coordinates(pts[i]), st_coordinates(pts[i+1])))
}
segments_sf <- st_sfc(segments, crs = 25830) |> st_sf(segment_id = 1:100, geometry = _)

# Transform segments back to WGS84 for extraction and overlay
segments_wgs84 <- st_transform(segments_sf, 4326)

# Extract average elevation for each segment
elev_vals <- terra::extract(elev, vect(segments_wgs84), fun = mean, na.rm = TRUE)
segments_wgs84$elevation <- elev_vals[, 2]

# Map segments to districts using their centroids
spod_set_data_dir('data')
zones <- spod_get_zones(zones = "distr", ver = 2)
bbox_north <- st_bbox(c(xmin = -3.1, ymin = 42.0, xmax = 4.0, ymax = 43.4), crs = 4326)
bbox_sf <- st_as_sfc(bbox_north)
zones_wgs84 <- st_transform(zones, 4326)
sf_use_s2(FALSE)
zones_north <- zones_wgs84 |> st_filter(bbox_sf, .predicate = st_intersects)

segments_centroid <- st_centroid(segments_wgs84)
segments_zone <- st_join(segments_centroid, zones_north)
segments_wgs84$zone_id <- segments_zone$id

# Calculate flow for each zone from the precomputed dataset
flows_north_time <- readRDS("flows_north_time.rds")
zone_flows <- flows_north_time |>
  group_by(origin) |>
  summarise(total_flow = sum(count, na.rm = TRUE), .groups = "drop")

# Join flow data and handle missing data
segments_final <- segments_wgs84 |>
  left_join(zone_flows, by = c("zone_id" = "origin")) |>
  replace_na(list(total_flow = 0, elevation = 0))

# Calculate Pearson correlation coefficient
cor_val <- cor(segments_final$elevation, segments_final$total_flow, use = "complete.obs")
cat(paste("Pearson Correlation between Elevation and Flow:", round(cor_val, 4), "\n"))

# Define start and end point labels for the map
transect_points <- st_sfc(
  st_point(c(-1.8, 43.3)),
  st_point(c(1.8, 42.3)),
  crs = 4326
) |> st_sf(name = c("Start", "End"), geometry = _)

# A. Create the static Map using tmap
current_mode <- tmap_mode("plot")
map_plot <- tm_shape(elev) +
  tm_raster(col.scale = tm_scale_continuous(values = "terrain"),
            col.legend = tm_legend("Elevation (m)")) +
  tm_shape(zones_north) +
  tm_borders(col = "black", lwd = 0.5) +
  tm_shape(line) +
  tm_lines(col = "blue", linewidth = 3) +
  tm_shape(transect_points) +
  tm_symbols(size = 0.5, col = "red") +
  tm_text("name", ymod = 0.5)

print(map_plot)
tmap_mode(current_mode)

# B. Plot the elevation profile
# Calculate distance along the line from the start point (in km)
start_pt <- st_sfc(st_point(c(-1.8, 43.3)), crs = 4326) |> st_transform(25830)
segments_final_proj <- st_transform(segments_final, 25830)
segments_final$dist_km <- as.numeric(st_distance(st_centroid(segments_final_proj), start_pt)) / 1000

p_elev <- ggplot(segments_final, aes(x = dist_km, y = elevation)) +
  geom_line(color = "darkgreen", linewidth = 1) +
  geom_point(color = "darkgreen", size = 1.5) +
  labs(
    title = "Elevation Profile Along the Transect Line",
    x = "Distance from Start (km)",
    y = "Average Elevation (m a.s.l.)"
  ) +
  theme_minimal()
print(p_elev)

# C. Plot the elevation vs flow correlation
p_cor <- ggplot(segments_final, aes(x = elevation, y = total_flow)) +
  geom_point(color = "blue", alpha = 0.6, size = 2) +
  geom_smooth(method = "lm", color = "red", se = TRUE) +
  scale_y_continuous(labels = scales::comma) +
  labs(
    title = "Correlation: Elevation vs District Flow",
    subtitle = paste("Pearson Correlation:", round(cor_val, 4)),
    x = "Average Elevation (m a.s.l.)",
    y = "Total Zone Origin Flow"
  ) +
  theme_minimal()
print(p_cor)
