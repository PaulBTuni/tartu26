pkgs <- c(
    "tidyverse", # Data manipulation and visualisation
    "sf", # Spatial data handling
    "sfnetworks", # Spatial network operations
    "osmextract", # Download OSM data
    "tmap", # Static and interactive maps
    "tmap.networks", # Network-specific map functions
    "tidygraph", # Tidy graph operations
    "igraph", # Graph algorithms
    "crsuggest" # Suggest appropriate coordinate reference systems
)

pkgs_installed = all(pkgs %in% installed.packages())

# Install the packages
if (!pkgs_installed) {
    pak::pkg_install(pkgs)
}
# Load all packages
sapply(pkgs, require, character.only = TRUE)


release_url <- "https://github.com/tdscience/tartu26/releases/download/v1/"
for (f in c("flows_north_time.rds", "locations_north.rds")) {
    download.file(paste0(release_url, f), f, mode = "wb")
}

# Load the flows and location data
flows_north_time <- readRDS("flows_north_time.rds")
locations_north <- readRDS("locations_north.rds")

# Find the zones involved in the top 50 flows
max_flow_zones <- flows_north_time |>
    slice_max(n = 50, order_by = count) |>
    select(origin, dest, count) |>
    pivot_longer(names_to = "type", values_to = "zone", cols = -count) |>
    mutate(zone = as.character(zone)) |>
    # Filter out external zones (represented by underscores and "external")
    filter(str_detect(zone, "_|external", negate = TRUE)) |>
    pull(zone) |>
    unique()

# Set seed for reproducibility
set.seed(1234)

# Convert zone locations to sf object with geographic coordinates
all_zones <- locations_north |>
    st_as_sf(coords = c("lon", "lat"), crs = 4326)

# Randomly sample 3 zones from the high-flow areas
sample_zones <- all_zones |>
    filter(id %in% max_flow_zones) |>
    sample_n(3)

# Create a buffer around all zones to define the study area
zones_buffer <- all_zones |>
    st_union() |>
    st_buffer(5e3) |> # 5 km buffer
    st_convex_hull() # Create convex hull

# Download driving network for Spain from OSM
# Warning: this can take some time!
net <- oe_get_network(
    "Spain",
    mode = "driving",
    extra_tags = c("maxspeed")
    # Optionally filter by bounding box:
    # boundary = zones_buffer,
    # boundary_type = "clip"
)

# Define road types to retain (motorways, trunk, primary, secondary roads)
highway_types_valid <- c("motorway", "trunk", "primary", "secondary")

# Simplify the highway classification by removing "_link" suffix
# (e.g., "primary_link" becomes "primary")
net$highway <- str_remove(net$highway, "_link")

# Filter to only the selected road types
net <- net |>
    filter(highway %in% highway_types_valid)


# Ensure all geometries are single-part linestrings (not multi-part)
net <- net |> st_cast("LINESTRING")

# Remove unnecessary columns to reduce data size
net$other_tags <- NULL

# Calculate the most common speed limit for each road type
# by finding the limit that covers the greatest distance
speed_limits <- net |>
    mutate(dist = as.numeric(st_length(geometry))) |>
    st_drop_geometry() |>
    drop_na(maxspeed) |>
    summarise(across(dist, sum), .by = c(highway, maxspeed)) |>
    slice_max(dist, by = "highway", n = 1)

# Create a lookup column for missing speed limits based on road type
net$maxspeed_ifna <- as.numeric(
    speed_limits$maxspeed[match(net$highway, speed_limits$highway)]
)

# Convert speed limits to numeric (some are stored as text)
net$maxspeed <- as.numeric(net$maxspeed)

# Fill missing speed limits with the mode (most common) for that road type
net$maxspeed <- coalesce(net$maxspeed, net$maxspeed_ifna)

# Keep only essential columns for routing
net <- net |>
    select(osm_id, name, highway, maxspeed)

# Filter network to study area
zones_mask <- st_within(net, zones_buffer) |> vapply(length, integer(1))
zones_mask <- zones_mask > 0

# Save the complete network and the study area subset
write_rds(net, "clean_net.rds")
write_rds(net[zones_mask, ], "clean_net_study_area.rds")

# Upload to GitHub releases
# system("gh release upload v1 clean_net.rds clean_net_study_area.rds --clobber")

# Check if the processed network exists locally; if not, download
if (!file.exists("clean_net_study_area.rds")) {
    message(
        "Pre-processed network not found. Downloading from GitHub releases..."
    )
    release_url <- "https://github.com/tdscience/tartu26/releases/download/v1/"
    download.file(
        paste0(release_url, "clean_net_study_area.rds"),
        "clean_net_study_area.rds",
        mode = "wb"
    )
}

# Load the network
net <- readRDS("clean_net_study_area.rds")

# Find the best projected coordinate reference system for our area
best_crs <- suggest_top_crs(net, units = "m")

# Create the sfnetwork with the best CRS
# directed = FALSE means we can travel in both directions on roads
sfnet <- net |>
    st_transform(best_crs) |>
    as_sfnetwork(directed = FALSE)

# Create junctions where road segments overlap
# This converts implicit intersections into explicit nodes
sf_net_subdiv <- convert(sfnet, to_spatial_subdivision)

# Keep only the largest connected component (ignoring isolated segments)
# and calculate travel times for each edge
sf_net_subdiv <- sf_net_subdiv |>
    activate(nodes) |>
    # Filter to connected nodes (group_components() labels each connected component)
    filter(group_components() == 1) |>
    activate(edges) |>
    # Calculate travel time based on distance and speed
    mutate(
        speed = units::set_units(maxspeed[cur_group_id()], "km/h"),
        dist = st_length(geometry),
        travel_time = units::set_units(dist / speed, "min")
    )

# Remove nodes of degree 2 (intermediate nodes on straight road segments)
# and aggregate edge attributes appropriately
sf_net_simpl <- convert(
    sf_net_subdiv,
    to_spatial_smooth,
    summarise_attributes = list(
        dist = "sum", # Sum distances
        travel_time = "sum", # Sum travel times
        osm_id = "first", # Keep first OSM ID
        highway = "first" # Keep first highway type
    )
)

# Create a map showing the network coloured by road type
tm_shape(sf_net_simpl) +
    tm_edges("highway") +
    # Overlay the sample zones in red
    tm_shape(sample_zones) +
    tm_dots(col = "red", size = 0.5)

# Diameter in travel time (minutes)
diam_time <- diameter(
    sf_net_simpl,
    directed = FALSE,
    weights = pull(sf_net_simpl, "travel_time")
)
cat("Network diameter (travel time):", as.numeric(diam_time), "minutes\n")

# Diameter in distance (kilometres)
diam_dist <- diameter(
    sf_net_simpl,
    directed = FALSE,
    weights = pull(sf_net_simpl, "dist")
) *
    1e-3 # Convert from m to km
cat("Network diameter (distance):", diam_dist, "km\n")

# Calculate edge betweenness centrality weighted by travel time
sf_net_simpl <- sf_net_simpl |>
    activate(edges) |>
    mutate(
        # betweenness: higher values = more central to network connectivity
        betweenness = centrality_edge_betweenness(weights = travel_time)
    )

# Visualise centrality as a map
ggplot() +
    geom_sf(
        data = st_as_sf(sf_net_simpl, "edges"),
        aes(colour = log10(betweenness)),
        linewidth = 1
    ) +
    scale_colour_viridis_c(name = "Log10(Betweenness)") +
    theme_void() +
    labs(title = "Road Network Centrality")

# Extract nodes as spatial features
sf_nodes <- st_as_sf(sf_net_simpl, "nodes")

# Find the nearest node for each sample zone
node_indexes <- sample_zones |>
    st_transform(st_crs(sf_net_simpl)) |>
    st_nearest_feature(sf_nodes)

# Create a lookup table linking zones to nodes
zone_to_node <- tibble(
    zone = sample_zones$id,
    node = node_indexes
)

# Create an origin-destination table
# We'll route from one zone to all others
sample_od <- expand_grid(
    origin = sample_zones$id[2],
    dest = sample_zones$id) |>
    filter(origin != dest) |>
    # Join with node indices
    left_join(zone_to_node, by = c("origin" = "zone")) |>
    left_join(zone_to_node, by = c("dest" = "zone"),
              suffix = c(".o", ".d"))

# Aggregate flows by hour and find the peak
max_flow_time <- flows_north_time |>
    semi_join(sample_od,
              by = c("origin","dest")) |>
    summarise(across(count,sum),.by = time) |>
    slice_max(n = 1,order_by = count) |>
    pull(time)

cat("Peak flow hour:", max_flow_time, "\n")

sample_od <- sample_od |>
    # Join with flow data at peak hour
    left_join(
        flows_north_time |>
            filter(time == max_flow_time) |>
            select(-time),
        by = c("origin", "dest")
    )

# Calculate straight-line distance between zones
sample_od$straight_line_dist <- st_distance(
    sample_zones[match(sample_od$origin, sample_zones$id), ],
    sample_zones[match(sample_od$dest, sample_zones$id), ],
    by_element = TRUE
) |>
    as.numeric()

# Calculate quickest paths from one origin to all destinations
# weights = "travel_time" means we minimise travel time, not distance

# This code has been extracted from the sfnetworks documentation and adapted for our dataset

paths <- st_network_paths(
    sf_net_simpl,
    from = sample_od$node.o[1], # Start from first origin
    to = sample_od$node.d, # End at all destinations
    weights = "travel_time"
)

# Helper function to plot a path on the network
plot_path <- function(node_path) {
    sf_net_simpl %>%
        activate("nodes") %>%
        slice(node_path) %>%
        plot(cex = 1.5, lwd = 1.5, add = TRUE)
}

# Set up plot with sample colours
colours <- sf.colors(4, categorical = TRUE)

# Plot the network and overlay all paths
plot(sf_net_simpl, col = "grey")
paths %>%
    pull(node_paths) %>%
    walk(plot_path)

# Highlight sample zones
sf_net_simpl %>%
    activate("nodes") %>%
    st_as_sf() %>%
    slice(node_indexes) %>%
    plot(col = colours, pch = 8, cex = 2, lwd = 2, add = TRUE)

# Extract total distance for each path
sample_od$network_dist <- vapply(
    paths$edge_paths,
    function(edges) {
        pull(sf_net_simpl, "dist")[edges] |> sum()
    },
    numeric(1)
)

# Extract total travel time for each path
sample_od$travel_time <- vapply(
    paths$edge_paths,
    function(edges) {
        pull(sf_net_simpl, "travel_time")[edges] |> sum()
    },
    numeric(1)
)

sample_od_summary <- sample_od |>
    mutate(
        directness = straight_line_dist / network_dist,
        # Convert travel time from seconds to minutes
        travel_time_min = as.numeric(travel_time) / 60
    ) |>
    select(
        origin,
        dest,
        straight_line_dist,
        network_dist,
        directness,
        travel_time_min,
        count
    )

# Display a summary
print(sample_od_summary)

# Extract edges as spatial features
sf_edges <- st_as_sf(sf_net_simpl, "edges")

# Assign flows to edges
# For each OD pair, we add its flow count to all edges in its shortest path
edge_flows <- lapply(
    1:nrow(sample_od),
    function(i) {
        # Get edges for this path
        path_edges <- sf_edges[paths$edge_paths[[i]], ] |>
            st_drop_geometry()
        # Add flow count
        path_edges$flow <- sample_od$count[i]
        path_edges
    }
) |>
    bind_rows() |>
    # Aggregate flows for each edge (in case multiple paths use the same edge)
    summarise(across(flow, sum), .by = c(from, to))

sf_edges |>
    left_join(edge_flows, by = c("from", "to")) |>
    tm_shape() +
    # Colour and width roads by flow volume
    tm_lines(
        col = "flow",
        col.scale = tm_scale_intervals(
            n = 5,
            values = "carto.ag_sunset"
        ),
        col.legend = tm_legend(
            title = "Traffic flows assigned to shortest paths",
            legend.position = c("left", "bottom")),
        lwd = "flow",
        lwd.scale = tm_scale_intervals(
            n = 3,
            values = c(2, 3, 4),
            value.na = 1
        ),
        lwd.legend = tm_legend_hide()
    )
