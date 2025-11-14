#title: "2.2 Trip Distribution Exercise"
#original author: "Xuan He, IVT, ETHZ"
#changes by: Group A

## Introduction

#In this part, you will have to code in R the trip distribution for the calculated generated trips with the help of the attracted trips. 
#So that we can later check whether the performed trip distribution is plausible, 
#in a first step (a) we visualize the origin-destination flows between the zones in the Zurich Region based on the MZMV's data. 
#Then in the second step (b), we implement the Furness-Method with a function/loop in R to execute the distribution of the trips between the zones.

## Step-by-Step Instructions with Code

## Part a) Origin-destination flows between the zones from MZMV

### the steps do not match the ones on the worksheet

### Step 0: Load packages and define base paths

# Load necessary libraries
library(tidyverse)
library(data.table)
library(sf)
library(ggplot2)
library(geodist)
library(dodgr)
library(osmdata)
library(patchwork)
library(viridis)
library(RColorBrewer)
library(dplyr)
library(tidyr)
library(purrr)
library(stringr)


### Step 1: Read Data - here for Julians PC

# Load zones and trips data
npvm_zones <- read_sf("/Users/julianbirkel/Documents/Master/Sem 3/Transport Planning Methods/Exercise 2.2/npvm_zones_with_trips.gpkg") #here is the results from Assignment 2.1
df_tripsCH <- fread("/Users/julianbirkel/Documents/Master/Sem 3/Transport Planning Methods/Exercise 2.2/wegeinland.csv")

### Step 2: Spatial operations
# Check and transform CRS
st_crs(npvm_zones)[1]
npvm_zones <- st_transform(npvm_zones, crs = 4326)

### Step 3: Assign Zones to Trips
# 3.1 Generate an unique-IDs for trips needed for later keep overview on spatial operations and convert trips to spatial data frames
df_tripsCH <- df_tripsCH %>%
  mutate(weg.id = row_number()) %>%
  relocate(weg.id)

# 3.2 Convert trips to spatial dataframe, one start df, one end df #WGS 84 coordinates
trips_Start <- df_tripsCH %>%
  st_as_sf(coords = c("S_X", "S_Y"), crs = 4326)

trips_Ziel <- df_tripsCH %>%
  st_as_sf(coords = c("Z_X", "Z_Y"), crs = 4326)


# 3.3 Assign the start/ziel-zone ("start/ziel_row.id"=unique ID of NPVM zones) to the trips
zone_Start <- trips_Start %>%
  mutate(start_row.id=(st_intersects(trips_Start$geometry, npvm_zones))) %>% 
  relocate(start_row.id) %>%
  mutate(start_row.id=as.integer(start_row.id))

zone_Ziel <- trips_Ziel %>%
  mutate(ziel_row.id = (st_intersects(trips_Ziel$geometry, npvm_zones))) %>%
  relocate(ziel_row.id) %>%
  mutate(ziel_row.id = as.integer(ziel_row.id))


# 3.4 Merge spatial information with main trips data
trips <- df_tripsCH %>%
  left_join(zone_Start[, c("weg.id", "start_row.id", "geometry")], by = "weg.id") %>% 
  left_join(zone_Ziel[, c("weg.id", "ziel_row.id", "geometry")], by = "weg.id", suffix = c("_S", "_Z"))


# 3.5 Filter trips within the study area
trips <- trips %>% filter(!is.na(start_row.id) & !is.na(ziel_row.id))


### Step 4: Visualize Origin-Destination Flows

# 4.1 Function to create linesstrings for trips
create_linestring <- function(point1, point2) {
  st_linestring(rbind(point1, point2))
}

# 4.2 Create linestrings by calling function

lines_sf <- mapply(create_linestring, trips$geometry_S, trips$geometry_Z, SIMPLIFY = FALSE)
lines_sf <- st_sf(geometry = st_sfc(lines_sf), crs = 4326)

# 4.3 Plot zones and OD lines - this plot will be full of lines
ggplot() +
  geom_sf(data = npvm_zones) +
  geom_sf(data = lines_sf)

#save the plot
ggsave(
  filename = "/Users/julianbirkel/Documents/Master/Sem 3/Transport Planning Methods/Exercise 2.2/OD_Lines_uncleaned.png",
  width = 8,
  height = 6,
  dpi = 300
)


### Step 5: Aggregated OD-Flows between zones
# Interzonal: Lines which are different colour depending on the amount of trips
# Intrazonal(=in same zone): Different background colour of zones depending on amount of trips

# 5.1 Aggregate commuting trips by OD-relation
tripsOD <- trips %>%
  filter(wzweck1 == 2) %>% # 2 are work trips
  group_by(start_row.id, ziel_row.id, wzweck1) %>% 
  summarise(n = sum(WP, na.rm = TRUE)) %>% 
  pivot_wider(names_from = ziel_row.id, values_from = n, values_fill = 0)

# 5.2 Generate centroids for zones
#**This is a simplification. 
#*The actual centroid in a transport model should be the center of activities or inhabitants, not the geographical center of a political boundary.
npvm_centroids <- npvm_zones %>% st_centroid()

# 5.3 Create matrix for all OD-relations and add point geometry for Origin and Destination
npvm_centroids_matr <- expand_grid(npvm_centroids$row_id,npvm_centroids$row_id) %>%
  setNames(c("start_row.id", "ziel_row.id")) %>%
  left_join(npvm_centroids %>% select(row_id, geom),
            by = c("start_row.id" = "row_id"))%>%
  left_join(npvm_centroids %>% select(row_id, geom),
            by = c("ziel_row.id" = "row_id"))
  
# 5.4 Create lines between all OD relations and add them to the matrix
od_lines_all_sf <- mapply(create_linestring, npvm_centroids_matr$geom.x, npvm_centroids_matr$geom.y, SIMPLIFY = FALSE)
od_lines_all_sf <- st_sf(geometry = st_sfc(od_lines_all_sf),crs=4326) 
od_lines_all_sf$start_row.id = npvm_centroids_matr$start_row.id
od_lines_all_sf$ziel_row.id = npvm_centroids_matr$ziel_row.id
  
# add linestrings to Matrix
od_lines_MZMV_sf= left_join(npvm_centroids_matr,
                            od_lines_all_sf,
                            by = c("start_row.id", "ziel_row.id"))


#### Step 6 Visualize them separated by intrazonal and interzonal trips
# 6.1 Calculate intrazonal trips
# start_row.id must have same number as where a number is present in the corresponding column
wegeIntra_MZMV <- tripsOD %>%
  rowwise() %>%
  mutate(intrazonal_trips = {
    colname <- as.character(start_row.id)
    if (colname %in% names(cur_data())) {
      cur_data()[[colname]]
    } else {
      NA_real_   # return NA if the destination column doesn’t exist
    }
  }) %>%
  ungroup() %>%
  select(start_row.id, intrazonal_trips)

#Sum up intrazonal trips
wegeIntra_MZMV <- npvm_zones %>%
  left_join(wegeIntra_MZMV, join_by("row_id" == "start_row.id"))

# 6.2 Sorting data for visualisation

tripsOD_long <- tripsOD %>%
  select(-wzweck1) %>%  # remove purpose column
  pivot_longer(
    cols = -start_row.id,          # all columns except start_row.id
    names_to = "ziel_row.id",      # new column for destination zone
    values_to = "n"                # new column for number of trips
  )

tripsOD_long <- tripsOD_long %>%
  mutate(ziel_row.id = as.numeric(ziel_row.id))

od_lines_MZMV_sf <- st_as_sf(od_lines_MZMV_sf)
od_lines_MZMV_sf <- st_sf(od_lines_MZMV_sf, geometry = od_lines_MZMV_sf$geometry, crs = 4326)
od_lines_MZMV_sf <- st_as_sf(od_lines_MZMV_sf, wkt = "geometry", crs = 4326)
od_lines_MZMV_sf <- od_lines_MZMV_sf %>%
  left_join(tripsOD_long, by = c("start_row.id", "ziel_row.id"))

# remove zeros
od_lines_MZMV_sf_nonzero <- od_lines_MZMV_sf %>%
  filter(!is.na(n) & n > 0)

# 6.3 Visualisation function
visu_gg = function(zones, od_lines) {
  
  #Filter: keep only top 20% of trips
  od_lines_filtered <- od_lines %>%
    filter(!is.na(n)) %>%
    filter(n > quantile(n, 0.8, na.rm = TRUE) & 
             n < quantile(n, 1, na.rm = TRUE))
  
  #i) Intrazonal map (zones only)
  p_intra <- ggplot() +
    geom_sf(data = zones, aes(fill = intrazonal_trips), color = "grey40", size = 0.2) +
    scale_fill_viridis_c(option = "plasma", na.value = "white", name = "Intrazonal trips") +
    theme_minimal() +
    theme(
      legend.position = "right",
      panel.grid = element_blank(),
      axis.text = element_blank(),
      axis.ticks = element_blank()
    ) +
    labs(title = "Intrazonal Trips",
         subtitle = "Zones colored by intrazonal trips")
  
  #ii) Interzonal map (lines over zone background)
  p_inter <- ggplot() +
    # zone background
    geom_sf(data = zones, fill = "grey90", color = "white", size = 0.1) +
    # interzonal lines (filtered)
    geom_sf(data = od_lines_filtered, aes(color = n, linewidth = n), alpha = 0.7) +
    scale_color_viridis_c(option = "magma",
                          limits = c(quantile(od_lines$n, 0.05, na.rm = TRUE),
                                     quantile(od_lines$n, 0.99, na.rm = TRUE)), #best colour grading
                          name = "Interzonal trips") +
    scale_linewidth_continuous(range = c(0.1, 1.2), guide = "none") +  # adjust line width range
    theme_minimal() +
    theme(
      legend.position = "right",
      panel.grid = element_blank(),
      axis.text = element_blank(),
      axis.ticks = element_blank()
    ) +
    labs(title = "Interzonal Trips",
         subtitle = "Top 20% of interzonal trips")
  
  #Combine both plots side by side
  combined_plot <- p_intra + p_inter + plot_layout(ncol = 2)
  
  return(combined_plot)
}

# 6.4 Call function 
combined_plot <- visu_gg(wegeIntra_MZMV, od_lines_MZMV_sf_top)
combined_plot

# 6.5 Save to file
ggsave(
  filename = "/Users/julianbirkel/Documents/Master/Sem 3/Transport Planning Methods/Exercise 2.2/OD_Lines_and_Zones_combined.png",
  plot = combined_plot,
  width = 12,  # wider for two side-by-side maps
  height = 6,
  dpi = 300
)




####
#   #
#    #
#   #
####
#   #
#    #
#   #
####


## Part b) Implement the Furness Method or Iterative Proportional Fitting to estimate trip distribution between the zones

### Step 1: Balance Produced and Attracted trips per zone

# 1.1 Check number of generated and produced trips -> not equal
sum(npvm_zones$trips_work)
sum(npvm_zones$attractor_work) 

# 1.2 Since sum of all attractions must equal sum of all productions, we have to scale the attractors ups
npvm_zones$trips_work_FUR <- npvm_zones$trips_work

# 1.3 Balance attractions for Furness
scaling_factor <- sum(npvm_zones$trips_work) / sum(npvm_zones$attractor_work)
npvm_zones$attractor_work_FUR <- npvm_zones$attractor_work * scaling_factor

# 1.4 Check again -> equal
sum(npvm_zones$trips_work_FUR)
sum(npvm_zones$attractor_work_FUR)


### Step 2: Get a network for travel time estimation
#We use dodgr for this, a fast and simple way for calculating shortest paths in R:
#<https://cran.r-project.org/web/packages/dodgr/vignettes/dodgr.html>
#Based on the zones, we first need to get a bounding box (polygon) to download the network. 
#We should use a buffer, because there could be streets leaving our zones and entering again

# 2.1 Since the zones are in the wrong CRS we need to convert to another CRS to put a metric buffer.
st_crs(npvm_zones)[1]

bbox <- st_bbox(st_transform(npvm_zones, crs = 32633)) # Transform to a suitable CRS for distance calculations (unit=m)

bbox_polygon <- st_buffer(st_as_sfc(bbox, crs = 32633), dist = 1000) # Create a polygon from the bounding box Add a 1 km buffer

bbox_polygon <- st_transform(bbox_polygon, crs = 4326) # Transform back to the original CRS

bbox <- st_bbox(bbox_polygon)

st_crs(npvm_zones)[1]

print(bbox)

# 2.1 Create the OSM query for roads (takes a while to run)
osm_data_raw = osmdata_sf(opq(bbox = bbox) %>%
                            add_osm_feature(key = "highway", # Look what you are interested in
                                            value = c("motorway_link", "secondary", "tertiary",
                                                      "primary", "motorway", "road",
                                                      "residential","living_street",
                                                      "unclassified")))
# Get line elements only
osm_data = osm_poly2line(osm_data_raw)$osm_lines

# 2.2 Create routable network with dodgr's weightning function (https://www.routino.org/xml/routino-profiles.xml)
network <- weight_streetnet(osm_data, wt_profile = "motorcar")
gc() # free unused memory


### Step 3: Calculate travel time between zones

# 3.1 Rename the columns in npvm_centroids
npvm_centroids_matr <- npvm_centroids_matr %>%
  rename(
    geometry_start = geom.x,
    geometry_ziel  = geom.y
  )

# 3.1 Extract coordinates for ODs from df from a)
from_coords <- do.call(rbind, lapply(npvm_centroids_matr$geometry_start, st_coordinates))  # Extract coordinates 
to_coords <- do.call(rbind, lapply(npvm_centroids_matr$geometry_ziel, st_coordinates))

# 3.2 Calculate travel times for all pairs at once
travel_times <- dodgr_times(graph=network, from=unique(from_coords), to=unique(to_coords)) #need to understand the dodgr_times function
travel_times = as.data.frame(as.table(travel_times))

# 3.3 assign travel times to matrix
npvm_centroids_matr$travelTime <- travel_times$Freq

# 3.4 adjust travel times
# current unit: seconds → convert to minutes
npvm_centroids_matr$travelTime <- npvm_centroids_matr$travelTime / 60 

# 3.5 Adjust travel times, because dodgr is underestimating them
npvm_centroids_matr$travelTime <- npvm_centroids_matr$travelTime * 1.3

# 3.6 Check for NAs
sum(is.na(npvm_centroids_matr$travelTime))
# There were none

# 3.7 drop the column start_row.id
gc_trips_matr_2 <- gc_trips_matr[, !(names(gc_trips_matr) == "start_row.id")]


### Step 4: Preparation for Furness Algorithm

# 4.1 Apply distance decay function: For example $\exp(-\beta \cdot tt)$

# 4.2 Choose beta (decay parameter)
beta <- 0.01

# 4.3 Compute distance decay matrix (same dimension as gc_trips_matr)
gc_trips_matr_DD <- exp(-beta * gc_trips_matr_2)

# 4.4 Force cost matrix to be a clean numeric matrix
gc_trips_matr_DD <- as.matrix(gc_trips_matr_DD)
storage.mode(gc_trips_matr_DD) <- "numeric"

# Quick check if it really is numeric
is.matrix(gc_trips_matr_DD)
is.numeric(gc_trips_matr_DD)
dim(gc_trips_matr_DD)

#### Step 5: Furness Method

# 5.1 Furness Function
Furness <- function() {
  
  # Inputs
  zones    <- npvm_zones$row_id
  outgoing <- npvm_zones$trips_work_FUR
  incoming <- npvm_zones$attractor_work_FUR
  gcosts   <- gc_trips_matr_DD
  
  # Setup
  n <- length(zones)
  gcosts <- as.matrix(gcosts)
  storage.mode(gcosts) <- "numeric"
  if (!all(dim(gcosts) == c(n, n))) stop("Cost matrix has wrong dimensions")
  
  alpha_o_new <- rep(1, n)
  alpha_d_new <- rep(1, n)
  error <- 1e-12
  
  trips  <- matrix(0, nrow = n + 1, ncol = n + 1)
  f_cost <- 1 / (gcosts + 1e-6)
  
  iteration <- 1L
  
  repeat {
    alpha_o <- alpha_o_new
    alpha_d <- alpha_d_new
    
    alpha_o_mat <- matrix(alpha_o, nrow = n, ncol = n)
    alpha_d_mat <- matrix(alpha_d, nrow = n, ncol = n, byrow = TRUE)
    
    # Update flows
    trips[1:n, 1:n] <- alpha_o_mat * alpha_d_mat * f_cost
    
    # Compute row/col sums
    row_sum <- rowSums(trips[1:n, 1:n])
    col_sum <- colSums(trips[1:n, 1:n])
    
    # Avoid division by zero (replace 0 with a very small value)
    row_sum[row_sum == 0] <- 1e-12
    col_sum[col_sum == 0] <- 1e-12
    
    # Update balancing factors -> the 0.5 values act as dampers, so it does not converge endlessly
    alpha_o_new <- 0.5 * alpha_o_new + 0.5 * (outgoing / row_sum)
    alpha_d_new <- 0.5 * alpha_d_new + 0.5 * (incoming / col_sum)
    
    # Replace invalid values with previous iteration values
    alpha_o_new[!is.finite(alpha_o_new)] <- alpha_o[!is.finite(alpha_o_new)]
    alpha_d_new[!is.finite(alpha_d_new)] <- alpha_d[!is.finite(alpha_d_new)]
    
    
    # Fill margins
    trips[1:n, n + 1] <- row_sum
    trips[n + 1, 1:n] <- col_sum
    
    if (iteration %% 10 == 0) cat("Iteration:", iteration, "\n")
    
    # Compute change safely (ignore NA)
    change <- max(abs(alpha_o_new - alpha_o), abs(alpha_d_new - alpha_d), na.rm = TRUE)
    
    if (is.na(change) || change < error) {
      cat("Goal achieved after", iteration, "iterations.\n")
      break
    }
    
    iteration <- iteration + 1L
    
    # Safety cap (to prevent infinite loop)
    if (iteration > 10000) {
      warning("Maximum iterations reached without convergence.")
      break
    }
  }
  
  return(list(
    iterations = iteration,
    trips = trips[1:n, 1:n],
    alpha_o = alpha_o_new,
    alpha_d = alpha_d_new
  ))
}

#5.2 Apply Furness Algorithm to our data by calling function
trips <- Furness()$trips 


### Step 6: Visualization and export of results

# 6.1 Reshape from matrix to long-format
# convert matrix to a df
od_df=as.data.frame(trips) # convert matrix to a df

# Add start_row.id
od_df$start_row.id <- 1:nrow(od_df) 

# Reshape the dataframe to long format
od_df_long_Furness <- od_df %>%
  pivot_longer(
    cols = -start_row.id, # All columns except start_row.id
    names_to = "ziel_row.id", # New column for column names
    values_to = "n_Work") %>%   # New column for values
  mutate(ziel_row.id = as.numeric(str_remove(ziel_row.id, "V")))%>% # Convert   ziel_row.id from character to numeric
  filter(!ziel_row.id == 144)  %>%  # Filter former rowsums
  filter(!start_row.id == 144)    # Filter former columnsums

# 6.3 Check distribution of trips
boxplot(as.vector(trips),
        main = "Distribution of Furness trips",
        ylab = "Number of trips per OD pair")

# 6.4 Calculate intrazonal trips in a separate dataframe
wegeIntra_Furness <- od_df_long_Furness %>%
  filter(start_row.id == ziel_row.id)   # keep only trips within same zone

# 6.5 Join with zone attributes (e.g., geometry, population, etc.)
wegeIntra_Furness <- npvm_zones %>%
  left_join(wegeIntra_Furness, by = c("row_id" = "start_row.id"))

# 6.6 Add linestrings to OD data (joins geometries of each OD pair)
od_df_long_Furness <- left_join(
  od_df_long_Furness,
  od_lines_all_sf,
  by = c("start_row.id" = "start_row.id", "ziel_row.id" = "ziel_row.id")
)

# 6.7 Exclude small numbers (to show only relevant/visible flows)
od_lines_sf_display <- od_df_long_Furness %>%
  filter(n_Work > 10)  # adjust threshold depending on your data scale



# 6.8 Extract coordinates of each point and make a LINESTRING from them
 line_geoms <- map2(
   npvm_centroids_matr$geometry_start,
  npvm_centroids_matr$geometry_ziel,
  ~ {
    coords <- rbind(st_coordinates(.x), st_coordinates(.y))
    st_linestring(coords)
  }
)

# 6.9 Combine into sf object
od_lines_all_sf <- st_sf(
  start_row.id = npvm_centroids_matr$start_row.id,
  ziel_row.id  = npvm_centroids_matr$ziel_row.id,
  geometry = st_sfc(line_geoms, crs = st_crs(npvm_centroids_matr))
 )


# 6.10 Join lines with Furness trip values

 od_df_long_Furness <- od_df_long_Furness %>%
   left_join(
     od_lines_all_sf,
     by = c("start_row.id", "ziel_row.id")
   )

# 6.11 Rebuild a clean join (ignore old messy geometries)
 od_lines_sf_display <- od_df_long_Furness %>%
   select(start_row.id, ziel_row.id, n_Work) %>%             # keep only what we need
   left_join(od_lines_all_sf, by = c("start_row.id", "ziel_row.id")) %>%  # add geometry
   filter(n_Work > 45) %>%                                   # optional threshold
   st_as_sf() %>%
   filter(!st_is_empty(geometry))

# 6.12 Assign CRS (not transform!)
 od_lines_sf_display <- st_set_crs(od_lines_sf_display, 4326)

# 6.13 Visualization
  ### Intrazonal

p_intra <- ggplot() +
  # Polygons (zones)
  geom_sf(data = st_as_sf(npvm_zones), 
          aes(fill = trips_work_FUR/100),
          color = NA) +
  scale_fill_distiller(
    palette = "Blues",
    direction = 1,
    name = "Intrazonal trips"
  ) +
  
  # Title and layout
  labs(title = "Furness Work Trip Distribution – Intrazonal") +
  theme_minimal() +
  theme(
    legend.position = "right",
    panel.grid = element_blank(),
    plot.margin = margin(2, 2, 2, 2, unit = "mm"),
    plot.title = element_text(hjust = 0.5)
  )

# Save the intrazonal plot
ggsave(
  filename = "/Users/julianbirkel/Documents/Master/Sem 3/Transport Planning Methods/Exercise 2.2/intrazonal_fur.png",
  plot = p_intra,
  width = 8,
  height = 6,
  dpi = 300
)

  ### interzonal

# Filter for interzonal flows (adjust column names if needed)
od_lines_inter <- od_lines_sf_display %>%
  filter(start_row.id != ziel_row.id)

# Plot: interzonal flows with white zone background
p_inter <- ggplot() +
  # White background zones
  geom_sf(data = st_as_sf(npvm_zones), 
          fill = "white",   # solid white fill
          color = "grey80", # optional: faint outline for zone borders
          linewidth = 0.2) +
  
  # Interzonal OD flow lines
  geom_sf(data = od_lines_inter,
          aes(size = n_Work, color = n_Work)) +
  scale_size_continuous(
    range = c(0.3, 0.8),
    name = "Trips between zones"
  ) +
  scale_color_viridis_c(
    option = "plasma",
    name = "Trips between zones"
  ) +
  guides(size = "none") +  # remove thickness legend
  
  # Title and layout
  labs(title = "Furness Work Trip Distribution – Interzonal") +
  theme_minimal() +
  theme(
    legend.position = "right",
    panel.grid = element_blank(),
    plot.margin = margin(2, 2, 2, 2, unit = "mm"),
    plot.title = element_text(hjust = 0.5)
  )

# Save the interzonal plot
ggsave(
  filename = "/Users/julianbirkel/Documents/Master/Sem 3/Transport Planning Methods/Exercise 2.2/interzonal_fur.png",
  plot = p_inter,
  width = 8,
  height = 6,
  dpi = 300
)

#Export results
write.csv(od_lines_sf_display[,c(1:3)],"/Users/julianbirkel/Documents/Master/Sem 3/Transport Planning Methods/Exercise 2.2/OD.csv",row.names = F)
