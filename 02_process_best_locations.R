# Load packages
library(sf)
library(leaflet)
library(htmlwidgets)
library('geosphere')

### Step 1: Find the centroid of all meshblocks (in australia - will restrict to Sydney later)

# Define the path to the shapefile - ABS data
shapefile_path <- "../data/MB_2021_AUST_GDA2020.shp"

# Read the shapefile
meshblocks <- st_read(shapefile_path)

# Calculate the centroids
meshblocks_centroids <- st_centroid(meshblocks)

# Extract the meshblock identifier and coordinates
# Adjust the column name for the meshblock identifier as necessary
meshblocks_data <- data.frame(
  MB_code21 = meshblocks$MB_CODE21,  # Adjust the column name if different
  MB_cat21 = meshblocks$MB_CAT21,  # Adjust the column name if different
  Lon = st_coordinates(meshblocks_centroids)[,1],
  Lat = st_coordinates(meshblocks_centroids)[,2]
)

# Tidy  the resulting table
meshblocks_data2 <- meshblocks_data[,-2]
colnames(meshblocks_data2) <- c("MB_CODE_2021" ,"Lon" ,  "Lat")

# Save
write.csv(meshblocks_data2, "mesh_block_centroids.csv")


# need to import population of meshblock - again import ABS data (here NSW only)
mesh_pop <- data.frame(read.csv("../data/meshblock_nsw_pop.csv"))

# merge on centroids to NSW list
mesh_pop2 <- merge(mesh_pop, meshblocks_data2, by = c("MB_CODE_2021"), all.x=FALSE, all.y=FALSE)

# delete unknown locations
mesh_pop3 <- mesh_pop2[mesh_pop2$MB_CODE_2021 != 10000009499,]

# list of all meshblocks with nonzero population, for looking at market share
resi_mesh <- mesh_pop3[mesh_pop3$Person >0,c(1,5,7,8)]

# Potential locations for pharmacies - the centroid of all commercial meshblocks
comm_mesh <- mesh_pop3[mesh_pop3$MB_CATEGORY_NAME_2021  == "Commercial", ]
dim(resi_mesh) 
dim(comm_mesh)

# import pharmacy locations
pharma1 <- data.frame(read.csv("C:/Users/hugh_miller/Documents/GitHub/pharmacy/pharmacies_in_sydney_grid_finer.csv"))


#### Step 2: calculation of distances between actual / potential locations and sydney population


### Calculate distances between residential locations and current pharmacies

# Initialize an empty matrix to store distances
distance_matrix <- matrix(0, nrow = nrow(resi_mesh), ncol = nrow(pharma1))

start.time <- Sys.time()
pharma_coords <- cbind(pharma1$Longitude, pharma1$Latitude)
#for (i in 1:100) {
for (i in 1:nrow(resi_mesh)) {
    # Extract coordinates
    resi_coords <- c(resi_mesh$Lon[i], resi_mesh$Lat[i])

    # Calculate distance and store in matrix
    distance_matrix[i, ] <- distHaversine(resi_coords, pharma_coords) / 1000  # Convert meters to kilometers
  if (i %% 200 == 0){print(i ) }
}
end.time <- Sys.time()
end.time - start.time

# Add row and column names to the matrix
rownames(distance_matrix) <- resi_mesh$MB_CODE_2021


# delete any where the minimum distance is >10km (indication of outside Sydney)
mindist <- apply(distance_matrix, 1, min)   
distance_matrix2 <- distance_matrix[mindist<10,]


### Calculate distances between residential locations and potential pharmacies

resi_mesh2 <- resi_mesh[mindist<10,]

start.time <- Sys.time()
distance_matrix_comm <- matrix(0, nrow = nrow(resi_mesh2), ncol = nrow(comm_mesh))

# Calculate distances
comm_coords <- cbind(comm_mesh$Lon, comm_mesh$Lat)
#for (i in 1:100) {
for (i in 1:nrow(resi_mesh2)) {
    # Extract coordinates
    resi_coords <- c(resi_mesh2$Lon[i], resi_mesh2$Lat[i])

    # Calculate distance and store in matrix
    distance_matrix_comm[i, ] <- distHaversine(resi_coords, comm_coords) / 1000  # Convert meters to kilometers
  if (i %% 200 == 0){print(i ) }
}
end.time <- Sys.time()
end.time - start.time

#### Step 3 - calculate market shares and best locations 

# convert into exponential decay and find distance weight sums for 
decay_parm <- log(0.7) # parameter for how far people will drive to the next pharmacy. log(0.7) means that for each kilometre, willingness / share drops by a factor of 0.7


# Calc share weights
distance_wgt <- exp(decay_parm*distance_matrix2)
distance_wgt_mb <- rowSums(distance_wgt)
distance_wgt_mb[1:20]
hist(distance_wgt_mb)

distance_matrix_comm_wgt <-  exp(decay_parm*distance_matrix_comm)

# market share of each meshblock for each potential location
distance_matrix_mkt_share <- distance_matrix_comm_wgt / t(t(distance_matrix_comm_wgt)+distance_wgt_mb)
hist(distance_matrix_mkt_share)

#population adjust each meshblock and add up. Assume notional $1000 of pharma rev per person, shared across nearby
sales <- 1000*colSums(distance_matrix_mkt_share*resi_mesh2$Person)
hist(sales)
max(sales)/1000

# top 400 locations - will knockout ones too close together
top400 <- order(-sales)[1:400]
myloc <- comm_mesh[top400,]

#remove nearby

remove_nearby <- function(df, distance_threshold) {
  filtered_df <- df[1, ]  # Start with the first coordinate
  
  for (i in 2:nrow(df)) {
    dists <- distHaversine(df[i, c("Lon", "Lat")], filtered_df[, c("Lon", "Lat")])
    if (all(dists > distance_threshold)) {
      filtered_df <- rbind(filtered_df, df[i, ])
    }
  }
  
  return(filtered_df)
}

# Remove coordinates within 1 km of each other, keep top 15
distance_threshold <- 1000  # Distance in meters
filtered_data <- remove_nearby(myloc, 1000)
filtered_data <- filtered_data[1:15,]


# Create and save a Leaflet map
m <- leaflet() %>%
  addProviderTiles(providers$CartoDB.Positron) %>%
  #addTiles() %>%
  addMarkers(data = filtered_data, ~Lon, ~Lat,clusterOptions = markerClusterOptions())

# Save the map to an HTML file
saveWidget(m, file = "C:/Users/hugh_miller/Documents/GitHub/pharmacy/map.html")


#### Step 4 - output map

# Create a Leaflet map
m <- leaflet() %>%
  addProviderTiles(providers$CartoDB.Positron) %>%
  addMarkers(data = pharma1, ~Longitude, ~Latitude ,clusterOptions = markerClusterOptions())

# Save the map to an HTML file
saveWidget(m, file = "C:/Users/hugh_miller/Documents/GitHub/pharmacy/pharma_map.html")




