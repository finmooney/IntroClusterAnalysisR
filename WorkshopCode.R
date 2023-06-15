setwd("/Users/finmooney/Desktop/R_Workshop/")

# load required packages
library(sf)
library(tidyverse)
library(terra)
library(tigris)
library(raster)
library(rgdal)
library(ggplot2)
library(gstat)
library(leaflet.providers)


### Getting spatial data into r with the st_read function from the SF package and census data 
### from the tigris package

# load Remediation site boundaries from file
Remediation_sites <- st_read("Remediation_site_borders/Remediation_site_borders.shp")

# explore data structure
str(Remediation_sites)

# view the first few rows of data
head(Remediation_sites)

# plot the Remediation site boundaries
ggplot() + 
  geom_sf(data = Remediation_sites)

# download New York State boundary from TIGER/Line shapefiles
ny_state <- states(cb = TRUE, resolution = "20m") %>% 
  filter(NAME == "New York") %>% 
  st_as_sf()

# plot the Remediation site boundaries and New York State boundary
ggplot() + 
  geom_sf(data = ny_state, fill = NA, color = "black") +
  geom_sf(data = Remediation_sites, fill = "red", color = NA) +
  ggtitle("Remediation Sites in New York State")

### Creating a basic merge of spatial data 

# load Census county boundaries for New York State from TIGER/Line shapefiles
ny_counties <- counties(state = "NY", cb = TRUE, resolution = "20m") %>% 
  st_as_sf()

# explore data structure
st_crs(Remediation_sites) 
st_crs(ny_counties) # not projected into UTM
st_crs(ny_state) # not projected into UTM

# make sure the CRS of all datasets match
ny_counties <- st_transform(ny_counties, st_crs(Remediation_sites))
ny_state <- st_transform(ny_state, st_crs(Remediation_sites))

# make sure the geometries in Remediation_sites are valid
Remediation_sites <- st_make_valid(Remediation_sites)

# merge Remediation sites to Census counties
## The st_join() function merges the Remediation site boundaries to the Census counties using the join = st_intersects argument, which joins the two datasets based on their spatial intersection.

ny_counties_Remediation <- st_join(ny_counties, Remediation_sites, join = st_intersects)


# summarize the number of Remediation sites per county
county_summary <- ny_counties_Remediation %>% 
  group_by(NAME) %>% # The group_by() and summarize() functions create a summary table that shows the number of Remediation sites per county, sorted in descending order.
  summarize(num_sites = n()) %>% 
  arrange(desc(num_sites))

ny_counties_Remediation$num_sites <- county_summary$num_sites[match(ny_counties_Remediation$NAME, county_summary$NAME)]

# plot the county boundaries and highlight the counties with the most Remediation sites
ggplot() +
  geom_sf(data = ny_state, fill = NA, color = "black") +
  geom_sf(data = ny_counties_Remediation, aes(fill = num_sites), color = "gray") +
  geom_sf_text(data = county_summary, aes(label = num_sites), size = 3, color = "white", check_overlap = TRUE) +
  labs(fill = "Number of Sites") +
  ggtitle("Remediation Sites by County in New York State")

### The scale_fill_gradient() function creates a color gradient legend to show the number of Remediation sites in each county.

# scale_fill_gradient() produces a two-colour gradient
# scale_fill_gradient2() produces a three-colour gradient with specified midpoint
# scale_fill_gradientn() produces an n-colour gradient

# example:
ggplot() +
  geom_sf(data = ny_state, fill = NA, color = "black") +
  geom_sf(data = ny_counties_Remediation, aes(fill = num_sites), color = "gray") +
  geom_sf_text(data = county_summary, aes(label = num_sites), size = 3, color = "white", check_overlap = TRUE) +
  labs(fill = "Number of Sites") +
  scale_fill_gradient(low = "grey", high = "brown") +
  ggtitle("Remediation Sites by County in New York State")


### We can also map point data with the sf package, let's convert the Remediation boundaries into point data

# create centroid for Remediation boundaries
Remediation_centroid <- st_centroid(Remediation_sites) # we can ignore the warning

# plot the county boundaries and the Remediation centroids
ggplot() +
  geom_sf(data = ny_state, fill = NA, color = "black") +
  geom_sf(data = Remediation_centroid, color = "red", size = .1) +
  ggtitle("Remediation Sites in New York State with Centroids")

### Reading raster files

# Read in the PM 2.5 ADF file as a RasterLayer object, ADF is a common ESRI raster file
pm25 <- raster("AnnAvg1_13_300mRaster/aa12_pm300m/dblbnd.adf")

# Plot the raster
par(mar = c(5, 4, 4, 2) + 0.1)
plot(pm25, main = "PM2.5 Raster Map", col = rev(terrain.colors(255)))

## Plot the raster with new classification

# Plot distribution of raster values 
pm25hist<-hist(pm25,
              breaks=5,
              main="Histogram PM 2.5 NYC",
              col="wheat3",  # changes bin color
              xlab= "PM 2.5 Emissions")  # label the x-axis

# Where are breaks and how many pixels in each category?
pm25hist$breaks

# Set custom breaks and color scale
brks <- c(4, 5, 6, 7, 8, 9, 10)
cols <- heat.colors(length(brks)-1)

# Plot raster with custom breaks and color scale
plot(pm25, 
     breaks = brks, 
     col = cols,
     main="PM2.5 Raster Map with Breaks")

## Plot the raster with reclassified values

# Define a table of old and new values for reclassification
reclass_table <- cbind(c(0, 4, 5, 6, 7, 10),
                       c(NA, 1, 2, 3, 4, 5))

# Reclassify raster values using the reclassify() function
pm25_reclass <- reclassify(pm25, reclass_table)

# Plot the reclassified raster
plot(pm25_reclass, col = heat.colors(5), main = "PM2.5 Raster Map with Reclassified Values")

# Create a custom legend for the reclassified raster
legend("bottomright", legend = c("No Data", "Low", "Very Low", "Moderate", "High", "Very High"),
       fill = cols, title = "PM2.5 Levels")

### Combining Vector and Raster files for Spatial Analysis

# Get the Census Bureau's shapefiles for NYC census tracts
nyc_tracts <- tracts(state = "NY", county = c("Kings", "New York", "Bronx", "Queens", "Richmond"))

# Extract the PM2.5 values for each census tract
pm25_values <- extract(pm25, nyc_tracts, na.rm = TRUE) # the rgdal package is required for spTransform

# Calculate the mean PM2.5 value for each census tract
pm25_means <- sapply(pm25_values, mean)

# Add the mean PM2.5 values to the census tract data
nyc_tracts$pm25_mean <- pm25_means

# Plot the census tracts with color based on the mean PM2.5 value
ggplot(nyc_tracts) + 
  geom_sf(aes(fill = pm25_mean)) +
  scale_fill_gradientn(colors = terrain.colors(7), na.value = "transparent") +
  theme_void()

# Add more detail to the map!

library(tmap) 

nyc_tracts <- nyc_tracts[!is.na(nyc_tracts$pm25_mean), ] # remove tracts with NULL PM 2.5 mean

tm_map <- tm_shape(nyc_tracts) +
  tm_polygons(col = "pm25_mean", alpha = 0.7, 
              colorNA = NULL, 
              title = "Mean PM 2.5 Value"
  ) +
  tm_layout(
    title = "PM2.5 Emission per Census Tract",
    title.position = c("LEFT", "TOP"),
    title.size = 3,
    legend.position = c("LEFT", "TOP"), # position of the legend
    legend.title.size = 3, # adjust size of legend title
    legend.width = 2, # double the width of the legend
    legend.height = 2, # double the height of the legend
    legend.text.size = 1.5, # increase the size of the values in the legend
  )

tm_map

# Interactive Map 

library(leaflet)

# create leaflet map object
lmap <- leaflet(data = nyc_tracts) %>%
  # add OpenStreetMap basemap
  addTiles() %>%
  # add census tract polygons with color fill and popup
  addPolygons(
    # Need to covert to numeric
    fillColor = ~colorNumeric(palette = "YlOrRd", domain = nyc_tracts$pm25_mean)(pm25_mean),
    fillOpacity = 0.9,
    color = "#FFFFFF",
    weight = 1,
    popup = paste("Census Tract ID: ", nyc_tracts$GEOID, "<br>",
                  "Mean PM2.5 Emission: ", nyc_tracts$pm25_mean, " µg/m³")
  ) %>%
  # set map view and zoom level
  setView(lng = -73.95, lat = 40.7, zoom = 10) %>%
  # add legend
  addLegend(position = "bottomright", 
            pal = colorNumeric(palette = "YlOrRd", domain = nyc_tracts$pm25_mean),
            values = nyc_tracts$pm25_mean,
            title = "Mean PM2.5 Emission (µg/m³)") %>%
  # delete this to keep just OpenStreetMap basemap
  addProviderTiles(providers$CartoDB.Positron)

# display map
lmap 

# Interpolation is possible on R, however, it is too advanced for this workshop. I also would suggest using ArcGIS to interpolate. 

# Raster Leaflet

pal = colorNumeric(c("green","yellow", "orange", "red", "darkred"), values(pm25),
                   na.color = "transparent")

lmap_raster <- leaflet() %>%
  addTiles() %>%
  setView(lng = -73.95, lat = 40.7, zoom = 10) %>%
  addRasterImage(pm25, colors = pal, opacity = 0.8) %>%
  addLegend(position = "bottomright", 
            pal = pal,
            values = values(pm25),
              title = "Mean PM2.5 Emission (µg/m³)") 

lmap_raster



### Spatstat Kernel Density Analysis

library(spatstat)

# Create spatial window
nyOwin <- as.owin(sf::st_as_sf(ny_counties))
class(nyOwin)

# Create ppp object
coords <- st_coordinates(Remediation_centroid)
head(coords)
p <- ppp(coords[,1], coords[,2], window=nyOwin)
par(mai=c(0,0,0,0))
plot(p)

# plot kernel density estimate
par(mar = c(1, 1, 1, 1))
plot(density(p, sigma = 20000, kernel = "epanechnikov"), main = "Kernel Density Estimate of Remediation Centroid Points")


## Zoom in on NYC

# Need to project tracts first
nyc_tracts_proj <- st_transform(nyc_tracts, st_crs(Remediation_sites))

# Create spatial window
nycOwin <- as.owin(sf::st_as_sf(nyc_tracts_proj))
class(nycOwin)

# Create a ppp object
p_nyc <- ppp(coords[,1], coords[,2], window = as.owin(nycOwin))

# Create a kernel density estimate
kde <- density.ppp(p_nyc, sigma = 50, kernel = "gaussian") # need to lower the sigma

# Plot the kernel density estimate
par(mar = c(1, 1, 1, 1))
plot(kde, main = "Kernel Density Estimate of Remediation Centroid Points in NYC")

# Calculate the K-function
# The k-function is a way to measure the degree of clustering or dispersion in a point pattern

K <- Kest(p_nyc)

# Plot the K-function
plot(K, main = "K-function for NYC Remediation Points")

# Interpretation: the expected points (solid line) is below the observed points (red dash), indicating that the points in pattern are more dispersed than expected
# It is important to consider spatial scale, if we clipped the points by borough we may see more clustering
