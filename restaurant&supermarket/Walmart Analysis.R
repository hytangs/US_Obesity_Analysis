# setwd("~/Desktop/BT4015 Geospatial Analytics/Project")

# Load Required Packages
library(usmap) 
library(ggplot2) 
library(tmap)
library(sf)
library(stringr)
library(sp)
library(dplyr)
require(maps)
require(maptools)
library(ks)
library(rgdal)
library(raster)
library(spatstat)
library(fMultivar)


################################ Plot points ################################ 

# Filter out data for Alaska, Hawaii and Puerto Rico
# Only Keeping Data in the contiguous United State
usa <- map("state", fill = TRUE)
IDs <- sapply(strsplit(usa$names, ":"), function(x) x[1])
usa <- map2SpatialPolygons(usa, IDs=IDs, proj4string=CRS("+proj=longlat +datum=WGS84"))
walmart_data <- read.csv("/Users/Sigrid/Documents/GitHub/US_Obesity_Analysis/restaurant&supermarket/walmart_2018_11_06.csv")
walmart_data <- walmart_data[,c(17,16,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,18,19,20,21,22)]
df <- SpatialPointsDataFrame(coords = walmart_data[, c("longitude", "latitude")], data = walmart_data,
                             proj4string = CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"))
walmart_sp <- df[!is.na(over(df, as(usa, "SpatialPolygons"))), ]

# Load State shape file downloaded from census.gov
states_sf <- st_read('/Users/Sigrid/Documents/GitHub/US_Obesity_Analysis/restaurant&supermarket/data/cb_2018_us_state_20m/cb_2018_us_state_20m.shp')
states_sf <- states_sf[!(states_sf$NAME %in% c('Alaska', 'Hawaii', 'Puerto Rico')),]

# Plot distribution of Walmart in the US
tm_shape(states_sf) + tm_borders() +
  tm_shape(walmart_sp) + tm_dots(size=0.03, col='darkblue', title='Walmart') +
  tm_layout(main.title='Locations of Walmart in the U.S.',
            main.title.size=1,
            main.title.fontface='bold',
            main.title.position=c('center', 'center')) +
  tm_compass(type='rose', size=2, position=c(0.02, 0.15)) +
  tm_scale_bar(position=c(0.02, 0.02))



################################### KDE ###################################

# Change SpatialPointsDataFrame back to DataFrame
new_walmart <- as.data.frame(walmart_sp)

# Calcualte Walmart Density
KDE <- kde(new_walmart[c('longitude','latitude')])

# This time only keeping the US border for clarity
usa <- map("usa", fill = TRUE)
IDs <- sapply(strsplit(usa$names, ":"), function(x) x[1])
usa <- map2SpatialPolygons(usa, IDs=IDs, proj4string=CRS("+proj=longlat +datum=WGS84"))

# Plot KDE together with the US border
plot(KDE, display="filled.contour", cont=seq(10,90,by=20), lwd=1 ,main=paste("KDE of Walmart"),xlim=c(-125,-67),ylim=c(25,49))
plot(usa, add=TRUE)



################################### ANN ###################################

# Create a ppp point layer of states
states_ppp <- as_Spatial(states_sf)
crs(states_ppp) <- NA
states_ppp <- as.owin(states_ppp)

# Create a ppp point layer of walmart
walmart_ppp <- as.ppp(new_walmart[c('longitude','latitude','phone_number_1')], W = owin(c(-125,-67), c(25,49)))
walmart_ann <- mean(nndist(walmart_ppp, k=1))
walmart_ann #0.1571437

n <- 100L #define no. of simulations
ann.r <- vector(length=n) #store simulated ANN values

for (i in 1:n) { #loop to create different simulated distributions
  rand.p <- rpoint(n=walmart_ppp$n, win=states_ppp)
  ann.r[i] <- mean(nndist(rand.p, k=1))
}

# Plot histogram of expected values under the null hypothesis
hist(ann.r, main=NULL, las=1, breaks=40, col="bisque", xlim=range(walmart_ann, ann.r))
abline(v=walmart_ann, col="blue")

# Compute a pseudo p-value for the simulation
N.greater <- sum(ann.r > walmart_ann)
p <- min(N.greater+1, n+1-N.greater)/(n+1)
p #0.00990099


################################# Hexagonal Binning #################################

# Include separate code snippet to convert coordinates to SpatialPolygonsDataFrame 
# consisting of the hexagons
hexbin_map <- function(spdf, ...) {
  hbins <- fMultivar::hexBinning(coordinates(spdf), ...)
  
  #set up hexagons to plot as polygons
  u <- c(1, 0, -1, -1, 0, 1)
  u <- u * min(diff(unique(sort(hbins$x))))
  v <- c(1, 2, 1, -1, -2, -1)
  v <- v * min(diff(unique(sort(hbins$y))))/3
  
  #construct each polygon in sp model 
  hexes_list <- vector(length(hbins$x), mode='list')
  for (i in 1:length(hbins$x)) {
    pol <- Polygon(cbind(u+hbins$x[i], v+hbins$y[i]), hole=FALSE)
    hexes_list[[i]] <- Polygons(list(pol), i)
  }
  
  #build spdf
  hex_cover_sp <- SpatialPolygons(hexes_list, proj4string=CRS(proj4string(spdf)))
  hex_cover <- SpatialPolygonsDataFrame(hex_cover_sp, data.frame(z=hbins$z), match.ID=FALSE)
  
  #return result
  return(hex_cover)
}

# Plot Hexagonal Binning Map
walmart_hex <- hexbin_map(walmart_sp, bins=30)
tm_shape(states_sf) + tm_polygons() +
  tm_shape(walmart_hex) +
  tm_fill(col='z', alpha=0.9,
          title='No. of Walmarts') +
  tm_layout(main.title='Walmart Density in the US',
            main.title.size=0.8,
            main.title.fontface='bold',
            main.title.position=c('left', 'top'),
            legend.outside=TRUE) +
  tm_compass(type='rose', size=1.5, position=c(0.85, 0.03)) +
  tm_scale_bar(position=c(0.02, 0.03))


