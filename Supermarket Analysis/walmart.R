# setwd("/Users/Sigrid/Documents/GitHub/US_Obesity_Analysis/Supermarket Analysis")

# Loading Required Libraries
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
library(GISTools)

########################################## Data Preprocessing ##########################################

# Eliminate data in Alaska, Hawaii and Puerto Rico
usa <- maps::map("state", fill = TRUE)
IDs <- sapply(strsplit(usa$names, ":"), function(x) x[1])
usa <- map2SpatialPolygons(usa, IDs=IDs, proj4string=CRS("+proj=longlat +datum=WGS84"))
walmart_data <- read.csv("walmart_2018_11_06.csv")
walmart_data <- walmart_data[,c(17,16,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,18,19,20,21,22)]
df <- SpatialPointsDataFrame(coords = walmart_data[, c("longitude", "latitude")], data = walmart_data,
                             proj4string = CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"))
walmart_sp <- df[!is.na(over(df, as(usa, "SpatialPolygons"))), ]

# Load state-wise obesity data
obesity <- read.csv("ObeDataSet.csv")
x <- raster(xmn=-130, xmx=-60, ymn=20, ymx=55, res=2.2, crs="+proj=longlat +datum=WGS84")
us_obesity <- rasterize(obesity[, c('long', 'lat')], x, obesity[, 'obesity.rate'], fun=mean)
us_obesity  <- as.im(us_obesity)

################################################## EDA ##################################################

# Plot points 
states_sf <- st_read('cb_2018_us_state_20m/cb_2018_us_state_20m.shp')
states_sf <- states_sf[!(states_sf$NAME %in% c('Alaska', 'Hawaii', 'Puerto Rico')),]
tm_shape(states_sf) + 
  tm_borders() +
  tm_shape(walmart_sp) + 
  tm_dots(size=0.03, title='Walmart',col= "orange") +
  tm_layout(main.title='Locations of Walmart in the U.S.',
            main.title.size=1,
            main.title.fontface='bold',
            main.title.position=c('center', 'center')) +
  tm_compass(type='rose', size=2, position=c(0.02, 0.15)) +
  tm_scale_bar(position=c(0.02, 0.02))

# Count number of Walmarts in Each state
choropleth(usa,poly.counts(walmart_sp, usa))
shades <- auto.shading(poly.counts(walmart_sp, usa), n = 5)
choro.legend(-125, 31, shades, cex=0.5, title = "Count of Warmart")
# Add a title to the map
title("Walmart Count By State")

# Print the states with more than 200 fast food restaurants 
result <- poly.counts(walmart_sp, usa)
for (i in 1:length(result)){
  if (result[i] > 200){
    print(result[i]) # value with name
  } 
}

# Plot US obesity density distribution
plot(us_obesity)

############################################ Quadrat Density  ############################################

# Create a ppp point layer of states
states_ppp <- as_Spatial(states_sf)
crs(states_ppp) <- NA
states_ppp <- as.owin(states_ppp)
Window(us_obesity) <- states_ppp

# Create a ppp point layer of 'full'
new_walmart <- as.data.frame(walmart_sp)
walmart_ppp <- as.ppp(new_walmart[c('longitude','latitude','name')], W = owin(c(-125,-67), c(25,49)))
Window(walmart_ppp) <- states_ppp


# Calculate Quadrat count
Q <- quadratcount(walmart_ppp, nx=10, ny=5)
plot(walmart_ppp, pch=20, cols="grey70", main=NULL)  # Plot points
plot(Q, add=TRUE)  # Add quadrat grid
plot(Q)

# Calculate density of each quadrat
Q.d <- intensity(Q)
plot(intensity(Q, image=TRUE), main=NULL, las=1)  # Plot density raster


# Use US obesity of as tesselated surface and 
# Calculte quadrat density on this tessellated surface
brk  <- c( -Inf, 20, 25, 30, 35, 40, Inf)  # Break Obeisty density into 6 bins
Zcut <- cut(us_obesity, breaks=brk, labels=1:6)  # Classify the raster
E    <- tess(image=Zcut)  # Create a tesselated surface
plot(E, main="", las=1)

Q   <- quadratcount(walmart_ppp, tess = E)  # Tally counts
Q.d <- intensity(Q)  # Compute density
Q.d
plot(intensity(Q, image=TRUE), las=1, main=NULL)

############################################### KDE  ###############################################

K1 <- density(walmart_ppp) # Using the default bandwidth
plot(K1, main=NULL, las=1)
contour(K1, add=TRUE)

K2 <- density(walmart_ppp, sigma=2) # Using a 2km bandwidth
plot(K2, main=NULL, las=1)
contour(K2, add=TRUE)

K3 <- density(walmart_ppp, kernel = "disc", sigma=2) # Using a 2km bandwidth with disc smoothing function
plot(K3, main=NULL, las=1)
contour(K3, add=TRUE)

####################################### Poisson Point Process Model #######################################

# Compute rho using the ratio method
walmart_ppp_1 <- as.ppp(new_walmart[c('longitude','latitude')], W = owin(c(-125,-67), c(25,49)))
PPM1 <- ppm(walmart_ppp_1 ~ us_obesity)
PPM1

# Plot the relationship
plot(effectfun(PPM1, "us_obesity", se.fit=TRUE), main=NULL, 
     las=1, legendargs=list(cex=0.8, xpd=TRUE, inset=c(1.01, 0) ))


####################################### Hypothesis Testing: ANN #######################################

ann.p <- mean(nndist(walmart_ppp_1, k=1))
ann.p

n     <- 500L
ann.r <- vector(length=n)
for (i in 1:n){
  rand.p   <- rpoint(n=walmart_ppp$n, f=us_obesity) 
  ann.r[i] <- mean(nndist(rand.p, k=1))
}

Window(rand.p) <- states_ppp  # Replace raster mask with ma.km window

# hist(ann.r.new, main=NULL, las=1, breaks=40, col="bisque", xlim=range(ann.p, ann.r.new))
hist(ann.r, main=NULL, las=1, breaks=40, col="bisque", xlim=range(ann.p, ann.r))
abline(v=ann.p, col="blue")


# Computing a pseudo p-value from the simulation
N.greater <- sum(ann.r > ann.p)
p <- min(N.greater + 1, n + 1 - N.greater) / (n +1)
p


####################################### Hypothesis Testing: Poisson point process model #######################################

# Test for a poisson point process model with a covariate effect
PPM1 <- ppm(walmart_ppp_1 ~ us_obesity)
PPM1

# Fit the model that assumes that the process’ intensity is not a function of population density (the null hypothesis)
PPM0 <- ppm(walmart_ppp_1 ~ 1)
PPM0


anova(PPM0, PPM1, test="LRT")

