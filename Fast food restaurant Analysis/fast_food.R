# setwd("/Users/Sigrid/Documents/GitHub/US_Obesity_Analysis/Fast food restaurant Analysis")

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

# Importing all fast food restaurant data
kfc <- read.csv("KFC_location.csv")
mc  <- read.csv("McDonalds_location.csv")
ph  <- read.csv("PizzaHut_location.csv")
wb  <- read.csv("Whataburger_location_2018_11_06.csv")

# Adding Restaurant Name
kfc$name ='KFC'
mc$name  = 'McDonalds'
ph$name  = 'PizzaHut'
wb$name  = 'Whataburger'

# Clean McDonald's data
out <- str_split_fixed(mc$geometry.coordinates, ",", 2)
mc <- cbind(mc, out)
mc$`1` <- as.numeric(mc$`1`)
mc$`2` <- as.numeric(mc$`2`)
colnames(mc)[which(names(mc) == "1")] <- "longitude"
colnames(mc)[which(names(mc) == "2")] <- "latitude"

# Keeping only essential information
kfc <- kfc[,c(7,6,5)]
mc  <- mc[,c(28,29,30)]
ph  <- ph[,c(8,6,7)]
wb  <- wb[,c(1,17,16)]

# Standardise Column names
colnames(kfc)[which(names(kfc) == "LON")] <- "longitude"
colnames(kfc)[which(names(kfc) == "LAT")] <- "latitude"
colnames(ph)[which(names(ph) == "LON")] <- "longitude"
colnames(ph)[which(names(ph) == "LAT")] <- "latitude"

# Combine all data together
full <- rbind(kfc,mc,ph,wb)

# Eliminate data in Alaska, Hawaii and Puerto Rico
usa <- maps::map("state", fill = TRUE)
IDs <- sapply(strsplit(usa$names, ":"), function(x) x[1])
usa <- map2SpatialPolygons(usa, IDs=IDs, proj4string=CRS("+proj=longlat +datum=WGS84"))
df <- SpatialPointsDataFrame(coords = full[, c("longitude", "latitude")], data = full,
                             proj4string = CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"))
full_sp <- df[!is.na(over(df, as(usa, "SpatialPolygons"))), ]


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
  tm_shape(full_sp) + 
  tm_dots(size=0.03, title='Fast Food',col= "darkblue") +
  tm_layout(main.title='Locations of Fast Food Restaurants in the U.S.',
            main.title.size=1,
            main.title.fontface='bold',
            main.title.position=c('center', 'center')) +
  tm_compass(type='rose', size=2, position=c(0.02, 0.15)) +
  tm_scale_bar(position=c(0.02, 0.02))

# Count number of fast food restaurants in Each state
choropleth(usa,poly.counts(full_sp, usa))
shades <- auto.shading(poly.counts(full_sp, usa), n = 5)
choro.legend(-125, 30, shades, cex=0.4, title = "Count of Fast Food Restaurants")

# Print the states with more than 900 fast food restaurants 
result <- poly.counts(full_sp, usa)
for (i in 1:length(result)){
  if (result[i] > 900){
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
new_full <- as.data.frame(full_sp)
full_ppp <- as.ppp(new_full[c('longitude','latitude','name')], W = owin(c(-125,-67), c(25,49)))
Window(full_ppp) <- states_ppp


# Calculate Quadrat count
Q <- quadratcount(full_ppp, nx=10, ny=5)
plot(full_ppp, pch=20, cols="grey70", main=NULL)  # Plot points
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

Q   <- quadratcount(full_ppp, tess = E)  # Tally counts
Q.d <- intensity(Q)  # Compute density
Q.d
plot(intensity(Q, image=TRUE), las=1, main=NULL)

############################################### KDE  ###############################################

K1 <- density(full_ppp) # Using the default bandwidth
plot(K1, main=NULL, las=1)
contour(K1, add=TRUE)

K2 <- density(full_ppp, sigma=2) # Using a 2km bandwidth
plot(K2, main=NULL, las=1)
contour(K2, add=TRUE)

K3 <- density(full_ppp, kernel = "disc", sigma=2) # Using a 2km bandwidth with disc smoothing function
plot(K3, main=NULL, las=1)
contour(K3, add=TRUE)

####################################### Poisson Point Process Model #######################################

# Compute rho using the ratio method
full_ppp_1 <- as.ppp(new_full[c('longitude','latitude')], W = owin(c(-125,-67), c(25,49)))
PPM1 <- ppm(full_ppp_1 ~ us_obesity)
PPM1

# Plot the relationship
plot(effectfun(PPM1, "us_obesity", se.fit=TRUE), main=NULL, 
     las=1, legendargs=list(cex=0.8, xpd=TRUE, inset=c(1.01, 0) ))


####################################### Hypothesis Testing: ANN #######################################

ann.p <- mean(nndist(full_ppp_1, k=1))
ann.p

n     <- 100L
ann.r.new <- vector(length=n)
for (i in 1:n){
  rand.p.new   <- rpoint(n=full_ppp$n, f=us_obesity) 
  ann.r.new[i] <- mean(nndist(rand.p.new, k=1))
}

Window(rand.p.new) <- states_ppp  # Replace raster mask with ma.km window

# hist(ann.r.new, main=NULL, las=1, breaks=40, col="bisque", xlim=range(ann.p, ann.r.new))
hist(ann.r.new, main=NULL, las=1, breaks=40, col="bisque", xlim=range(ann.p, ann.r.new+0.001))
abline(v=ann.p, col="blue")


# Computing a pseudo p-value from the simulation
N.greater <- sum(ann.r.new > ann.p)
p <- min(N.greater + 1, n + 1 - N.greater) / (n +1)
p


####################################### Hypothesis Testing: Poisson point process model #######################################

# Test for a poisson point process model with a covariate effect
PPM1 <- ppm(full_ppp_1 ~ us_obesity)
PPM1

# Fit the model that assumes that the process’ intensity is not a function of population density (the null hypothesis)
PPM0 <- ppm(full_ppp_1 ~ 1)
PPM0


anova(PPM0, PPM1, test="LRT")

