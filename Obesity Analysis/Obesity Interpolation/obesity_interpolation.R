library(readxl)
setwd("~/Desktop/4015GIT/Interpolation")

county_health <- read_excel("county_health.xlsx") 
county_health_sp <- SpatialPointsDataFrame(coords = county_health[, c("LON", "LAT")], data = county_health,
                             proj4string = CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"))

county_sf <- st_read('cb_2018_us_county_20m/cb_2018_us_county_20m.shp')
county_sf <- county_sf[!(county_sf$STATEFP %in% c('	02', '15', '72')),]

states_sf <- st_read('cb_2018_us_state_20m/cb_2018_us_state_20m.shp')
states_sf <- states_sf[!(states_sf$NAME %in% c('Alaska', 'Hawaii', 'Puerto Rico')),]


##################################### Plot County Obesity #####################################

tm_shape(states_sf) + 
  tm_polygons() +
  tm_shape(county_health_sp) +
  tm_dots(col="ObesityPercent", palette = "RdBu", title="ObesityPercent", size=0.1) +
  tm_legend(legend.outside=TRUE)

################################################# IDW #################################################

library(gstat) # Use gstat's idw routine
library(sp)    # Used for the spsample function

# Create an empty grid where n is the total number of cells
grd              <- as.data.frame(spsample(county_health_sp, "regular", n=50000))
names(grd)       <- c("X", "Y")
coordinates(grd) <- c("X", "Y")
gridded(grd)     <- TRUE  # Create SpatialPixel object
fullgrid(grd)    <- TRUE  # Create SpatialGrid object

# Add P's projection information to the empty grid
# proj4string(county_health_sp) <- proj4string(county_health_sp) # Temp fix until new proj env is adopted
proj4string(grd) <- proj4string(county_health_sp)

# Interpolate the grid cells using a power value of 2 (idp=2.0)
P.idw <- gstat::idw(ObesityPercent ~ 1, county_health_sp, newdata=grd, idp=5.0)

# Convert to raster object then clip to Texas
r       <- raster(P.idw)
r.m     <- mask(r, states_sf)

# Plot
tm_shape(r.m) + 
  tm_raster(n=10,palette = "RdBu", auto.palette.mapping = FALSE,
            title="Obeisty IDW") + 
  tm_legend(legend.outside=TRUE)
  
 

################################################# Fine-tuning the interpolation #################################################
 
 # Leave-one-out validation routine
IDW.out <- vector(length = length(county_health_sp))
for (i in 1:length(county_health_sp)) {
  IDW.out[i] <- gstat::idw(ObesityPercent ~ 1, county_health_sp[-i,], county_health_sp[i,], idp=2.0)$var1.pred
}
 
# Plot the differences
OP <- par(pty="s", mar=c(4,3,0,0))
plot(IDW.out ~ county_health_sp$ObesityPercent, asp=1, xlab="Observed", ylab="Predicted", pch=16,
      col=rgb(0,0,0,0.5))
abline(lm(IDW.out ~ county_health_sp$ObesityPercent), col="red", lw=2,lty=2)
abline(0,1)
par(OP)
 
sqrt( sum((IDW.out - county_health_sp$ObesityPercent)^2) / length(county_health_sp))


################################################# Cross-validation #################################################

# Implementation of a jackknife technique to estimate 
# a confidence interval at each unsampled point.

# Create the interpolated surface
img <- gstat::idw(ObesityPercent~1, county_health_sp, newdata=grd, idp=2.0)
n   <- length(county_health_sp)
Zi  <- matrix(nrow = length(img$var1.pred), ncol = n)

# Remove a point then interpolate (do this n times for each point)
st <- stack()
for (i in 1:n){
  Z1 <- gstat::idw(ObesityPercent~1, county_health_sp[-i,], newdata=grd, idp=2.0)
  st <- addLayer(st,raster(Z1,layer=1))
  # Calculated pseudo-value Z at j
  Zi[,i] <- n * img$var1.pred - (n-1) * Z1$var1.pred
}

# Jackknife estimator of parameter Z at location j
Zj <- as.matrix(apply(Zi, 1, sum, na.rm=T) / n )

# Compute (Zi* - Zj)^2
c1 <- apply(Zi,2,'-',Zj)            # Compute the difference
c1 <- apply(c1^2, 1, sum, na.rm=T ) # Sum the square of the difference

# Compute the confidence interval
CI <- sqrt( 1/(n*(n-1)) * c1)

# Create (CI / interpolated value) raster
img.sig   <- img
img.sig$v <- CI /img$var1.pred 

# Clip the confidence raster to Texas
r <- raster(img.sig, layer="v")
r.m     <- mask(r, states_sf)

# Plot the map
tm_shape(r.m) + tm_raster(n=7,title="95% confidence interval") +
  tm_legend(legend.outside=TRUE)
 

##################################### 1st order polynomial fit #####################################


# Define the 1st order polynomial equation
f.1 <- as.formula(ObesityPercent ~ X + Y) 

# Add X and Y to P
county_health_sp$X <- coordinates(county_health_sp)[,1]
county_health_sp$Y <- coordinates(county_health_sp)[,2]

# Run the regression model
lm.1 <- lm( f.1, data=county_health_sp)

# Use the regression model output to interpolate the surface
dat.1st <- SpatialGridDataFrame(grd, data.frame(var1.pred = predict(lm.1, newdata=grd))) 

# Clip the interpolated raster to Texas
r   <- raster(dat.1st)
r.m     <- mask(r, states_sf)

# Plot the map
tm_shape(r.m) + 
  tm_raster(n=10, palette="RdBu", auto.palette.mapping=FALSE, 
            title="Predicted Obesity Percentage") +
  tm_legend(legend.outside=TRUE)
 
 

##################################### 2nd order polynomial fit #####################################

# Define the 2nd order polynomial equation
f.2 <- as.formula(ObesityPercent ~ X + Y + I(X*X)+I(Y*Y) + I(X*Y))

# Add X and Y to P
county_health_sp$X <- coordinates(county_health_sp)[,1]
county_health_sp$Y <- coordinates(county_health_sp)[,2]

# Run the regression model
lm.2 <- lm( f.2, data=county_health_sp)

# Use the regression model output to interpolate the surface
dat.2nd <- SpatialGridDataFrame(grd, data.frame(var1.pred = predict(lm.2, newdata=grd))) 

# Clip the interpolated raster to Texas
r   <- raster(dat.2nd)
r.m     <- mask(r, states_sf)

# Plot the map
tm_shape(r.m) + 
  tm_raster(n=10, palette="RdBu", auto.palette.mapping=FALSE,
            title="Predicted Obesity Percentage") +
  tm_legend(legend.outside=TRUE)

##################################### Fit the variogram model #####################################

# Define the 1st order polynomial equation
f.1 <- as.formula(ObesityPercent ~ X + Y) 

# Compute the sample variogram; note that the f.1 trend model is one of the
# parameters passed to variogram(). This tells the function to create the 
# variogram on the de-trended data.
var.smpl <- variogram(f.1, county_health_sp, cloud = FALSE, cutoff=100, width=89.9)

# Compute the variogram model by passing the nugget, sill and range values
# to fit.variogram() via the vgm() function.
dat.fit  <- fit.variogram(var.smpl, fit.ranges = FALSE, fit.sills = FALSE,
                          vgm(psill=14, model="Sph", range=59, nugget=0))

# The following plot allows us to assess the fit
plot(var.smpl, dat.fit, xlim=c(0,100))


##################################### Krigging #####################################

# Define the trend model
f.1 <- as.formula(ObesityPercent ~ X + Y) 

# Perform the krige interpolation (note the use of the variogram model
# created in the earlier step)
dat.krg <- krige( f.1, county_health_sp, grd, dat.fit)

# Convert kriged surface to a raster object for clipping
r <- raster(dat.krg)
r.m     <- mask(r, states_sf)

# Plot the map
tm_shape(r.m) + 
  tm_raster(n=10, palette="RdBu", auto.palette.mapping=FALSE, 
            title="Krigging Predicted Obesity Percentage") +
  tm_legend(legend.outside=TRUE)



# Generate the variance and confidence interval maps

r   <- raster(dat.krg, layer="var1.var")
r.m     <- mask(r, states_sf)

tm_shape(r.m) + 
  tm_raster(n=7, palette ="Reds", title="Variance map") +
  tm_legend(legend.outside=TRUE)


# 95% confidence interval map

r   <- sqrt(raster(dat.krg, layer="var1.var")) * 1.96
r.m     <- mask(r, states_sf)

tm_shape(r.m) + 
  tm_raster(n=7, palette ="Reds",
            title="95% CI map") +
  tm_legend(legend.outside=TRUE)


