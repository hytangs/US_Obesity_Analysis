#https://www.youtube.com/watch?v=NQZNpyEgVss
#https://www.cpc.ncep.noaa.gov/products/GIS/GIS_DATA/us_tempprcpfcst/seasonal.php

setwd("~/Desktop/4015GIT/Climate")

library(ggplot2)
library(raster)
library(tidyr)
library(rnaturalearth)
library(sf)
#Need to reinstall package "Raster"

temp <- getData("worldclim", var ="bio", res = 2.5)
temp <- crop(temp, extent(-125.0011,-66.9326,24.9493,49.5904))
#plot(climate)

rasterTemp <- temp$bio1
state_boundary_us <- st_read("cb_2018_us_state_20m/cb_2018_us_state_20m.shp")
state_boundary_us <- state_boundary_us[!(state_boundary_us$STUSPS %in% c("AK","HI","PR")),]

#plot(america)

rasdfTemp <- as.data.frame(rasterTemp, xy = TRUE)
rasdfTemp <- drop_na(rasdfTemp)
#head(rasdf)

ggplot()+
  geom_raster(aes(x=x,y=y,fill = bio1), data = rasdfTemp)+
  geom_sf(fill = "transparent", data = state_boundary_us)+
  scale_fill_viridis_c(name ='°F', direction = 1)+
  labs(x= 'Longitude', y = 'Latitude', title = "US Climate",
       subtitle = "Annual Temperature Map",
       caption = "Source: WorldClim, 2020")+
  cowplot::theme_cowplot()


#Correlation Analysis
tempState <- st_read("seastemp_202110")

rtemp <- raster(ncol=300, nrow=300)
extent(rtemp) <- extent(tempState)
Demo_ras_temp = rasterize(tempState,rtemp, 'Prob')

agri_s2 = raster::extract(x=Demo_ras_temp, y=state_boundary_us, fun=mean, df=TRUE)
Combined$temp=agri_s2$layer
Combined <- Combined[!(Combined$state_code %in% c("AK","HI","PR")),]

cor.test(Combined$obesity.rate, Combined$temp, method = "pearson")

#Precipitation
Preci <- getData("worldclim", var ="bio", res = 2.5)
Preci <- crop(Preci , extent(-125.0011,-66.9326,24.9493,49.5904))
rasterPreci <- Preci$bio12

#plot(america)

rasdfPreci <- as.data.frame(rasterPreci, xy = TRUE)
rasdfPreci <- drop_na(rasdfPreci)
#head(rasdf)

ggplot()+
  geom_raster(aes(x=x,y=y,fill = bio12), data = rasdfPreci)+
  geom_sf(fill = "transparent", data = state_boundary_us)+
  scale_fill_viridis_c(name ='mm/yr', direction = 1)+
  labs(x= 'Longitude', y = 'Latitude', title = "US Climate",
       subtitle = "Annual Precipitation Map",
       caption = "Source: WorldClim, 2020")+
  cowplot::theme_cowplot()


#Correlation Analysis
prcpState <- st_read("seasprcp_202110")

rprcp <- raster(ncol=300, nrow=300)
extent(rprcp) <- extent(prcpState)
Demo_ras_prcp = rasterize(prcpState, rprcp, 'Prob')

agri_s2 = raster::extract(x=Demo_ras_prcp, y=state_boundary_us, fun=mean, df=TRUE)
Combined$prcp=agri_s2$layer
Combined <- Combined[!(Combined$state_code %in% c("AK","HI","PR")),]

cor.test(Combined$obesity.rate, Combined$prcp, method = "pearson")

#Elevation
elev <- getData("worldclim", var ='alt', res = 2.5)
elev <- crop(elev, extent(-125.0011,-66.9326,24.9493,49.5904))
#plot(climate)

rasterElev <- elev$alt
rasdfElev <- as.data.frame(rasterElev, xy = TRUE)%>%drop_na()
head(rasdfElev)

ggplot()+
  geom_raster(aes(x=x,y=y,fill = alt), data = rasdfElev)+
  geom_sf(fill = "transparent", data = state_boundary_us)+
  scale_fill_viridis_c(name ='metres', direction = 1)+
  labs(x= 'Longitude', y = 'Latitude', title = "US Terrain",
       subtitle = "Altitude Map",
       caption = "Source: WorldClim, 2020")+
  cowplot::theme_cowplot()
