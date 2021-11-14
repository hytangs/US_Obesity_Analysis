require(dplyr)
library(sp)
library(sf)
library(spData)
library(spDataLarge)
library(spatstat) # point pattern analysis
library(spdep) # spatial autocorrelation
library(leaflet) # for interactive maps
library(tmap)    # for static and interactive maps
library(tigris) #download shp file from U.S. census website
library(tmaptools)
library(tidyverse)
library(RColorBrewer)
library(rgdal)
library(raster)
library(readr)
library(maptools)
library(ggrepel)
library(ggplot2)
library(fMultivar) 
library(usmap)

setwd("~/Desktop/4015GIT/Obesity")
"""""
countyObe <- read.csv(file = '/Users/taoranzheng/Downloads/study/21_22_y4_sem1/BT4015/Project/Database/countyObesity.csv')
db<- read.delim("/Users/taoranzheng/Downloads/study/21_22_y4_sem1/BT4015/Project/Database/2021_Gaz_counties_national.txt")
obeDF <- as.data.frame(countyObe)
countyDF <- as.data.frame(db)
totalDF <- merge(obeDF, countyDF, by.obeDF="fips", by.countyDF="GEOID")
totalDF <- totalDF %>% filter(totalDF$fips == totalDF$GEOID)
totalDF$LAT <- as.numeric(totalDF$INTPTLAT)
totalDF$LON <- as.numeric(totalDF$INTPTLONG)
totalDF <- totalDF[,c(26,25,1,15,2,3,4,5,6,7,8,9,10,11,12,13,14)]
ObeDataSet <- subset(totalDF, !(totalDF$abbr == "Alaska" | totalDF$abbr == "Hawaii") )
write.csv(ObeDataSet,"/Users/taoranzheng/Downloads/study/21_22_y4_sem1/BT4015/Project/Database/ObeDataSet.csv", row.names = FALSE)
"""""

ObeDataSet <- read_csv('ObeDataSet.csv')
summary(ObeDataSet$obesity.rate)

#ObeDataSet <- ObeDataSet %>%
#  mutate(obeLev = if_else(obesity.rate >= 43, 3, if_else(obesity.rate >= 27, 2, 1)))

#Ploting Obeisty Rate in county basis
county_obe_map <-
  plot_usmap(data = ObeDataSet, values = "obesity.rate", size = 0.2) +
  labs(title = "Obesity Rate by US Counties") +
  theme(panel.background = element_rect(colour = "black"))+
  scale_fill_continuous(low = 'yellow',high = "blue",name = "Obesity Rate (%)",label = scales::comma) +
  theme(legend.position = "right")+
  scale_x_continuous(expand = c(0, 0)) + scale_y_continuous(expand = c(0, 0))                       

county_obe_map

#Exploring clustering by moran's I
geoDataSet <- st_read('Obesity/us_county/us_county.shp', stringsAsFactors=FALSE)
geoDataSet$fips <- as.numeric(geoDataSet$fips)
GeoDataSet<- unique(geoDataSet[ , ] )

countyBasedFull <- GeoDataSet %>% merge(ObeDataSet, by="fips") %>% dplyr::filter(!state %in% c("Puerto Rico", "Hawaii", "Alaska")) 
countyGeoInfo <- countyBasedFull[,c("fips","county","geometry")]
countyBasedFull  <- drop_na(countyBasedFull )
countyGeoInfo <- drop_na(countyGeoInfo)

countyBasedFull.sf <- st_as_sf(countyBasedFull)

#Plotting
#palette_explorer()
# Obesity rate by county
summary(ObeDataSet$obesity.rate)
hist(ObeDataSet$obesity.rate)

tm_shape(countyBasedFull.sf) + 
  tm_polygons("obesity.rate", breaks=seq(0,60,by=20), title="Obesity Rate",palette = "Greens") +
  tm_layout(main.title="Obesity Rate per American County",
            main.title.size=2, main.title.position="centre",
            legend.title.size=1.5,legend.text.size=0.8, legend.position=c("right", "bottom"),
            frame=TRUE) +
  tm_compass(type="4star", position = c("left", "bottom")) +
  tm_scale_bar(width=0.5, position = c("left", "bottom")) +
  tmap_style("cobalt")


# Smoking rate by county
summary(ObeDataSet$smoking)

tm_shape(countyBasedFull.sf) + 
  tm_polygons("smoking", breaks=seq(0,50,by=10), title="Smoking Rate") +
  tm_layout(main.title="Smoking Rate per American County",
            main.title.size=1.5, main.title.position="centre",
            legend.title.size=1,legend.text.size=0.7, legend.position=c("right", "bottom"),
            frame=TRUE) +
  tm_compass(type="4star", position = c("left", "bottom")) +
  tm_scale_bar(width=0.5, position = c("left", "bottom")) +
  tmap_style("cobalt")

#Correlation Analysis
cor.test(countyBasedFull.sf$obesity.rate, countyBasedFull.sf$smoking, method = "pearson")

# Access to healthy food by county
summary(ObeDataSet$healthy_food)

tm_shape(countyBasedFull.sf) + 
  tm_polygons("healthy_food", breaks=seq(0,10,by=1), title="Access to healthy food",palette = "Blues") +
  tm_layout(main.title="Access to healthy_food per American County",
            main.title.size=1.5, main.title.position="centre",
            legend.title.size=1,legend.text.size=0.7, legend.position=c("right", "bottom"),
            frame=TRUE) +
  tm_compass(type="rose", position = c("left", "bottom")) +
  tm_scale_bar(width=0.5, position = c("left", "bottom")) +
  tmap_style("beaver")

cor.test(countyBasedFull.sf$obesity.rate, countyBasedFull.sf$healthy_food, method = "pearson")


# Education level by county
countyBasedFull.sf$education_rate <- countyBasedFull.sf$Completed_High_School/countyBasedFull.sf$Population
summary(countyBasedFull.sf$education_rate)

tm_shape(countyBasedFull.sf) + 
  tm_polygons("education_rate", breaks=seq(0.6,1,by=0.01), title="Education Rate",palette = "PuRd") +
  tm_layout(main.title="High School Completion Rate per American County",
            main.title.size=1, main.title.position="centre",
            legend.title.size=0.5,legend.text.size=0.5, legend.position=c("right", "bottom"),
            frame=TRUE) +
  tm_compass(type="4star", position = c("left", "bottom")) +
  tm_scale_bar(width=0.5, position = c("left", "bottom")) +
  tmap_style("natural")

cor.test(countyBasedFull.sf$obesity.rate, countyBasedFull.sf$education_rate, method = "pearson")


# Unemployment rate by county
countyBasedFull.sf$unemployment_rate <- countyBasedFull.sf$Unemployed/countyBasedFull.sf$Population
summary(countyBasedFull.sf$unemployment_rate)

tm_shape(countyBasedFull.sf) + 
  tm_polygons("unemployment_rate", breaks=seq(0.01,0.15,by=0.01), title="Uneployment Rate",palette = "PuBuGn") +
  tm_layout(main.title="Unemployment Rate per American County",
            main.title.size=1, main.title.position="centre",
            legend.title.size=0.5,legend.text.size=0.5, legend.position=c("right", "bottom"),
            frame=TRUE) +
  tm_compass(type="4star", position = c("left", "bottom")) +
  tm_scale_bar(width=0.5, position = c("left", "bottom")) +
  tmap_style("gray")

cor.test(countyBasedFull.sf$obesity.rate, countyBasedFull.sf$unemployment_rate, method = "pearson")

# Income Level by county
countyBasedFull.sf$AvrIncome <- (countyBasedFull.sf$X80th_Percentile_Income+countyBasedFull.sf$X20th_Percentile_Income)/2
summary(countyBasedFull.sf$AvrIncome)

tm_shape(countyBasedFull.sf) + 
  tm_polygons("AvrIncome", breaks=seq(30000,160000,by=15000), title="Average Income",palette = "YlGnBu") +
  tm_layout(main.title="Average Annual Income per American County",
            main.title.size=1.5, main.title.position="centre",
            legend.title.size=1,legend.text.size=0.7, legend.position=c("right", "bottom"),
            frame=TRUE) +
  tm_compass(type="rose", position = c("left", "bottom")) +
  tm_scale_bar(width=0.5, position = c("left", "bottom")) +
  tmap_style("beaver")

cor.test(countyBasedFull.sf$obesity.rate, countyBasedFull.sf$AvrIncome, method = "pearson")

# Drive to work by county
summary(ObeDataSet$Drive_Alone_to_Work)

tm_shape(countyBasedFull.sf) + 
  tm_polygons("Drive_Alone_to_Work", breaks=seq(0,100,by=10), title="Drive to work percent", palette = "Reds") +
  tm_layout(main.title="Drive to work rate per American County",
            main.title.size=1.5, main.title.position="centre",
            legend.title.size=1,legend.text.size=0.7, legend.position=c("right", "bottom"),
            frame=TRUE) +
  tm_compass(type="4star", position = c("left", "bottom")) +
  tm_scale_bar(width=0.5, position = c("left", "bottom")) +
  tmap_style("beaver")
cor.test(countyBasedFull.sf$obesity.rate, countyBasedFull.sf$Drive_Alone_to_Work, method = "pearson")

# Drive to work by county
summary(ObeDataSet$Drive_Alone_to_Work)

tm_shape(countyBasedFull.sf) + 
  tm_polygons("Drive_Alone_to_Work", breaks=seq(0,100,by=10), title="Drive to work percent", palette = "Reds") +
  tm_layout(main.title="Drive to work rate per American County",
            main.title.size=1.5, main.title.position="centre",
            legend.title.size=1,legend.text.size=0.7, legend.position=c("right", "bottom"),
            frame=TRUE) +
  tm_compass(type="4star", position = c("left", "bottom")) +
  tm_scale_bar(width=0.5, position = c("left", "bottom")) +
  tmap_style("beaver")
cor.test(countyBasedFull.sf$obesity.rate, countyBasedFull.sf$Drive_Alone_to_Work, method = "pearson")

#Diabetes
summary(ObeDataSet$Diabetes_prevalence)

tm_shape(countyBasedFull.sf) + 
  tm_polygons("Diabetes_prevalence", breaks=seq(2,30,by=4), title="Diabetes Prevalence",palette = "Blues") +
  tm_layout(main.title="Diabetes Prevalance per American County",
            main.title.size=1, main.title.position="centre",
            legend.title.size=0.5,legend.text.size=0.5, legend.position=c("right", "bottom"),
            frame=TRUE) +
  tm_compass(type="4star", position = c("left", "bottom")) +
  tm_scale_bar(width=0.5, position = c("left", "bottom")) +
  tmap_style("cobalt")

cor.test(countyBasedFull.sf$obesity.rate, countyBasedFull.sf$Diabetes_prevalence, method = "pearson")


#Spatial Autocorrelation
#Finding Moran's I for Obesity 
#https://mgimond.github.io/Spatial/spatial-autocorrelation-in-r.html
require(spdep)
county.nb <- poly2nb(countyBasedFull.sf, queen=TRUE)
county.nb

county.lw <- nb2listw(county.nb,style="W",zero.policy=TRUE)
county.lw$weights[1]
Obe.lag <- lag.listw(county.lw, countyBasedFull.sf$obesity.rate)
#plot(Obe.lag)

# Create a regression model
M <- lm(Obe.lag~ countyBasedFull.sf$obesity.rate)

# Plot the data
plot( Obe.lag ~ countyBasedFull.sf$obesity.rate, pch=20, asp=1, las=1)
coef(M)[2]

n <- 1000L   # Define the number of simulations
I.r <- vector(length=n)  # Create an empty vector

for (i in 1:n){
  # Randomly shuffle income values
  x <- sample(countyBasedFull.sf$obesity.rate, replace=FALSE)
  # Compute new set of lagged values
  x.lag <- lag.listw(county.lw, x)
  # Compute the regression slope and store its value
  M.r    <- lm(x.lag ~ x)
  I.r[i] <- coef(M.r)[2]
}

# Plot the histogram of simulated Moran's I values
# then add our observed Moran's I value to the plot
hist(I.r, main=NULL, xlab="Moran's I", xlim = c(-0.4,0.4), las=1)
abline(v=coef(M)[2], col="red")

N.greater <- sum(coef(M)[2] > I.r)
p <- min(N.greater + 1, n + 1 - N.greater) / (n + 1)
p

#Global Moran I
moran.test(countyBasedFull.sf$obesity.rate,county.lw,randomisation=FALSE,zero.policy = TRUE)

#Easier Moran's I
moran.test(countyBasedFull.sf$obesity.rate,county.lw,randomisation=FALSE,zero.policy = TRUE)
MC<- moran.mc(countyBasedFull.sf$obesity.rate,county.lw, nsim=1000, zero.policy = TRUE)
MC

plot(MC, main="", las=1)


#-----------------------------------------------------------------------------------------------------------------------
summary(ObeDataSet$Population)

county_pop <-
  plot_usmap(data = ObeDataSet, values = "Population", size = 0.2) +
  scale_fill_continuous( type = "gradient") +
  scale_x_continuous(expand = c(0, 0)) + scale_y_continuous(expand = c(0, 0))

county_pop

#or

#Population Density
library(maps)
library(socviz)
library(ggthemes)
us_states <- map_data("state")
county_map %>% sample_n(5)

county_data %>%select(id, name, state, pop_dens, pct_black) %>%sample_n(5)

county_full <- left_join(county_map,county_data, by = 'id')

p <- ggplot(data = county_full,
            mapping = aes(x = long, y = lat,
                          fill = pop_dens, 
                          group = group))

p1 <- p + geom_polygon(color = "gray90", size = 0.05) + coord_equal()

p2 <- p1 + scale_fill_brewer(palette="Blues",
                             labels = c("0-10", "10-50", "50-100", "100-500",
                                        "500-1,000", "1,000-5,000", ">5,000"))

p2 + labs(fill = "Population per\nsquare mile") +
  theme_map() +
  guides(fill = guide_legend(nrow = 1)) + 
  theme(legend.position = "bottom")


p <- ggplot(data = county_full,
            mapping = aes(x = long, y = lat, fill = pct_black, 
                          group = group))
p1 <- p + geom_polygon(color = "gray90", size = 0.05) + coord_equal()
p2 <- p1 + scale_fill_brewer(palette="Greens")
#-----------------------------------------------------------------------------------------------------------------------
county_diabete <-
  plot_usmap(data = ObeDataSet, values = "Diabetes_prevalence", size = 0.2) +
  scale_fill_continuous(low = 'green',high = "red", guide = FALSE) +
  scale_x_continuous(expand = c(0, 0)) + scale_y_continuous(expand = c(0, 0))         

county_diabete
#-----------------------------------------------------------------------------------------------------------------------
ObeDataSet$Diabetes_Obe_rate <- ObeDataSet$Diabetes_prevalence/ObeDataSet$obesity.rate
county_Diabetes_Obe_rate <-
  plot_usmap(data = ObeDataSet, values = "Diabetes_Obe_rate", size = 0.2) +
  scale_fill_continuous(low = 'yellow',high = "purple", guide = FALSE) +
  scale_x_continuous(expand = c(0, 0)) + scale_y_continuous(expand = c(0, 0))         

county_Diabetes_Obe_rate
#-----------------------------------------------------------------------------------------------------------------------
library(tidyverse)
library(RColorBrewer)
library(sf)
library(tmap)
library(rgdal)
library(spatstat) # point pattern analysis
library(maptools)
library(spdep) # spatial autocorrelation
library(ggrepel)
library(fMultivar) # hexagonal binning for quadrant analysis
library(Select)


#counties
countyGeoInfo <- countyBasedFull[,c("fips","county","geometry")]
countyGeoInfo <- drop_na(countyGeoInfo)

head(countyBasedFull)


#Need to change to state level
#tm_shape(countyBasedFull.sf) + tm_polygons('obesity.rate')+tm_bubbles('healthy_food')+tm_compass(position=c('right',0.4))+tm_scale_bar(position=c('left',0.1))

#Change to State
stateBasedFull <- countyBasedFull %>% group_by(state, state_code) %>% summarize() %>% ungroup()

StateObe <- countyBasedFull %>% group_by(state_code) %>% summarise(obesity.rate = mean(obesity.rate)) %>% data.frame()
Combined <- StateObe %>% merge(stateBasedFull, by="state_code")
Combined <- Combined %>% dplyr::select(-geometry.y) %>% rename(geometry = geometry.x)


StateHealthyFood <- countyBasedFull %>% group_by(state_code) %>% summarise(healthy_food = mean(healthy_food)) %>% data.frame()
Combined <- StateHealthyFood %>% merge(Combined, by="state_code") 
Combined <- Combined %>% dplyr::select(-geometry.y) %>% rename(geometry = geometry.x)

StateSmoking <- countyBasedFull %>% group_by(state_code) %>% summarise(smoking = mean(smoking)) %>% data.frame()
Combined <- StateSmoking %>% merge(Combined, by="state_code")
Combined <- Combined %>% dplyr::select(-geometry.y) %>% rename(geometry = geometry.x)

StateEdu <- countyBasedFull %>% group_by(state_code) %>% summarise(education_rate = sum(Completed_High_School)/sum(Population)) %>% data.frame()
Combined <- StateEdu %>% merge(Combined, by="state_code")
Combined <- Combined %>% dplyr::select(-geometry.y) %>% rename(geometry = geometry.x)

StateUnemploy <- countyBasedFull %>% group_by(state_code) %>% summarise(unemployment_rate = sum(Unemployed)/sum(Population)) %>% data.frame()
Combined <- StateUnemploy %>% merge(Combined, by="state_code")
Combined <- Combined %>% dplyr::select(-geometry.y) %>% rename(geometry = geometry.x)

StateIncome <- countyBasedFull %>% group_by(state_code) %>% summarise(income = (mean(X80th_Percentile_Income)+mean(X20th_Percentile_Income))/2) %>% data.frame()
Combined <- StateIncome  %>% merge(Combined, by="state_code")
Combined <- Combined %>% dplyr::select(-geometry.y) %>% rename(geometry = geometry.x)

StateDrive <- countyBasedFull %>% group_by(state_code) %>% summarise(Drive_Alone_to_Work = mean(Drive_Alone_to_Work)) %>% data.frame()
Combined <- StateDrive  %>% merge(Combined, by="state_code")
Combined <- Combined %>% dplyr::select(-geometry.y) %>% rename(geometry = geometry.x)

StateDiabetes <- countyBasedFull %>% group_by(state_code) %>% summarise(diabetes_rate = mean(Diabetes_prevalence)) %>% data.frame()
Combined <- StateDiabetes  %>% merge(Combined, by="state_code")
Combined <- Combined %>% dplyr::select(-geometry.y) %>% rename(geometry = geometry.x)

Combined.sf <- st_as_sf(Combined)

tm_shape(Combined.sf) + 
  tm_polygons("obesity.rate", breaks=seq(0,60,by=5), title="Obesity Rate",palette = "-Greens") +
  tm_layout(main.title="Obesity Rate per American State",
            main.title.size=1, main.title.position="centre",
            legend.title.size=0.5,legend.text.size=0.5, legend.position=c("right", "bottom"),
            frame=TRUE) +
  tm_compass(type="4star", position = c("left", "bottom")) +
  tm_scale_bar(width=0.5, position = c("left", "bottom")) +
  tmap_style("cobalt")

#Spatial Autocorrelation
#Finding Moran's I for Obesity
require(spdep)
state.nb <- poly2nb(Combined.sf)

state.lw <- nb2listw(state.nb,zero.policy=TRUE)
moran.test(Combined.sf$obesity.rate,state.lw,randomisation=FALSE,zero.policy = TRUE)

Combined.sf$lI <- localmoran(Combined.sf$obesity.rate,state.lw)[, 1]
tm_shape(Combined.sf,unit='miles') + tm_polygons(col= 'lI',title= "Local Moran’s I (Obesity Rate)",legend.format=list(flag= "+"),palette = "-Blues") +
  tm_style('col_blind') + tm_scale_bar(width= 0.15,position = c('right',0.2)) + tm_layout(legend.position = c("left", "bottom"), legend.text.size= 0.4, asp=1.8, main.title = "Local Moran's I for Obesity Rate")+tm_compass()

#plot p value for local moran's I for case rate
Combined.sf$pval <- localmoran(Combined.sf$obesity.rate,state.lw)[, 5]
tm_shape(Combined.sf,unit= 'miles') + tm_polygons(col= 'pval', title= "p-value (Obesity Rate)" , breaks= c(0, 0.01, 0.05, 0.10, 1), border.col = "black",palette = "-Greens") +
  tm_scale_bar(width=0.15) +
  tm_layout(legend.position = c("left", "bottom"),asp=1.8)+tm_compass()

#--------------------------------Histograme-------------------------------------------
require(spdep)
USstate.nb <- poly2nb(Combined.sf, queen=TRUE)
USstate.nb

USstate.lw <- nb2listw(USstate.nb,style="W",zero.policy=TRUE)
USstate.lw$weights[1]
Obe.lag <- lag.listw(USstate.lw, Combined.sf$obesity.rate)
#plot(Obe.lag)

# Create a regression model
M <- lm(Obe.lag~ Combined.sf$obesity.rate)

# Plot the data
plot( Obe.lag ~ Combined.sf$obesity.rate, pch=20, asp=1, las=1)
coef(M)[2]

n <- 1000L   # Define the number of simulations
I.r <- vector(length=n)  # Create an empty vector

for (i in 1:n){
  # Randomly shuffle income values
  x <- sample(Combined.sf$obesity.rate, replace=FALSE)
  # Compute new set of lagged values
  x.lag <- lag.listw(USstate.lw, x)
  # Compute the regression slope and store its value
  M.r    <- lm(x.lag ~ x)
  I.r[i] <- coef(M.r)[2]
}

# Plot the histogram of simulated Moran's I values
# then add our observed Moran's I value to the plot
hist(I.r, main=NULL, xlab="Moran's I", xlim = c(-0.5,0.5), las=1)
abline(v=coef(M)[2], col="red")

N.greater <- sum(coef(M)[2] > I.r)
p <- min(N.greater + 1, n + 1 - N.greater) / (n + 1)
p

#Global Moran I
moran.test(Combined.sf$obesity.rate,USstate.lw,randomisation=FALSE,zero.policy = TRUE)

#Easier Moran's I
moran.test(Combined.sf$obesity.rate,USstate.lw,randomisation=FALSE,zero.policy = TRUE)
MC<- moran.mc(Combined.sf$obesity.rate,USstate.lw, nsim=1000, zero.policy = TRUE)
MC

plot(MC, main="", las=1)

#Spatial Regression
library(spatialreg)
require(spdep)

Combined.sf <- st_as_sf(Combined)

state.nb <- poly2nb(Combined.sf)

state.lw <- nb2listw(state.nb,zero.policy=TRUE)

sar.res <- spautolm (obesity.rate~ diabetes_rate + income + smoking + healthy_food +education_rate
                     + unemployment_rate + Drive_Alone_to_Work, listw=state.lw, data=Combined.sf)
summary(sar.res)

tm_shape(Combined.sf) + tm_polygons('obesity.rate', palette = "Reds")+tm_dots('healthy_food',size = 0.1, palette = "Greens")+tm_compass(position=c('right',0.4))+tm_scale_bar(position=c('left',0.1))
