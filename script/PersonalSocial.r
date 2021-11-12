# Set Working Dir
setwd("C:/Users/cwche/Desktop/US_Obesity_Analysis/data")


# Personal and Social Economic Factors


# Library Load
library(usmap)
library(ggplot2)
library(ggpubr)
library(readxl)
library(dplyr)
library(spatialreg)
library(spdep)
library(sf)


# Dataset Load
CHR21_county <- read_excel("CountryHealthRankings21_Cleaned.xlsx", 
                           sheet = "county")
CHR21_state <- read_excel("CountryHealthRankings21_Cleaned.xlsx", 
                           sheet = "state")
head(CHR21_county)
head(CHR21_state)
CHR21_county <- as.data.frame(CHR21_county)
CHR21_state <- as.data.frame(CHR21_state)


# Import Spatially Processed CHR21 Dataset 
CHR21_sf <- read_excel("CountryHealthRankings21_Cleaned.xlsx", 
                          sheet = "sf")

require(rgdal)
shape <- readOGR(dsn = ".", layer = "cb_2018_us_county_20m")

require(sf)
shape <- read_sf(dsn = ".", layer = "cb_2018_us_county_20m")

CHR21_sf <- merge(CHR21_sf, shape, by.x="fips", by.y="GEOID")


# County Level Obesity Percent Map
plot_usmap(data = CHR21_county, regions = c("counties"), values = "ObesityPercent", color = "darkred", label_color = "white") + 
  scale_fill_continuous(low = "mistyrose", high = "blue", name = "Obesity Percent", label = scales::comma) + 
  theme(legend.position = "right")

plot_usmap(data = CHR21_state, regions = "states", values = "ObesityPercent", color = "darkred", label_color = "white") + 
  scale_fill_continuous(low = "mistyrose", high = "blue", name = "Obesity Percent", label = scales::comma) + 
  theme(legend.position = "right")


# Normalization of Non-Rate / Non-Percent Factors
summary(CHR21_county$MedianIncome)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
#  24732   46243   53341   55713   62059  151806       1 
CHR21_county$MedianIncomeNormalized = (CHR21_county$MedianIncome - 24732) / 55713 * 50

summary(CHR21_state$MedianIncome)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#  45928   57503   63290   65616   75117   90395 
CHR21_state$MedianIncomeNormalized = (CHR21_state$MedianIncome - 45928) / 63290 * 50

summary(CHR21_sf$MedianIncome)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 24732   46249   53362   55702   62038  151806    1 
CHR21_sf$MedianIncomeNormalized = (CHR21_sf$MedianIncome - 24732) / 53362 * 50


# To sf:
CHR21_sf.sf <- st_as_sf(CHR21_sf)
state.nb <- poly2nb(CHR21_sf.sf)
state.lw <- nb2listw(state.nb, zero.policy = TRUE)


# A. Population Demographics


# A1. Age Range


# Life Expectancy
# Regular Regression
lm_life_fit <- lm(ObesityPercent ~ LifeExpectancy, data = CHR21_county)
summary(lm_life_fit)

# Spatial Autoregression
sar.res_life <-spautolm(ObesityPercent ~ LifeExpectancy, listw = state.lw, data = CHR21_sf.sf, zero.policy = TRUE)
summary(sar.res_life)

# Plot
plot_usmap(data = CHR21_county, regions = c("counties"), values = "LifeExpectancy", color = "darkred", label_color = "white") + 
  scale_fill_continuous(low = "mistyrose", high = "blue", name = "Expected Ages", label = scales::comma) + 
  theme(panel.background = element_rect(colour = "black", fill = "lightblue"), legend.position = "right") +
  labs(title = "Life Expectancy", subtitle = "US County Level Life Expectancy Choropleth Map") 

plot_usmap(data = CHR21_state, values = "LifeExpectancy", color = "darkred", label_color = "white") + 
  scale_fill_continuous(low = "mistyrose", high = "blue", name = "Expected Ages", label = scales::comma) + 
  theme(panel.background = element_rect(colour = "black", fill = "lightblue"), legend.position = "right") +
  labs(title = "Life Expectancy", subtitle = "US State Level Life Expectancy Choropleth Map") 


# Percentage of Child
lm_child_fit <- lm(ObesityPercent ~ ChildPercent, data = CHR21_county)
summary(lm_child_fit)

sar.res_child <-spautolm(ObesityPercent ~ ChildPercent, listw = state.lw, data = CHR21_sf.sf, zero.policy = TRUE)
summary(sar.res_child)

# Plot
plot_usmap(data = CHR21_county, values = "ChildPercent", color = "darkred", label_color = "white") + 
  scale_fill_continuous(low = "mistyrose", high = "blue", name = "Child Percentage", label = scales::comma) + 
  theme(panel.background = element_rect(colour = "black", fill = "lightblue"), legend.position = "right") +
  labs(title = "Child Percent", subtitle = "US County Level Child Percentage Choropleth Map") 

plot_usmap(data = CHR21_state, values = "ChildPercent", color = "darkred", label_color = "white") + 
  scale_fill_continuous(low = "mistyrose", high = "blue", name = "Child Percentage", label = scales::comma) + 
  theme(panel.background = element_rect(colour = "black", fill = "lightblue"), legend.position = "right") +
  labs(title = "Child Percent", subtitle = "US State Level Child Percentage Choropleth Map") 


# Percentage of Elderly
lm_elder_fit <- lm(ObesityPercent ~ ElderlyPercent, data = CHR21_county)
summary(lm_elder_fit)

sar.res_elderly <-spautolm(ObesityPercent ~ ChildPercent, listw = state.lw, data = CHR21_sf.sf, zero.policy = TRUE)
summary(sar.res_elderly)

plot_usmap(data = CHR21_county, values = "ElderlyPercent", color = "darkred", label_color = "white") + 
  scale_fill_continuous(low = "mistyrose", high = "blue", name = "Elderly Percentage", label = scales::comma) + 
  theme(panel.background = element_rect(colour = "black", fill = "lightblue"), legend.position = "right") +
  labs(title = "Elderly Percent", subtitle = "US County Level Elderly Percentage Choropleth Map") 

plot_usmap(data = CHR21_state, values = "ElderlyPercent", color = "darkred", label_color = "white") + 
  scale_fill_continuous(low = "mistyrose", high = "blue", name = "Elderly Percentage", label = scales::comma) + 
  theme(panel.background = element_rect(colour = "black", fill = "lightblue"), legend.position = "right") +
  labs(title = "Elderly Percent", subtitle = "US State Level Elderly Percentage Choropleth Map") 


# A2. Gender
plot_usmap(data = CHR21_county, regions = c("counties"), values = "FemalePercent", color = "white", label_color = "white") + 
  scale_fill_continuous(low = "mistyrose", high = "blue", name = "Percentage", label = scales::comma) + 
  theme(panel.background = element_rect(colour = "black", fill = "lightblue"), legend.position = "right") +
  labs(title = "Female Percentage", subtitle = "US County Level Female Percentage Choropleth Map") 

plot_usmap(data = CHR21_state, values = "FemalePercent", color = "darkred", label_color = "white") + 
  scale_fill_continuous(low = "mistyrose", high = "blue", name = "Percentage", label = scales::comma) + 
  theme(panel.background = element_rect(colour = "black", fill = "lightblue"), legend.position = "right") +
  labs(title = "Female Percentage", subtitle = "US State Level Female Percentage Choropleth Map") 


# Gender dominance zonal map
getDominantGender <- function(gender) {
  if (gender > 50) {
    male_dominant <- 0
  } else {
    male_dominant <- 1
  }
  return(male_dominant)
}


# Creating Dominant Gender Column.
CHR21_county$DominantGender <- lapply(CHR21_county$FemalePercent,getDominantGender)
CHR21_county <- as.data.frame(lapply(CHR21_county, unlist))
CHR21_county$DominantGender <- as.factor(CHR21_county$DominantGender)
typeof(CHR21_county$DominantGender)

CHR21_state$DominantGender <- lapply(CHR21_state$FemalePercent,getDominantGender)
CHR21_state <- as.data.frame(lapply(CHR21_state, unlist))
CHR21_state$DominantGender <- as.factor(CHR21_state$DominantGender)
typeof(CHR21_state$DominantGender)


# Plot Factor map of Gender Dominance:
# 0 - Male Dominant 1 - Female Dominant
plot_usmap(data = CHR21_county, regions = "county", values = "DominantGender",color="grey")+ 
  theme(panel.background = element_rect(colour = "black")) +
  scale_fill_manual(values = c(`0` = "white", `1` = "blue"),
                    labels = c("female", "male"), name = "Dominant") + 
  theme(legend.position = "right") +
  labs(title = "Dominant Gender", subtitle = "US County Level Dominant Gender Choropleth Map") 

plot_usmap(data = CHR21_state, values = "DominantGender",color="grey")+ 
  theme(panel.background = element_rect(colour = "black")) +
  scale_fill_manual(values = c(`0` = "white", `1` = "blue"), 
                    labels = c("female", "male"), name = "Dominant") + 
  theme(legend.position = "right") +
  labs(title = "Dominant Gender", subtitle = "US State Level Dominant Gender Choropleth Map") 


# Summary of Gender Levels to Obesity
CHR21_county %>%
  group_by(DominantGender) %>%
  summarise(mean = mean(ObesityPercent), sd = sd(ObesityPercent), population = sum(Population), count = n())

CHR21_state %>%
  group_by(DominantGender) %>%
  summarise(mean = mean(ObesityPercent), sd = sd(ObesityPercent), population = sum(Population), count = n())

lm_gender_fit <- lm(ObesityPercent ~ DominantGender, data = CHR21_county)
summary(lm_gender_fit)


# A3. Race

# By Percentage of White Majority Choropleth Map
plot_usmap(data = CHR21_county, regions = c("counties"), values = "WhitePercent", color = "white", label_color = "white") + 
  scale_fill_continuous(low = "lightyellow", high = "darkred", name = "Percentage", label = scales::comma) + 
  theme(panel.background = element_rect(colour = "black", fill = "lightblue"), legend.position = "right") +
  labs(title = "White Percentage", subtitle = "US County Level White Majority Percentage Choropleth Map") 

plot_usmap(data = CHR21_state, values = "WhitePercent", color = "white", label_color = "white") + 
  scale_fill_continuous(low = "lightyellow", high = "darkred", name = "Percentage", label = scales::comma) + 
  theme(panel.background = element_rect(colour = "black", fill = "lightblue"), legend.position = "right") +
  labs(title = "White Percentage", subtitle = "US State Level White Majority Percentage Choropleth Map") 

# Plot Minority Race Percentage
plot_usmap(data = CHR21_county, regions = c("counties"), values = "BlackPercent", color = "white", label_color = "white") + 
  scale_fill_continuous(low = "lightyellow", high = "brown", name = "Percentage", label = scales::comma) + 
  theme(panel.background = element_rect(colour = "black", fill = "lightblue"), legend.position = "right") +
  labs(title = "Black Percentage", subtitle = "US County Level Black People Percentage Choropleth Map") 

plot_usmap(data = CHR21_county, regions = c("counties"), values = "NativeAmericanPercent", color = "white", label_color = "white") + 
  scale_fill_continuous(low = "lightyellow", high = "blue", name = "Percentage", label = scales::comma) + 
  theme(panel.background = element_rect(colour = "black", fill = "lightblue"), legend.position = "right") +
  labs(title = "Native American Percentage", subtitle = "US County Level Native American Percentage Choropleth Map") 

plot_usmap(data = CHR21_county, regions = c("counties"), values = "AsianPercent", color = "white", label_color = "white") + 
  scale_fill_continuous(low = "lightyellow", high = "pink", name = "Percentage", label = scales::comma) + 
  theme(panel.background = element_rect(colour = "black", fill = "lightblue"), legend.position = "right") +
  labs(title = "Asian Percentage", subtitle = "US County Level Asian People Percentage Choropleth Map") 

plot_usmap(data = CHR21_county, regions = c("counties"), values = "IslanderPercent", color = "white", label_color = "white") + 
  scale_fill_continuous(low = "lightyellow", high = "black", name = "Percentage", label = scales::comma) + 
  theme(panel.background = element_rect(colour = "black", fill = "lightblue"), legend.position = "right") +
  labs(title = "Islander Percentage", subtitle = "US County Level Pacific Islander People Percentage Choropleth Map") 

plot_usmap(data = CHR21_county, regions = c("counties"), values = "HispanicPercent", color = "white", label_color = "white") + 
  scale_fill_continuous(low = "lightyellow", high = "brown", name = "Percentage", label = scales::comma) + 
  theme(panel.background = element_rect(colour = "black", fill = "lightblue"), legend.position = "right") +
  labs(title = "Hispanic Percentage", subtitle = "US County Level Hispanic People Percentage Choropleth Map") 


# Dominant Minority Race Function
getMinority <- function(BlackPercent, NativeAmericanPercent, AsianPercent, IslanderPercent, HispanicPercent)
 {
  maxMinor <- max(BlackPercent, NativeAmericanPercent, AsianPercent, IslanderPercent, HispanicPercent)
  if (maxMinor == BlackPercent) {
    return(1)
  } else if (maxMinor == NativeAmericanPercent) {
    return(2)
  } else if (maxMinor == AsianPercent) {
    return(3)
  } else if (maxMinor == IslanderPercent) {
    return(4)
  } else if (maxMinor == HispanicPercent) {
    return(5)
  }
}


# Operation: Zoning by Dominant Minority Rate
CHR21_county$DominantMinority <- mapply(getMinority, CHR21_county$BlackPercent, CHR21_county$NativeAmericanPercent,
                                        CHR21_county$AsianPercent, CHR21_county$IslanderPercent, 
                                        CHR21_county$HispanicPercent)
CHR21_county$DominantMinority <- as.factor(CHR21_county$DominantMinority)

CHR21_state$DominantMinority <- mapply(getMinority, CHR21_state$BlackPercent, CHR21_state$NativeAmericanPercent,
                                        CHR21_state$AsianPercent, CHR21_state$IslanderPercent, 
                                        CHR21_state$HispanicPercent)
CHR21_state$DominantMinority <- as.factor(CHR21_state$DominantMinority)
  

# Plot Dominant Minority Chropoleth Map:
plot_usmap(data = CHR21_county, regions = "county", values = "DominantMinority",color="grey")+ 
  theme(panel.background = element_rect(colour = "black")) +
  scale_fill_manual(values = c(`1` = "red", `2` = "green", `3` = "purple", `4` = "orange", `5` = "blue"),
                    labels = c("Black", "Native American", "Asian", "Islander", "Hispanic"), name = "Dominant Minority") + 
  theme(legend.position = "right") +
  labs(title = "Dominant Minority Race", subtitle = "US County Level Dominant Minority Race Choropleth Map") 

plot_usmap(data = CHR21_state, values = "DominantMinority",color="grey")+ 
  theme(panel.background = element_rect(colour = "black")) +
  scale_fill_manual(values = c(`1` = "red", `2` = "green", `3` = "purple", `4` = "orange", `5` = "blue"),
                    labels = c("Black", "Native American", "Asian", "Islander", "Hispanic"), name = "Dominant Minority") + 
  theme(legend.position = "right") +
  labs(title = "Dominant Minority Race", subtitle = "US State Level Dominant Minority Race Choropleth Map") 


# Operation: Zonal Obesity Rate Summary
CHR21_county %>%
  group_by(DominantMinority) %>%
  summarise(mean = mean(ObesityPercent), sd = sd(ObesityPercent), population = sum(Population), count = n())

CHR21_state %>%
  group_by(DominantMinority) %>%
  summarise(mean = mean(ObesityPercent), sd = sd(ObesityPercent), population = sum(Population), count = n())


# A4. Migration Flow

# Read in data from Migration Table
data_migration <- read_excel("State_to_State_Migrations_Table_2019.xlsx", 
                          sheet = "Migration")
CHR21_state <- merge(CHR21_state, data_migration)

# Plot
plot_usmap(data = CHR21_state, values = "migration", color = "darkred", label_color = "white") + 
  scale_fill_continuous(low = "mistyrose", high = "blue", name = "Migration Size", label = scales::comma) + 
  theme(panel.background = element_rect(colour = "black", fill = "lightblue"), legend.position = "right") +
  labs(title = "Migration Data", subtitle = "US State Level Migration Size Choropleth Map") 

CHR21_state$MigrationLevel <- CHR21_state$migration / CHR21_state$Population

plot_usmap(data = CHR21_state, values = "MigrationLevel", color = "darkred", label_color = "white") + 
  scale_fill_continuous(low = "mistyrose", high = "blue", name = "Migration Level", label = scales::comma) + 
  theme(panel.background = element_rect(colour = "black", fill = "lightblue"), legend.position = "right") +
  labs(title = "Migration Level Data", subtitle = "US State Level Migration Level Choropleth Map") 


# B. Social Economic Influential Factors


# Feature Selection by SAR Spatial Autoregression

sar.res_first_round <-spautolm(ObesityPercent ~ AdultSmokingPercent + DrinkingPercent +
                          FoodEnvironmentIndex + PhysicalInactiveRate + ExerciseAccessPercent + 
                          SexualDiseaseRate + TeenBirthRate + UninsuredPercent + 
                          PrimaryCarePhysiciansRate + DentistRate + HighSchoolCompletionRate +
                          UnemployPercent + MedianIncomeNormalized + IncomeInequilityRatio +
                          ChildrenPovertyPercent + HousingProblemPercent + ViolentCrimeRate +
                          PoorHealthPercent + FoodInsecurePercent +
                          LimitedHealthyFoodAccessPercent + InsufficientSleepPercent, 
                        data = CHR21_sf.sf, listw = state.lw, zero.policy = TRUE)
summary(sar.res_first_round)

sar1.res_second_round <- spautolm(ObesityPercent ~ AdultSmokingPercent + 
                                    FoodEnvironmentIndex + PhysicalInactiveRate + 
                                    UninsuredPercent + UnemployPercent + IncomeInequilityRatio +
                                    ChildrenPovertyPercent + HousingProblemPercent +
                                    PoorHealthPercent + FoodInsecurePercent +
                                    LimitedHealthyFoodAccessPercent + InsufficientSleepPercent, 
                                  data = CHR21_sf.sf, listw = state.lw, zero.policy = TRUE)
summary(sar1.res_second_round)


# Alternative : OLS Linear Regression (Unused)
lm_model <- lm(ObesityPercent ~ AdultSmokingPercent + DrinkingPercent + DrinkingPercent +
                 FoodEnvironmentIndex + PhysicalInactiveRate + ExerciseAccessPercent + 
                 SexualDiseaseRate + TeenBirthRate + UninsuredPercent + 
                 PrimaryCarePhysiciansRate + DentistRate + HighSchoolCompletionRate +
                 UnemployPercent + MedianIncomeNormalized + IncomeInequilityRatio +
                 ChildrenPovertyPercent + HousingProblemPercent + ViolentCrimeRate +
                 PoorHealthPercent + FoodInsecurePercent +
                 LimitedHealthyFoodAccessPercent + InsufficientSleepPercent, 
               data = CHR21_county)
summary(lm_model)

lm_model_second_round <- lm(ObesityPercent ~ FoodEnvironmentIndex + PhysicalInactiveRate + 
                              UninsuredPercent + UnemployPercent + MedianIncomeNormalized + 
                              IncomeInequilityRatio + ChildrenPovertyPercent + 
                              HousingProblemPercent + PoorHealthPercent + FoodInsecurePercent +
                              LimitedHealthyFoodAccessPercent + InsufficientSleepPercent, 
                            data = CHR21_county)
summary(lm_model_second_round)



# Regions with poor Physical Health 
# PhysicalInactiveRate, InsufficientSleepPercent, PoorHealthPercent
# EDA
ggscatter(CHR21_county, y = "ObesityPercent", x = "PhysicalInactiveRate", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson",
          title = "Inspection on Physical Inactive Rate and Obesity of all counties",
          ylab = "Obesity % of Population", xlab = "Physical Inactive Rate")

ggscatter(CHR21_county, y = "ObesityPercent", x = "AdultSmokingPercent", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson",
          title = "Inspection on Adult Smoke Rate and Obesity of all counties",
          ylab = "Obesity % of Population", xlab = "Adult Smoke %")

ggscatter(CHR21_county, y = "ObesityPercent", x = "PoorHealthPercent", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson",
          title = "Inspection on Poor Health Percent and Obesity of all counties",
          ylab = "Obesity % of Population", xlab = "Poor Health %")

ggscatter(CHR21_county, y = "ObesityPercent", x = "InsufficientSleepPercent", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson",
          title = "Inspection on Insufficient Sleep and Obesity of all counties",
          ylab = "Obesity % of Population", xlab = "Insufficient Sleep %")


# Spatial Analysis on Health Factors
plot_usmap(data = CHR21_county, regions = c("counties"), values = "PhysicalInactiveRate", color = "white", label_color = "white") + 
  scale_fill_continuous(low = "lightgreen", high = "darkblue", name = "Physical Inactive Rate", label = scales::comma) + 
  theme(panel.background = element_rect(colour = "black", fill = "lightblue"), legend.position = "right") +
  labs(title = "White Percentage", subtitle = "US County Level Physical Inactive Rate Choropleth Map") 

plot_usmap(data = CHR21_state, values = "PhysicalInactiveRate", color = "white", label_color = "white") + 
  scale_fill_continuous(low = "lightgreen", high = "darkblue", name = "Physical Inactive Rate", label = scales::comma) + 
  theme(panel.background = element_rect(colour = "black", fill = "lightblue"), legend.position = "right") +
  labs(title = "White Percentage", subtitle = "US State Level Physical Inactive Rate Choropleth Map") 


#  Related Factor: Regions with lower food environment 
# Inspection on Food Environment
ggscatter(CHR21_county, y = "ObesityPercent", x = "FoodEnvironmentIndex", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson",
          title = "Inspection on Food Environment and Obesity of all counties",
          ylab = "Obesity % of Population", xlab = "Food Environment Index")

# Food Environment Index Chropoleth Map
ggscatter(CHR21_county, y = "ObesityPercent", x = "LimitedHealthyFoodAccessPercent", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson",
          title = "Inspection on Healthy Food Limited Access and Obesity of all counties",
          ylab = "Obesity % of Population", xlab = "Limited Healthy Food Access %")

ggscatter(CHR21_county, y = "ObesityPercent", x = "FoodEnvironmentIndex", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson",
          title = "Inspection on Food Environment and Obesity of all counties",
          ylab = "Obesity % of Population", xlab = "Food Environment Index %")

ggscatter(CHR21_county, y = "ObesityPercent", x = "FoodInsecurePercent", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson",
          title = "Inspection on Food Insecurity and Obesity of all counties",
          ylab = "Obesity % of Population", xlab = "Food Insecure %")


plot_usmap(data = CHR21_county, regions = c("counties"), values = "FoodInsecurePercent", color = "darkred", label_color = "white") + 
  scale_fill_continuous(low = "mistyrose", high = "blue", name = "Insecured Percent", label = scales::comma) + 
  theme(panel.background = element_rect(colour = "black", fill = "white"), legend.position = "right") +
  labs(title = "Food Insecurity", subtitle = "US County Level Food insecuirty Choropleth Map") 

plot_usmap(data = CHR21_state, regions = "states", values = "FoodInsecurePercent", color = "darkred", label_color = "white") + 
  scale_fill_continuous(low = "mistyrose", high = "blue", name = "Insecured Percent", label = scales::comma) + 
  theme(panel.background = element_rect(colour = "black", fill = "white"), legend.position = "right") +
  labs(title = "Food Insecurity", subtitle = "US State Level Food insecuirty Choropleth Map") 

# Workspace Related Scatterplots
ggscatter(CHR21_county, y = "ObesityPercent", x = "IncomeInequilityRatio", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson",
          title = "Inspection on Income Inequility and Obesity of all counties",
          ylab = "Obesity % of Population", xlab = "Income Inequility %")

ggscatter(CHR21_county, y = "ObesityPercent", x = "UninsuredPercent", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson",
          title = "Inspection on Food Environment and Obesity of all counties",
          ylab = "Obesity % of Population", xlab = "Uninsured %")

ggscatter(CHR21_county, y = "ObesityPercent", x = "UnemployPercent", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson",
          title = "Inspection on Food Insecurity and Obesity of all counties",
          ylab = "Obesity % of Population", xlab = "Unemployed %")




