# Set Working Dir
setwd("C:/Users/cwche/Desktop/US_Obesity_Analysis/data")


# Personal and Social Economic Factors


# Library Load
library(usmap)
library(ggplot2)
library(ggpubr)
library(readxl)
library(dplyr)


# Dataset Load
CHR21_county <- read_excel("CountryHealthRankings21_Cleaned.xlsx", 
                           sheet = "county")
CHR21_state <- read_excel("CountryHealthRankings21_Cleaned.xlsx", 
                           sheet = "state")
head(CHR21_county)
head(CHR21_state)
CHR21_county <- as.data.frame(CHR21_county)
CHR21_state <- as.data.frame(CHR21_state)


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


# A. Population Demographics


# A1. Age Range
# Life Expectancy
lm_life_fit <- lm(ObesityPercent ~ LifeExpectancy, data = CHR21_county)
summary(lm_life_fit)

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

plot_usmap(data = CHR21_county, values = "ElderlyPercent", color = "darkred", label_color = "white") + 
  scale_fill_continuous(low = "mistyrose", high = "blue", name = "Elderly Percentage", label = scales::comma) + 
  theme(panel.background = element_rect(colour = "black", fill = "lightblue"), legend.position = "right") +
  labs(title = "Elderly Percent", subtitle = "US County Level Elderly Percentage Choropleth Map") 

plot_usmap(data = CHR21_state, values = "ElderlyPercent", color = "darkred", label_color = "white") + 
  scale_fill_continuous(low = "mistyrose", high = "blue", name = "Elderly Percentage", label = scales::comma) + 
  theme(panel.background = element_rect(colour = "black", fill = "lightblue"), legend.position = "right") +
  labs(title = "Elderly Percent", subtitle = "US State Level Elderly Percentage Choropleth Map") 


# A2. Gender
plot_usmap(data = CHR21_county, regions = c("counties"), values = "FemalePercent", color = "darkred", label_color = "white") + 
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
  scale_fill_continuous(low = "mistyrose", high = "blue", name = "Percentage", label = scales::comma) + 
  theme(panel.background = element_rect(colour = "black", fill = "lightblue"), legend.position = "right") +
  labs(title = "White Percentage", subtitle = "US County Level White Majority Percentage Choropleth Map") 

plot_usmap(data = CHR21_state, values = "WhitePercent", color = "white", label_color = "white") + 
  scale_fill_continuous(low = "mistyrose", high = "blue", name = "Percentage", label = scales::comma) + 
  theme(panel.background = element_rect(colour = "black", fill = "lightblue"), legend.position = "right") +
  labs(title = "White Percentage", subtitle = "US State Level White Majority Percentage Choropleth Map") 

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


# B. Social Economic Influential Factors

# B1. Feature Identification and Selection
# Simple Linear Regression For Feature Selection
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


# Hypothesis 1: Regions with poor Physical Health 
# PhysicalInactiveRate, InsufficientSleepPercent, PoorHealthPercent
# EDA
ggscatter(CHR21_county, y = "ObesityPercent", x = "PhysicalInactiveRate", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson",
          title = "Inspection on Physical Inactive Rate and Obesity of all counties",
          ylab = "Obesity % of Population", xlab = "Physical Inactive Rate")

ggscatter(CHR21_county, y = "ObesityPercent", x = "InsufficientSleepPercent", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson",
          title = "Inspection on Insufficient Sleep and Obesity of all counties",
          ylab = "Obesity % of Population", xlab = "Insufficient Sleep %")

ggscatter(CHR21_county, y = "ObesityPercent", x = "PoorHealthPercent", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson",
          title = "Inspection on Poor Health Percent and Obesity of all counties",
          ylab = "Obesity % of Population", xlab = "Poor Health %")


# Spatial Analysis on Health Factors
plot_usmap(data = CHR21_county, regions = c("counties"), values = "PhysicalInactiveRate", color = "white", label_color = "white") + 
  scale_fill_continuous(low = "lightgreen", high = "darkblue", name = "Physical Inactive Rate", label = scales::comma) + 
  theme(panel.background = element_rect(colour = "black", fill = "lightblue"), legend.position = "right") +
  labs(title = "White Percentage", subtitle = "US County Level Physical Inactive Rate Choropleth Map") 



# Hypothesis 2: Food Related Factor: Regions with lower food environment 
# Inspection on Food Environment
ggscatter(CHR21_county, y = "ObesityPercent", x = "FoodEnvironmentIndex", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson",
          title = "Inspection on Food Environment and Obesity of all counties",
          ylab = "Obesity % of Population", xlab = "Food Environment Index")

# Food Environment Index Chropoleth Map
plot_usmap(data = CHR21_county, regions = c("counties"), values = "ObesityPercent", color = "darkred", label_color = "white") + 
  scale_fill_continuous(low = "mistyrose", high = "blue", name = "Obesity Percent", label = scales::comma) + 
  theme(legend.position = "right")

plot_usmap(data = CHR21_state, regions = "states", values = "ObesityPercent", color = "darkred", label_color = "white") + 
  scale_fill_continuous(low = "mistyrose", high = "blue", name = "Obesity Percent", label = scales::comma) + 
  theme(legend.position = "right")


# Hypothesis 3: Lower Income Regions have higher Obesity Rates.
# Variables: IncomeInequilityRatio, UnemployPercent




# Hypothesis 4: Education Level doesn't affect Obesity Rate Much.
# HighSchoolCompletionRate



# Modeling Social Economic Risk Prediction

# Use Linear Regression?


