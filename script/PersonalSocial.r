# Set Working Dir
setwd("C:/Users/cwche/Desktop/US_Obesity_Analysis/data")


# Personal and Social Economic Factors


# Library Load
library(usmap)
library(ggplot2)
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
plot_usmap(data = CHR21_county, regions = c("counties"), values = "LifeExpectancy", color = "darkred", label_color = "white") + 
  scale_fill_continuous(low = "mistyrose", high = "blue", name = "Expected Ages", label = scales::comma) + 
  theme(panel.background = element_rect(colour = "black", fill = "lightblue"), legend.position = "right") +
  labs(title = "Life Expectancy", subtitle = "US County Level Life Expectancy Choropleth Map") 

plot_usmap(data = CHR21_state, values = "LifeExpectancy", color = "darkred", label_color = "white") + 
  scale_fill_continuous(low = "mistyrose", high = "blue", name = "Expected Ages", label = scales::comma) + 
  theme(panel.background = element_rect(colour = "black", fill = "lightblue"), legend.position = "right") +
  labs(title = "Life Expectancy", subtitle = "US State Level Life Expectancy Choropleth Map") 


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



# A3. Race

# Race Choropleth Map


# Operation: Zoning by Dominant Minority Rate


# Operation: Get Zonal Value


# Operation: 
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
                 PoorHealthPercent + LifeExpectancy + FoodInsecurePercent +
                 LimitedHealthyFoodAccessPercent + InsufficientSleepPercent, 
               data = CHR21_county)
summary(lm_model)

lm_model_second_round <- lm(ObesityPercent ~ FoodEnvironmentIndex + PhysicalInactiveRate + 
                              UninsuredPercent + UnemployPercent + IncomeInequilityRatio +
                              HousingProblemPercent + LifeExpectancy + FoodInsecurePercent +
                              LimitedHealthyFoodAccessPercent, data = CHR21_county)
summary(lm_model_second_round)

# Inspect Potential Influential Factors
ggscatter(chd21, y = "ObesityPercent", x = "FoodEnvironmentIndex", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson",
          ylab = "Obesity % of Population", xlab = "Food Environment Index")
ggscatter(chd21, y = "ObesityPercent", x = "PEInactive", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson",
          ylab = "Obesity % of Population", xlab = "PE Inactive % of Population")
ggscatter(chd21, y = "ObesityPercent", x = "UnemployPercent", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson",
          ylab = "Obesity % of Population", xlab = "Unemployed % of Population")
ggscatter(chd21, y = "ObesityPercent", x = "smokepercent", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson",
          ylab = "Obesity % of Population", xlab = "Smoking % of Population")

chd21$predicted = 10.3685635+0.4150168*chd21$FoodEnvir+0.4554356*chd21$PEInactive+0.3730278*chd21$UnemployPercent+0.2997735*chd21$smokepercent
plot_usmap(data = chd21, regions = c("counties"), values = "predicted", color = "darkred", label_color = "white") + 
  scale_fill_continuous(low = "mistyrose", high = "blue", name = "Obesity Prediction", label = scales::comma) + 
  theme(legend.position = "right")


# Significant Features Selected: 
# Food, 


# B2. Career (Employment, Income)


# B3. Dining (Food Access)


# B4. Education

