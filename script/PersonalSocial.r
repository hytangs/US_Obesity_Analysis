# Personal and Social Economic Factors

# Library Load
library(usmap)
library(ggplot2)
library(readxl)
CHR21_county <- read_excel("CountryHealthRankings21_Cleaned.xlsx", 
                           sheet = "county")
head(CHR21_county)
CHR21_county <- as.data.frame(CHR21_county)

# County Level Obesity Percent Map
plot_usmap(data = CHR21_county, regions = c("counties"), values = "ObesityPercent", color = "darkred", label_color = "white") + 
  scale_fill_continuous(low = "mistyrose", high = "blue", name = "Obesity Percent", label = scales::comma) + 
  theme(legend.position = "right")

# Normalization of Non-Rate / Non-Percent Factors
summary(CHR21_county$MedianIncome)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
#  24732   46243   53341   55713   62059  151806       1 
CHR21_county$MedianIncomeNormalized = (CHR21_county$MedianIncome - 24732) / 55713 * 50

# A. Population Demographics
# A1. Age Range
plot_usmap(data = CHR21_county, regions = c("counties"), values = "LifeExpectancy", color = "darkred", label_color = "white") + 
  scale_fill_continuous(low = "mistyrose", high = "blue", name = "Expected Ages", label = scales::comma) + 
  theme(panel.background = element_rect(colour = "black", fill = "lightblue"), legend.position = "right") +
  labs(title = "Life Expectancy", subtitle = "US County Level Life Expectancy Choropleth Map") 

# A2. Gender
plot_usmap(data = CHR21_county, regions = c("counties"), values = "FemalePercent", color = "darkred", label_color = "white") + 
  scale_fill_continuous(low = "mistyrose", high = "blue", name = "Percentage", label = scales::comma) + 
  theme(panel.background = element_rect(colour = "black", fill = "lightblue"), legend.position = "right") +
  labs(title = "Female Percentage", subtitle = "US County Level Female Percentage Choropleth Map") 

# Gender dominance zonal map

# Summary of Gender Levels to Obesity

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

# Significant Features Selected: 
# Food, 

# B2. Career (Employment, Income)


# B3. Dining (Food Access)


# B4. Education

