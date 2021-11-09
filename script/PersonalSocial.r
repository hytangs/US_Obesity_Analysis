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

# Normalization of Non- Rate / Percent Factors
summary(CHR21_county$MedianIncome)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
#  24732   46243   53341   55713   62059  151806       1 
CHR21_county$MedianIncomeNormalized = (CHR21_county$MedianIncome - 24732) / 55713 * 50

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





