# Insurance dataset 
# Load the dataset into a data frame first
# refer to notes on Blackboard for discusisons on 
# dummy variables and how they are generated

insurance_data <- read.csv("insurance.csv", na = "")
str(insurance_data)

# several variables need to be converted
# sex - male = 0, female = 1
# Smoker - yes = 1, no = 0
# Region contains 4 categories
# N = 4, so we need n-1 indicator variables
# = 3 indicator variables
# Code variables in alphabetical order
head(insurance_data$region, 15)

# Convert variables as described above

attach(insurance_data)

insurance_data$sex <- factor(sex,
                             levels = c("male", "female"), 
                             ordered = FALSE)

insurance_data$smoker <- factor(smoker, 
                                levels = c("yes", "no"), 
                                ordered = FALSE)

insurance_data$region <- factor(region,  
                                levels = c("northeast", "northwest", "southeast", "southwest"), 
                                ordered = FALSE)

str(insurance_data)

# View the split of categorical variables within the data frame
# to examine balance
table(insurance_data$sex)
table(insurance_data$smoker)
table(insurance_data$region)


# Initial investigation of data variables
# and their correlations
# Be careful of your interpretation of this chart
windows(20,10)
pairs(insurance_data)
install.packages("psych")
library(psych)

# Seems there could be a positive correlation between 
# smoker and charges, perhaps charges and age
# and BMI and charges
pairs.panels(insurance_data,
             smooth = FALSE,      # If TRUE, draws loess smooths
             scale = FALSE,      # If TRUE, scales the correlation text font
             density = TRUE,     # If TRUE, adds density plots and histograms
             ellipses = FALSE,    # If TRUE, draws ellipses
             method = "pearson",# Correlation method (also "pearson" or "kendall")
             pch = 21,           # pch symbol
             lm = FALSE,         # If TRUE, plots linear fit rather than the LOESS (smoothed) fit
             cor = TRUE,         # If TRUE, reports correlations
             jiggle = FALSE,     # If TRUE, data points are jittered
             factor = 2,         # Jittering factor
             hist.col = 4,       # Histograms color
             stars = TRUE,       # If TRUE, adds significance level with stars
             ci = TRUE)          # If TRUE, adds confidence intervals

# if we build the model now, R will automatically split the
# factor variables
# Alternatively we will control this process

# in linear regression, model represented by:
# y = b0 + B1x1 + B2x2 + B3x3..... + e
# where y = insurance charges
# x1 = age of the person
# x2  sex of the person
# x3 = bmi of the person
# x4 = children
# x5 = smoker
# x6 = region
# It is clear that x1, and x3 are continuous and x2, x4, x5, x6 are categorical
# therefore we need to create dummy variables for the categorical
# variables
# Eg for the smoker variable x5
# x5 = 1 if person is smoker
# x5 = 0 if person is non-smoker

# Initial build of th MLR model
# Dummy varaibles created automatically by R
#set.seed(1)
attach(insurance_data)
model <- lm(charges ~ 
              age + 
              sex + 
              bmi + 
              children + 
              smoker + 
              region, 
            data = insurance_data)

model
summary(model)
saveRDS(model, "./model.rds")

#Removing the variable that are not important
model2 <- lm(charges ~
               age +
               bmi+
               children +
               smoker +
               region,
             data = insurance_data)

model2
summary(model2)
saveRDS(model2, "./model2.rds")

#Regression equation for model2
charges ~ 11846 + 256.97 * age + 338.66 * BMI + 474.57 * children - 23836.30 * Smoker -1034.36 * regionsoutheast
- 959.37 * regionsouthwest

AIC(model)
AIC(model2)
paste('AIC of First model', round(AIC(model),2))
paste('AIC of Second model', round(AIC(model2),2))
BIC(model)
BIC(model2)
paste('BIC of First model', round(BIC(model),2))
paste('BIC of Second model', round(BIC(model2),2))

sprintf('BIC of second model %e', BIC(model2))

#