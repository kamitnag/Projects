# regression analysis

# set up the environment
library(readxl)
library(sqldf)
library(corrplot)

# import the data
cereal<- read.csv('cereal.csv')

# DATA EXPLORATION

# check for data formats
str(cereal)
#check for NAs
sapply(cereal, function(x) sum(is.na(x)))
summary(cereal)
#no missing data but notice that carbohydrates and sugars have the min value of -1 which
# is illogical so I will remove those observations
cereal<- cereal[cereal$sugars != -1, ]
cereal<- cereal[cereal$carbo != -1, ] #turns out it was the same observation

# CHECK ASSUMPTIONS

# linearity with the DV and colinnearity
# only select numerical variables that will be in the initial model
cereal_sht<- sqldf('SELECT calories, protein, fat, sodium, fiber, carbo, sugars, potass, vitamins, shelf, weight, cups, rating FROM cereal')
#check for linearity and colinnearity
cor_coeff<- round(cor(cereal_sht, method = 'pearson'),2)
corrplot(cor_coeff, type = "upper", order = "hclust", tl.col = "black", tl.srt = 45)
# rating
# calories -0.68876328
# protein   0.47030522
# fat      -0.42052619
# sodium   -0.39713585
# fiber     0.58390015
# carbo     0.08871244 - really low
# sugars   -0.76390195
# potass    0.37960414
# vitamins -0.23377517
# shelf     0.03700208 - really low
# weight   -0.29734622 - low
# cups     -0.19913976 - low
# rating    1.00000000

# step1: fit the full model with all variables
mod0<- lm(rating~calories+protein+fat+sodium+fiber+carbo+sugars+potass+vitamins+shelf+weight+cups, cereal_sht)
summary(mod0)
# shelf, weight, and cups are not significant in the model so we will omit them in model2

#step2:
mod01<- lm(rating~calories+protein+fat+sodium+fiber+carbo+sugars+potass+vitamins, cereal_sht)
summary(mod01)
# R^adj is still 1 which means that we are probably overfitting the model

#step3: calories has low correlation with rating so we will omit it
mod02<- lm(rating~protein+fat+sodium+fiber+carbo+sugars+potass+vitamins, cereal_sht)
summary(mod02)

#step4: carbs also has low correlation so we will omit
mod03<- lm(rating~protein+fat+sodium+fiber+sugars+potass+vitamins, cereal_sht)
summary(mod03)

#step5: vitamins will be omitted due to low correlation
mod04<- lm(rating~protein+fat+sodium+fiber+sugars+potass, cereal_sht)
summary(mod04)

#step6: potassium is colloquially non-important when looking at nutrition labels, so will be omitted as well
mod05<- lm(rating~protein+fat+sodium+fiber+sugars, cereal_sht)
summary(mod05)
#almost no change to R^2-adj

#step 7: just from looking at the predictors in mod05, they seem like the most commonly looked ingredients on
#food labels, however, we wanted to check if sodium had any impact, when omitted, R^adj dropped below 90% which is not ideal
mod06<- lm(rating~protein+fat+fiber+sugars, cereal_sht)
summary(mod06)

# BEST MODEL
# the best model is mod05 which is the closest to a parsimounious model we can get, it has R^2-adj of 98.58%
# and all predictors are strongly correlated to the dependant variable and have low correlation to each other

# EQUATION: rating_hat = 58.15 + 1.97(protein) - 3.9(fat) - 0.057(sodium) + 2.42(fiber) - 1.8(sugars)


# VERIFYING THE MODEL
# check for normal residuals
hist(mod05$residuals)
# the residuals are mostly normal with a little skew to the right which indicates that the model is a good fit
#check for constant variability
plot(cereal_sht$rating~mod05$residuals)
abline(lm(cereal_sht$rating~mod05$residuals), col= "red")

#plot predictions
plot(cereal_sht$rating~mod05$fitted.values)
abline(lm(cereal_sht$rating~mod05$fitted.values), col= "red")


# APPENDIX A: PERFORMANCE OF THE MODEL
performance_test<- data.frame(cereal_sht$rating, mod05$fitted.values, mod05$residuals)
summary(mod05$residuals)


