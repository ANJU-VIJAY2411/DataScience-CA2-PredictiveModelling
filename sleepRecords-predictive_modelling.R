#PART 1 - READING THE DATASET

#Load the data in sleep.csv
sleepRecords <- read.csv("sleep.csv", na = "")
sleepRecords


print(is.data.frame(sleepRecords))

print(ncol(sleepRecords))
#print the number of columns in the dataset : 9

print(nrow(sleepRecords))
#*print the number of records in the dataset : 630


str(sleepRecords)

sum(is.na(sleepRecords))
#validating the null values in the dataset : 0

names(sleepRecords)
#we rename all columns to make the data more understandable
names(sleepRecords)[names(sleepRecords)=="sr"] <- "snoring_rate"
names(sleepRecords)[names(sleepRecords)=="rr"] <- "respiration_rate"
names(sleepRecords)[names(sleepRecords)=="t"] <- "body_temperature"
names(sleepRecords)[names(sleepRecords)=="lm"] <- "limb_movement"
names(sleepRecords)[names(sleepRecords)=="bo"] <- "blood_oxygen"
names(sleepRecords)[names(sleepRecords)=="rem"] <- "rapid_eye_movement"
names(sleepRecords)[names(sleepRecords)=="sr.1"] <- "sleeping_hours"
names(sleepRecords)[names(sleepRecords)=="hr"] <- "heart_rate"
names(sleepRecords)[names(sleepRecords)=="sl"] <- "stress_level"

names(sleepRecords)
summary(sleepRecords)

str(sleepRecords)



#pairplot

install.packages("psych")
library(psych)

pairs(sleepRecords)
# Initial investigation of data variable
pairs.panels(sleepRecords,
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


#correlation plot to check the correlation betweeen variables

correlation_tab <- cor(sleepRecords)
install.packages("corrplot")
library(corrplot)
col <- colorRampPalette(c("#BB4444", "#EE9988", "#FFFFFF", "#77AADD", "#4477AA"))
#png(file="corr2.png",res=150,width=900,height=700)                        
corrplot(correlation_tab, method = "color", shade.col = NA, tl.col = "black", tl.srt = 45,tl.cex =1,cl.cex=1,col = col(200), addCoef.col = "black", order = "AOE",number.cex = .5)

#* Normality check for all variables using qq plot

attach(sleepRecords)
opar <- par(no.readonly = TRUE)
par(mar = c(3, 3, 2, 1)) 

qqnorm(sleepRecords$snoring_rate, main="QQ plot for  Snoring Rate", ylab = "Snoring Rate")
qqline(sleepRecords$snoring_rate)

qqnorm(sleepRecords$respiration_rate, main="QQ plot for  Respiration Rate", pch=19, ylab = "Respiration Rate")
qqline(sleepRecords$respiration_rate)

qqnorm(sleepRecords$body_temperature, main="QQ plot for  Body Temperature", pch=19, ylab = "Body Temperature")
qqline(sleepRecords$body_temperature)

qqnorm(sleepRecords$limb_movement, main="QQ plot for Limb movement", pch=19, ylab = "Limb movement")
qqline(sleepRecords$limb_movement)


qqnorm(sleepRecords$blood_oxygen, main="QQ plot for  Blood Oxygen", pch=19, ylab = "Blood oxygen")
qqline(sleepRecords$blood_oxygen)

qqnorm(sleepRecords$rapid_eye_movement, main="QQ plot for Rapid Eye movement", pch=19, ylab = "Rapid eye movement")
qqline(sleepRecords$rapid_eye_movement)

qqnorm(sleepRecords$sleeping_hours, main="QQ plot for Sleeping hours", pch=19, ylab = "Sleeping hours")
qqline(sleepRecords$sleeping_hours)

qqnorm(sleepRecords$heart_rate, main="QQ plot for Heart Rate", pch=19, ylab = "Heart Rate")
qqline(sleepRecords$heart_rate)

qqnorm(sleepRecords$stress_level, main="QQ plot for Stress level", pch=19, ylab = "Stress level")
qqline(sleepRecords$stress_level)



cor(sleepRecords$blood_oxygen, sleepRecords$snoring_rate)

scatter.smooth(x = sleepRecords$snoring_rate,
               y = sleepRecords$blood_oxygen,
               main = "Correlation of blood_oxygen ~ snoring_rate",
               xlab = "snoring_rate %",
               ylab = "blood_oxygen %")


cor(sleepRecords$blood_oxygen, sleepRecords$respiration_rate)

scatter.smooth(x = sleepRecords$respiration_rate,
               y = sleepRecords$blood_oxygen,
               main = "Correlation of blood_oxygen ~ respiration_rate",
               xlab = "respiration_rate %",
               ylab = "blood_oxygen %")


cor(sleepRecords$blood_oxygen, sleepRecords$body_temperature)

scatter.smooth(x = sleepRecords$body_temperature,
               y = sleepRecords$blood_oxygen,
               main = "Correlation of blood_oxygen ~ body_temperature",
               xlab = "body_temperature %",
               ylab = "blood_oxygen %")


cor(sleepRecords$blood_oxygen, sleepRecords$limb_movement)

scatter.smooth(x = sleepRecords$limb_movement,
               y = sleepRecords$blood_oxygen,
               main = "Correlation of blood_oxygen ~ limb_movement",
               xlab = "limb_movement %",
               ylab = "blood_oxygen %")


cor(sleepRecords$blood_oxygen, sleepRecords$rapid_eye_movement)

scatter.smooth(x = sleepRecords$rapid_eye_movement,
               y = sleepRecords$blood_oxygen,
               main = "Correlation of blood_oxygen ~ rapid_eye_movement",
               xlab = "rapid_eye_movement %",
               ylab = "blood_oxygen %")



cor(sleepRecords$blood_oxygen, sleepRecords$sleeping_hours)

scatter.smooth(x = sleepRecords$sleeping_hours,
               y = sleepRecords$blood_oxygen,
               main = "Correlation of blood_oxygen ~ sleeping_hours",
               xlab = "sleeping_hours %",
               ylab = "blood_oxygen %")



cor(sleepRecords$blood_oxygen, sleepRecords$heart_rate)

scatter.smooth(x = sleepRecords$heart_rate,
               y = sleepRecords$blood_oxygen,
               main = "Correlation of blood_oxygen ~ heart_rate",
               xlab = "heart_rate %",
               ylab = "blood_oxygen %")



cor(sleepRecords$blood_oxygen, sleepRecords$stress_level)

scatter.smooth(x = sleepRecords$stress_level,
               y = sleepRecords$blood_oxygen,
               main = "Correlation of blood_oxygen ~ stress_level",
               xlab = "stress_level %",
               ylab = "blood_oxygen %")


shapiro.test(sleepRecords$snoring_rate) # p-value < 2.2e-16 , not normally distributed
shapiro.test(sleepRecords$respiration_rate) # p-value = 1.571e-15 , not normally distributed
shapiro.test(sleepRecords$body_temperature) # p-value = 6.969e-09 , not normally distributed
shapiro.test(sleepRecords$limb_movement) # p-value = 3.211e-14 , not normally distributed
shapiro.test(sleepRecords$blood_oxygen) # p-value = 1.324e-11 , not normally distributed
shapiro.test(sleepRecords$rapid_eye_movement) # p-value = 1.313e-15 , not normally distributed
shapiro.test(sleepRecords$sleeping_hours) # p-value < 2.2e-16 , not normally distributed
shapiro.test(sleepRecords$heart_rate) # p-value = 1.571e-15 , not normally distributed
shapiro.test(sleepRecords$stress_level) # p-value < 2.2e-16 , not normally distributed



#PART 3 - OUTLIER DETETCTION AND SOLUTION
#There are multiple ways to detect outliers within a dataset. 
#scatter plots and bar plots are quite commonly used
#trying to use the boxplot analysis to detect outliers.


attach(sleepRecords)
opar <- par(no.readonly = TRUE)
par(mar = c(3,3,2,1))  # Adjust the margin values as needed
boxplot(sleepRecords$snoring_rate,
        main = "snoring_rate",
        sub = paste("Outlier rows: ", boxplot.stats(sleepRecords$snoring_rate)$out)
)


boxplot(sleepRecords$respiration_rate,
        main = "respiration_rate",
        sub = paste("Outlier rows: ", boxplot.stats(sleepRecords$respiration_rate)$out)
)


boxplot(sleepRecords$body_temperature,
        main = "body_temperature",
        sub = paste("Outlier rows: ", boxplot.stats(sleepRecords$body_temperature)$out)
)


boxplot(sleepRecords$limb_movement,
        main = "limb_movement",
        sub = paste("Outlier rows: ", boxplot.stats(sleepRecords$limb_movement)$out)
)

boxplot(sleepRecords$blood_oxygen,
        main = "blood_oxygen",
        sub = paste("Outlier rows: ", boxplot.stats(sleepRecords$blood_oxygen)$out)
)

boxplot(sleepRecords$rapid_eye_movement,
        main = "rapid_eye_movement",
        sub = paste("Outlier rows: ", boxplot.stats(sleepRecords$rapid_eye_movement)$out)
)

boxplot(sleepRecords$sleeping_hours,
        main = "sleeping_hours",
        sub = paste("Outlier rows: ", boxplot.stats(sleepRecords$sleeping_hours)$out)
)

boxplot(sleepRecords$heart_rate,
        main = "heart_rate",
        sub = paste("Outlier rows: ", boxplot.stats(sleepRecords$heart_rate)$out)
)

boxplot(sleepRecords$stress_level,
        main = "stress_level",
        sub = paste("Outlier rows: ", boxplot.stats(sleepRecords$stress_level)$out)
)


detach(sleepRecords)
par <- opar


outlier_values <- boxplot.stats(sleepRecords$respiration_rate)$out # outlier values.
paste("Respiration rate outliers: ", paste(outlier_values, collapse=", "))

outlier_value <- boxplot.stats(sleepRecords$snoring_rate)$out
paste("Snoring rate outliers: ", paste(outlier_values, collapse=", "))

outlier_value <- boxplot.stats(sleepRecords$body_temperature)$out
paste("body temperature outliers: ", paste(outlier_values, collapse=", "))

outlier_value <- boxplot.stats(sleepRecords$limb_movement)$out
paste("limb movement outliers: ", paste(outlier_values, collapse=", "))

outlier_value <- boxplot.stats(sleepRecords$blood_oxygen)$out
paste("Blood oxygen outliers: ", paste(outlier_values, collapse=", "))

outlier_value <- boxplot.stats(sleepRecords$rapid_eye_movement)$out
paste("Rapid eye movement outliers: ", paste(outlier_values, collapse=", "))

outlier_value <- boxplot.stats(sleepRecords$sleeping_hours)$out
paste("Sleeping hours outliers: ", paste(outlier_values, collapse=", "))

outlier_value <- boxplot.stats(sleepRecords$heart_rate)$out
paste("Heart Rate outliers: ", paste(outlier_values, collapse=", "))

outlier_value <- boxplot.stats(sleepRecords$stress_level)$out
paste("Stress level outliers: ", paste(outlier_values, collapse=", "))

str(sleepRecords)



library(MASS)

# find optimal lambda for box-cox transform
# Plotting lambda from -6 to 6 in steps of 0.1
Box = boxcox(sleepRecords$respiration_rate ~ 1, lambda = seq(-6,6,0.1))
# Create a data frame with the results
Cox = data.frame(Box$x, Box$y)   
# Order the new data frame by decreasing y
cox_smallest_y = Cox[with(Cox, order(-Cox$Box.y)),] 
# Display the lambda with the greatest
# log likelihood
cox_smallest_y[1,]                                  
# Extract that lambda
lambda = cox_smallest_y[1, "Box.x"]                 
lambda

transformed_respiration_rate = (sleepRecords$respiration_rate ^ lambda - 1)/lambda

shapiro.test(transformed_respiration_rate)

#p-value = 0.000000000003216
# p-value indictes that the data is still not normally distributed



# find optimal lambda for box-cox transform
# Plotting lambda from -6 to 6 in steps of 0.1
Box = boxcox(sleepRecords$snoring_rate ~ 1, lambda = seq(-6,6,0.1))
# Create a data frame with the results
Cox = data.frame(Box$x, Box$y)   
# Order the new data frame by decreasing y
cox_smallest_y = Cox[with(Cox, order(-Cox$Box.y)),] 
# Display the lambda with the greatest
# log likelihood
cox_smallest_y[1,]                                  
# Extract that lambda
lambda = cox_smallest_y[1, "Box.x"]                 
lambda

transformed_snoring_rate = (sleepRecords$snoring_rate ^ lambda - 1)/lambda

shapiro.test(transformed_snoring_rate)

#p-value < 0.00000000000000022
# p-value indictes that the data is still not normally distributed




# find optimal lambda for box-cox transform
# Plotting lambda from -6 to 6 in steps of 0.1
Box = boxcox(sleepRecords$body_temperature ~ 1, lambda = seq(-6,6,0.1))
# Create a data frame with the results
Cox = data.frame(Box$x, Box$y)   
# Order the new data frame by decreasing y
cox_smallest_y = Cox[with(Cox, order(-Cox$Box.y)),] 
# Display the lambda with the greatest
# log likelihood
cox_smallest_y[1,]                                  
# Extract that lambda
lambda = cox_smallest_y[1, "Box.x"]                 
lambda

transformed_body_temperature = (sleepRecords$body_temperature ^ lambda - 1)/lambda

shapiro.test(transformed_body_temperature)

# p-value = 0.0000001839
# p-value indictes that the data is still not normally distributed



# find optimal lambda for box-cox transform
# Plotting lambda from -6 to 6 in steps of 0.1
Box = boxcox(sleepRecords$limb_movement ~ 1, lambda = seq(-6,6,0.1))
# Create a data frame with the results
Cox = data.frame(Box$x, Box$y)   
# Order the new data frame by decreasing y
cox_smallest_y = Cox[with(Cox, order(-Cox$Box.y)),] 
# Display the lambda with the greatest
# log likelihood
cox_smallest_y[1,]                                  
# Extract that lambda
lambda = cox_smallest_y[1, "Box.x"]                 
lambda

transformed_limb_movement = (sleepRecords$limb_movement ^ lambda - 1)/lambda

shapiro.test(transformed_limb_movement)

# p-value = 0.0000001839
# p-value indictes that the data is still not normally distributed


# find optimal lambda for box-cox transform
# Plotting lambda from -6 to 6 in steps of 0.1
Box = boxcox(sleepRecords$blood_oxygen ~ 1, lambda = seq(-6,6,0.1))
# Create a data frame with the results
Cox = data.frame(Box$x, Box$y)   
# Order the new data frame by decreasing y
cox_smallest_y = Cox[with(Cox, order(-Cox$Box.y)),] 
# Display the lambda with the greatest
# log likelihood
cox_smallest_y[1,]                                  
# Extract that lambda
lambda = cox_smallest_y[1, "Box.x"]                 
lambda

transformed_blood_oxygen = (sleepRecords$blood_oxygen ^ lambda - 1)/lambda

shapiro.test(transformed_blood_oxygen)

# p-value = 0.0000000002537
# p-value indictes that the data is still not normally distributed



# find optimal lambda for box-cox transform
# Plotting lambda from -6 to 6 in steps of 0.1
Box = boxcox(sleepRecords$rapid_eye_movement ~ 1, lambda = seq(-6,6,0.1))
# Create a data frame with the results
Cox = data.frame(Box$x, Box$y)   
# Order the new data frame by decreasing y
cox_smallest_y = Cox[with(Cox, order(-Cox$Box.y)),] 
# Display the lambda with the greatest
# log likelihood
cox_smallest_y[1,]                                  
# Extract that lambda
lambda = cox_smallest_y[1, "Box.x"]                 
lambda

transformed_rapid_eye_movement = (sleepRecords$rapid_eye_movement ^ lambda - 1)/lambda

shapiro.test(transformed_rapid_eye_movement)

# p-value = 0.0000000000004123
# p-value indictes that the data is still not normally distributed





#*===========Model building====================

#import caTools library
install.packages("caTools")
install.packages("pscl")
library(caTools)
#splitting the dataset
set.seed(100)
split = sample.split(sleepRecords$blood_oxygen, SplitRatio = 0.80)
train_set = subset(sleepRecords, split == TRUE)
test_set = subset(sleepRecords, split == FALSE)
#Model ran with all variables
model1 <- lm( blood_oxygen~ snoring_rate + heart_rate + body_temperature + limb_movement + respiration_rate  + rapid_eye_movement + 
                sleeping_hours, data=train_set)
summary(model1)

#Model ran with selected variables
model2 <- lm( blood_oxygen~   heart_rate + body_temperature +  rapid_eye_movement + 
                sleeping_hours, data=train_set)
summary(model2)
coef(model2)

library(pscl)
pR2(model2)


confint(model2)


#* Outlier if the entire model

install.packages("car")
library(car)
qqPlot(model2,
       labels = row.names(train_set$name),
       id.method = "Identify" , simulate = TRUE,
       main = "Q- Q plot for unmodified model")


#View the errors on a histogram
#Studentized residuals larger than 2 or < -2 then they require attention
studentized_fit <- rstudent(model2)

hist(studentized_fit, 
     breaks = 10, 
     freq = FALSE,
     xlab = "Studentized residuals",
     main = "Distribution of errors")


rug(jitter(studentized_fit), col = "red")
curve(dnorm(x, 
            mean = mean(studentized_fit),
            sd = sd(studentized_fit)),
      add = TRUE,
      col = "blue", 
      lwed = 2)


# model fit 
lines(density(studentized_fit)$x,
      density(studentized_fit)$y,
      lwd = 2,
      col = "red",
      lty = 2
)

outlierTest(model2)

# Influential observations
# Cook's D value > 4/(n-k-1)
# where n = sample size , 
# k = number of predicator values

cutoff <- 4 / (nrow(train_set) - length(model2$coefficients) - 1)
plot(model2, which = 4 , cook.levels = cutoff)
abline(h = cutoff , lty = 2, col = "red")

influencePlot(model2, 
              main = "Influence plot for model2",
              sub = "Circle size is proportional to Cook's distance")

#* homoscedacity =  the data needs to have a constant
#* varience in the residuals
#* if the p value <0.05 ten the rror varience
#* changes with the level of fitted value


ncvTest(model2)


#*transforming  a model

spreadLevelPlot(model2)



#============DATA VALIDATION==================#

predicted_model <- predict(model2, test_set)

actual_prediction <- data.frame(cbind(actuals = test_set$blood_oxygen, predicted = predicted_model))

head(actual_prediction)

# accuracy percentage caluculation
cor_accuracy <- cor(actual_prediction)
cor_accuracy

#====MODEL FORECASTING==============

forecast <- data.frame(heart_rate = c(49,55,68,75,88)
                       ,body_temperature = c(93.256,93.250,92.452,91.630,90.856)
                       ,rapid_eye_movement = c(60.00,74.88,85.55,100.36,105.36)
                       ,sleeping_hours = c(8,7.5,5.5,2.6,0)
)

forecasted_blood_oxygen <- predict(model2,question)
forecasted_blood_oxygen

forecast$forecasted_blood_oxygen <- forecasted_blood_oxygen

forecast_cor <- cor(forecast)
forecasted_blood_oxygen

cor(sleepRecords$blood_oxygen,sleepRecords$heart_rate)
cor(forecast$forecasted_blood_oxygen,forecast$heart_rate)


cor(sleepRecords$blood_oxygen,sleepRecords$body_temperature)
cor(forecast$forecasted_blood_oxygen,forecast$body_temperature)



cor(sleepRecords$blood_oxygen,sleepRecords$rapid_eye_movement)
cor(forecast$forecasted_blood_oxygen,forecast$rapid_eye_movement)



cor(sleepRecords$blood_oxygen,sleepRecords$sleeping_hours)
cor(forecast$forecasted_blood_oxygen,forecast$sleeping_hours)

#=========================================================================#
