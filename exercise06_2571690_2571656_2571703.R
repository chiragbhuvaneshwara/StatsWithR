### Stats with R Exercise sheet 6

##########################
#Week 7: Correlation and Regression
##########################


## This exercise sheet contains the exercises that you will need to complete and 
## submit by 23:55 on Sunday, December 2. Write the code below the questions. 
## If you need to provide a written answer, comment this out using a hashtag (#). 
## Submit your homework via moodle.
## You are allowed to work together in group up to three students, but everybody 
## needs to submit the group version of the homework via moodle.


## Please write below your (and your teammates) name, matriculation number. 
## Name: Ayan Majumdar, Chirag Bhuvaneshwara, Egla Hajdini
## Matriculation number: 2571656, 2571703, 2571690

## Change the name of the file by adding your matriculation numbers
## (exercise0N_firstID_secondID_thirdID.R)

###########################################################################################
###########################################################################################


library(reshape)
library(languageR)
library(ggplot2)
library(gridExtra)

#######################
### PART 1: Correlation
#######################

########
### Please, use ggplot to make plots in all exercises below!
########

# Get some data - access the ratings data set in languageR and name it "data".
# Subjective frequency ratings and their length averaged over subjects, for 81 concrete English nouns.
data <- ratings


# Take a look at the data frame.
head(data)

# Let's say you're interested in whether there is a linear relationship between the word frequency of 
# the 81 nouns and their length.
# Take look at the relationship between the frequency and word length data by means a of a scatterplot 
# (from ggplot library).
ggplot(data, aes(x=Frequency, y=Length)) +
  geom_point()  

# Judging from the graphs, do you think that word frequency and word length are in any way correlated 
# with one another?

#From the scatter plot it doesn't look that the word frequency and word length are correlated to each other. 

# Compute the Pearson correlation coefficient between the two variables by means of cor().
# Tell R to only include complete pairs of observations.
# As a reminder: Pearson coefficient denotes the covariance of the two variable divided by the product 
# of their variance. It is scaled between 1 (for a perfect positive correlation) to -1 (for a perfect 
# negative correlation).
cor(data$Frequency,data$Length,method="pearson")

# Does the correlation coefficient suggest a small, medium or large effect?
# What about the direction of the effect?

# The correlation coefficient suggests a medium effect in the negative direction, since corr value is -0.4281462.

# Note that we have a large number of tied ranks in word length data (since there are multiple words 
# with the length of, e.g. 5).
# Thus, we might draw more accurate conclusions by setting the method to Kendall's tau instead of 
# Pearson (which is the default).
# How do you interpret the difference between these 2 correlation coefficients?
cor(data$Frequency,data$Length,method="kendall")


# What about significance? Use the more user-friendly cor.test!
cor.test(data$Length,data$Frequency, method = "kendall")


# Take a look at the output and describe what's in there.
# What do you conclude?
# z = -3.9186, p-value = 8.907e-05
# Since p-value is less than 0.05, the null hypothesis that variables are correlated holds.

# Finally, we can also calculate Spearman's rank correlation for the same data.
cor(data$Length,data$Frequency, method = "spearman")
# -0.4311981


###################################################################################################


#######################
### PART 2: Regression
#######################

# Fit a linear regression model to the data frame for the variables frequency (outcome variable) 
# and Length (predictor variable).
# General form: "modelname <- lm(outcome ~ predictor, data = dataFrame, na.action = an action)"
model <- lm (Frequency ~ Length,data )


# How do you interpret the output? Is the relationship between the two variables positive or negative?
# Plot the data points and the regression line.

summary(model)
#   Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
#   (Intercept)  6.50150    0.43270  15.025  < 2e-16 ***
#   Length      -0.29429    0.06989  -4.211 6.69e-05 ***

# The relationship between the two variables is negative 


ggplot(data, aes(x = Frequency, y = Length)) + 
  geom_point() +
  stat_smooth(method = "lm", col = "red")

# Run the plotting command again and have R display the actual words that belong to each point. 
# (Don't worry about readability of overlapping words.)

ggplot(data, aes(x = Frequency, y = Length,label=Word)) + 
  geom_text() +
  stat_smooth(method = "lm", col = "red")


###################################################################################################


# Try this again for another example:
# Let's go back to our digsym data set.
# Set your wd and load the data frame digsym_clean.csv
# You can download this data frame from material of week 6: T-tests

getwd()
setwd("/Users/eglahajdini/Desktop/Statistics with R")
digsym_data<-read.csv("digsym_clean.csv")

# Suppose you want to predict reaction times in the digit symbol task by people's age.
# Fit a linear regression model to the data frame for the variables correct_RT_2.5sd (outcome variable) 
# and Age (predictor variable).
# General form: "modelname <- lm(outcome ~ predictor, data = dataFrame, na.action = an action)"
digsym_model<- lm(correct_RT_2.5sd ~ Age, data = digsym_data)

# Let's cast the data to compute an RT mean (use correct_RT_2.5sd) for each subject, so that we have only one Age 
# observation by Subject.
# In case you're wondering why we still have to do this - like the t-test, linear regression assumes 
# independence of observations.
# In other words, one row should correspond to one subject or item only.
casted_data<- cast(digsym_data, Subject + Age ~., fun.aggregate = 'mean', value = 'correct_RT_2.5sd', na.rm = TRUE)

# Fit the regression model.
digsym_model2<- lm(casted_data$`(all)` ~ Age, data = casted_data)

# Let's go over the output - what's in there?
# How do you interpret the output?
summary(digsym_model2)
# In this case we have a positive relationship between the two variables 

# Again plot the data points and the regression line. 
ggplot(casted_data, aes(x = casted_data$`(all)`, y = Age)) + 
  geom_point() +
  stat_smooth(method = "lm", col = "red")


# Plot a histogram and qq-plot of the residuals. Does their distribution look like the normal distribution?
qplot(residuals(digsym_model2),geom="histogram")
# qqplot(residuals(digsym_model2), main="QQ Plot")

# Plot Cooks distance which estimates the residuals (i.e. distance between actual values and the 
# regression line) for individual data points in the model.
plot(cooks.distance(digsym_model2))


# It actually looks like we have 1 influential observation in there that has potential to distort 
# (and pull up) our regression line.
# The last observation (row 37) in cast yielded a Cooks D is very high (greater than 0.6).
# In other words, the of the entire regression function would change by more than 0.6 when this 
# particular case would be deleted.

# What is the problem with observation 37?
# it is an outlier

# Run the plotting command again and have R display the subjects that belong to each point.
ggplot(casted_data, aes(x = 1:37, y = casted_data$`(all)` ,label=Subject)) + 
  geom_text() 


# Make a subset of "cast" by excluding this subject and name it cast2.
cast2<-casted_data[-c(37), ] 


# Fit the model again, using cast2, and take a good look at the output.
digsym_model3<- lm(cast2$`(all)` ~ Age, data = cast2)
summary(digsym_model3)

# What's different about the output?
# Coefficient for Age will be 11.98 

# How does that change your interpretation of whether age is predictive of RTs?
# Age will not be a statistically significant predictor anymore, because its p-value is now 0.27506 > 0.05

# Plot the regression line again - notice the difference in slope in comparison to our earlier model fit?
ggplot(cast2, aes(x = cast2$`(all)`, y = Age)) + 
  geom_point() +
  stat_smooth(method = "lm", col = "red")


# Display the two plots side by side to better see what's going on.
par(mfrow=c(1,1))

plot1 <- ggplot(casted_data, aes(x = casted_data$`(all)`, y = Age)) + 
  geom_point() +
  stat_smooth(method = "lm", col = "red")

plot2<- ggplot(cast2, aes(x = cast2$`(all)`, y = Age)) + 
  geom_point() +
  stat_smooth(method = "lm", col = "red")

grid.arrange(plot1, plot2, ncol=2)

# Compute the proportion of variance in RT that can be accounted for by Age.
# In other words: Compute R Squared.
# Refer to Navarro (Chapter on regression) if you have trouble doing this.
summary(digsym_model3)$r.squared 

# How do you interpret this number?
# R Squared will be 3.493231%, it indicates that the model doesn't fit our data. 
