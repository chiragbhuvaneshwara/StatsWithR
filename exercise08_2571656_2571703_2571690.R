### Stats with R Exercise sheet 8

##########################
#Week9: Checking Assumptions underlying ANOVA and linear regression
##########################


## This exercise sheet contains the exercises that you will need to complete and 
## submit by 23:55 on Sunday, December 16. Write the code below the questions. 
## If you need to provide a written answer, comment this out using a hashtag (#). 
## Submit your homework via moodle.
## You are allowed to work together in group up to three students, but everybody 
## needs to submit the group version of the homework via moodle.
## You need to provide a serious attempt to each exercise in order to have
## the assignment graded as complete.

## Please write below your (and your teammates) name, matriculation number. 
## Name: Ayan Majumdar, Chirag Bhuvaneshwara, Egla Hajdini
## Matriculation number: 2571656, 2571703, 2571690

## Change the name of the file by adding your matriculation numbers
## (exercise0N_firstID_secondID_thirdID.R)

###############################################################################
###############################################################################

########
### Exercise 1
########

########
### Please, use ggplot to make plots in all exercises below!
########


# a) Read in the data kidiq.txt (available in the Moodle) and take a look
#    at the data summary. It contains information about the mum's iq and 
#    their child's iq. 
#    mom_hs indicates whether the mother has a high school degree
#    1= high school education, 0= no high school degree.

#setwd('D:/UdS/WS1819/Stats_R/Assignments')
#setwd("C:/Users/Chirag Bhuvaneshwara/Documents/Uni/Sem 3/StatsR/Assignments/A8")
data <- read.delim('kidiq.txt',sep=" ")
summary(data)

# b) Plot kid_score against mom_iq in a scatter plot, and add a regression line 
#    (kid_score should be the response variable and mom_iq the predictor 
#    variable) to the plot. 
#    Name the plot and the axis in sensible ways.

library(ggplot2)
ggplot(data, aes(x=mom_iq,y=kid_score)) + geom_point() + geom_smooth(method="lm",se = FALSE) + labs(title="Kid Score vs Mom IQ", x="Mom IQ", y="Kid Score") +  theme(plot.title = element_text(hjust = 0.5))

# c) Calculate a simple regression model for kid_score with mom_hs as a 
#    predictor and interpret the results.

#data$mom_hs <- as.factor(data$mom_hs)
lm_model1 <- lm(kid_score ~ mom_hs, data = data)
summary(lm_model1)

# Kid Score seems to be positively affected by mom_hs. If mom of kid has a high school degree,
# then the IQ score can increase by 11.771 points in the IQ-scale.
# The residual error is 19.85. The R-squared is just 0.056, which means that the fitted model does not explain enough of the variance of kid_score.
# This can be as mom_hs is just a binary attribute, and for mom_hs=0 and 1, we will have multiple different kid score values at the same mom_hs value.

# d) Next, fit a regression model with two predictors: mom_hs and mom_iq. 
#    Interpret the model and compare to the previous model.

lm_model2 <- lm(kid_score ~ mom_hs + mom_iq, data = data)
summary(lm_model2)

# Both mom_hs and mom_iq seem to have positive relation to kid score, but mom_hs has much higher weight
# than mom_iq (5.95 vs 0.5639).
# The residual error for this model is less than the last one, here it is 18.14 (vs 19.85).
# The multiple R-squared value is 0.2141, which means this model is a much better fit as it can explain more than
# 21% of the variance in kid_score. The last model had just 0.056.

# e) Now plot a model where both predictors are shown. Do this by plotting 
#    data points for mothers with high school degree==1 in one color and those 
#    without one in another color. Then also fit two separate regression lines such 
#    that these lines reflect the model results.
#	   HINT: One solution in ggplot is to calculate fitted values of the regression model 
#    and then plot them along with the original data points:
#    pred = data.frame(mom_iq=kidiq$mom_iq, mom_hs=kidiq$mom_hs, kid_score_pred=fitted(your_model))

#data$mom_hs <- as.factor(data$mom_hs)
ggplot(data, aes(x=mom_iq,y=kid_score, col=as.factor(mom_hs))) + geom_point() + geom_smooth(method = "lm",formula = y~x,se=FALSE) + scale_color_brewer(palette = "Set1") + labs(title = "Model with two predictors") + theme(plot.title = element_text(hjust = 0.5))

pred = data.frame(mom_iq=data$mom_iq, mom_hs=data$mom_hs, kid_score_pred=fitted(lm_model2))
ggplot() + geom_point(data=data, aes(x=mom_iq,y=kid_score, col=as.factor(mom_hs))) + geom_line(data = pred, aes(x=mom_iq,y=kid_score_pred, col=as.factor(mom_hs)), size=1) + scale_color_brewer(palette = "Set1") + labs(title = "Model with two predictors") + theme(plot.title = element_text(hjust = 0.5))

# f) Next, we will proceed to a model including an interaction between mom_hs
#    and mom_iq. Interpret your results.

#data$mom_hs <- as.numeric(data$mom_hs)
lm_model3 <- lm(kid_score ~ mom_hs * mom_iq, data = data)
summary(lm_model3)

# For this model, mom_hs and mom_iq have positive effect on kid score, with mom_hs having the highest
# weight of 51.2682, mom_iq having 0.9689. The interaction of mom_hs and mom_iq together on the kid score
# however is negative, as the weight is -0.4843. The residual error is even less now, 17.97, and the 
# multiple R-squared value is 0.2301, which is higher than the previous models. This means that this
# model explains more variance in the kid score.

# g) Next, let's plot the results of this model.

pred2 = data.frame(mom_iq=data$mom_iq, mom_hs=data$mom_hs, kid_score_pred=fitted(lm_model3))
ggplot() + geom_point(data=data, aes(x=mom_iq,y=kid_score, col=as.factor(mom_hs))) + geom_line(data = pred2, aes(x=mom_iq,y=kid_score_pred, col=as.factor(mom_hs)), size=1) + scale_color_brewer(palette = "Set1") + labs(title = "Model with two predictors and interaction") + theme(plot.title = element_text(hjust = 0.5))



# h) Next, let's explore the "predict.lm" function. Please first generate
#    a new dataframe with one datapoint (a mother with high school degree
#    and iq of 100). Then, use the predict function to predict the child's iq. 
#    Please specify the predict function to also give you the 0.95 confidence 
#    interval.

test_data = data.frame(mom_iq=100, mom_hs=1)
predict.lm(lm_model3, test_data,interval = "confidence")
# fit      lwr      upr
# 1 88.24766 86.31365 90.18167

# i) Meaning of confidence intervals for regression line.
#    Let's go back to the exercise b) and plot again the data points with the 
#    regression line. By default, there should also be displayed the borders of 
#    the confidence interval. What is the meaning of the confidence interval?


ggplot(data, aes(x=mom_iq,y=kid_score)) + geom_point() + geom_smooth(method="lm",se = TRUE) + labs(title="Kid Score vs Mom IQ", x="Mom IQ", y="Kid Score") +  theme(plot.title = element_text(hjust = 0.5))
# The shaded region shows that the for regression model that has been plotted, the shaded region indicates
# that if we resample and fit multiple times, then the regression line will be in that shaded region 95%
# of the times. It gives the lower and upper bounds of the confidence.

# j) Finally, do model checking on your model with the interaction, i.e. inspect 
#    the standard model plots provided by R, and interpret what you see.

#par(mfcol=c(2,3))
#plot(lm_model3,which=seq(1:6))

#install.packages("gridExtra")
#install.packages("ggfortify")
library(ggfortify)
autoplot(lm_model3, which=seq(1:6))

# The residuals vs fitted shows that the residual value falls to negative when fitted value is high, 
# otherwise it is more or less linear. The scale location plot shows that the residuals 
# are more or less randomly spread throughout the fitted values, with the relationship 
# showing slightly negative relationship in the distribution with higher fits.
# The Residuals vs Leverage shows that we do not have any significant outliers that can cause change to the 
# regression output. The QQ plot shows that the residuals are indeed normally distributed.
