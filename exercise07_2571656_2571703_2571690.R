### Stats with R Exercise sheet 7

##########################
#Week 8: ANOVA
##########################


## This exercise sheet contains the exercises that you will need to complete and 
## submit by 23:55 on Sunday, December 9. Write the code below the questions. 
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

###########################################################################################



#######################
### PART 1: Preparation
#######################

library(boot)
library(ggplot2)
library(reshape)

# This time we will be working with the "amis" data frame (package 'boot') that has 
# 8437 rows and 4 columns.

# In a study into the effect that warning signs have on speeding patterns, Cambridgeshire
# County Council considered 14 pairs of locations. The locations were paired to account 
# for factors such as traffic, volume and type of road. One site in each pair had a sign 
# erected warning of the dangers of speeding and asking drivers to slow down. No action 
# was taken at the second site. Three sets of measurements were taken at each site. 
# Each set of measurements was nominally of the speeds of 100 cars but not all sites 
# have exactly 100 measurements. These speed measurements were taken before the erection 
# of the sign, shortly after the erection of the sign, and again after the sign had been 
# in place for some time.

# 1. For the further reference please use ?amis. It may take some time to understand the dataset. 
?amis

# 2. Load the dataset and briefly inspect it. 
# Feel free to make some plots, calculate some statistics in order to understand the data.
data <- amis
str(amis)
# 3. All our columns have numeric type. Convert the categorial columns to factors.
data$period <- as.factor(data$period)
data$warning <- as.factor(data$warning)
data$pair <- as.factor(data$pair)
str(data)
# 4. Build a boxplot for the distribution of `speed` for each of `period` values 
# (before, immediately after and after some time). Build 2 plots side by side
# depending on the `warning` variable.
# (for all plots here and below please use ggplot)
library(cowplot)
plt1 <- ggplot(data[data$warning=="1",], aes(x=period, y=speed)) + geom_boxplot() + ggtitle("Warning 1")
plt2 <- ggplot(data[data$warning=="2",], aes(x=period, y=speed)) + geom_boxplot() + ggtitle("Warning 2")
plot_grid(plt1, plt2)
# 5. What can you conclude according this plot? What can you say about people behaviour in
# different periods: before, immediately after and after some time?

# When there is a sign, after seeing the sign speed reduces, but again increases when measured after a longer time.
# When there is no sign, not much difference in periods 1 and 2, but slightly increases in period 3.

# 6. What are your ideas about why the data with warning==2 (which correspond to the
# measurements in different times on sites where no sign was erected) was collected?

# This data can show if the posting of warning signs have immediate and long term effect on people for speed control.

#######################
### PART 2: 1-way ANOVA
#######################

#1. First let's create a new data frame which will be used for all PART 2.
# For 1-way ANOVA we will be working with the subset of `amis` where the 
# warning sign was erected, which corresponds to warning==1, therefore first
# subset your data to filter out warning==2 and then apply cast() to average
# speed over each "pair" and "period. Assign this new data frame to the variable casted_data.

data1 <- subset(data, data$warning==1)
data1 <- cast(data1, period + pair~., fun.aggregate = 'mean', value = 'speed', na.rm = TRUE)
colnames(data1)[3] <- "mean_speed"

# 2. Build a boxplot of the average speed depending on period
ggplot(data1, aes(x=period,y=mean_speed)) + geom_boxplot() 

# 3. Is there a difference between the periods?

# There seems to be significant difference between period 1 and 2 and 2 and 3 but little between 1 and 3.

# 4. Now, let's check each ANOVA assumptions and whether they are violated or not and why.

# a) Independence assumption
# (you need to figure out the best way to do it and give a detailed justified answer)

# In the data frame data1, there is exactly one unique entry for period-pair combination. So, the independence assumption is met.

# b) Normality of residuals
# (you need to figure out the best way to do it and give a detailed justified answer)

shapiro.test(data1$mean_speed)
# We can apply Shapiro test. The null hypothesis is that the data is normally distributed. 
# From output, we see that the p-value is 0.3494, which is significantly large, and so we cannot reject the null hypothesis.
# Hence, we can assume that the data satisfies the normality of residuals.

# c) Homogeneity of variance of residuals
# (you need to figure out the best way to do it and give a detailed justified answer)

#install.packages("car")
library(car)
leveneTest(data1$mean_speed ~ data1$period, center="mean")
# Levene Test can be applied to check the homogenity of variance. As P value is very big, we can assume that
# there is homogenity of variance.

# 5.Now we are ready to perform 1-way ANOVA: please use the function aov() on the speed
# depending on the period,report p-value and interpret the result in details

aov_1 <- aov(data1$mean_speed ~ data1$period)
summary(aov_1)

# p-value is 0.382. We see that as there are 3 periods, the degrees of freedom = 2. The F-value = 0.986.
# Hence, the ratio of between group variance to within group variance is not high enough.

# 6. Please do a pairwise t-test with pairwise.t.test()

(pairwise.t.test(data1$mean_speed, data1$period, p.adjust="none"))

# 7. Report pair-wise p-values and interpret the result in details

# Period 1 to 2 is 0.59, 1 to 3 is 0.4, 2 to 3 is 0.17. All of the p values are large.
# So there seems to be no significant relationship between any pair of period values.

# 8. Try to use no adjustment for pairwise testing and then Bonferroni correction.
# Does the result change?
(pairwise.t.test(data1$mean_speed, data1$period, p.adjust="bonferroni"))
# The results change, the p values increase significantly if we use Bonferroni.

#######################
### PART 3: 2-way ANOVA
#######################
# 1. Now we want to analyze the influence of 2 categorial variables (period and warning) on the speed.
# So let's turn back to our initial dataset amis (not its subset with warning==1)
# First, we need to again average the speed over each `pair`, `warning` and `period
# Cast your data again and assign the resuts to casted_data2

casted_data2 <- cast(data, warning + period + pair~., fun.aggregate = 'mean', value = 'speed', na.rm = TRUE)
colnames(casted_data2)[4] <- 'mean_speed'
# 2. Calculate the mean for each of 6 pairs of `period` and `warning`

aggregate(casted_data2$mean_speed, list(casted_data2$period, casted_data2$warning), mean)

# 3. Do you think there is a significant difference in some of the groups?

# There is some difference between (Period=2,Warning=1) vs (Period=2,Warning=2) and (Period=3,Warning=2)
# and (Period=1,Warning=1) and (Period=1,Warning=2).
# So, when warning changes, in these periods we see some difference in means.

# 4. Now apply 2-way ANOVA: please use the function aov() on the speed depending on the period and warning
# report p-value and interpret the result in details

aov_2 <- aov(casted_data2$mean_speed ~ casted_data2$period * casted_data2$warning)
summary(aov_2)
# From the 2 way test, we see that the main test on period vs mean speed is not significant. The test on
# warning and mean speed was significant at p-value of 0.00488 (< 0.01). Here, the F value is 8.396. 
# So, the between group variance of warning was significantly higher than the variance within the groups.
# Again, the interaction effect between warning and period was inconclusive as it was not statistically significant.
# The F value of the interaction effect was the lowest, then the main test on period, and finally the largest was warning.

# 5. What do you conclude about the behaviour of drivers based on the 2-way ANOVA?

# We conclude that drivers did indeed change speeds when they saw warning sign in the streets. However, 
# there was no significant effect of the measurement time period on the speed, and there was no significant
# interaction effect between period and warning.

