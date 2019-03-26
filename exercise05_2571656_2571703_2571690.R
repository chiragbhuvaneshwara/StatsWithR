### Stats with R Exercise sheet 5

##########################
#Week6: t-test and friends
##########################


## This exercise sheet contains the exercises that you will need to complete and 
## submit by 23:55 on Sunday, November 25. Write the code below the questions. 
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

###############
### Cleaning Data
###############

library(lsr)
library(tidyr)
library(effsize)


# set your wd and load the data frame digsym_clean.csv
#setwd("D:\\UdS\\WS1819\\Stats_R\\Assignments")
setwd("C:/Users/Chirag Bhuvaneshwara/Documents/Uni/Sem 3/StatsR/Assignments/A5")
getwd()
data <- read.csv('digsym_clean.csv')
# get rid of the column "X"
data$X <- NULL
# Say you're interested in whether people respond with different accuracy to 
# right vs wrong picture-symbol combinations.
# In other words, you want to compare the average accuracy for the digsym-right 
# and digsym-wrong condition.
# Like the conscientious researcher you are, you want to take a look at the data 
# before you get into the stats.
# Therefore, you will need to create a barplot of the mean accuracy data 
# (split out by condition) using ggplot and the summarySE function (given below).
# Let's do it step by step.

summarySE <- function(data=NULL, measurevar, groupvars=NULL, na.rm=FALSE, conf.interval=.95) {
  # data: an input dataframe
  # measurevar: a column name of <data> (as string), on which we would like to calculate standard 
  #             deviation (SD), standard error (SE) and confidence interval (CI).
  # groupvars: categorical columns of <data> (as vector of strings ) which we would like to use
  #            to make all possible combinations for which we calculate SD, SE, CI based 
  #            on <measurevar>.
  # na.rm: should we remove NA
  # conf.interval: confidence interval
  library(doBy)
  
  length2 <- function (x, na.rm=FALSE) {
    if (na.rm) sum(!is.na(x))
    else       length(x)
  }
  
  # Collapse the data
  formula <- as.formula(paste(measurevar, paste(groupvars, collapse=" + "), sep=" ~ "))
  datac <- summaryBy(formula, data=data, FUN=c(length2,mean,sd), na.rm=na.rm)
  
  # Rename columns
  names(datac)[ names(datac) == paste(measurevar, ".mean",    sep="") ] <- measurevar
  names(datac)[ names(datac) == paste(measurevar, ".sd",      sep="") ] <- "sd"
  names(datac)[ names(datac) == paste(measurevar, ".length2", sep="") ] <- "N"
  
  # Calculate standard error of the mean
  datac$se <- datac$sd / sqrt(datac$N)  
  
  # Confidence interval multiplier for standard error
  # Calculate t-statistic for confidence interval: 
  # e.g., if conf.interval is .95, use .975 (above/below), and use df=N-1
  ciMult <- qt(conf.interval/2 + .5, datac$N-1)
  datac$ci <- datac$se * ciMult
  
  return(datac)
}

# apply the function summarySE on the accuracy data grouping by right/wrong condition
# (use the provided documentation inside the function for the arguments description)
result <- summarySE(data, measurevar = "accuracy", groupvars = c("condition"))

# Create the barplot (use ggplot2 for this and all tasks below) with error bars 
# (which the function summarySE readily provided).
# Gauging from the plot, does it look like there's a huge difference in accuracy 
# for responses to the right and wrong condition?
library("ggplot2")
ggplot(result, aes(x=condition,y=accuracy,fill=condition)) + geom_bar(stat = 'identity') + geom_errorbar(aes(x=condition,ymin=accuracy-se,ymax=accuracy+se), position='dodge', width=0.25)
# No it does not look like there is a huge difference. 

# Let's go back to our data frame "data", which is still loaded in your console
# Now that you've taken a look at the data, you want to get into the stats.
# You want to compute a t-test for the average accuracy data in the right and 
# wrong condition.
# Why can't you compute a t-test on the data as they are now? 
# Hint: which assumption is violated?

# t-test assumes that the data being fed into it is independent but since we have multiple entries in the same 
# column for a single subject and therefore the independence assumption is violated.

# Student's t-test requires the population standard deviation to be same in both groups, but in our case it is not the same as 
# indicated by the sd variable in result data frame. Therefore, we need to use Welch test(which is the default for t.test()) which does not require the 
# equal standard deviation assumption. 


# we need to reshape( - cast) the data to only one observation (average accuracy)
# per subject and right/wrong condition 
# Collapse the data, using 
# cast(data, var1 + var2 + var3 ... ~, function, value = var4, na.rm = T)

library(reshape)
dat2 <- cast(data, Subject + condition~., fun.aggregate = 'mean', value = 'accuracy', na.rm = TRUE)
colnames(dat2)[3] <- "mean_accuracy"

# Create a histogram of the accuracy data depending on the right and wrong 
# condition and display them side by side
#par(mfrow=c(1,2))
#hist(dat2$mean_accuracy[dat2$condition=="right"])
#install.packages('cowplot')
library(cowplot)
theme_set(theme_grey())
plot1 <- ggplot(dat2[dat2$condition=='right',], aes(x=mean_accuracy)) + geom_histogram(fill='blue') + ggtitle("Right")
plot2 <- ggplot(dat2[dat2$condition=='wrong',], aes(x=mean_accuracy)) + geom_histogram(fill='red') + ggtitle("Wrong")
plot_grid(plot1, plot2, labels = "AUTO", align = 'h')
#hist(dat2$mean_accuracy[dat2$condition=="wrong"])

# Display the same data in a density plot 
par(mfrow=c(1,2))
#d1 <- density(dat2$mean_accuracy[dat2$condition=="right"])
#plot(d1)
#d2 <- density(dat2$mean_accuracy[dat2$condition=="wrong"])
plot1 <- ggplot(dat2[dat2$condition=='right',], aes(x=mean_accuracy)) + geom_density(col='blue')  + ggtitle("Right")
plot2 <- ggplot(dat2[dat2$condition=='wrong',], aes(x=mean_accuracy)) + geom_density(col='red')  + ggtitle("Wrong")
plot_grid(plot1, plot2, labels = 'AUTO')
#plot(d2)
# Based on the histograms and the density plots - are these data normally 
# distibuted?

# They are negatively skewed normal distributions.

# Create a boxplot of the accuracy data
#par(mfrow=c(1,1))
#boxplot(dat2$mean_accuracy)
ggplot(dat2, aes(condition, mean_accuracy, fill = condition)) + stat_boxplot(geom ='errorbar') + geom_boxplot()

# Compute the t-test to compare the mean accuracy between wrong and right picture
# combinations.
# Do you need a paired t-test or independent sample t-test? why?

# We need a paired test as the same subject is shown the right and wrong sets. Hence, they are coupled.

# What does the output tell you? What conclusions do you draw?

t.test(dat2$mean_accuracy[dat2$condition=="right"], dat2$mean_accuracy[dat2$condition=="wrong"], paired = TRUE)
# As the p-value is < 0.05, we reject the null hypothesis. Hence, we conclude there is a significant difference
# in the means of the two conditions.

# Compute the effect size using CohensD 

library(lsr)
cohensD(dat2$mean_accuracy[dat2$condition=="right"], dat2$mean_accuracy[dat2$condition=="wrong"], method = "paired")

# How big it is? How do you interpret this result?
# The effect is 0.619 sd. So it is a moderately large effect.

# In addition to the long-format data we've just been working on, you may also 
# encounter data sets in a wide format 
# (this is the format we have been using in class examples.)
# Let's do a transformation of our data set to see how it would like in a wide 
# format.
# Use "spread" in tidyr.

library(tidyr)
dat2_spread <- spread(dat2,key = condition, value = mean_accuracy)
head(dat2_spread)

# Compute the t test again on the wide format data - note that for wide-format 
# data you need to use a different annotation for the t-test.
t.test(dat2_spread$right, dat2_spread$wrong, paired=TRUE)

# Compare the t-test results from the wide-format and the long-format data.

# The p-value is exactly the same as the long format.

# Compute CohensD on the wide format data.

cohensD(dat2_spread$right, dat2_spread$wrong, method = 'paired')

# Let's try the t-test again, but for a different question:
# Suppose you are interested in whether reaction times in the digit symbol 
# task differ depending on gender.
# In other words, you want to test whether or not men perform significantly 
# faster on average than women, or vice versa.
# Collapse the data again, using 
# cast(data, var1 + var2 + var3 ... ~ ., function, value = var4, na.rm = T)

dat3 <- cast(data, Subject + Gender~., fun.aggregate = 'mean', value = 'accuracy', na.rm = TRUE)
colnames(dat3)[3] <- "mean_accuracy"

# Take a look at the resulting data frame using head()
head(dat3,10)


# Compute the t-test to compare the accuracy means of female and male 
# participants.
# Which t-test do you need and why? How do you interpret the result?

# We need independent t-test.
t.test(dat3$mean_accuracy[dat3$Gender=="male"], dat3$mean_accuracy[dat3$Gender=="female"])
# p-value is > 0.05, we cannot reject the null hypothesis. So there is no significant difference between
# the mean accuracy of the two gender groups.

