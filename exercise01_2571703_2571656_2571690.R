### Stats with R Exercise sheet 1 

## This exercise sheet contains the exercises that you will need to complete and 
## submit by 23:55 on Sunday, October 28. Write the code below the questions. 
## If you need to provide a written answer, comment this out using a hashtag (#). 
## Submit your homework via moodle.
## You are allowed to work together in group up to three students, but everybody 
## needs to submit the group version of the homework via moodle.
## You need to provide a serious attempt to each exercise in order to have
## the assignment graded as complete. 

## Please write below your (and your teammates) name, matriculation number. 
## Name: Chirag Bhuvaneshwara
## Matriculation number: 2571703

## Team: Ayan Majumdar 2571656 ; Egla Hajdini 2571690

## Change the name of the file by adding your matriculation numbers
## (exercise01_firstID_secondID_thirdID.R)



## Many of the things on this exercise sheet have not been discussed in class. 
## The answers will therefore not be on  the slides. You are expected to find 
## the answers using the help function in R, in the textbooks and online. If you 
## get stuck on these exercises, remember: Google is your friend.
## If you have any questions, you can ask these during the tutorial, or use the 
## moodle discussion board for the course.

###############
### Exercise 1: Getting started
###############
## a) Look at your current working directory.
getwd()

## b) Get help with this function.
?getwd

## c) Change your working directory to another directory.
# setwd("C:/Users/Chirag Bhuvaneshwara/Documents/Uni/Sem 3/StatsR/Assignments/A1")



###############
### Exercise 2: Participants' age & boxplots
###############
## In this exercise, we will deal with data from a package.

## a) Install the package "languageR" and load it.
#install.packages("languageR")

## b) Specifically, we will deal with the dataset 'dutchSpeakersDistMeta'. 
##    This dataset should be available to you once you've loaded languageR.
##    The dataset contains information on the speakers included in the Spoken 
##    Dutch Corpus. Inspect 'dutchSpeakersDistMeta'. Look at the head, tail, 
##    and summary. What do head and tail show you?
library(languageR)
head(dutchSpeakersDistMeta)
tail(dutchSpeakersDistMeta)
summary(dutchSpeakersDistMeta)
## Head shows the first 6 observations in the dataset and tail shows the last 6 entries in the dataset.

## c) Each line in this file provides information on a single speaker. How many 
##    speakers are included in this dataset? In other words, use a function to 
##    retrieve the number of rows for this dataset.
nrow(dutchSpeakersDistMeta)


## d) Let's say we're interested in the age of the speakers included in the 
##    corpus, to see whether males and females are distributed equally. 
##    Create a boxplot for Sex and AgeYear.
par(mfrow = c(1,2))

malesVec <- (dutchSpeakersDistMeta["Sex"] == "male")
male_df <- dutchSpeakersDistMeta[malesVec,]
boxplot(male_df$AgeYear, main= "males")

femalesVec <- (dutchSpeakersDistMeta["Sex"] == "female")
female_df <- dutchSpeakersDistMeta[femalesVec,]
boxplot(female_df$AgeYear, main="females")

## e) Does it seem as if either of the two groups has more variability in age?
#The plots signify that female dutch speakers have more variability in age than the male group.

## f) Do you see any outliers in either of the two groups?
# There are 2 outliers in the boxplot of the male group.

## g) Now calculate the mean and standard deviation of the AgeYear per group. 
##    Do this by creating a subset for each group.
##    Do the groups seem to differ much in age?
print("Male group mean:")
mean(male_df$AgeYear, na.rm = T)
print("Male group sd:")
sd(male_df$AgeYear,na.rm = T)

print("Female group mean:")
mean(female_df$AgeYear, na.rm = T)
print("Female group sd:")
sd(female_df$AgeYear, na.rm = T)

## h) What do the whiskers of a boxplot mean?
## The whiskers of the boxplot mark the most extreme datapoints inside 1.5 times the interquartile range.

###############
### Exercise 3: Children's stories & dataframes
###############
# A researcher is interested in the way children tell stories. More specifically,
# she wants to know how often children use 'and then'. She asks 25 children to
# tell her a story, and counts the number of times they use 'and then'.
# The data follow:

# 18 15 22 19 18 17 18 20 17 12 16 16 17 21 25 18 20 21 20 20 15 18 17 19 20 


## a) What measurement scale is this data? Is it discrete or continuous? Explain
##    in one sentence why? (remember, comment out written answers)
## Measurement scale is discrete because the number of times anything is uttered is always a whole number.

## b) In the next questions (c-e), you will create a dataframe of this data, 
##    which will also include participant IDs.
##    Why is a dataframe better suited to store this data than a matrix?
## Dataframe is better because we can store variables of multiple types in it where as in matrix we can store multiple variables of only one datatype.


## c) First create a vector with participant IDs. Your vector should be named 
##    'pps', and your participants should be labeled from 1 to 25
pps <- 1:25
pps

## d) Next, create a vector containing all the observations. Name this vector 'obs'.
obs <- c(18, 15, 22, 19, 18, 17, 18, 20, 17, 12, 16, 16, 17, 21, 25, 18, 20, 21, 20, 20, 15, 18, 17, 19, 20) 

## e) Create a dataframe for this data. Assign this to 'stories'. 
stories <- data.frame(pps,obs)


## f) Take a look at the summary of your dataframe, and at the classes of your 
##    columns. What class is the variable 'pps'?
summary(stories)
## pps variable is numeric

## g) Change the class of 'pps' to factor. Why is factor a better class for this
##    variable?
stories <- data.frame(factor(pps),obs)
str(stories)


## h) Plot a histogram (using hist()) for these data. Set the number of breaks 
##    to 8.
hist(stories$obs, breaks = 8)


## i) Create a kernel density plot using density().
plot(density(stories$obs))

## j) What is the difference between a histogram and a kernel density plot?
#A histogram gives a discrete plot where the values of the data are discretized into intervals 
#and for each interval, the frequency of datapoints appearing in it are plotted.But these plots 
#do not have a strict specification as to where each interval should begin from or the number of 
#intervals the plot should have. Therefore, the plot can vary and look very different depending on this.
# But in a kernel density plot, we obtain a continuous plot of the probability density of the data which 
# gives a better estimate of the overall distribution and does not vary in its shape like the histogram
# as there is no hassle with deciding the right intervals.

## k) Overlay the histogram with the kernel density plot 
##    (hint: the area under the curve should be equal for overlaying the graphs 
##    correctly.)
myhist <- hist(stories$obs, breaks = 8)
multipliers <- myhist$counts / myhist$density
d <- density(stories$obs)
d$y <- d$y * multipliers[1]
par(mfrow = c(1,1))
plot(myhist)
lines(d)

###############
### Exercise 4: Normal distributions
###############
## In this exercise, we will plot normal distributions.

## a) First, use seq() (?seq) to select the x-values to plot the range for
##    (will become the x-axis in the plot).
##    Get R to generate the range from -5 to 5, by 0.1. Assign this to the 
##    variable x.
x <- seq(-5, 5, by=0.1)

## b) Now we need to obtain the y-values of the plot (the density). We do this 
##    using the density function for the normal distribution. 
##    Use "help(dnorm)" to find out about the standard functions for the normal 
##    distribution.
y <- dnorm(x)

## c) Now use plot() to plot the normal distribution for z values of "x". 
plot(x, y, main = "Normal Distribution", xlab = "x", ylab = "p(x)")

## d) The plot now has a relatively short y-range, and it contains circles 
##    instead of a line. 
##    Using plot(), specify the y axis to range from 0 to 0.8, and plot a line 
##    instead of the circles.
plot(x, y, main = "Normal Distribution", xlab = "x", ylab = "p(x)", type = "l", ylim = c(0, 0.8))


## e) We want to have a vertical line to represent the mean of our distribution.
##    'abline()' can do this for us. Look up help for abline(). 
##    Use abline() to create the vertical line. Specify the median of x using
##    the argument 'v'.
##    In order to get a dashed line, set the argument 'lty' to 2.
abline(v=median(x), lty = 2)

## f) Take a look at the beaver1 dataset. (You can see it by typing "beaver1".) 
##    Then select only the temperature part and store it in a variable "b1temp".
b1temp <- beaver1$temp

## g) Calculate the mean and standard deviation of this dataset and plot a normal
##    distribution with these parameters.
b1tempMean <- mean(b1temp)
b1tempSD <- sd(b1temp)
plot(b1temp, dnorm(b1temp, mean = b1tempMean, sd = b1tempSD))

## h) We observe two tempareatures (36.91 and 38.13). What's the likelihood that
##    these temepratures (or more extreme ones) respectively come 
## from the normal distribution from g)?
1 - pnorm(36.91, mean = b1tempMean, sd = b1tempSD)
1 - pnorm(38.13, mean = b1tempMean, sd = b1tempSD)

## Therefore, 38.13 has a higher likelihood of 40.24% to come from the normal distribution in g.

## i) Use the random sampling function in R to generate 20 random samples from
##    the normal distribution from g), and draw a histrogram based on this sample.
##    Repeat 5 times. What do you observe?

hist(rnorm(20, b1tempMean, b1tempSD))
hist(rnorm(20, b1tempMean, b1tempSD))
hist(rnorm(20, b1tempMean, b1tempSD))
hist(rnorm(20, b1tempMean, b1tempSD))
hist(rnorm(20, b1tempMean, b1tempSD))

## Each time the histogram is different due to random sampling but the samples are clustered around the mean of our distribution.

