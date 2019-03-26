### Stats with R Exercise Sheet 3

##########################
#Week4: Hypothesis Testing
##########################

## This exercise sheet contains the exercises that you will need to complete and 
## submit by 23:55 on Sunday, November 11. Write the code below the questions. 
## If you need to provide a written answer, comment this out using a hashtag (#). 
## Submit your homework via moodle.
## You are allowed to work together in group up to three students, but everybody 
## needs to submit the group version of the homework via moodle.
## You need to provide a serious attempt to each exercise in order to have
## the assignment graded as complete. 


## Please write below your (and your teammates) name, matriculation number. 
## Name:  Chirag Bhuvaneshwara, Ayan Majumdar, Egla Hajdini
## Matriculation number:  2571703, 2571656, 2571690

## Change the name of the file by adding your matriculation numbers
## (exercise0N_firstID_secondID_thirdID.R)


###############
### Exercise 1: Deriving sampling distributions
###############
## In this exercise, we're going to derive 5 sampling distributions with 
## different sizes.

## a) Load the package languageR. We're going to work with the dataset 'dative'. 
## Look at the help and summary for this dataset.
library(languageR)
?dative
summary(dative)
## The term dative alternation is used to refer to the alternation between 
## a prepositional indirect-object construction
## (The girl gave milk (NP) to the cat (PP)) and a double-object construction 
## (The girl gave the cat (NP) milk (NP)).
## The variable 'LenghtOfTheme' codes the number of words comprising the theme.

## b) Create a contingency table of 'LenghtOfTheme' using table(). 
##    What does this table show you?
table(dative$LengthOfTheme)
# It shows the count of each length of theme.

## c) Look at the distribution of 'LenghtOfTheme' by plotting a histogram and a boxplot. 
##    Do there appear to be outliers? Is the data skewed?
hist(dative$LengthOfTheme)
boxplot(dative$LengthOfTheme)
# There are outliers as seen from the boxplot. Also the distribution is skewed.

## d) Now we're going to derive sampling distributions of means for different 
##    sample sizes. 
##    What's the difference between a distribution and a sampling distribution?

# A distribution tells us how the sample scores can be described. A sampling distribution woud tell us how a particular measure, e.g. mean of the sample is distributed.

## e) We are going to need a random sample of the variable 'LengthOfTime'. 
##    First create a random sample of 5 numbers using sample(). 
##    Assign the outcome to 'randomsampleoflengths'

randomsampleoflengths <- sample(dative$LengthOfTheme, 5)

## f) Do this again, but assign the outcome to 'randomsampleoflengths2'. 

randomsampleoflengths2 <- sample(dative$LengthOfTheme, 5)

## g) Now calculate the mean of both vectors, and combine these means 
##    into another vector called 'means5'.

means5 <- c(mean(randomsampleoflengths), mean(randomsampleoflengths2))

## h) In order to draw a distribution of such a sample, we want means of 
##    1000 samples. However, we don't want to repeat question e and f 
##    1000 times. We can do this in an easier way: 
##    by using a for-loop. See dataCamp or the course books for 
##    how to write loops in R.
means5 <- c()
for (i in 1:1000) {
  randomsampleoflengths3 <- sample(dative$LengthOfTheme, 5)
  means5 <- c(means5, mean(randomsampleoflengths3))
}

## i) Repeat the for-loop in question h, but use a sample size of 50. 
##    Assign this to 'means50' instead of 'means5'.
means50 <- c()
for (i in 1:1000) {
  randomsampleoflengths3 <- sample(dative$LengthOfTheme, 50)
  means50 <- c(means50, mean(randomsampleoflengths3))
}
## j) Explain in your own words what 'means5' and 'means50' now contain. 
##    How do they differ?

# means5 contains the mean of 1000 random samplings, where each sampling was on 5 points from dative LengthOfTheme.
# means50 contains the mean of 1000 random samplings, where each sampling was on 50 points from dative LengthOfTheme.
# Each entry of means50 is the mean of 50 random samples, whereas that of means5 is of just 5.

## k) Look at the histograms for means5 and means50. Set the number of breaks to 15.
##    Does means5 has a positive or negative skew?

hist(means5, breaks = 15)
hist(means50, breaks = 15)
# means5 has a positive skew.

## l) What causes this skew? In other words, why does means5 have bigger 
##    maximum numbers than means50?

# This is because in case of means5, we have just sample size of 5. So, in some sampling, 
# all 5 values can be large, which result in larger means. But in means50, we have 50 random samples.
# Hence, they would be more uniform than 5 samples.

###############
### Exercise 2: Confidence interval
###############

## A confidence interval is a range of values that is likely to contain an 
## unknown population parameter.
## The population parameter is what we're trying to find out. 
## Navarro discusses this in more depth in chapter 10.

## a) What does a confidence interval mean from the perspective of experiment replication?
# It means that if we replicate the experiment, the probability of the outcome being in the confidence interval is very high. Hence, it can happen by the sampling randomly.

## b) Let's calculate the confidence interval for our means from the previous 
##    question.
##    First, install and load the packages 'lsr' and 'sciplot'
install.packages('lsr')
install.packages('sciplot')
library(lsr)
library(sciplot)
## c) Look at the description of the function ciMean to see which arguments it takes.
?ciMean

## d) Use ciMean to calculate the confidence interval of the dataset dative from
##    the previous exercise.
##    Also calculate the means for the variable LengthOfTheme.
ciMean(dative)
mean(dative$LengthOfTheme)
## e) Does the mean of the sample fall within the obtained interval? 
##    What does this mean?
# Yes it does fall within the interval, meaning that the probability of the value being sampled from a sampling distribution is 95%.

## f) As the description of dative mentions, the dataset describes the 
##    realization of the dative as NP or PP in two corpora.
##    The dative case is a grammatical case used in some languages 
##    (like German) to indicate the noun to which something is given.
##    This dataset shows us, among other things, how often the theme is 
##    animate (AnimacyOfTheme) and how long the theme is (LengthOfTheme).
##    Plot this using the function bargraph.CI(). Look at the help for this function. 
##    Use the arguments 'x.factor' and 'response'.
bargraph.CI(x.factor = dative$AnimacyOfTheme, response = dative$LengthOfTheme)

## g) Expand the plot from question f with the ci.fun argument 
##    (this argument takes 'ciMean'). 
##    Why does the ci differ in this new plot compared to the previous plot?
bargraph.CI(x.factor = dative$AnimacyOfTheme, response = dative$LengthOfTheme, ci.fun = 'ciMean')
# Previous plot computed with mean +/- 1 standard error, with NA values removed
# Here the confidence interval is computed using ciMean, which computes it assuming data is normally distributed, and returns 95% confidence interval.
#***********#

###############
### Exercise 3: Plotting graphs using ggplot.
###############
# There are many ways of making graphs in R, and each has its own advantages 
# and disadvantages. One popular package for making plots is known as ggplot2. 
# The graphs produced with ggplot2 look professional and the code is quite easy 
# to manipulate.
# In this exercise, we'll plot a few graphs with ggplot2 to show its functionalities.
# You'll find all the information you'll need about plotting with ggplot2 here: 
# http://www.cookbook-r.com/Graphs/
# Also, you have been assigned the ggplot2 course in DataCamp. Please work through 
# this course (Please, set up your name in the datacamp profile. 
# So I can find you quickly.)

## a) First install and load the ggplot2 package. Look at the help for ggplot.
install.packages('ggplot2')
library(ggplot2)

## b) We're going to be plotting data from the dataframe 'ratings' 
##    (included in languageR). 
##    Look at the description of the dataset and the summary.

str(ratings)
summary(ratings)
## For each word, we have three ratings (averaged over subjects), one for the 
## weight of the word's referent, one for its size, and one for the words' 
## subjective familiarity. Class is a factor specifying whether the word's 
## referent is an animal or a plant. 
## Furthermore, we have variables specifying various linguistic properties, 
## such a word's frequency, its length in letters, the number of synsets 
## (synonym sets) in which it is listed in WordNet [Miller, 1990], its 
## morphological family size (the number of complex words in which 
## the word occurs as a constituent), and its derivational entropy (an 
## information theoretic variant of the family size measure). 
## Don't worry, you don't have to know what all this means yet in order to 
## be able to plot it in this exercise!

## c) Let's look at the relationship between the class of words and the length. 
##    In order to plot this, we need a dataframe with the means.
##    Below you'll find the code to create a new dataframe based on the existing 
##    dataset ratings.
##    Plot a barplot of ratings.2 using ggplot. Map the two classes to two 
##    different colours. 
##    Remove the legend.
summary(ratings)
condition <- c("animal", "plant")
frequency <- c(mean(subset(ratings, Class == "animal")$Frequency), mean(subset(ratings, Class == "plant")$Frequency))
length <- c(mean(subset(ratings, Class == "animal")$Length), mean(subset(ratings, Class == "plant")$Length))
ratings.2 <- data.frame(condition, frequency, length)
ratings.2
ggplot(ratings.2,aes(x=condition,y=length,fill=condition)) + geom_bar(show.legend = FALSE, stat = "identity")

## d) Let's assume that we have additional data on the ratings of words. 
##    This data divides the conditions up into exotic and common animals 
##    and plants.
##    Below you'll find the code to update the dataframe with this additional data.
##    Draw a line graph with multiple lines to show the relationship between 
##    the frequency of the animals and plants and their occurrence.
##    Map occurrence to different point shapes and increase the size 
##    of these point shapes.
condition <- c("animal", "plant")
frequency <- c(7.4328978, 3.5864538)
length <- c(5.15678625, 7.81536584)
ratings.add <- data.frame(condition, frequency, length)
ratings.3 <- rbind(ratings.2, ratings.add)
occurrence <- c("common", "common", "exotic", "exotic")
ratings.3 <- cbind(ratings.3, occurrence)
ratings.3
ggplot(ratings.3, aes(x=occurrence, y=frequency, group=condition, col=condition)) + geom_point(aes(shape=occurrence),size=5.0) + geom_line()


## e) Based on the graph you produced in question d, 
##    what can you conclude about how frequently 
##    people talk about plants versus animals, 
##    with regards to how common they are?

# For animals higher frequency for exotic, for plants higher frequency for common