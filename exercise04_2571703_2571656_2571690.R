### Stats with R Exercise sheet 4

##########################
#Week5: Tests for Categorial Data
##########################

## This exercise sheet contains the exercises that you will need to complete and 
## submit by 23:55 on Sunday, November 18. Write the code below the questions. 
## If you need to provide a written answer, comment this out using a hashtag (#). 
## Submit your homework via moodle.
## You are allowed to work together in group up to three students, but everybody 
## needs to submit the group version of the homework via moodle.


## Please write below your (and your teammates) name, matriculation number. 
## Name: Chirag Bhuvaneshwara, Ayan Majumdar, Egla Hajdini
## Matriculation number: 2571703, 2571656, 2571690

## Change the name of the file by adding your matriculation numbers
## (exercise0N_firstID_secondID_thirdID.R)

#################################################################################
#################################################################################

##########
##Exercise 1. Binomial distribution
##########
## Suppose there are 12 multiple choice questions in a quiz. 
## Each question has 5 possible answers, and only one of them is correct. 

## a) Please calculate the probability of getting exactly 4 answers right 
##    if you answer by chance. Calculate this using the dbinom() function.
dbinom(4,12,1/5)

## b) Next please calculate the probability of answering 4 or less questions 
##    correctly by chance. 
sum(dbinom(0:4,12,1/5))

##########
##Exercise 2. Chi-square test
##########
## a) Consider the dataset dutchSpeakersDistMeta from our first tutorial again. 
##    Load the package (languageR) and look at the summary of the variables, 
##    as well as their classes. Which variables are factors?
#install.packages("languageR")
library(languageR)
summary(dutchSpeakersDistMeta)
class(dutchSpeakersDistMeta$Speaker)

str(dutchSpeakersDistMeta)
# Speaker, Sex, AgeYear, AgeGroup, ConversationType, EduLevel are all factor variables


## b) We want to find out whether there is a difference between males and females 
##    with respect to the age groups they are in.
##	  First use the function 'table()' to get the counts and create 
##    a contingency table of AgeGroup by Sex.
age <- table(dutchSpeakersDistMeta$AgeGroup, dutchSpeakersDistMeta$Sex)
age

##    Visualize your data with a single bar plot (use ggplot) that represents the counts with respect to each age group 
##	  and each sex.
library(ggplot2)
library(tidyr)
age_df <- as.data.frame(age)
colnames(age_df) <- c("AgeGroup","Sex","Freq")
age_df
ggplot(age_df, aes(x=AgeGroup, y=Freq, group=Sex, fill=Sex)) + geom_bar(stat='identity',position = 'dodge')

## c) Inspect the table 'age' you created. Does it look like there could be a significant 
##    difference between the sexes?
age
# Yes there seems to be a significant difference between male and female as there are more females in each Age group.

## d) We are going to calculate whether there's a difference between males and females 
##    regarding their age group
##    using the function chisq.test. Look at the help of this function. Then use the 
##    function to calculate whether there's a difference in our table 'age'. 
##    Is there a significant difference in age group?
?chisq.test
chisq.test(age)
# Since the p value is above alpha = 0.05, there is no difference between the 2 Sexes.

## e) What are the degrees of freedom for our data? How are they derived?
# The degrees of freedom for our data is 4. It is derived from the table with the formula: (rows-1) * (cols-1).
# The degrees of freedom represents how many squared gaussian distributions are summed to obtain the distribution for the current test.

##########
##Exercise 3. Binomial versus chi-square
##########
##    In this exercise, we'll do significance tests for a paper on therapeutic touch 
##    (google it if you want to know what that is...) that was published in the Journal 
##    of the American Medical Association (Rosa et al., 1996).
##    The experimenters investigated whether therapeutic touch is real by using the 
##    following method:
##    21 practitioners of therapeutic touch were blindfolded. The experimenter 
##    placed her hand over one of their hands. If therapeutic touch is a real 
##    phenomenon, the principles behind it suggest that the participant should 
##    be able to identify which of their hands is below the experimenter's hand. 
##    There were a total of 280 trials, of which the therapeutic touch therapists 
##    correctly indicated when a hand was placed over one of their hands 123 times.

## a) What is the null hypothesis, i.e. how often would we expect the participants to 
##    be correct by chance (in raw number and in percentage)?
# The null hypothesis is that the participants are correct 140 times out of the 280 trials or 50% of the time.


## b) Using a chisquare test, what do you conclude about whether therapeutic touch 
##    works? 
chi_obs = ((123 - 140)^2 + (157 - 140)^2) / 140
chi_obs

qchisq(.95,1)
# chi_obs > alpha value of 0.05 in chisquared distribution. Therefore, we accept the alternate hypothesis 
# i.e thereputic touch is real.


## c) Now calculate significance using the binomial test as we used it in exercise 1.
2 * pbinom(123,280,.5, lower.tail = T) # two tailed test
# The above result is less than alpha value of 0.05. Which implies that we can reject the null hypothesis.

## d) The results from these two tests are slightly different. Which test do you think 
##    is better for our data, and why?
# The binomial test is an exact test to compare the observed distribution to the expected distribution 
# when there are only two categories. Chi square can only give an approximation of this, as it uses
# the normal distribution for an approximation of the binomial distribution.

# But we also have: 
# N*P = (280 * .5) > 5 . => The binom distribution can be approximated by Gaussian in a good enough manner
# and the chisquared test developed from it is also good enough. 

##########
##Exercise 4.
##########
## Describe a situation where you would choose McNemar's test over the ChiSquare test. 
## What would be the problem of using the normal ChiSquare test in a case where 
## McNemar's test would be more appropriate?

# McNemar's test is more apt in a situation where we have to model the dependency of our obsevations. 
# For example studying the reactions of reviewers before and after a certain change in our product.
# If we use a normal Chisquare Test, we assume that the observations are independent which is an incorrect assumption
# and this leads to incorrect results.
