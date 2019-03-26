###############
### Cleaning Data
###############

# Please do the "Cleaning Data with R" exercise that was assigned in dataCamp.
# We recommend that you take notes during the DataCamp tutorial, so you're able to use the commands 
# you learned there in the exercise below.
# This week, the exercise will be about getting data into a good enough shape to start analysing. 
# Next week, there will be a tutorial on running t tests for this data.
## You need to provide a serious attempt to each exercise in order to have
## the assignment graded as complete. 

#############################################
# Chirag Bhuvaneshwara, 2571703
# Ayan Majumdar, 2571656
# Egla Hajdini, 2571690
#############################################

# 1. Download the data file "digsym.csv" from the moodle and save it in your working directory. 
getwd()
setwd("C:/Users/Chirag Bhuvaneshwara/Documents/Uni/Sem 3/StatsR/Assignments/A2")

# 2. Read in the data into a variable called "dat".
dat <- read.csv("digsym.csv")


# 3. Load the libraries languageR, stringr, dplyr and tidyr.
library(languageR)
library(stringr)
library(dplyr)
library(tidyr)

# 4. How many rows, how many columns does that data have?
dim(dat)
# 3700 rows, 11 columns

# 5. Take a look at the structure of the data frame using "glimpse"
glimpse(dat)

# 6. View the first 20 rows, view the last 20 rows
head(dat, n=20)
tail(dat,20)

# 7. Is there any missing data in any of the columns?
# Yes there is missing data in the columns.
sum(is.na(dat))

# 8. Get rid of the row number column
dat <- subset(dat, select = -1)

# 9. Put the Sub_Age column second

n <- colnames(dat)
new_names <- c(n[1], n[length(n)], n[3:length(n)-1])
dat <- dat[new_names]

# 10. Replace the values of the "ExperimentName" column with something shorter, more legible
dat$ExperimentName <-str_replace(dat$ExperimentName, "Digit Symbol - Kopie", "DSK")

# 11. Keep only experimental trials (encoded as "Trial:2" in List), get rid of practice trials 
# (encoded as "Trial:1"). When you do this, assign the subset of the data to a variable "data2", 
# then assign data2 to dat and finally remove data2.
data2 <- subset(dat,List == "Trial:2")
dat <- data2
rm(data2)

# 12. Separate Sub_Age column to two columns, "Subject" and "Age", using the function "separate"
dat <- separate(dat, Sub_Age, c("Subject","Age"))

# 13. Make subject a factor
dat$Subject <- factor(dat$Subject)

# 14. Extract experimental condition ("right" vs. "wrong") from the "File" column:
# i.e. we want to get rid of digit underscore before and the digit after the "right" and "wrong".
exp_cond <- gsub("[0-9]*_(right|wrong)[0-9]*","\\1",dat$File)
dat$File <- exp_cond

# 15. Using str_pad to make values in the File column 8 chars long, by putting 0 on the end  (i.e., 
# same number of characters, such that "1_right" should be replaced by "1_right0" etc)
str_pad(dat$File, width = 8, side = "right", pad = 0)

# 16. Remove the column "List"
dat <- subset(dat, select = -6)

# 17. Change the data type of "Age" to integer
dat$Age = as.integer(dat$Age)

# 18. Missing values, outliers:
# do we have any NAs in the data, and if so, how many and where are they?
# No, we do not have any NAs.
sum(is.na(dat))

# 19. Create an "accuracy" column using if-statement
# if actual response (StimulDS1.RESP) is the same as the correct response (StimulDS1.CRESP), put 
# in value 1, otherwise put 0
dat$accuracy= NA
for(i in 1: length(dat$Subject)){
  if (dat$StimulDS1.RESP[i] == dat$StimulDS1.CRESP[i])  {
    dat$accuracy[i] = 1
  }
  else{
    dat$accuracy[i] = 0
  }
}
head(dat)

# 20. How many wrong answers do we have in total?
# 185 wrong answers
sum(dat$accuracy == 0)

# 21. Whats the percentage of wrong responses?
# 5.55 % wrong responses
100 - (sum(dat$accuracy)*100/nrow(dat))


# 22. Create a subset "correctResponses" that only contains those data points where subjects responded correctly. 
correctResponses <- subset(dat, File == "right")
str(correctResponses)

# 23. Create boxplot of StimulDS1.RT - any outliers?
# Yes, theres seem to be many outliers.
boxplot(correctResponses$StimulDS1.RT)

# 24. Create histogram of StimulDS1.RT with bins set to 50
hist(correctResponses$StimulDS1.RT, breaks = 50)

# 25. Describe the two plots - any tails? any suspiciously large values?
# Both plots show the distribution of the data attributes. The histogram shows that the data is almost 
# normally distributed but is positively skewed. The boxplot also show that many data points lie outside 
# the interquartile range.

# 26. View summary of correct_RT
summary(correctResponses)

# 27. There is a single very far outlier. Remove it and save the result in a new dataframe named "cleaned".
cleaned <- correctResponses[-which(correctResponses$StimulDS1.RT == max(correctResponses$StimulDS1.RT)),]


## EXTRA Exercises:
##You can stop here for your submission of this week's assignment,
##but you are encouraged to try it now. 
##All these exercises will be discussed and solved in the tutorial!

# 28. Dealing with the tail of the distribution: outlier removal
# Now, remove all correct_RT which are more than 2.5. SD away from the grand mean


# 29. Create new "correct_RT_2.5sd" column in data which prints NA if an RT value is below/above the cutoff


# 30. Take a look at the outlier observations
# any subjects who performed especially poorly?


# 31. How many RT outliers in total?


# 32. Plot a histogram and boxplot of the correct_RT_2.5sd columns again - nice and clean eh?


# 33. Next, we'd like to take a look at the avrg accuracy per subject
# Using the "cast" function from the library "reshape", create a new data.frame which shows the 
# average accuracy per subject. Rename column which lists the average accuracy as "avrg_accuracy".


# 34. Sort in ascending order or plot the average accuracies per subject.


# 35. Would you exclude any subjects, based on their avrg_accuracy performance?


# 36. Congrats! Your data are now ready for analysis. Please save the data frame you created into a new 
# file "digsym_clean.csv".