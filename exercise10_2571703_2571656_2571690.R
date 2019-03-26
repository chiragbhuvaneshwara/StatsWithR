### Stats with R Exercise sheet 10

##########################
#Week 11: Model Selection, Transformations, Power
##########################


## This exercise sheet contains the exercises that you will need to complete and 
## submit by 23:55 on Sunday, January 20. Write the code below the questions. 
## If you need to provide a written answer, comment this out using a hashtag (#). 
## Submit your homework via moodle.
## You are allowed to work together in group up to three students, but everybody 
## needs to submit the group version of the homework via moodle.


## Please write below your (and your teammates) name, matriculation number. 
## Name: Chirag Bhuvaneshwara, Ayan Majumdar, Egla Hajdini
## Matriculation number: 2571703, 2571656, 2571690

## Change the name of the file by adding your matriculation numbers
## (exercise0N_firstID_secondID_thirdID.R)

###########################################################################################
###########################################################################################


#Play around with the simulation code. The code simulates how a dataset may be generated. 
#The advantage over using a real data set is that we know exactly how the data was generated, 
#and can observe whether the model manages to correctly identify the original model structure and coefficients.
# IMPORTANT! Run each model simulation code several times to see how stable the results are -- this is necessary
#because we are sampling the data randomly, so it could be that we sometimes get more or less "lucky" draws.

library(lme4)
library(car)

n <- 200 # number of observations to be simulated
predA <- rnorm(n, 100, 20)
predB <- rnorm (n, 60, 30)
interact <- 0.002*(predA*predB) 
error <- rnorm (n, 0, 30)

resp <- 25 + predA + 1.2*predB - interact + error

d <- data.frame(predA, predB, resp)

# 1. Write down what values you would hope for the model to estimate in the ideal case:
# a)intercept= ? --> 25
# b)predA= ? --> 1
# c)predB= ? --> 1.2
# d)predA:predB = ? --> -0.002

 m1<- lm(resp~predA*predB, data=d)
# Ignore the warning message about rescaling for now, we'll get to that below.
summary(m1)  
# 2. Can the model recover the original model structure and estimate correct coefficients for the predictors?

# The model is unable to recover the original model structure, as the R-squared varies from 0.5 to 0.6.
# It is also unable to estimate the correct coefficients for the predictors. Plus, multiple iterations
# of data generation and model fitting results in largely varying estimates.

# 3. What happens if you change the number of subjects?

n <- 5000 # number of observations to be simulated
predA <- rnorm(n, 100, 20)
predB <- rnorm (n, 60, 30)
interact <- 0.002*(predA*predB) 
error <- rnorm (n, 0, 30)

resp <- 25 + predA + 1.2*predB - interact + error

d <- data.frame(predA, predB, resp)

m1<- lm(resp~predA*predB, data=d)

summary(m1)  

# If we increase the number of subjects, the parameter estimates become slightly better. 
# R-squared value improves slightly. Reducing the number of subjects increases the prediction variation.

# 4. What happens if you change the variance of the error term?

n <- 200 # number of observations to be simulated
predA <- rnorm(n, 100, 20)
predB <- rnorm (n, 60, 30)
interact <- 0.002*(predA*predB) 
error <- rnorm (n, 0, 60)

resp <- 25 + predA + 1.2*predB - interact + error

d <- data.frame(predA, predB, resp)

m1<- lm(resp~predA*predB, data=d)

summary(m1)  

# Reducing the variance of error term increases the fit performance as the R-squared value increases/
# Increasing the variance of the error term reduces the fit performance significantly.

# 5. What happens if you change the effect sizes?

n <- 200 # number of observations to be simulated
# predA <- rnorm(n, 20, 20) # Increasing effect size

predA <- rnorm(n, 250, 20) # Decreasing effect size

predA <- rnorm(n, 100, 20) # with the given effect size
predB <- rnorm (n, 65, 30)
interact <- 0.002*(predA*predB) 
error <- rnorm (n, 0, 30)

resp <- 25 + predA + 1.2*predB - interact + error

d <- data.frame(predA, predB, resp)

m1<- lm(resp~predA*predB, data=d)

# mean(d$predA + d$predB)
# mean(d$resp)

summary(m1)  

# Increasing the effect size leads to a better fit as indicated by the R^2 value and 
# decreasing the effect size leads to a bad fit as the distributions corresponding to the 
# null and alternate hypotheses will be overlapping.

# Next, we want to observe the effect of scaling the predictors. 
# By hand, we could do: normpredA <- (predA - mean(predA)) / sd(predA)
# this is the same as calling "normpredA <- scale(predA)"
# we can do this for the whole data frame:
nd <- as.data.frame(scale(d))
sm1<- lm(resp~predA*predB, data=nd)
summary(sm1)
vif(m1)
vif(sm1)

# 6. Are the predictors currently correlated? What does the vif value mean?

cor(predA, predB)

# The output of cor is almost zero, and also after scaling, the vif values for the predictors 
# are around one, which means that the predictors are not correlated.
# The VIF measure provides an index that measures how much the variance of an estimated regression 
# coefficient is increased because of collinearity. It quantifies the multicollinearity in the model.

# 7. Check whether normalization also has a large effect when there is no interaction present in the model

sm2<- lm(resp~predA+predB, data=nd)
m2<- lm(resp~predA+predB, data=d)
summary(sm2)
summary(m2)
vif(m2)
vif(sm2)

# Normalization has no effect when there is no interaction in the models. The R-squared values are same, so are VIF.

# 8. Try out what happens if there was originally no interaction in the data.

n_1 <- 200 # number of observations to be simulated
predA_1 <- rnorm(n, 100, 20)
predB_1 <- rnorm (n, 60, 30)
#interact <- 0.002*(predA*predB) 
error_1 <- rnorm (n, 0, 30)

resp_1 <- 25 + predA_1 + 1.2*predB_1 + error_1

d_1 <- data.frame(predA_1, predB_1, resp_1)

m1_1<- lm(resp_1~predA_1+predB_1, data=d_1)

summary(m1_1)  

nd_1 <- as.data.frame(scale(d_1))
sm1_1<- lm(resp_1~predA_1+predB_1, data=nd_1)
summary(sm1_1)
vif(m1_1)
vif(sm1_1)

m1_2<- lm(resp_1~predA_1*predB_1, data=d_1)

summary(m1_2)  

sm1_2<- lm(resp_1~predA_1*predB_1, data=nd_1)
summary(sm1_2)
vif(m1_2)
vif(sm1_2)

# Again, normalization has an effect if the model has interaction in it. 

#Next, we want to calculate interpretable estimates

names(sm1)
coef(sm1)

 denormPredA <- coef(sm1)[2] *sd(d$resp)/ sd(d$predA) 
 denormPredA

# 10. Explain in your own words, why the denormalization for predictor A works this way.
 
  # ************ TODO ************#
 # This is because the normalized coefficient can be computed as original_coefficient_X * (s.d. of X / s.d. of Y)
 # for X being predictor and Y being the outcome variable. 
 # Standardized coefficients refer to how many standard deviations a dependent variable will change, per standard 
 # deviation increase in the predictor variable.


denormPredB <- coef(sm1)[3] * sd(d$resp)/ sd(d$predB)
 denormPredB
# expected: 1.2

 denormIntercept<-coef(sm1)[1] * sd(d$resp)+mean(d$resp)-
 (denormPredA*mean(d$predA) + denormPredB* mean(d$predB))
 denormIntercept
# expected: 25

denormInteract <- coefficients(sm1)[4] / (sd(d$predA)*sd(d$predB)) * sd(d$resp)
denormInteract


# Next, we create correlated variables 
n <- 200 # number of observations to be simulated
predA <- rnorm(n, 100, 20)
predB<- rnorm (n, 60, 30)
predC <- -1* predA + rnorm(n,0,10)
error <- rnorm (n, 0, 30)
respcor <- 25 + predA + 3* predB+ 2*predC - (0.02*(predA*predC)) + error
d2<-data.frame(predA, predB, predC, respcor)
summary(lm(respcor ~ predA * predC + predB, data=d2))

sd2 <-as.data.frame(scale(d2))
summary(lm(respcor ~ predA * predC + predB, data=sd2))

# 11. What do you observe regarding the results from the models? Do the models obtain the same or different results 
# with / without normalization?

# The R-squared values are exactly the same, so are the F-statistic. However, before normalization predC
# was considered statistically significant and the effect of predA may or may not have been classified as statistically significant,
# but after normalization predA is correctly classified as significant and predC as not significant.

# 12. Denormalize the coefficients.

denorm_predA <- coef(lm(respcor ~ predA * predC + predB, data=sd2))[2] * sd(d2$respcor) / sd(d2$predA)
denorm_predA #***TODO***#
denorm_predB <- coef(lm(respcor ~ predA * predC + predB, data=sd2))[4] * sd(d2$respcor) / sd(d2$predB)
denorm_predB
denorm_predC <- coef(lm(respcor ~ predA * predC + predB, data=sd2))[3] * sd(d2$respcor) / sd(d2$predC)
denorm_predC #***TODO***#


denorm_intercept <- coef(lm(respcor ~ predA * predC + predB, data=sd2))[1] * sd(d2$respcor) + (
  mean(d2$respcor) - (denorm_predA*mean(d2$predA)) - (denorm_predB*mean(d2$predB)) - (denorm_predC*mean(d2$predC))
)
denorm_intercept #***TODO***#
denorm_interact <- coef(lm(respcor ~ predA * predC + predB, data=sd2))[5]  / (sd(d2$predA)*sd(d2$predC)) * sd(d2$respcor)
denorm_interact
 
# Finally, we will generate repeated measures!
# For this, we will use the dataframe d; for simplicity of interpretation, we will do no normalization here.
n<-400
predA <- rnorm(n, 100, 20)
predB <- rnorm (n, 60, 30)
error <- rnorm (n, 0, 30)
subjno <- 20 #number of subjects; 
itemno<- 20 # number of items;
subj<-as.factor(rep(1:subjno,itemno))
item<-as.factor(rep(1:itemno, each=subjno))
lmerd <- data.frame(predA, predB, resp, subj, item)
# basic data frame done; now on to by subject and by item random effects:
subjid<-as.factor(1:subjno)
subjint<- rnorm(subjno, 0, 8)
subjeffdf<-data.frame(subjid, subjint)
itemid<-as.factor(1:itemno)
itemint<-rnorm(itemno, 0, 4)
itemeffdf<-data.frame(itemid, itemint)
newd <- merge(lmerd, itemeffdf, by.x="item", by.y="itemid")
newd<- merge(newd, subjeffdf, by.x="subj", by.y = "subjid")
lmerd <- newd
# add by-subject and by-item effects here!
lmerd$respr <- 25+newd$subjint + newd$itemint + newd$predA + 1.2*newd$predB + error

m0<-lm(respr ~ predA + predB , data=lmerd)
summary(m0)
m1<-lmer(respr ~ predA + predB + (1|subj) + (1| item), data=lmerd)
summary(m1)

lmerd$resp <- 25 + newd$predA + 1.2*newd$predB + error
m2<-lmer(resp ~ predA + predB + (1|subj) + (1| item), data=lmerd)
summary(m2)

#13. Explain the difference between models m0 m1 and m2

# Model m0 and m1 are both fitted to resp variable when the resp variable has by item and by subject effects.
# m0 has only fixed effects and predictors are just predA and predB. m1 has both fixed effect and random intercept
# with respect to both subject and item. Model m2 is fitted on resp variable that originally has only
# fixed effect. But model m2 has both fixed and per-subject and per-item random effects. 
# ***** BOTH m1 and m2 are sometimes having singular fits. **** #


#14. Play around with the size of the by item and by subject effects (here: intercepts only)

# ********** TODO *********** #
# a.
lmerd$respr <- 25+5*newd$subjint + newd$itemint + newd$predA + 1.2*newd$predB + error

m<-lmer(resp ~ predA + predB + (1|subj) + (1| item), data=lmerd)
summary(m)

# b.
lmerd$respr <- 25+newd$subjint + 5*newd$itemint + newd$predA + 1.2*newd$predB + error

m<-lmer(resp ~ predA + predB + (1|subj) + (1| item), data=lmerd)
summary(m)

# c.
lmerd$respr <- 25+ 5*newd$subjint + 5*newd$itemint + newd$predA + 1.2*newd$predB + error

m<-lmer(resp ~ predA + predB + (1|subj) + (1| item), data=lmerd)
summary(m)

#15. Generate the data such that subjects differ in terms of how much predictor A affects them.

n<-400
predA <- rnorm(n, 100, 20)
predB <- rnorm (n, 60, 30)
error <- rnorm (n, 0, 30)
subjno <- 20 #number of subjects; 
itemno<- 20 # number of items;
subj<-as.factor(rep(1:subjno,itemno))
item<-as.factor(rep(1:itemno, each=subjno))
lmerd <- data.frame(predA, predB, resp, subj, item)

subjid<-as.factor(1:subjno)
subjint<- rnorm(subjno, 0, 8)
subjslope <- rnorm(subjno, 0, 5) #by-subject slope
subjeffdf<-data.frame(subjid, subjint, subjslope)
itemid<-as.factor(1:itemno)
itemint<-rnorm(itemno, 0, 4)
itemeffdf<-data.frame(itemid, itemint)
newd <- merge(lmerd, itemeffdf, by.x="item", by.y="itemid")
newd<- merge(newd, subjeffdf, by.x="subj", by.y = "subjid")
lmerd <- newd
# add by-subject and by-item effects here! Also adding random slope effect by-subject.
lmerd$respr <- 25+newd$subjint + newd$itemint + newd$subjslope*newd$predA + newd$predA + 1.2*newd$predB + error

#16. Then build a mixed effects model that includes a random slope for subjects.


m<-lmer(resp ~ predA + predB + (1+predA|subj) + (1| item), data=lmerd)
summary(m)

m <- lmer(resp ~ predA + predB + (1|subj) + (0+predA|subj) + (1|item), data=lmerd)
summary(m)
