### Stats with R Exercise sheet 9

##########################
#Week 10: Linear Mixed Effects Models
##########################


## This exercise sheet contains the exercises that you will need to complete and 
## submit by 23:55 on Sunday, December 30. Write the code below the questions. 
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

library(lme4)
library(lattice)
library(Matrix)

# Read in the data file from gender.Rdata, sem.Rdata or relclause.Rdata.
# You can choose the one you'd like best based on the description of the items below, 
# and explore the analysis for that dataset. Afterwards, you can adapt your analysis 
# for the other datasets (that should be considerably less work.)

# The files contain data from an experiment where people were reading sentences, 
# and pressed the space bar to see the next word. The duration for which a word was 
# viewed before pressing the space bar again is the reading time of the word, and is 
# stored in the file as "WORD_TIME". The experiment had 24 items (given as "ITEM_ID") 
# and 24 subjects (given as "PARTICIPANT"). The order in which the different sentences 
# were presented in the experiment is given in the variable "itemOrder". 

# For each of the files, the sentences that were shown had a different property. 

# Sentences in the sem.Rdata experiment had a semantic violation, i.e. a word that 
# didn't fit in with the previous words in terms of its meaning. The experiment 
# contained two versions of each item, which were identical to one another except 
# for the one sentence containing a semantic violation, while the other one was 
# semantically correct. These conditions are named "SG" for "semantically good" 
# and "SB" for "semantically bad".

# Semantic materials (the experiment is in German, English translation given 
# for those who don't speak German')

# Christina schießt / raucht eine Zigarette nach der Arbeit. 
# "Christina is shooting / smoking a cigarette after work."

# The crticial word here is "Zigarette", as this would be very surprising in the 
# context of the verb "schießt", but not in the context of the verb "smoke". 
# Reading times are comparable because the critical word "Zigarette" is identical 
# in both conditions.

# Syntactic items:
# Simone hatte eine(n) schreckliche(n) Traum und keine Lust zum Weiterschlafen. 
# "Simone had a[masc/fem] horrible[masc/fem] dreammasc and didn't feel like sleeping 
# any longer."

# Here, there are again two conditions, one using correct grammatical gender on 
# "einen schrecklichen" vs. the other one using incorrect grammatical gender 
# "eine schreckliche". The critical word is "Traum" (it's either consistent or 
# inconsistent with the marking on the determiner and adjective)

# Relative clause items:
# Die Nachbarin, [die_sg nom/acc einige_pl nom/acc der Mieter auf Schadensersatz  
# verklagt hat_sg/ haben_pl]RC, traf sich gestern mit Angelika. 
# "The neighbor, [whom some of the tenants sued for damages / who sued some of  the
# tenants for damages]RC, met Angelika yesterday."

# When reading such a sentence, people will usually interpret the relative pronoun 
# die as the subject of the relative clause and the following noun phrase 
# "einige der Mieter" as the object. This interpretation is compatible with 
# the embedded singular-marked (sg) verb hat at the end of the relative clause. 
# Encountering the verb haben, which has plural marking (pl), leads to processing 
# difficulty: in order to make sense of the relative clause, readers need to 
# reinterpret the relative pronoun die as the object of the relative clause 
# and the following noun phrase "einige der Mieter" as its subject. 
# (Note that the sentences are all grammatical, as the relative pronoun and 
# following NPs are chosen such that they are ambiguous between nominative (nom)
# and accusative (acc) case marking.)

# The number of the word in a sentence is given in column "SEMWDINDEX". 
# 0 designates the word where the semantic violation happens (in the SB condition; 
# in the SG condition, it's the corresponding word). We call this word the 
# "critical word" or "critical region". -1 is the word before that, -2 is 
# two words before that word, and 2 is two words after that critical word. 
# "EXPWORD" shows the words. We expect longer reading times for the violation 
# at word 0 or the word after that (word 1) (if people press the button quickly 
# before thinking properly).

#######################################################################################
#######################################################################################

# a) Take a look at the data.
#setwd("C:/Users/Chirag Bhuvaneshwara/Documents/Uni/Sem 3/StatsR/Assignments/A9")
setwd("D:\\UdS\\WS1819\\Stats_R\\Assignments")
sem <- read.delim("sem.Rdata", header = T,sep = " ")
str(sem)
tail(sem)
# b) Plot it (use ggplot for this task and all the tasks below).
#    (You can provide any plots we have seen so far to interpret the data.
#    For example, you can study the difference between the subjects (participants) 
#    in terms of responce time or the difference between items (sentences) in 
#    terms of response time).
library(ggplot2)
head(sem)

# shows by subject variance in responses: high
#p1 <- ggplot(sem, aes(x= itemOrder,y= WORD_TIME, group=PARTICIPANT, color=PARTICIPANT))
p1 <- ggplot(sem, aes(x= PARTICIPANT,y= WORD_TIME, group=PARTICIPANT, color=PARTICIPANT))
p1+geom_boxplot()

# shows by item variance in reponses: low
p2 <- ggplot(sem, aes(x= ITEM_ID,y= WORD_TIME, group=ITEM_ID, color=ITEM_ID))
p2+geom_boxplot()

# shows by ITEM_TYPE variance in responses: no variance 
#p3 <- ggplot(sem, aes(x= itemOrder,y= WORD_TIME, group=ITEM_TYPE, color=ITEM_TYPE))
#p3+geom_boxplot()

# shows by SEMWDINDEX variance in responses: very less
#p4 <- ggplot(sem, aes(x= itemOrder,y= WORD_TIME, group=factor(SEMWDINDEX), color=factor(SEMWDINDEX)))
#p4+geom_boxplot()

#    Below you also find the plot for the dataset 'sleepstudy' from the package 'lme4'.
#    The figure shows relationships between days without sleeping and reaction 
#    time for each participant (subject) separately.

# summary(sleepstudy)
# str(sleepstudy)
# print(xyplot(Reaction ~ Days | Subject, sleepstudy, aspect = "xy",
#              layout = c(9,2), type = c("g", "p", "r"),
#              index.cond = function(x,y) coef(lm(y ~ x))[1],
#              xlab = "Days of sleep deprivation",
#              ylab = "Average reaction time (ms)"))

#    Your task is also to figure out how to adapt this plot for our data. What do you 
#    conclude regarding the reading sentences experiment?
head(sem)
print(xyplot(WORD_TIME ~ itemOrder | PARTICIPANT, sem, aspect = "xy",
             layout = c(8,3), type = c("g", "p", "r"),
             index.cond = function(x,y) coef(lm(y ~ x))[1],
             col.line = "darkorange",
             xlab = "Item Order",
             ylab = "Response time (ms)"))

# The responses in the experiment for all participants are mostly between 0 and 2000 ms.
# Some subjects seem to be able to respond quicker on average with short response times than others.


# c) Decide whether you want to exclude any data points (provide not only the code,
#    but also a detailed (!) explanation);
# It is unlikely that the subjects can respond within 100 ms when their average response 
# seems to be around 1000ms. So these data points can be removed as the subjects might've 
# responded without reading or even by mistake.
# The points with response times greater than 2000ms can also be removed since they are very far 
# from the average responses. We can assume that the subjects got bored or distracted resulting 
# in such long response times.

keep <- !(sem$WORD_TIME <100 | sem$WORD_TIME >2000)

semNew <- sem[keep,]

str(sem)
str(semNew)

print(xyplot(WORD_TIME ~ itemOrder | PARTICIPANT, semNew, aspect = "xy",
             layout = c(8,3), type = c("g", "p", "r"),
             index.cond = function(x,y) coef(lm(y ~ x))[1],
             col.line = "darkorange",
             xlab = "Item Order",
             ylab = "Response time (ms)"))

# d) Try to make a plot where for each word, the average reading 
#    (collapsing across items and subjects) is shown; in this plot all violations 
#    are at point 0. Of course, you should not collapse the semantically good vs. 
#    bad condition.
library(reshape)

head(semNew)

#cond <- semNew$SEMWDINDEX==0 & semNew$ITEM_TYPE =="SG"

cond <- semNew$SEMWDINDEX==0
casted_data<- cast(semNew[cond,], EXPWORD + ITEM_TYPE ~., fun.aggregate = 'mean', value = 'WORD_TIME')

colnames(casted_data)[3] <- "mean_WORD_TIME"
head(casted_data)

p2 <- ggplot(casted_data, aes(x=EXPWORD, y=mean_WORD_TIME, color=ITEM_TYPE, label=EXPWORD))
p2 + geom_point() + geom_text(size=3)

# e) Experiment with calculating a linear mixed effects model for this study, 
#    and draw the appropriate conclusions (give a detailed explanation 
#    for each model).
head(semNew)

# Random intercept only model
# Here we design a model where we model random intercept for both item and subject.
# Along with this we test SEMWDINDEX and the itemOrder as predictor variables.
# As there was variance in per-term and per-subject basis, we use include both these random intercept effect.
model_1 <- lmer(WORD_TIME ~ SEMWDINDEX + itemOrder
                + (1|ITEM_ID)
                + (1|PARTICIPANT), 
                data = semNew, REML = F)

summary(model_1)

# To check which predictor variable is effective, we now test two other models where we remove each of 
# itemOrder and SEMWDINDEX.
model_1_null_1 <- lmer(WORD_TIME ~ itemOrder
                     + (1|ITEM_ID)
                     + (1|PARTICIPANT), 
                     data = semNew, REML = F)
summary(model_1_null_1)

anova(model_1_null_1, model_1)

model_1_null_2 <- lmer(WORD_TIME ~ SEMWDINDEX
                       + (1|ITEM_ID)
                       + (1|PARTICIPANT), 
                       data = semNew, REML = F)
anova(model_1_null_2, model_1)
# From the above ANOVA analysis we see that including both SEMWDINDEX and itemOrder gives better result.
# Now we test to see if ITEM_TYPE can also help.
model_2 <- lmer(WORD_TIME ~ SEMWDINDEX + itemOrder + ITEM_TYPE
                + (1|ITEM_ID)
                + (1|PARTICIPANT), 
                data = semNew, REML = F)
anova(model_1, model_2)

# So we see that a model where SEMWDINDEX and itemOrder are the predictors have the statistically significant
# outcome over the other models. Hence we choose this. We also include both the random intercepts for 
# ITEM and PARTICIPANT (Subject).

# Now we try a random intercept-slope model. We see the effect of word index position, itemOrder, and 
# random slope effect of the word index w.r.t. participant and item_id.

model_mixed_slope_1 <- lmer(WORD_TIME ~ SEMWDINDEX + itemOrder
                            + (1+SEMWDINDEX|PARTICIPANT)
                            + (1+SEMWDINDEX|ITEM_ID), 
                            data = semNew, REML = F)

model_mixed_slope_null <- lmer(WORD_TIME ~ itemOrder
                            + (1+SEMWDINDEX|PARTICIPANT)
                            + (1+SEMWDINDEX|ITEM_ID), 
                            data = semNew, REML = F)

anova(model_mixed_slope_null, model_mixed_slope_1)

# We indeed have statistically significant difference between the null model and the full model for random slope.

summary(model_mixed_slope_1)

anova(model_1, model_mixed_slope_1)

# We also have a statistically significant difference between the random intercept model and the random slope models.

summary(model_1)
summary(model_mixed_slope_1)

# We can see that the residual for the random slope model has less variance and std.dev.

model_mixed_slope_2 <- lmer(WORD_TIME ~ SEMWDINDEX + itemOrder + ITEM_TYPE
                            + (1+SEMWDINDEX|PARTICIPANT)
                            + (1+SEMWDINDEX|ITEM_ID),
                            data = semNew, REML = F)

anova(model_mixed_slope_1, model_mixed_slope_2)

# We just tested to see if including ITEM_TYPE improves this model. But, we do not see any statistically
# significant effect in adding this additional variable.

## ~~~~~~~~~~~~~~~~~~~~~~~~ ##

# semNew.mixedMod <- lmer(WORD_TIME ~ EXPWORD +
#                           ITEM_TYPE +
#                           (1|itemOrder) +
#                           (1|SEMWDINDEX) +
#                           (1|PARTICIPANT) +
#                           (1|ITEM_ID) , 
#                         data=semNew, REML=F)
# summary(semNew.mixedMod)
# 
# semNew.null <- lmer(WORD_TIME ~ITEM_TYPE +
#                       (1|itemOrder) +
#                       (1|SEMWDINDEX) +
#                       (1|PARTICIPANT) +
#                       (1|ITEM_ID) , 
#                         data=semNew, REML=F)
# summary(semNew.null)
# 
# anova(semNew.null,semNew.mixedMod)
#Thus, EXPWORD has a significant effect on the response time.

################################

# Random slope and intercept model: did something wrong here
# semNew.mixedSlopeMod <- lmer(WORD_TIME ~ ITEM_TYPE +
#                               EXPWORD +
#                               (1+EXPWORD|PARTICIPANT) +
#                               (1+EXPWORD|ITEM_ID) ,
#                             data=semNew, REML=F)

#summary(semNew.mixedSlopeMod)
 
# semNew.nullSlope <- lmer(WORD_TIME ~ ITEM_TYPE +
#                                (1+EXPWORD|PARTICIPANT) +
#                                (1+EXPWORD|ITEM_ID) , 
#                              data=semNew, REML=F)
# summary(semNew.nullSlope)
# 
# anova(semNew.nullSlope,semNew.mixedSlopeMod)
#####################
#coef(semNew.mixedMod)

#    Let's get back to the dataset 'sleepstudy'. The following plot shows 
#    subject-specific interception and slope. Adapt this plot for our study 
#    and make conclusions.

# model = lmer(Reaction ~ Days + (Days|Subject), sleepstudy)
# print(dotplot(ranef(model,condVar=TRUE),  scales = list(x = list(relation = 'free')))[["Subject"]])

print(dotplot(ranef(model_mixed_slope_1,condVar=TRUE),  scales = list(x = list(relation = 'free')))[["PARTICIPANT"]])

# From this plot we see how our mixed effect model is able to model separate intercepts and slopes
# for different participants. If we go to the Subject-wise plots we did before, we would see that the 
# user p0421fr in general took longer time for all words, whereas user p0125fr took exceptionally low time.
# We can see here that our model was able to adjust and model separate intercept values for these users 
# (and all other users) to take this into effect. We similarly see how we can also fit separate slopes
# with respect to the SEMWDINDEX, which models the different reading behaviours of the different subjects better.

######################################################################
# for gender.Rdata
data <- read.delim("gender.Rdata", header = T, sep = " ")
head(data)

# shows by subject variance in responses: high
p1 <- ggplot(data, aes(x= PARTICIPANT,y= WORD_TIME, group=PARTICIPANT, color=PARTICIPANT))
p1+geom_boxplot()

# shows by item variance in reponses: low
p2 <- ggplot(data, aes(x= ITEM_ID,y= WORD_TIME, group=ITEM_ID, color=ITEM_ID))
p2+geom_boxplot()

# shows by ITEM_TYPE variance in responses: no variance 
#p3 <- ggplot(data, aes(x= itemOrder,y= WORD_TIME, group=ITEM_TYPE, color=ITEM_TYPE))
#p3+geom_boxplot()

# shows by RELWDINDEX variance in responses: very less
#p4 <- ggplot(data, aes(x= itemOrder,y= WORD_TIME, group=factor(RELWDINDEX), color=factor(RELWDINDEX)))
#p4+geom_boxplot()

head(data)
print(xyplot(WORD_TIME ~ itemOrder | PARTICIPANT, data, aspect = "xy",
             layout = c(8,3), type = c("g", "p", "r"),
             index.cond = function(x,y) coef(lm(y ~ x))[1],
             col.line = "darkorange",
             xlab = "Item Order",
             ylab = "Response time (ms)"))


keep <- !(data$WORD_TIME <100 | data$WORD_TIME >2000)
dataNew <- data[keep,]

str(data)
str(dataNew)

print(xyplot(WORD_TIME ~ itemOrder | PARTICIPANT, dataNew, aspect = "xy",
             layout = c(8,3), type = c("g", "p", "r"),
             index.cond = function(x,y) coef(lm(y ~ x))[1],
             col.line = "darkorange",
             xlab = "Item Order",
             ylab = "Response time (ms)"))


levels(dataNew$ITEM_TYPE)
#cond <- dataNew$RELWDINDEX==0 & dataNew$ITEM_TYPE =="GG"
cond <- dataNew$RELWDINDEX==0
casted_data<- cast(dataNew[cond,], EXPWORD + ITEM_TYPE ~., fun.aggregate = 'mean', value = 'WORD_TIME')

colnames(casted_data)[3] <- "mean_WORD_TIME"
head(casted_data)

p2 <- ggplot(casted_data, aes(x=EXPWORD, y=mean_WORD_TIME, color=ITEM_TYPE, label=EXPWORD))
p2 + geom_text(size=3)

# Random intercept only model
model_1 <- lmer(WORD_TIME ~ RELWDINDEX + itemOrder
                + (1|ITEM_ID)
                + (1|PARTICIPANT), 
                data = dataNew, REML = F)

summary(model_1)

model_2 <- lmer(WORD_TIME ~ RELWDINDEX + itemOrder + ITEM_TYPE 
                + (1|ITEM_ID)
                + (1|PARTICIPANT), 
                data = dataNew, REML = F)
anova(model_1, model_2)

summary(model_1)
summary(model_2)

# We now have a statistically significant difference between model_1 and model_2. So, we choose model_2.
# model_2 takes into account effect of relative word index, itemOrder, and the item_type, along with 
# random intercept effect for item and participant.

model_mixed_slope_1 <- lmer(WORD_TIME ~ RELWDINDEX + itemOrder + ITEM_TYPE 
                            + (1 + RELWDINDEX|ITEM_ID)
                            + (1 + RELWDINDEX|PARTICIPANT),
                            data = dataNew, REML = F)

model_mixed_slope_2 <- lmer(WORD_TIME ~ RELWDINDEX + itemOrder + ITEM_TYPE 
                            + (1 + RELWDINDEX + ITEM_TYPE|ITEM_ID)
                            + (1 + RELWDINDEX + ITEM_TYPE|PARTICIPANT), 
                            data = dataNew, REML = F)
summary(model_mixed_slope_1)

summary(model_mixed_slope_2)

anova(model_mixed_slope_1, model_mixed_slope_2)

# We have a statistical significant difference between models 1 and 2. So we choose 2, as the additional
# complexity does indeed produce better fit.

print(dotplot(ranef(model_mixed_slope_2,condVar=TRUE),  scales = list(x = list(relation = 'free')))[["PARTICIPANT"]])

## ~~~~~~~~~~~~~~~~~~~~~~~ ##

# Random intercept only model
# dataNew.mixedMod <- lmer(WORD_TIME ~ EXPWORD+
#                            ITEM_TYPE +
#                           (1|itemOrder) +
#                           (1|RELWDINDEX) +
#                           (1|PARTICIPANT) +
#                           (1|ITEM_ID) , 
#                         data=dataNew, REML=F)
# summary(dataNew.mixedMod)
# 
# dataNew.null <- lmer(WORD_TIME ~ITEM_TYPE +
#                        (1|itemOrder) +
#                       (1|RELWDINDEX) +
#                       (1|PARTICIPANT) +
#                       (1|ITEM_ID) , 
#                     data=dataNew, REML=F)
# summary(dataNew.null)
# 
# anova(dataNew.null,dataNew.mixedMod)
# #Thus, EXPWORD has a significant effect on the response time.
# print(dotplot(ranef(semNew.mixedMod,condVar=TRUE),  scales = list(x = list(relation = 'free')))[["PARTICIPANT"]])


##############################################
# for relclause.Rdata
data_rel <- read.delim("relclause.Rdata", header = T, sep = " ")
head(data_rel)

# shows by subject variance in responses: high
p1 <- ggplot(data_rel, aes(x= PARTICIPANT,y= WORD_TIME, group=PARTICIPANT, color=PARTICIPANT))
p1+geom_boxplot()

# shows by item variance in reponses: low
p2 <- ggplot(data_rel, aes(x= ITEM_ID,y= WORD_TIME, group=ITEM_ID, color=ITEM_ID))
p2+geom_boxplot()

# shows by ITEM_TYPE variance in responses: no variance 
#p3 <- ggplot(data, aes(x= itemOrder,y= WORD_TIME, group=ITEM_TYPE, color=ITEM_TYPE))
#p3+geom_boxplot()

# shows by RELWDINDEX variance in responses: very less
#p4 <- ggplot(data, aes(x= itemOrder,y= WORD_TIME, group=factor(RELWDINDEX), color=factor(RELWDINDEX)))
#p4+geom_boxplot()

head(data_rel)
print(xyplot(WORD_TIME ~ itemOrder | PARTICIPANT, data_rel, aspect = "xy",
             layout = c(8,3), type = c("g", "p", "r"),
             index.cond = function(x,y) coef(lm(y ~ x))[1],
             col.line = "darkorange",
             xlab = "Item Order",
             ylab = "Response time (ms)"))


keep <- !(data_rel$WORD_TIME <100 | data_rel$WORD_TIME >2750)
dataNew_rel <- data_rel[keep,]

str(data_rel)
str(dataNew_rel)

print(xyplot(WORD_TIME ~ itemOrder | PARTICIPANT, dataNew_rel, aspect = "xy",
             layout = c(8,3), type = c("g", "p", "r"),
             index.cond = function(x,y) coef(lm(y ~ x))[1],
             col.line = "darkorange",
             xlab = "Item Order",
             ylab = "Response time (ms)"))


levels(dataNew_rel$ITEM_TYPE)
#cond <- dataNew$RELWDINDEX==0 & dataNew$ITEM_TYPE =="ra"
cond <- dataNew_rel$RELWDINDEX==0 
casted_data<- cast(dataNew_rel[cond,], EXPWORD + ITEM_TYPE ~., fun.aggregate = 'mean', value = 'WORD_TIME')

colnames(casted_data)[3] <- "mean_WORD_TIME"
head(casted_data)

p2 <- ggplot(casted_data, aes(x=EXPWORD, y=mean_WORD_TIME, color=ITEM_TYPE, label=EXPWORD))
p2 + geom_text(size=3)

model_1 <- lmer(WORD_TIME ~ RELWDINDEX + itemOrder
                + (1|ITEM_ID)
                + (1|PARTICIPANT), 
                data = dataNew_rel, REML = F)

summary(model_1)

model_2 <- lmer(WORD_TIME ~ RELWDINDEX + itemOrder + ITEM_TYPE 
                + (1|ITEM_ID)
                + (1|PARTICIPANT), 
                data = dataNew_rel, REML = F)
anova(model_1, model_2)

# Again we have a statistically significant difference so we choose model_2.
# We model the effect of relative word position, item order and the item type on the reading time.
# We also have the random intercept effect for participant and item.

model_mixed_slope_1 <- lmer(WORD_TIME ~ RELWDINDEX + itemOrder + ITEM_TYPE 
                            + (1 + RELWDINDEX|ITEM_ID)
                            + (1 + RELWDINDEX|PARTICIPANT),
                            data = dataNew_rel, REML = F)

model_mixed_slope_2 <- lmer(WORD_TIME ~ RELWDINDEX + itemOrder + ITEM_TYPE 
                            + (1 + RELWDINDEX + ITEM_TYPE|ITEM_ID)
                            + (1 + RELWDINDEX + ITEM_TYPE|PARTICIPANT), 
                            data = dataNew_rel, REML = F)

anova(model_mixed_slope_1, model_mixed_slope_2)

# Now we see that there is not any statistically significant difference between the two random slope-intercept
# models, so we choose model_mixed_slope_1 as it has less complexity.

summary(model_mixed_slope_1)

# So we build on the random intercept model and add the slope conditioning of RELWDINDEX wrt Participant and Item.

print(dotplot(ranef(model_mixed_slope_1,condVar=TRUE),  scales = list(x = list(relation = 'free')))[["PARTICIPANT"]])

## ~~~~~~~~~~~~~~~~~~~~~~~~ ##

# Random intercept only model
# dataNew.mixedMod <- lmer(WORD_TIME ~ EXPWORD +
#                            ITEM_TYPE +
#                            (1|itemOrder) +
#                            (1|RELWDINDEX) +
#                            (1|PARTICIPANT) +
#                            (1|ITEM_ID) , 
#                          data=dataNew, REML=F)
# summary(dataNew.mixedMod)
# 
# dataNew.null <- lmer(WORD_TIME ~ITEM_TYPE +
#                        (1|itemOrder) +
#                        (1|RELWDINDEX) +
#                        (1|PARTICIPANT) +
#                        (1|ITEM_ID) , 
#                      data=dataNew, REML=F)
# summary(dataNew.null)
# 
# anova(dataNew.null,dataNew.mixedMod)
# #Thus, EXPWORD has a significant effect on the response time.
# 
# print(dotplot(ranef(dataNew.mixedMod,condVar=TRUE),  scales = list(x = list(relation = 'free')))[["PARTICIPANT"]])
##############################################