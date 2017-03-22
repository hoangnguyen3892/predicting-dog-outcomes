#Read file
train <-read.csv("input/train.csv",header = T)
is.na(data)

#Variables
OutcomeType <- factor(train[,2])			    

Age <- train[,3]
Size <- train[,4]

HasName <- factor(train[,5])

train$TimeofDay <- relevel(train$TimeofDay, ref = "lateday")
TimeofDay <- factor(train$TimeofDay)

train$WeekDay <- relevel(train$WeekDay, ref = "Sunday")
WeekDay <- factor(train$WeekDay)

IsIntact <- factor(train[,8])

Sex <- factor(train[,9])

train$PureMixedHybrid <- relevel(train$PureMixedHybrid, ref = "Mixed")
PureMixedHybrid <- factor(train$PureMixedHybrid)

Group <- factor(train[,11])
relevel(Group, ref="Toy")

IsMixeColor <- factor(train[,12])

#--------------Multinomial Regression--------------
#install.packages("nnet")
library(nnet)
model1 <- multinom(OutcomeType~Age+Size+HasName+TimeofDay+WeekDay+IsIntact
                   +Sex+PureMixedHybrid+Group+IsMixeColor, data = train)
summary(model1)


# Other package
#install.packages("VGAM")
library(VGAM)
model2 <- vglm(OutcomeType~Age+Size+HasName+TimeofDay+WeekDay+IsIntact
               +Sex+PureMixedHybrid+Group+IsMixeColor, family=multinomial(refLevel = 'Adoption'), data=train)
summary(model2)

#--------------Posthoc Analysis--------------

kruskal.test(OutcomeType ~ WeekDay, data = train)
library(MASS)
tbl = table(OutcomeType, WeekDay)
chisq.test(tbl)
library("PMCMR")
posthoc.kruskal.nemenyi.test(x=OutcomeType, g=WeekDay, method="Tukey")
posthoc.kruskal.nemenyi.test(x=OutcomeType, g=WeekDay, method="Chisq")



