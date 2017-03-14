library(nnet)   # multinomial regression
library(VGAM)   # multinomial regression

train <- read.csv('train_processed_R.csv', header = T, stringsAsFactors = F, na.strings = "", strip.white=TRUE)


# Variables
OutcomeType <- as.factor(train$OutcomeType)

HasName <- as.factor(train$HasName)
TimeofDay <- as.factor(train$TimeofDay)
WeekDay <- as.factor(train$WeekDay)
Sex <- as.factor(train$Sex)
IsIntact <- as.factor(train$IsIntact)
Age <- as.factor(train$Age)
IsMixBreed <- as.factor(train$IsMixBreed)
MixorMultipleorSimple <- as.factor(train$MixorMultipleorSimple)
Group <- as.factor(train$Group)
Size <- as.factor(train$Size)
IsMixColor <- as.factor(train$IsMixColor)


# Use MixorMultipleorSimple/ not use IsMixBreed
model1 = multinom(OutcomeType ~ HasName+TimeofDay+WeekDay+Sex+IsIntact+Age+MixorMultipleorSimple
                  +Group+Size+IsMixColor)
summary(model1)

model2 = vglm(OutcomeType ~ HasName+TimeofDay+WeekDay+Sex+IsIntact+Age+MixorMultipleorSimple
              +Group+Size+IsMixColor, family = multinomial(refLevel = 1), data = train)
summary(model2)

# Use IsMixBreed/ not use MixorMultipleorSimple
model3 = multinom(OutcomeType ~ HasName+TimeofDay+WeekDay+Sex+IsIntact+Age+IsMixBreed
                  +Group+Size+IsMixColor)
summary(model3)

model4 = vglm(OutcomeType ~ HasName+TimeofDay+WeekDay+Sex+IsIntact+Age+IsMixBreed
              +Group+Size+IsMixColor, family = multinomial(refLevel = 1), data = train)
summary(model4)
