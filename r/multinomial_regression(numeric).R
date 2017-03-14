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
Age <- train$Age
IsMixBreed <- as.factor(train$IsMixBreed)
MixorMultipleorSimple <- as.factor(train$MixorMultipleorSimple)
Group <- as.factor(train$Group)
Size <- train$Size
IsMixColor <- as.factor(train$IsMixColor)

model2 = vglm(OutcomeType ~ HasName+TimeofDay+WeekDay+Sex+IsIntact+Age+MixorMultipleorSimple
              +Group+Size+IsMixColor, family = multinomial(refLevel = "Adoption"), data = train)
summary(model2)

# Just for interest, calculating t-stats
model1 = multinom(OutcomeType ~ HasName+TimeofDay+WeekDay+Sex+IsIntact+Age+MixorMultipleorSimple
                  +IsMixBreed+Group+Size+IsMixColor)
#summary(model1)
t <- summary(model1)$coefficients/summary(model1)$standard.errors
t


# R squared
model = vglm(OutcomeType ~ 1, family = multinomial(refLevel = 1), data = train)
lrtest(model2, model)
LLf1 <- VGAM::logLik(model2)
as.vector(1 - (LLf1/LL0))

# Confident interval
confint(model2)

# Predict
p <- predict(model, newdata = test, "probs")

# Use MixorMultipleorSimple/ not use IsMixBreed


#lrtest(model1, model)
#LLf <- VGAM::logLik(model1)
#LL0 <- VGAM::logLik(model)
#as.vector(1 - (LLf/LL0))



# # Use IsMixBreed/ not use MixorMultipleorSimple
# model3 = multinom(OutcomeType ~ HasName+TimeofDay+WeekDay+Sex+IsIntact+Age+IsMixBreed
#                   +Group+Size+IsMixColor)
# summary(model3)
# 1 - pchisq(30145.06, 30345.06)
# 
# model4 = vglm(OutcomeType ~ HasName+TimeofDay+WeekDay+Sex+IsIntact+Age+IsMixBreed
#               +Group+Size+IsMixColor, family = multinomial(refLevel = 1), data = train)
# summary(model4)
# 
# model5 = vglm(OutcomeType ~ 1, family = multinomial(refLevel = 1), data = train)
# lrtest(model5)
# LLf <- VGAM::logLik(model4)
# LL0 <- VGAM::logLik(model5)
# as.vector(1 - (LLf/LL0))
# 
# 
# model6 = vglm(OutcomeType ~ HasName+TimeofDay+WeekDay+Sex+IsIntact+Age+IsMixBreed
#               +Group+Size, family = multinomial(refLevel = 1), data = train)
# summary(model6)
# LLf <- VGAM::logLik(model6)
# as.vector(1 - (LLf/LL0))
# 
# 
# model7 = vglm(OutcomeType ~ HasName+TimeofDay+WeekDay+Sex+IsIntact+Age+IsMixBreed
#               +Group, family = multinomial(refLevel = 1), data = train)
# summary(model7)
# LLf <- VGAM::logLik(model7)
# as.vector(1 - (LLf/LL0))
# 
# model8 = vglm(OutcomeType ~ HasName+TimeofDay+WeekDay+Sex+IsIntact+Age+IsMixBreed, family = multinomial(refLevel = 1), data = train)
# summary(model8)
# LLf <- VGAM::logLik(model8)
# as.vector(1 - (LLf/LL0))
# 
# library(mnlogit)
# model = mnlogit(OutcomeType ~ HasName | TimeofDay | WeekDay|Sex|IsIntact|Age|MixorMultipleorSimple
#                 |IsMixBreed|Group|Size|IsMixColor, data= train,choiceVar = "alt")
# summary(model)
