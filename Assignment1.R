#install.packages('caret')
#install.packages('car')
#install.packages('Hmisc')
#install.packages('pROC')
#install.packages('ggplot2')
#install.packages('rpart')
library(caret)
library(car)
library(Hmisc)
library(ggplot2)
library(pROC)
library(rpart)
library(rpart.plot)

dfSM <- read.csv('Downloads/inq2015.csv',na.strings=c('NA',''))
summary(dfSM)
str(dfSM)

# Remove some predictors
dfSM <- subset(dfSM, select = -c(ETHNICITY,
                                 ACADEMIC_INTEREST_1,
                                 ACADEMIC_INTEREST_2,
                                 CONTACT_DATE,
                                 CONTACT_CODE1,
                                 LEVEL_YEAR,
                                 IRSCHOOL,
                                 satscore,
                                 sex,
                                 telecq))

summary(dfSM)
str(dfSM)

# Change data types
dfSM$Enroll <- factor(dfSM$Enroll)
dfSM$Instate <- factor(dfSM$Instate)
dfSM$premiere <- factor(dfSM$premiere)
dfSM$stucell <- factor(dfSM$stucell)
dfSM$CAMPUS_VISIT <- factor(dfSM$CAMPUS_VISIT)
dfSM$TERRITORY <- factor(dfSM$TERRITORY)
dfSM$interest <- factor(dfSM$interest)
dfSM$mailq <- factor(dfSM$mailq)

summary(dfSM)
str(dfSM)

# Test multicollinearity
vif(glm(formula=Enroll~.,family= binomial(link='logit'),data=dfSM))

dfSM <- subset(dfSM, select = -c(TOTAL_CONTACTS,
                                 TRAVEL_INIT_CNTCTS,
                                 SOLICITED_CNTCTS,
                                 REFERRAL_CNTCTS
                                 ))

vif(glm(formula=Enroll~.,family= binomial(link='logit'),data=dfSM))

# Decision Tree

set.seed(101) # set random seed

# Data partition
trainIndex <- createDataPartition(dfSM$Enroll,
                                  p=0.7,
                                  list=FALSE,
                                  times=1)
# Create Training Data
dfSM.train <- dfSM[trainIndex,]

# Create Validation data
dfSM.valid <- dfSM[-trainIndex,]

# Build decision tree model
treeSM.model <- train(Enroll~.,
                      data=dfSM.train,
                      method="rpart",
                      na.action=na.pass)
treeSM.model

# Display decision tree plot
prp(treeSM.model$finalModel,type=2,extra=106)

# Evaluation model performance using validation dataset

# Criteria 1: confusion matrix
prediction <- predict(treeSM.model,newdata=dfSM.valid,na.action=na.pass)
confusionMatrix(prediction,dfSM.valid$Enroll)

# Criteria 2: ROC curve and AUC
tree.probabilities <- predict(treeSM.model,newdata=dfSM.valid,type='prob',na.action=na.pass)
tree.ROC <- roc(predictor=tree.probabilities$'1',
                response=dfSM.valid$Enroll,
                levels=levels(dfSM.valid$Enroll))
plot(tree.ROC)
tree.ROC$auc

# Baseline Logistic Regression

# Build baseline logistic regression model
baselineSM.model <- train(Enroll~.,
                            data=dfSM.train,
                            method='glm',
                            family='binomial',
                            na.action=na.pass)
summary(baselineSM.model)

# Evaluation model performance using validation dataset

# Criteria 1: confusion matrix
prediction <- predict(baselineSM.model,newdata=dfSM.valid)
dfSM.valid.nonmissing <- na.omit(dfSM.valid) # remove missing values from validation set for evaluation
confusionMatrix(prediction,dfSM.valid.nonmissing$Enroll)

#Criteria 2: ROC curve and AUC
pred.probabilities <- predict(baselineSM.model,newdata=dfSM.valid,type='prob')

regression.ROC <- roc(predictor=pred.probabilities$'1',
                      response=dfSM.valid.nonmissing$Enroll,
                      levels=levels(dfSM.valid.nonmissing$Enroll))
plot(regression.ROC)
regression.ROC$auc


# Improved Logistic Regression

# impute missing values
dfSM$TERRITORY <- with(dfSM,impute(TERRITORY,max))
dfSM$avg_income <- with(dfSM,impute(avg_income,mean))
dfSM$distance <- with(dfSM,impute(distance,mean))

summary(dfSM)
str(dfSM)

pl1 <- ggplot(dfSM,aes(x=SELF_INIT_CNTCTS))+geom_histogram()
pl1
pl2 <- ggplot(dfSM,aes(x=init_span))+geom_histogram()
pl2
pl3 <- ggplot(dfSM,aes(x=int1rat))+geom_histogram()
pl3
pl4 <- ggplot(dfSM,aes(x=int2rat))+geom_histogram()
pl4
pl5 <- ggplot(dfSM,aes(x=hscrat))+geom_histogram()
pl5
# regroup highly skewed variable hscrat
combine.hscrat <- function(x) {
  if(x>.033) {
    return("High")
  }
  else{
    return("Low")
  }
}
dfSM$hscrat <- sapply(dfSM$hscrat,combine.hscrat)
dfSM$hscrat <- factor(dfSM$hscrat)
pl6 <- ggplot(dfSM,aes(x=avg_income))+geom_histogram()
pl6
pl7 <- ggplot(dfSM,aes(x=distance))+geom_histogram()
pl7
# log transformation for highly skewed variable distance
dfSM$distance <- log10(dfSM$distance+1)
pl7 <- ggplot(dfSM,aes(x=distance))+geom_histogram()
pl7

summary(dfSM)
str(dfSM)

# Data partition
trainIndex <- createDataPartition(dfSM$Enroll,
                                  p=0.7,
                                  list=FALSE,
                                  times=1)
# Create training data
dfSM.train <- dfSM[trainIndex,]
#Create validation data
dfSM.valid <- dfSM[-trainIndex,]

# Build improved logistic regression model
regressionSM.model <- train(Enroll~.,
                            data=dfSM.train,
                            method='glm',
                            family='binomial',
                            na.action=na.pass)
summary(regressionSM.model)

# Evaluation model performance using validation dataset

# Criteria 1: confusion matrix
prediction <- predict(regressionSM.model,newdata=dfSM.valid)
confusionMatrix(prediction,dfSM.valid$Enroll)

#Criteria 2: ROC curve and AUC
pred.probabilities <- predict(regressionSM.model,newdata=dfSM.valid,type='prob')

regression.ROC <- roc(predictor=pred.probabilities$'1',
                      response=dfSM.valid$Enroll,
                      levels=levels(dfSM.valid$Enroll))
plot(regression.ROC)
regression.ROC$auc