library(magrittr)
library(dplyr)
library(ggplot2)
library(nlme)
library(smbinning)
library(InformationValue)
wdf <- train <- read.csv("C:/Users/Dhruba/Desktop/GitHub/R - GitHub/Kaggle/Titanic/train.csv")

colnames(train)

par(mfrow=c(1,1))
hist(train$Survived, col="red")
hist(train$Pclass, col="red")
barplot(table(train$Sex), col="red")
hist(train$Age, col="red")
hist(train$SibSp, col="red")
hist(train$Parch, col="red")
hist(train$Fare, col="red")
hist(train$cabin, col="red")


#===============================================
# Does Cabin Play a Role
#===============================================

cabin <- train %>%
  select(c("Survived","Cabin")) %>%
  mutate(cabintrue = ifelse(train$Cabin=="",0,1))

cabin <- subset(cabin[cabin$cabintrue == 1,]) %>%
  select(c("Survived","Cabin"))

par(mfrow=c(1,2))
hist(train$Survived, col="darkgreen")
hist(cabin$Survived, col="darkgreen")

actual_survival_rate = round(sum(train$Survived)/nrow(train),3)
cabin_survival_rate = round(sum(cabin$Survived)/nrow(cabin),3)

print(paste0("Proportion of Actual Survived is ", round(sum(train$Survived)/nrow(train),3)))
print(paste0("Proportion of Survived if Cabin is ", round(sum(cabin$Survived)/nrow(cabin),3)))

#===============================================
## Does Socio-Economic Class play a role
#===============================================

TicketClass <- train %>%
  select(c("Survived","Pclass"))

T1 <- subset(TicketClass[TicketClass$Pclass == 1,])
T2 <- subset(TicketClass[TicketClass$Pclass == 2,])
T3 <- subset(TicketClass[TicketClass$Pclass == 3,])


par(mfrow=c(2,2))
hist(train$Survived, col="darkgreen")
hist(T1$Survived, col="darkgreen")
hist(T2$Survived, col="darkgreen")
hist(T3$Survived, col="darkgreen")

print(paste0("Proportion of Actual Survived is ", round(sum(train$Survived)/nrow(train),3)))
print(paste0("Proportion of Survived if Upper Class is ", round(sum(T1$Survived)/nrow(T1),3)))
print(paste0("Proportion of Survived if Middle Class is ", round(sum(T2$Survived)/nrow(T2),3)))
print(paste0("Proportion of Survived if Poor Class is ", round(sum(T3$Survived)/nrow(T3),3)))

#===============================================
## Does SEX play a role
#===============================================

Sex <- train %>%
  select(c("Survived","Sex"))

M <- Sex[Sex$Sex=="male",]
F <- Sex[Sex$Sex=="female",]

par(mfrow=c(1,3))
hist(train$Survived, col="darkgreen")
hist(M$Survived, col="darkgreen")
hist(F$Survived, col="darkgreen")

print(paste0("Proportion of Actual Survived is ", round(sum(train$Survived)/nrow(train),3)))
print(paste0("Proportion of Survived if Male is ", round(sum(M$Survived)/nrow(M),3)))
print(paste0("Proportion of Survived if Female is ", round(sum(F$Survived)/nrow(F),3)))

#===============================================
## Does AGE play a role: Kids, Working Adults, Seniors
#===============================================

agedf <- na.omit(train)
agedf <- agedf %>%
  select(c("Survived","Age")) %>%
  mutate(dummy = ifelse(agedf$Age <= 18,1,
                           ifelse(agedf$Age <= 60,2,3))) %>%
  select(c("Survived","dummy"))


A1 <- agedf[agedf$dummy == 1,]
A2 <- agedf[agedf$dummy == 2,]
A3 <- agedf[agedf$dummy == 3,]

par(mfrow=c(2,2))
hist(train$Survived, col="darkgreen")
hist(A1$Survived, col="darkgreen")
hist(A2$Survived, col="darkgreen")
hist(A3$Survived, col="darkgreen")

print(paste0("Proportion of Actual Survived is ", round(sum(train$Survived)/nrow(train),3)))
print(paste0("Proportion of Survived if Minor is ", round(sum(A1$Survived)/nrow(A1),3)))
print(paste0("Proportion of Survived if Working Adult is ", round(sum(A2$Survived)/nrow(A2),3)))
print(paste0("Proportion of Survived if Senior is ", round(sum(A3$Survived)/nrow(A3),3)))

#===============================================
## Does port of Embarkation play a role
#===============================================
E <- train %>%
  select(c("Survived","Embarked"))


E1 <- E[E$Embarked == "S",]
E2 <- E[E$Embarked == "C",]
E3 <- E[E$Embarked == "Q",]

par(mfrow=c(2,2))
hist(train$Survived, col="darkgreen")
hist(E1$Survived, col="darkgreen")
hist(E2$Survived, col="darkgreen")
hist(E3$Survived, col="darkgreen")

print(paste0("Proportion of Actual Survived is ", round(sum(train$Survived)/nrow(train),3)))
print(paste0("Proportion of Survived if Southhampton is ", round(sum(E1$Survived)/nrow(E1),3)))
print(paste0("Proportion of Survived if Cherbourgh is ", round(sum(E2$Survived)/nrow(E2),3)))
print(paste0("Proportion of Survived if Queenstown is ", round(sum(E3$Survived)/nrow(E3),3)))

##########################
# Model Building
##########################

# A split of the Test-Train Dataset

set.seed(123)  # setting seed to reproduce results of random sampling
train <- wdf
train <- train %>%
  mutate(Cabindummy = ifelse(train$Cabin=="",0,1))

train <- train %>%
  mutate(agedummy = ifelse(train$Age <= 18,1,
                        ifelse(train$Age <= 60,2,3)))

train$agedummy[is.na(train$agedummy)] <- 4 

trainingRowIndex <- sample(1:nrow(train), 0.8*nrow(train))  # row indices for training data
trainingData <- train[trainingRowIndex, ]  # model training data
testData  <- train[-trainingRowIndex, ]   # test data

# Some checks
print(paste0("Proportion of Survived in Training Data is ", round(sum(trainingData$Survived)/nrow(trainingData),3)))

print(paste0("Proportion of Survived in Test Data is ", round(sum(testData$Survived)/nrow(testData),3)))
# We can make this to be balanced to be about 50%. But a ~40% is okay to proceed with


# Variables like having Parents or Siblings should not be major determinants in probability of survival
# Checking Significance of variables
library(smbinning)
# segregate continuous and factor variables
factor_vars <- c ("Pclass", "Sex", "Embarked", "Cabindummy","agedummy")
continuous_vars <- c("SibSp", "Parch", "Fare")

iv_df <- data.frame(VARS=c(factor_vars, continuous_vars), IV=numeric(8))  # init for IV results

# compute IV for categoricals
for(factor_var in factor_vars){
  smb <- smbinning.factor(trainingData, y="Survived", x=factor_var)  # WOE table
  if(class(smb) != "character"){ # heck if some error occured
    iv_df[iv_df$VARS == factor_var, "IV"] <- smb$iv
  }
}

# compute IV for continuous vars
for(continuous_var in continuous_vars){
  smb <- smbinning(trainingData, y="Survived", x=continuous_var)  # WOE table
  if(class(smb) != "character"){  # any error while calculating scores.
    iv_df[iv_df$VARS == continuous_var, "IV"] <- smb$iv
  }
}

iv_df <- iv_df[order(-iv_df$IV), ]  # sort
iv_df




#=============================================
# Let us build the easiest Model: Logistic Regression
#=============================================

logit.model <- glm(Survived ~ Pclass + Sex + agedummy + Cabindummy + Embarked + Fare, data=trainingData, family=binomial(link="logit"))
summary(logit.model)
# Only Pclass, Sex and Agedummy are significant. Let us reduce champion model

logit.model <- glm(Survived ~ Pclass + Sex + agedummy + Cabindummy , data=trainingData, family=binomial(link="logit"))
summary(logit.model)

predicted.prob <- predict(logit.model, testData, type="response")


library(InformationValue)
optCutOff <- optimalCutoff(testData$Survived, predicted.prob)[1] 
print (paste0("The optimized cutoff is ", optCutOff))

prediction <- rep(0,nrow(testData))
prediction[predicted.prob > optCutOff] = 1

testData$Survived
prediction

mis.error <- misClassError(testData$Survived, predicted.prob, threshold = optCutOff)
print (paste0("The Misclassification error is ", mis.error))

plotROC(testData$Survived, prediction)
confusionMatrix(testData$Survived, prediction, threshold = optCutOff)


## Actual Prediction
wdf_test <- test <- read.csv("C:/Users/Dhruba/Desktop/GitHub/R - GitHub/Kaggle/Titanic/test.csv")

test <- test %>%
  mutate(Cabindummy = ifelse(test$Cabin=="",0,1))

test <- test %>%
  mutate(agedummy = ifelse(test$Age <= 18,1,
                           ifelse(test$Age <= 60,2,3)))

test$agedummy[is.na(test$agedummy)] <- 4



model <- glm(Survived ~ Pclass + Sex + agedummy + Cabindummy , data=train, family=binomial(link="logit"))
summary(logit.model)

predicted.prob <- predict(model, test, type="response")
optCutOff

prediction <- rep(0,nrow(test))
prediction[predicted.prob > optCutOff] = 1

final <- cbind(test,Survived = prediction)
sum(final$Survived)/nrow(final)

final.df <- final %>%
  select(c("PassengerId", "Survived" ))

write.csv(final.df, "C:/Users/Dhruba/Desktop/GitHub/R - GitHub/Kaggle/Titanic/Model1.csv")
