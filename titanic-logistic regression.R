
getwd()
setwd ("C:\\Users\\admin\\Desktop\\DS with R\\Kaggle")

titanic_train<- read.csv("train.csv")
titanic_test <- read.csv(("test.csv"))

str(titanic_train)
#Survived,Pclass,Sibsp,Parch,embarked
summary(titanic_train)

str(titanic_test)
summary(titanic_test)

#Missing value imputation
sum(is.na(titanic_train$Age))
titanic_train$Age[is.na(titanic_train$Age)==TRUE] <-median(titanic_train$Age,na.rm=TRUE)

sum(is.na(titanic_test$Age))
titanic_test$Age[is.na(titanic_test$Age)==TRUE] <-median(titanic_test$Age,na.rm=TRUE)

sum(is.na(titanic_test$Fare))
titanic_test$Fare[is.na(titanic_test$Fare)==TRUE] <-median(titanic_test$Fare,na.rm=TRUE)

# Embarked
titanic_train[titanic_train$Embarked =='',"Embarked"]  <-'S'

summary(titanic_train)
summary(titanic_test)

str(titanic_train)

#creating dummy variables
titanic_train$female=ifelse(titanic_train$Sex=='female',1,0)
titanic_train$Embarked_S=ifelse(titanic_train$Embarked=='S',1,0)
titanic_train$Embarked_C=ifelse(titanic_train$Embarked=='C',1,0)

titanic_train_new<-titanic_train[,-c(4,5,12)]
summary(titanic_train_new)

titanic_test$female=ifelse(titanic_test$Sex=='female',1,0)
titanic_test$Embarked_S=ifelse(titanic_test$Embarked=='S',1,0)
titanic_test$Embarked_C=ifelse(titanic_test$Embarked=='C',1,0)

titanic_test_new<-titanic_test[,-c(3,4,11)]
summary(titanic_test_new)

#cor(titanic_train_new)

mod1 <- glm(Survived~.,data=titanic_train_new,family="binomial")
mod1
summary(mod1)

step(mod1)
str(titanic_train)

mod2 <- glm(Survived ~  Pclass + Age+ SibSp  + Embarked_S +female, 
            family="binomial", data=titanic_train)
summary(mod2)



Survived=predict(mod2, titanic_test_new)

PassengerId <-titanic_test_new$PassengerId

output_df <-as.data.frame(PassengerId)
output_df$Survived <-ifelse(Survived>0.05,1,0)

head(output_df)

write.csv(output_df,"titanic_kaggle.csv", row.names = FALSE)
