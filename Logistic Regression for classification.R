train.titanic <- read.csv('titanic_train.csv')
head(train.titanic)
str(train.titanic)

install.packages('Amelia')
library(Amelia)
??missmap

# visualization of missing values
missmap(train.titanic, main='Missing value visualization', col = c('yellow', 'black'),legend=F)

# data EDA
library(ggplot2)
ggplot(train.titanic, aes(Survived)) +geom_bar()
ggplot(train.titanic, aes(Pclass)) +geom_bar(aes(fill=factor(Pclass)))
ggplot(train.titanic, aes(Sex)) +geom_bar(aes(fill=factor(Sex))) + labs(fill='Sex')

ggplot(train.titanic, aes(Age)) +geom_histogram(alpha=0.5, bins=30, color='black', fill='blue')
ggplot(train.titanic, aes(SibSp)) +geom_bar() # majority of ppl are single
ggplot(train.titanic, aes(Fare)) +geom_histogram(alpha=0.5, bins=30, color='black', fill='blue')
ggplot(train.titanic, aes(Pclass, Age)) + geom_boxplot(aes(group=Pclass, fill=factor(Pclass)), alpha=0.4) + scale_y_continuous(breaks = seq(min(0), max(80), by=2))+theme_bw()

impute.missing.age <- function(age, class){
  out <- age
  for (i in 1:length(age)){
    if(is.na(out[i])){
      if(class[i]==1){
        out[i] <- 37
      }else if(class[i]==2){
        out[i] <- 29
      }else {
        out[i] <- 24
      }
    }else{
      out[i] <- age[i] 
    }
  }
  return(out)
}

imputed.age <- impute.missing.age(train.titanic$Age, train.titanic$Pclass)
train.titanic$Age <- imputed.age

# now, let's go back to check the missmap of our data
print(missmap(train.titanic, main='Missing value visualization', col = c('yellow', 'black'),legend=F))

# feature engineering 
library('dplyr')

df.train <- select(train.titanic, -PassengerId, -Name, -Ticket, -Cabin)
head(df.train)
str(df.train)

df.train$Survived <- factor(df.train$Survived)
df.train$Pclass <- factor(df.train$Pclass)
df.train$Parch <- factor(df.train$Parch)
df.train$SibSp <- factor(df.train$SibSp)
str(df.train)

# build the logistic model as a example
log.model <- glm(Survived~., family = binomial(link = logit), df.train)
summary(log.model)

# split the dataset
set.seed(101)
sample <- sample.split(df.train, SplitRatio = 0.7) 
final.train <- subset(df.train, sample=T)
final.test <- subset(df.train, sample=F)

# build the real model

log.real <- glm(Survived~., family = binomial(link = logit), final.train)
summary(log.real)

# make predictions

pred.probability <- predict(log.real, final.test, type='response')
result <- ifelse(pred.probability>0.5, 1, 0)
misclasserror <- mean(result!= final.test$Survived)
print(1-misclasserror)

table(result, final.test$Survived)







 