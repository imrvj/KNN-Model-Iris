library(ISLR)
class(iris)
head(iris)

str(iris)
standardize.data<-scale(iris[1:4])
str(iris)
var(standardize.data[,1])
final.data <- cbind(standardize.data,iris[5])
head(final.data)

library(caTools)
set.seed(101)
sample <- sample.split(final.data$Species, SplitRatio = .70)
train<-subset(final.data,sample=T)
test<-subset(final.data,sample=F)


#KNN Model
library(class)
model<-knn(train[1:4],test[1:4],train$Species,k=1)
model

mean(test$Species != model)

model<-NULL
error<-NULL


for(i in 1:10){
  set.seed(101)
  model <- knn(train[1:4],test[1:4],train$Species,k=i)
  error[i] <- mean(test$Species != model)
}

library(ggplot2)
k <- 1:10
error.df <- data.frame(error,k)

pl<-ggplot(error.df,aes(k,error))+geom_point()+geom_line()
pl
  