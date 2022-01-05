install.packages("caret")
install.packages('caret', dependencies=c('Depends', 'Suggests'))
library(caret)
#load from csv
#olakunle <-read.csv("C:/Users/Desktop/RUG/Iris.csv")
#load the dataset to your environment 
data(iris)
#rename the dataset
olakunle <- iris

#set the column names in the dataset
colnames(olakunle) <- c('Sepal.Length', 'Sepal.Width','Petal.Length', 'Petal.Width','Species')
#create a list of 75% of the rows in the original dataset we can use for training our model
validation_index <- createDataPartition(olakunle$Species, p=0.75, list=FALSE)
#select 25% of the data for validation
validation <- olakunle[-validation_index,]
#use the remaining 75% for training and testing the model
olakunle <- olakunle[validation_index,]
#dimension of the dataset
dim(olakunle)
#list type of each attribute
sapply(olakunle, class)
#take a peek at the first 5 rows of the dataset 
head(olakunle)
#list the level for the class
levels(olakunle$Species)
#summarize the class distribution
percentage <- prop.table(table(olakunle$Species)) *100
cbind(freq=table(olakunle$Species), percentage=percentage)
#summarize the attribute distributions
summary(olakunle)
#split input and output
x <- olakunle[,1:4]
y <- olakunle[,5]

#boxplot for each attribute on one image
par(mfrow=c(1,4))
for (i in 1:4) {
  boxplot(x[,i], main=names(iris)[i])
}

plot(y)

#scatterplot matrix
featurePlot(x=x, y=y, plot='ellipse')
#box and whisker plots for each attribute
featurePlot(x=x, y=y, plot='box')
#density plots for each attribute by class value
scales <-list(x=list(relation='free'), y=list(relation='free'))
featurePlot(x=x, y=y, plot='density', scales=scales)
#Testing Harness
control <- trainControl(method='cv', number=10)
metric <-'Accuracy'

#Building Models
#linear algorithms 
set.seed(7)
fit.lda <- train(Species~., data=olakunle, method='lda', metric=metric, trControl=control)
#CART	
set.seed(7)
fit.cart <- train(Species~., data=olakunle, method='rpart', metric=metric, trControl=control)
#KNN
set.seed(7)
fit.knn <- train(Species~., data=olakunle, method='knn', metric=metric, trControl=control)
#SVM
set.seed(7)
fit.svm <- train(Species~., data=olakunle, method='svmRadial', metric=metric, trControl=control)
#Random Forest
set.seed(7)
fit.rf <- train(Species~., data=olakunle, method='rf', metric=metric, trControl=control)

#summarize accuracy of all models
results <- resamples(list(lda=fit.lda, cart=fit.cart, knn=fit.knn, svm=fit.svm, rf=fit.rf))
summary(results)
#compare accuracy of models
dotplot(results)
#summarize the best model
print(fit.knn)

#estimate skill of LDA on the validation dataset
predictions <- predict(fit.knn, validation)
confusionMatrix(predictions, validation$Species)