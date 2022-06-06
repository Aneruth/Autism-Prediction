test <- read.csv('/Users/aneruthmohanasundaram/Desktop/test_data.csv')
train <- read.csv('/Users/aneruthmohanasundaram/Desktop/train_data.csv')
train$Class.ASD <- as.factor(train$Class.ASD)

# Splitting the dataset into train test with 80:20 ratio
dt = sort(sample(nrow(train), nrow(train)*.8))
data_train<-train[dt,]
data_test<-train[-dt,]

library(randomForest)

rfm = randomForest(Class.ASD~.,data = data_train,ntree=100,proximity = TRUE)

Y_pred <- predict(rfm,data_test[,-ncol(data_test)])
Y <- data_test[,ncol(data_test)]

# Building the confusion matrix
confusion_matrix <- table(Y_pred,Y)
confusion_matrix

accuracy_randomForest = sum(diag(confusion_matrix)/sum(confusion_matrix))
paste0('Accuracy Score for random forest using train test split : ',round(accuracy_randomForest*100),'%')

accuracy_vec <- array(0,dim(data_train)[2])
for (i in 1:dim(data_train)[2]){ 
  model <- randomForest(x=data_train[,-dim(data_train)[2]],
                        y=as.factor(data_train[,dim(data_train)[2]]),
                        xtest=data_test[,-dim(data_train)[2]],
                        ytest=as.factor(data_test[,dim(data_train)[2]]),
                        ntree=i, 
                        importance = TRUE,
                        proximity = TRUE) 
  accuracy_vec[i] = (model$test$confusion[1,1]+model$test$confusion[2,2])/sum(model$test$confusion)
}











