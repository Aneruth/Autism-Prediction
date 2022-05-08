train_df <- read.csv("~/Documents/GitHub/Autism-Prediction/Data/train_df.csv")
test_df <- read.csv("~/Documents/GitHub/Autism-Prediction/Data/test_df.csv")

# Split the dataset
dt = sort(sample(nrow(train_df), nrow(train_df)*.8))
data_train<-train_df[dt,]
data_test<-train_df[-dt,]

# SVM package
library(e1071)
set.seed(123)

svm_before_hyper <- function(){
  svmCls = svm(formula = Class.ASD~.,data = data_train,type = 'C-classification',kernel = 'linear')
  
  Y_pred <- predict(svmCls,data_test[,-ncol(data_test)])
  Y <- data_test[,ncol(data_test)]
  
  # Building the confusion matrix
  confusion_matrix <- table(Y_pred,Y)
  confusion_matrix
  accuracy_randomForest = sum(diag(confusion_matrix)/sum(confusion_matrix))
  paste0('Accuracy Score for random forest using train test split : ',round(accuracy_randomForest*100),'%')
  
  # Final Prediction
  pred <- predict(svmCls,test_df)
  df <- data.frame(pred)
  
  # To fetch ID we load the test dataset
  test <- read.csv('~/Documents/GitHub/Autism-Prediction/Data/test.csv')
  
  svm_sub <- data.frame(test$ID,df$pred)
  
  # Renaming the columns
  names(svm_sub)[1] <- "ID"
  names(svm_sub)[2] <- "Class/ASD"
  
  write.csv(svm_sub,"~/Documents/GitHub/Autism-Prediction/Data/svm_sub.csv", row.names = FALSE)  
}

svm_before_hyper()

svm_after_hyper <- function(){
  
}
