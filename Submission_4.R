# Reading the dataset
train_df <- read.csv("~/Documents/GitHub/Autism-Prediction/Data/train.csv")
test_df <- read.csv("~/Documents/GitHub/Autism-Prediction/Data/test.csv")
train_df$Class.ASD <- as.factor(train_df$Class.ASD)

clean_dataset <- function(dataset) {
  col_to_remove <- c('ID','ethnicity','contry_of_res','used_app_before','relation','age_desc')
  for (col in col_to_remove) {
    dataset <- dataset[ , !names(dataset) %in% c(col)]
  }
  return(dataset)
}

train_df <- clean_dataset(train_df)
test_df <- clean_dataset(test_df)

labels_rename <- function(dataset){
  # Factorizing the gender label where male is 0 and female is 1
  dataset$gender <- ifelse(dataset$gender == "m",as.numeric(0),dataset$gender)
  dataset$gender <- ifelse(dataset$gender == "f",as.numeric(1),dataset$gender)
  
  # Factorizing the jaundice label where yes is 0 and no is 1
  dataset$jaundice <- ifelse(dataset$jaundice == "yes",as.numeric(0),dataset$jaundice)
  dataset$jaundice <- ifelse(dataset$jaundice == "no",as.numeric(1),dataset$jaundice)
  
  # Factorizing the jaundice label where yes is 0 and no is 1
  dataset$austim <- ifelse(dataset$austim == "yes",as.numeric(0),dataset$austim)
  dataset$austim <- ifelse(dataset$austim == "no",as.numeric(1),dataset$austim)
  
  return(dataset)
}

train_df <- labels_rename(train_df)
test_df <- labels_rename(test_df)

# Naive bayes algorithm
nb <- function(){
  library(e1071)
  
  set.seed(120)  # Setting Seed
  classifier_cl <- naiveBayes(Class.ASD ~ ., data = data_train)
  classifier_cl
  
  y_pred <- predict(classifier_cl, test_df)
  
  # Confusion Matrix
  cm <- table(test_df, y_pred)
  cm
  
  library(caTools)
  library(caret)
  # Model Evaluation
  confusionMatrix(cm)
  
  test <- read.csv('~/Documents/GitHub/Autism-Prediction/Data/test.csv')
  sub_4 <- data.frame(y_pred)
  sub_4 <- data.frame(test$ID,sub_4$y_pred)
  
  # Renaming the columns
  names(sub_4)[1] <- "ID"
  names(sub_4)[2] <- "Class/ASD"
  
  write.csv(sub_4,"~/Documents/GitHub/Autism-Prediction/Sub_data/NB_sub.csv", row.names = FALSE)  
}


# Splitting the dataset into train test with 80:20 ratio
dt = sort(sample(nrow(train_df), nrow(train_df)*.75))
data_train<-train_df[dt,]
data_test<-train_df[-dt,]

# Random Forest
library(randomForest)

rfm = randomForest(Class.ASD~.,data = data_train,ntree=100)

Y_pred <- predict(rfm,data_test[,-ncol(data_test)])
Y <- data_test[,ncol(data_test)]

# Building the confusion matrix
confusion_matrix <- table(Y_pred,Y)
confusion_matrix

accuracy_randomForest = sum(diag(confusion_matrix)/sum(confusion_matrix))
paste0('Accuracy Score for random forest using train test split : ',round(accuracy_randomForest*100),'%')

pred <- predict(rfm,test_df)
df <- data.frame(pred)

test <- read.csv('~/Documents/GitHub/Autism-Prediction/Data/test.csv')

final_sub <- data.frame(test$ID,df$pred)

# Renaming the columns
names(final_sub)[1] <- "ID"
names(final_sub)[2] <- "Class/ASD"

# write.csv(final_sub,"~/Documents/GitHub/Autism-Prediction/Sub_data/rf_sub_intro.csv", row.names = FALSE)

hyperPara <- function(){
  accuracy_vec <- array(0,16)
  for (i in 1:16){ 
    model <- randomForest(x=data_train[,-16],
                          y=as.factor(data_train[,16]),
                          xtest=data_test[,-16],
                          ytest=as.factor(data_test[,16]),
                          ntree=i, 
                          importance = TRUE,
                          proximity = TRUE) 
    accuracy_vec[i] = (model$test$confusion[1,1]+model$test$confusion[2,2])/sum(model$test$confusion)
    }
  return(accuracy_vec)
  }

accuracy_vec = hyperPara()

rfmm = randomForest(Class.ASD~.,data = data_train,ntree=which.max(accuracy_vec), 
                    importance = TRUE,
                    proximity = TRUE)
pr <- predict(rfmm,test_df)

table(pr,Y)



