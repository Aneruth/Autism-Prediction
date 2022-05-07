test <- read.csv('~/Documents/GitHub/Autism-Prediction/Data/test.csv')
train <- read.csv('~/Documents/GitHub/Autism-Prediction/Data/train.csv')
train$Class.ASD <- as.factor(train$Class.ASD)

train_dim <- dim(train) # Dimensions of the train dataset
print(train_dim)

test_dim <- dim(test) # Dimensions of the test dataset
print(test_dim)

head(data.frame(train$age_desc))

# We can say that the columns consist of only one unique value so we might not need it
train <- train[ , !names(train) %in% c('age_desc')]

# Doing the same for test dataset
test <- test[ , !names(test) %in% c('age_desc')]

# Printing the dimensions after reducing the columns
dim(train)
dim(test)

# After analyzing the dataset we can say that column ID,ethnicity,country_res,autism,used_app and relation not required for prediction
clean_dataset <- function(dataset) {
  col_to_remove <- c('ID','ethnicity','contry_of_res','austim','used_app_before','relation')
  for (col in col_to_remove) {
    dataset <- dataset[ , !names(dataset) %in% c(col)]
  }
  return(dataset)
}

train_df <- clean_dataset(train)
test_df <- clean_dataset(test)

labels_rename <- function(dataset){
  # Factorizing the gender label where male is 0 and female is 1
  dataset$gender <- ifelse(dataset$gender == "m",as.numeric(0),dataset$gender)
  dataset$gender <- ifelse(dataset$gender == "f",as.numeric(1),dataset$gender)
  
  # Factorizing the jaundice label where yes is 0 and no is 1
  dataset$jaundice <- ifelse(dataset$jaundice == "yes",as.numeric(0),dataset$jaundice)
  dataset$jaundice <- ifelse(dataset$jaundice == "no",as.numeric(1),dataset$jaundice)
  
  return(dataset)
}

train_df <- labels_rename(train_df)
test_df <- labels_rename(test_df)

write.csv(train_df,"~/Documents/GitHub/Autism-Prediction/Data/train_df.csv", row.names = FALSE)
write.csv(test_df,"~/Documents/GitHub/Autism-Prediction/Data/test_df.csv", row.names = FALSE)

dt = sort(sample(nrow(train_df), nrow(train_df)*.8))
data_train<-train_df[dt,]
data_test<-train_df[-dt,]

library(randomForest)
rfm = randomForest(Class.ASD~.,data = data_train,ntree=100,proximity = TRUE)

Y_pred <- predict(rfm,data_test[,-ncol(data_test)])
Y <- data_test[,ncol(data_test)]

# Building the confusion matrix
confusion_matrix <- table(Y_pred,Y)
confusion_matrix

accuracy_randomForest = sum(diag(confusion_matrix)/sum(confusion_matrix))
paste0('Accuracy Score for random forest using train test split : ',round(accuracy_randomForest*100),'%')

accuracy_vec <- array(0,15)
for (i in 1:15){ 
  model <- randomForest(x=data_train[,-15],
                        y=as.factor(data_train[,15]),
                        xtest=data_test[,-15],
                        ytest=as.factor(data_test[,15]),
                        ntree=i, 
                        importance = TRUE,
                        proximity = TRUE) 
  accuracy_vec[i] = (model$test$confusion[1,1]+model$test$confusion[2,2])/sum(model$test$confusion)
}
accuracy_vec


rfmm = randomForest(Class.ASD~.,data = data_train,ntree=which.max(accuracy_vec), 
                    importance = TRUE,
                    proximity = TRUE)
pred <- predict(rfmm,test_df)
df <- data.frame(pred)

final_sub <- data.frame(test$ID,df$pred)

# Renaming the columns
names(final_sub)[1] <- "ID"
names(final_sub)[2] <- "Class/ASD"

write.csv(final_sub,"~/Documents/GitHub/Autism-Prediction/Data/final_sub_2.csv", row.names = FALSE)
