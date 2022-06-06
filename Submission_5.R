train_df <- read.csv("~/Documents/GitHub/Autism-Prediction/Data/train.csv")
test_df <- read.csv("~/Documents/GitHub/Autism-Prediction/Data/test.csv")
id <- test_df$ID
train_df$Class.ASD <- as.factor(train_df$Class.ASD)

clean_dataset <- function(dataset) {
  col_to_remove <- c('ID','age_desc','gender','relation','used_app_before','jaundice')
  for (col in col_to_remove) {
    dataset <- dataset[ , !names(dataset) %in% c(col)]
  }
  return(dataset)
}

train_df <- clean_dataset(train_df)
test_df <- clean_dataset(test_df)

renameEthinicity <- function(dataset){
  dataset$ethnicity <- ifelse(dataset$ethnicity == "?","Others",dataset$ethnicity)
  return(dataset)
}

train_df <- renameEthinicity(train_df)
test_df <- renameEthinicity(test_df)

encodeDataset <- function(dataset){
  
  dataset$contry_of_res <- as.numeric((factor(dataset$contry_of_res)))
  dataset$ethnicity <- as.numeric(factor(dataset$ethnicity))
  dataset$austim <- as.numeric(factor(dataset$austim))
  
  return(dataset)
}

train_df <- encodeDataset(train_df)
test_df <- encodeDataset(test_df)

dt = sort(sample(nrow(train_df), nrow(train_df)*.75))
data_train<-train_df[dt,]
data_test<-train_df[-dt,]

library(e1071)

set.seed(20)  # Setting Seed
classifier_cl <- naiveBayes(Class.ASD ~ ., data = data_train)
classifier_cl

y_pred <- predict(classifier_cl, test_df)
Y <- data_test[,ncol(data_test)]
# Confusion Matrix
cm <- table(y_pred, Y)

sub_4 <- data.frame(y_pred)
sub_4 <- data.frame(id,sub_4$y_pred)

# Renaming the columns
names(sub_4)[1] <- "ID"
names(sub_4)[2] <- "Class/ASD"

write.csv(sub_4,"~/Documents/GitHub/Autism-Prediction/Sub_data/NB_sub_4.csv", row.names = FALSE)

