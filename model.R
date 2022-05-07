set.seed(123)

train_data <- read.csv('~/Documents/GitHub/Autism-Prediction/Data/train_df.csv')
test_data <- read.csv('~/Documents/GitHub/Autism-Prediction/Data/test_df.csv')
train_data$Class.ASD <- as.factor(train_data$Class.ASD)

library(randomForest)

model = randomForest(Class.ASD~.,data = train_data,ntree=100, mtry = 5,
                     proximity = TRUE)

predict <- predict(model, newdata = test_data)

df <- data.frame(predict)
final_sub <- data.frame(test$ID,df$pred)
write.csv(final_sub,"~/Documents/GitHub/Autism-Prediction/Data/final_sub.csv", row.names = FALSE)
