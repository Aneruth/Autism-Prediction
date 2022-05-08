library(xgboost)

train_df <- read.csv("~/Documents/GitHub/Autism-Prediction/Data/train_df.csv")
test_df <- read.csv("~/Documents/GitHub/Autism-Prediction/Data/test_df.csv")

# Split the dataset
dt = sort(sample(nrow(train_df), nrow(train_df)*.8))
train<-train_df[dt,]
test<-train_df[-dt,]

X_train = data.matrix(train[,-15])                  # independent variables for train
y_train = train[,15]                                # dependent variables for train

X_test = data.matrix(test[,-15])                    # independent variables for test
y_test = test[,15] 

# convert the train and test data into xgboost matrix type.
xgboost_train = xgb.DMatrix(data=X_train, label=y_train)
xgboost_test = xgb.DMatrix(data=X_test, label=y_test)

model <- xgboost(data = xgboost_train,                    # the data   
                 max.depth=3,                           # max depth 
                 nrounds=50)                              # max number of boosting iterations

summary(model)

pred_test = predict(model, xgboost_test)

pred_test


pred_y = as.factor((levels(y_test))[round(pred_test)])
