test <- read.csv('~/Documents/GitHub/Autism-Prediction/Data/test.csv')
train <- read.csv('~/Documents/GitHub/Autism-Prediction/Data/train.csv')

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

