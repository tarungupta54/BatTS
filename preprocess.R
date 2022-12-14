library(h2o)
h2o.init(nthreads = -1) # This means nthreads = num available cores
train_file <- "https://h2o-public-test-data.s3.amazonaws.com/bigdata/laptop/mnist/train.csv.gz"
test_file <- "https://h2o-public-test-data.s3.amazonaws.com/bigdata/laptop/mnist/test.csv.gz"
train <- h2o.importFile(train_file)
test <- h2o.importFile(test_file)

# To see a brief summary of the data, run the
following command
summary(train)
summary(test)

# We encode the response column as categorical for multinomial classification
train[,785] <- as.factor(train[,785])
test[,785] <- as.factor(test[,785])
traindf <- train
testdf <- test
