
#***********************************************************************************
# upload and preprocess data                                                      %%%
#***********************************************************************************


library(h2o)
h2o.init(ip = "localhost", port = 54321, startH2O = TRUE, max_mem_size = '30g') # This means nthreads = num available cores
train_file <- "https://h2o-public-test-data.s3.amazonaws.com/bigdata/laptop/mnist/train.csv.gz"
test_file <- "https://h2o-public-test-data.s3.amazonaws.com/bigdata/laptop/mnist/test.csv.gz"
train <- h2o.importFile(train_file)
test <- h2o.importFile(test_file)

# To see a brief summary of the data, run the following command
summary(train)
summary(test)

# We encode the response column as categorical for multinomial classificatio
train[,785] <- as.factor(train[,785])
test[,785] <- as.factor(test[,785])
traindf <- train
testdf <- test




#***********************************************************************************
# Defining a Class which contains all candiate solution, best solution, current %%%
# solution etc.                                                                 %%%
#***********************************************************************************

info <- setClass("info" , slots = c(H_Layer = "numeric" , H_Neurons = "vector",
                                    Train_error = "numeric", Test_error = "numeric",
                                    tabu_tenure = "numeric"))

#***********************************************************************************
# Defining a Class which will calculate fitness value and weight list           %%%
#***********************************************************************************

info2 <- setClass("info2" , slots = c(train = "numeric",test = "numeric"))
                                      

##*****************************************************************##
##*****************************************************************##
##*****************************************************************##
Input_features = 784
Output_features = 1
Iteration = 10
Min_hid = 1
Min_Neuron = c()
Max_Neuron = c()

G_Best        <-    new("info")
C_Best        <-    new("info")
tabu_list     <-    replicate(Iteration + 1, new("info"), simplify="list")
fit           <-    new("info2")
#*******************************************
# Initialize tabu tenure as zero
#*******************************************
for(i in 1:Iteration+1)
{
  slot(tabu_list[[i]] , "tabu_tenure") <- 0
}

input   <- Input_features
output  <- Output_features

#*******************************************
# Generate random neurons for hidden layers
#*******************************************
a = n_list = NULL

for(i in 1:Min_hid)
{
  a         <-  round((input + output)/2)
  b         <-  round((input + output)*2/3)
  n_list[i]   <-  round(runif(1 , a , b))
  Min_Neuron <- c(Min_Neuron ,a )
  Max_Neuron <- c(Max_Neuron , b)
  input     <-  n_list[i]
}

#*****************************************
# calculate fitness of initial solution
#*****************************************
source('fitness.R')
fit   <-  fitness(traindf,testdf, n_list)

slot(C_Best , "H_Layer")      <-     Min_hid
slot(C_Best , "H_Neurons")    <-     n_list
slot(C_Best , "Train_error")  <-    slot(fit , "train")
slot(C_Best , "Test_error")   <-    slot(fit , "test")
#slot(C_Best , "cross_val")    <-    slot(fit , "cross")
slot(C_Best , "tabu_tenure")  <-    4

#*****************************************

G_Best            <-    C_Best        
tabu_list[[1]]    <-    G_Best


#*****************************************
source("neighbour.R")
for(itrn in 1:Iteration)
{
  p <- sprintf("Start of iteration %d" , itrn)
  print(p)
  temp <- new("info")
  temp <- neighbour(C_Best, Min_Neuron, Max_Neuron, itrn, traindf , testdf)
  if(slot(G_Best, "Test_error") > slot(temp , "Test_error"))
  {
    for(j in 1:itrn)
    {
      if(slot(tabu_list[[j]] , "tabu_tenure") > 0 )
      {
        slot(tabu_list[[j]], "tabu_tenure")  <-   slot(tabu_list[[j]], "tabu_tenure") - 1  
      }
    }
    tabu_list[[itrn + 1]] <- temp
    slot(tabu_list[[itrn + 1]], "tabu_tenure") <- 4
    G_Best        <-    temp
    C_Best        <-    temp
  }
  else
  {
    for(j in 1:itrn)
    {
      if(slot(tabu_list[[j]] , "tabu_tenure") > 0 )
      {
        slot(tabu_list[[j]], "tabu_tenure")  <-   slot(tabu_list[[j]], "tabu_tenure") - 1  
      }
    }
    tabu_list[[itrn + 1]] <- temp
    slot(tabu_list[[itrn + 1]], "tabu_tenure") <- 4
    C_Best  <-  temp
  }
}
