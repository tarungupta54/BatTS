#***********************************************************************************
# Defining a function (neighbourhod)                                            %%%
#***********************************************************************************

neighbour<- function(C_Best, Min_Neuron , Max_Neuron, itrn, traindf, testdf)
{
  candidate <- replicate(20, new("info"), simplify="list")
  M <- c()
  
  for(j in 1:10)
  {
    for(k in 1:length(slot(C_Best , "H_Neurons")))
    {
      r1 <- round((runif(1 , 0 , 1)), digits = 1)
      if( r1 >=0.2)
      {
        M[k] <- round (slot(C_Best, "H_Neurons")[[k]] + 
                         j*slot(C_Best, "H_Neurons")[[k]]*2/100 )
        if(M[k] >= Max_Neuron[k])
        {
          M[k] <- M[k] - round(j*slot(C_Best, "H_Neurons")[[k]]*2/100)
        }
        
      }
      else
      {
        M[k] <- slot(C_Best, "H_Neurons")[[k]]
        
      }
      
    }
    slot(candidate[[j]] , "H_Layer") <- slot(C_Best , "H_Layer")
    slot(candidate[[j]] , "H_Neurons") <- M
    #   print("good")
    source("fitness.R")
    fit   <-  fitness(traindf, testdf, slot(candidate[[j]] , "H_Neurons"))
    p <- sprintf("complete of iteration %d and solution %d" , itrn , j)
    print(p)
    # sprintf("In Neighbourhod, iteration number: %d and candidate number: %d" , itrn , j)
    
    slot(candidate[[j]] , "Train_error")  <-    slot(fit , "train")
    slot(candidate[[j]] , "Test_error")   <-    slot(fit , "test")
   # slot(candidate[[j]] , "cross_val")   <-    slot(fit , "cross")
    
    slot(candidate[[j]] , "tabu_tenure")  <-    0
    
    M = NULL
  }
  for(j in 1:10)
  {
    for(k in 1:length(slot(C_Best , "H_Neurons")))
    {
      r1 <- round((runif(1 , 0 , 1)), digits = 1)
      if( r1 >= 0.2)
      {
        M[k]<- round(slot(C_Best, "H_Neurons")[[k]] - j*slot(C_Best, "H_Neurons")[[k]]*2/100)
        if(M[k] <= Min_Neuron[k])
        {
          M[k] <- M[k] - round(j*slot(C_Best, "H_Neurons")[[k]]*2/100)
        }
      }
      else
      {
        M[k] <- slot(C_Best, "H_Neurons")[[k]]
        
      }
      
    }
    slot(candidate[[j+10]] , "H_Layer") <- slot(C_Best , "H_Layer")
    slot(candidate[[j+10]] , "H_Neurons") <- M
    source("fitness.R")
    fit   <-  fitness(traindf, testdf , slot(candidate[[j+10]] , "H_Neurons"))
    p <- sprintf("complete of iteration %d and solution %d" , itrn , j+10)
    print(p)
    slot(candidate[[j+10]] , "Train_error")  <-    slot(fit , "train")
    slot(candidate[[j+10]] , "Test_error")   <-    slot(fit , "test")
   # slot(candidate[[j+10]] , "cross_val")   <-    slot(fit , "cross")
    slot(candidate[[j+10]] , "tabu_tenure")  <-    0
    M = NULL
  }
  b= 0
  for(i in 1:length(candidate)) 
  {
    b = c(b, slot(candidate[[i]] , "Test_error"))
  }
  r = which.min(b)
  candidate[[r]]
}
  

