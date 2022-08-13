#***********************************************************************************
# Defining a function which will calculate fitness value and return weights at each 
# layer and fitness value
#***********************************************************************************

fitness<- function(traindf, testdf , neuron_list)
{
  
  FN  <- new("info2")
  
  model <- h2o.deeplearning(x =1:784, y=785 , training_frame = traindf, 
                        validation_frame = testdf,nfolds =0 , epochs = 50,distribution = "multinomial",
                        activation = "RectifierWithDropout" , hidden = neuron_list,
                        mini_batch_size = 100 ,stopping_metric = "MSE" ,adaptive_rate = FALSE,
                        rate = 0.001,input_dropout_ratio = 0.2,momentum_start = 0.7,verbose = TRUE)
                        
  
  
  # filling values to the object
  slot(FN , "train")  <-  h2o.mse(model, train = TRUE)
  slot(FN , "test")   <-  h2o.mse(model, valid = TRUE)
  #slot(FN , "cross")   <-  h2o.mse(model, xval = TRUE)
  
  FN
  
}
