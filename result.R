#*******************************************
#     Generating Result matrix
#*******************************************


result_2 <- array(list(), dim = c(11,4))
for(i in 1:11)
{
  result_2[i,1]<-slot(tabu_list[[i]] , "H_Layer")
  result_2[[i,2]]<-slot(tabu_list[[i]] , "H_Neurons")
  result_2[i,3]<-slot(tabu_list[[i]] , "Train_error")
  result_2[i,4]<-slot(tabu_list[[i]] , "Test_error")
}
write.csv(result_2,file  = sprintf("E:/facedaset/hidd = 2/result_2.csv"))
