#Step 1 - define simulation function (if not previously defined in workspace)
#Step 2 - define current bar function (if not previously defined in workspace)
#Step 3 - generate data

#Step 4 - set up for simulation
time = 1:90
data = test_data_2[,2] #edit here !!!
title = "test_data_2"
runs = 5
iterations = 100
current_bar = bar0 #edit here !!!
make = 0.4
murder = 0.4

#Step 5 - run simulation

simulation = function(time, data, runs, iterations, current_bar, make, murder){
  
  current_list = list()
  current_list[[1]] = list()
  current_list[[2]] = data.frame(matrix(ncol=0, nrow=iterations)) 
  current_list[[3]] = data.frame(matrix(ncol=0, nrow=iterations))
  names(current_list) = c("AcceptRate", "MSE", "Breakpoints")
 
   #getting the initial points using the bai-perron test 
  library("strucchange")
  break_p = breakpoints(data ~ time, breaks = 5, h = 0.1) 
  starting_breakpoints = break_p$breakpoints
  
  for(i in 1:runs){
    
    current_result = current_bar(starting_breakpoints, time, data, iterations, make, murder)
    current_list[[1]] = c(current_list[[1]], current_result$AcceptRate[[1]], recursive = TRUE)
    current_list[[2]] = cbind(current_list[[2]], current_result$MSE[,1])
    current_list[[3]] = cbind(current_list[[3]], current_result$Breakpoints)
    
  }
  return(current_list)
}

sim_list = simulation(time, data, runs, iterations, current_bar, make, murder)

#Step 5 - clean up and save list object
split_num = NULL

for(i in 2:ncol(sim_list[[3]])){
  
  if(endsWith(colnames(sim_list[[3]])[i], "1") == TRUE){
    split_num = c(split_num, i)
  }
  
}

final_list = list()

for(i in 1:length(split_num)){
  
  if(i < length(split_num)){
    final_list[[i]] = sim_list[[3]][,1:(split_num[i]-1)]
  }else{
    final_list[[i]] = sim_list[[3]][,split_num[i-1]:(split_num[i]-1)]
    final_list[[i+1]] = sim_list[[3]][,split_num[i]:ncol(sim_list[[3]])]
  }
  
}



#plot of the MSE 
plot(sim_list$MSE[,1], ylab = "MSE" , xlab = "time", main = title)

#label set up 
x.label = "Location of Breakpoint"
#frequency of breakpoints 
if(dim(final_list[[5]])[2] == 1) {
  hist(final_list[[5]], xlab = x.label, main = title)
  
} else if(dim(final_list[[5]])[2] == 2) {
  hist(c(final_list[[5]]$X1,final_list[[5]]$X2 ), xlab = x.label, main = title)
  
} else if(dim(final_list[[5]])[2] == 3) {
  hist(c(final_list[[5]]$X1,final_list[[5]]$X2, final_list[[5]]$X3 ), xlab = x.label, main = title)
  
} else if(dim(final_list[[5]])[2] == 4) {
  hist(c(final_list[[5]]$X1,final_list[[5]]$X2, final_list[[5]]$X3, final_list[[5]]$X4 ), xlab = x.label, main = title)
  
} else if(dim(final_list[[5]])[2] == 5) {
  hist(c(final_list[[5]]$X1,final_list[[5]]$X2, final_list[[5]]$X3, final_list[[5]]$X4, final_list[[5]]$X5 ), xlab = x.label, main = title)
  
} else if(dim(final_list[[5]])[2] == 6) {
  hist(c(final_list[[5]]$X1,final_list[[5]]$X2, final_list[[5]]$X3, final_list[[5]]$X4, final_list[[5]]$X5, final_list[[5]]$X6 ), xlab = x.label, main = title)
  
} 

