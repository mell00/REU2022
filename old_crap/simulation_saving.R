#Step 1 - define simulation function (if not previously defined in workspace)
#Step 2 - define current bar function (if not previously defined in workspace)
#Step 3 - generate data

#Step 4 - set up for simulation
time = 1:90
data = test_data_2[,2] #edit here !!!
runs = 5
iterations = 50
current_bar = bar0 #edit here !!!
make_p = 0.4
murder_p = 0.4

#Step 5 - run simulation

simulation = function(time, data, runs, iterations, current_bar, make, murder){

	current_list = list()
	current_list[[1]] = list()
	current_list[[2]] = data.frame(matrix(ncol=0, nrow=iteration_n)) #why iteration_n???
	current_list[[3]] = data.frame(matrix(ncol=0, nrow=iteration_n))
	names(current_list) = c("AcceptRate", "MSE", "Breakpoints")
	starting_breakpoints = c("30,60")

	for(i in 1:runs){

		current_result = current_bar(starting_breakpoints, time, data, iterations, make, murder)
		current_list[[1]] = c(current_list[[1]], current_result$AcceptRate[[1]], recursive = TRUE)
		current_list[[2]] = cbind(current_list[[2]], current_result$MSE[,1])
		current_list[[3]] = cbind(current_list[[3]], current_result$Breakpoints)

	}
  
}


#Step 5 - clean up and save list object
split_num = NULL

for(i in 2:ncol(current_list[[3]])){

  if(endsWith(colnames(current_list[[3]])[i], "1") == TRUE){
    split_num = c(split_num, i)
  }

}

final_list = list()

for(i in 1:length(split_num)){

  if(i < length(split_num)){
    final_list[[i]] = current_list[[3]][,1:(split_num[i]-1)]
  }else{
    final_list[[i]] = current_list[[3]][,split_num[i-1]:(split_num[i]-1)]
    final_list[[i+1]] = current_list[[3]][,split_num[i]:ncol(current_list[[3]])]
  }

}

