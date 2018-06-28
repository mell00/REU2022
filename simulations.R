#Step 1 - define current bar function (if not previously defined in workspace)
#Step 2 - generate data

#Step 3 - set up for simulation
time = 1:90
data = test_data_1[,2] #edit here !!!
title = "Current Test Data" #edit here !!!
runs = 5
iterations = 50
current_bar = bar1 #edit here !!!
make = 0.4
murder = 0.4

#Step 4 - run simulation

simulation = function(time, data, runs, iterations, current_bar, make, murder){

	#initializing storage for returns from all BAR runs
	current_list = list()
	current_list[[1]] = list() #AcceptRate
	current_list[[2]] = data.frame(matrix(ncol=0, nrow=iterations)) #MSE
	current_list[[3]] = data.frame(matrix(ncol=0, nrow=iterations)) #Breakpoints
	current_list[[4]] = data.frame(matrix(ncol=3, nrow=0)) #ProposedSteps
	current_list[[5]] = data.frame(matrix(ncol=3, nrow=0)) #AcceptedSteps
	names(current_list) = c("AcceptRate", "MSE", "Breakpoints", "ProposedSteps", "AcceptedSteps")
 
	#getting the initial points using the Bai-Perron test 
	library("strucchange")
	break_p = breakpoints(data ~ time, breaks = 5, h = 0.1) 
	starting_breakpoints = break_p$breakpoints

	#running BAR the specified number of times and storing the results
	for(i in 1:runs){
    
		current_result = current_bar(starting_breakpoints, time, data, iterations, make, murder)
		current_list[[1]] = c(current_list[[1]], current_result$AcceptRate[[1]], recursive = TRUE)
		current_list[[2]] = cbind(current_list[[2]], current_result$MSE[,1])
		current_list[[3]] = cbind(current_list[[3]], current_result$Breakpoints)
		current_list[[4]] = rbind(current_list[[4]], current_result$ProposedSteps)
		current_list[[5]] = rbind(current_list[[5]], current_result$AcceptedSteps)

	}

	#returning results of all BAR runs
	return(current_list)
}

sim_list = simulation(time, data, runs, iterations, current_bar, make, murder)

#Step 5 - clean up and save final version of $Breakpoints from simulation results

split_num = NULL #initializing

colnames(sim_list[[3]]) <- gsub(x = colnames(sim_list[[3]]), pattern = "all_k_best...c..1...ncol.all_k_best...", replacement = "X1")  

for(i in 2:ncol(sim_list[[3]])){ #detecting where to split up columns in $Breakpoint object
  
	if(endsWith(colnames(sim_list[[3]])[i], "1") == TRUE){
		split_num = c(split_num, i)
	}
  
}

final_list = list() #initializing

for(i in 1:length(split_num)){ #splitting up columns in $Breakpoint object

	if(i == 1){ #breakpoints from first run
		final_list[[i]] = sim_list[[3]][,1:(split_num[i]-1)]
	}else if(i < length(split_num)){# breakpoints from middle runs
		final_list[[i]] = sim_list[[3]][,split_num[i-1]:(split_num[i]-1)]
	}else{ #breakpoints from penultimate and final runs
		final_list[[i]] = sim_list[[3]][,split_num[i-1]:(split_num[i]-1)]
		final_list[[i+1]] = sim_list[[3]][,split_num[i]:ncol(sim_list[[3]])]
	}
  
}

sim_list[[3]] = final_list #saving final version of $Breakpoint object

#Step 6 - saving the final list, make sure working directory goes to correct folder

saveRDS(sim_list, file="TestList.RData") #edit name here !!!

#sim_list = readRDS("TestList.RData") #to load an existing RDS

#Step 7 - graphing

#plotting the MSE
plot(sim_list$MSE[,1], ylab = "MSE" , xlab = "time", main = title)

#setup for plotting histograms 
x.label = "Location of Breakpoint" #label setup
which_run = 3 #which run you want to plot

#frequency of breakpoints 
if(is.atomic(sim_list$Breakpoints[[which_run]]) == TRUE) {
	hist(sim_list$Breakpoints[[which_run]], xlab = x.label, main = title, col="red", breaks=max(time), xlim=c(0,max(time))) 
}else if(dim(sim_list$Breakpoints[[which_run]])[2] >= 2) {
	column_list = NULL
	for(i in 1:dim(sim_list$Breakpoints[[which_run]])[2]){
		column_list = c(column_list, sim_list$Breakpoints[[which_run]][,i], recursive=TRUE)
	}
	hist(column_list, xlab = x.label, main = title, col="red", breaks=max(time), xlim=c(0,max(time))) 
}