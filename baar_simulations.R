# install.packages("devtools")
# library(devtools)
# install_version("FitAR", version = "1.94", repos = "http://cran.us.r-project.org")
library(FitAR)

#Step 1 - define current bar function (if not previously defined in workspace)
#Step 2 - generate time and data functions (see data_for_trials)


setwd("/Users/khaglich/Desktop/Edited REU Main/Variations_of_bars")
source("baar.R")

setwd("/Users/khaglich/Desktop/Edited REU Main")
source("data_for_trials.R")

#Step 3 - set up for simulation
data_fun = test_data_2 #edit here !!! (defines function for generating draws)
runs = 3
burn_in = 50
iterations = 250
current_bar = baar
save_name = "KH_check_061222.RData" #edit here !!! (format: "baar_data#.RData")
make = 0.5
percent = 0.02
col_num = 4
lambda = 1
jump = 0.25 #edit here !!!

#Step 4 - run simulation

simulation = function(data_fun, runs, iterations, burn_in, current_bar, make, percent, lambda, jump){

	#initializing storage for returns from all BAR runs
	current_list = list()
	current_list[[1]] = list() #AcceptRate
	current_list[[2]] = data.frame(matrix(ncol=col_num, nrow=0)) #ProposedSteps
	current_list[[3]] = data.frame(matrix(ncol=col_num, nrow=0)) #AcceptedSteps
	current_list[[4]] = data.frame(matrix(ncol=0, nrow=iterations)) #MSE
	current_list[[5]] = data.frame(matrix(ncol=0, nrow=iterations)) #BIC
	current_list[[6]] = data.frame(matrix(ncol=0, nrow=iterations)) #Breakpoints
	current_list[[7]] = data.frame(matrix(ncol=0, nrow=iterations)) #NumBkpts
	current_list[[8]] = list() #RunTimes
	names(current_list) = c("AcceptRate", "ProposedSteps", "AcceptedSteps", "MSE", "BIC", "Breakpoints", "NumBkpts", "RunTimes")
 
	#getting the initial points using the Bai-Perron test 
	library("strucchange")

	#running BAR the specified number of times and storing the results
	for(i in 1:runs){

		current_data = data_fun()
		break_p = breakpoints(current_data[,2] ~ current_data[,1], breaks = 5, h = 0.1) 
		starting_breakpoints = break_p$breakpoints
		start_time = Sys.time()
		current_result = current_bar(starting_breakpoints, current_data[,1], current_data[,2], iterations, burn_in, make, percent, lambda, jump)
		end_time = Sys.time()
		current_list[[1]] = c(current_list[[1]], current_result$AcceptRate[[1]], recursive = TRUE)
		current_list[[2]] = rbind(current_list[[2]], current_result$ProposedSteps)
		current_list[[3]] = rbind(current_list[[3]], current_result$AcceptedSteps)
		current_list[[4]] = cbind(current_list[[4]], current_result$MSE)
		current_list[[5]] = cbind(current_list[[5]], current_result$BIC)
		current_list[[6]] = cbind(current_list[[6]], current_result$Breakpoints)
		current_list[[7]] = cbind(current_list[[7]], current_result$NumBkpts)
		current_list[[8]] = c(current_list[[8]], end_time - start_time, recursive = TRUE)
		print(i)

	}

	#returning results of all BAR runs
	return(current_list)
}

sim_list = simulation(data_fun, runs, iterations, burn_in, current_bar, make, percent, lambda, jump)

#Step 5 - clean up and define final version of $Breakpoints from simulation results

split_num = NULL #initializing

colnames(sim_list[[6]]) <- gsub(x = colnames(sim_list[[6]]), pattern = "all_k_best...c..1...ncol.all_k_best...", replacement = "X1")  

for(i in 2:ncol(sim_list[[6]])){ #detecting where to split up columns in $Breakpoint object
  
	if(endsWith(colnames(sim_list[[6]])[i], "X1") == TRUE){
		split_num = c(split_num, i)
	}
  
}

final_list = list() #initializing

for(i in 1:length(split_num)){ #splitting up columns in $Breakpoint object

	if(i == 1){ #breakpoints from first run
		final_list[[i]] = sim_list[[6]][,1:(split_num[i]-1)]
	}else if(i < length(split_num)){# breakpoints from middle runs
		final_list[[i]] = sim_list[[6]][,split_num[i-1]:(split_num[i]-1)]
	}else{ #breakpoints from penultimate and final runs
		final_list[[i]] = sim_list[[6]][,split_num[i-1]:(split_num[i]-1)]
		final_list[[i+1]] = sim_list[[6]][,split_num[i]:ncol(sim_list[[6]])]
	}
  
}

sim_list[[6]] = final_list #saving final version of $Breakpoint object

#Step 6 - saving the final list, make sure working directory goes to correct folder

saveRDS(sim_list, file=save_name)

summary(sim_list)

#sim_list = readRDS() #to load an existing RDS