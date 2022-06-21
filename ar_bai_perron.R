#Bai-Perron Test for AR Models

#Key
#x_values = x variable for all observations
#y_values = y variable for all observations
#order = order of AR model to be fitted
#interval = minimum proportion of observations in each subsection (i.e. 0.15)
#max_breaks = maximum number of breaks to be considered
#progress = whether or not to show progress bar, TRUE/FALSE

bai_perron.ar<-function(x_values, y_values, order=1, interval=0.15, max_breaks=3, progress=T){

	n = length(x_values) #Number of observations
	x_values = 1:n #Turn x values into observations
	int = floor(n*interval) #Set minimum interval between breaks

	#Checking to make sure interval is larger than 3 points
	if(int < 3){
		print(paste("Interval is too small. Try with ", round(3/n, 2), ".", sep=""))
		return()
	}

	#Checking to make sure max breaks works with interval
	if(int > (n-1)/(max_breaks+1)){
		print(paste("Max breaks is too high. Try with ",floor((n-1)/int)-1,".", sep=""))
		return()
	}

	#Null model
	null_mod = ar(y_values, order=order)
	null_SSR = sum((null_mod$res[!is.na(null_mod$res)])^2)
	null_BIC = n + n*log(2*pi) + n*log(as.numeric(null_SSR)/n) + log(n)*(order+2)

	#Initializing data frame to store fit information for each subsection
	all_SSRs = data.frame()

	if(progress == TRUE){
		writeLines("\nFinding all SSRs.")
		SSR_progress <- txtProgressBar(min = 0, max = (n-int+1), style = 3)
	}

	for(i in 1:(n-int+1)){#Select starting observation of each subsection (constrained by interval size)

		for(j in (i+int-1):n){#Select end observation of each subsection (constrained by interval size)

			subsect_x = x_values[i:j]
			subsect_y = y_values[i:j]
			subsect_mod = ar(subsect_y, order=order)
			subsect_SSR = sum((subsect_mod$res[!is.na(subsect_mod$res)])^2)
			subsect_info = cbind(subsect_x[1], max(subsect_x), subsect_SSR)
			all_SSRs = rbind(all_SSRs, subsect_info)

		}

		if(progress == TRUE){
			setTxtProgressBar(SSR_progress, i)
		}

	}


	recurseSSR = function(r_int, r_max_breaks, r_n, r_all_SSRs){

		SSR <<- list()

		for(w in 1:r_max_breaks){

			SSR[[w]] <<- data.frame()

		}

		subRecurse = function(current_breaks, offset, current_position, previous_subsect){

			if(offset < (r_n-2*r_int)+1){

				for(y in (offset+r_int):(r_n-r_int)){

					first_subsect_2 = which(r_all_SSRs[,1] == offset & r_all_SSRs[,2] == y-1)
					second_subsect_2 = which(r_all_SSRs[,1] == y & r_all_SSRs[,2] == r_n)

					starting_subsect = list()
					starting_score = 0

					for(w in 1:length(previous_subsect)){

						starting_subsect = cbind(starting_subsect, r_all_SSRs[previous_subsect[w],1], r_all_SSRs[previous_subsect[w],2])
						starting_score = starting_score+r_all_SSRs[previous_subsect[w],3]

					}

					option_two = cbind(starting_subsect, r_all_SSRs[first_subsect_2,1], r_all_SSRs[first_subsect_2,2], r_all_SSRs[second_subsect_2,1], r_all_SSRs[second_subsect_2,2], starting_score+r_all_SSRs[first_subsect_2,3]+r_all_SSRs[second_subsect_2,3])

					SSR[[current_breaks]] <<- rbind(SSR[[current_breaks]], option_two)

					new_subsect = c(previous_subsect, first_subsect_2)

					new_position = current_position+2

					if(current_breaks + 1 <= r_max_breaks){
						SSR[[current_breaks+1]] <<- subRecurse(current_breaks+1, option_two[,new_position][[1]], new_position, new_subsect)
					}

				}
			}

		return(SSR[[current_breaks]])

		}

			if(progress == TRUE){
				writeLines("\nTesting all combinations.")
				combo_progress <- txtProgressBar(min=(r_int-1), max=(r_n-r_int), style = 3)
			}
			for(z in r_int:(r_n-r_int)){
			
				first_subsect = which(r_all_SSRs[,1] == 1 & r_all_SSRs[,2] == z) #Location of subsect that starts with 1 and goes to s
				second_subsect = which(r_all_SSRs[,1] == z+1 & r_all_SSRs[,2] == r_n) #Location of subsect that starts with s and goes to end
				option = cbind(r_all_SSRs[first_subsect,1], r_all_SSRs[first_subsect,2], r_all_SSRs[second_subsect,1], r_all_SSRs[second_subsect,2], r_all_SSRs[first_subsect,3]+r_all_SSRs[second_subsect,3])
				SSR[[1]] <<- rbind(SSR[[1]], option)

				if(r_max_breaks >= 2){
					subRecurse(2, option[,3], 3, first_subsect)
				}
				if(progress == TRUE){
					setTxtProgressBar(combo_progress, z)
				}

			}

	return(SSR)

	}

	SSR_final = recurseSSR(int, max_breaks, n, all_SSRs)

	if(progress == TRUE){      
		writeLines("\n")
	}

	for(x in 1:length(SSR_final)){

		SSR_final[[x]] = SSR_final[[x]][which.min(SSR_final[[x]][,3+x*2]),]

	}

	BICs = list()

	for(v in 1:length(SSR_final)){

		BIC = n + n*log(2*pi) + n*log(as.numeric(SSR_final[[v]][,3+v*2])/n) + log(n)*v*(order+2)
		BICs = c(BICs, BIC)

	}

	all_breakpoints = list()
	for(i in 1:length(SSR_final)){
		all_breakpoints[[i]] = c(SSR_final[[i]][seq(2,2*i,2)], recursive=T)
		names(all_breakpoints[[i]]) = c(1:length(all_breakpoints[[i]]))
	}

	SSRs = NULL
	for(i in 1:length(SSR_final)){
		SSRs = c(SSRs, SSR_final[[i]][length(SSR_final[[i]])], recursive=T)
	}

	SSRs = c(null_SSR, SSRs, recursive=T)
	names(SSRs) = c(0:(length(SSRs)-1))
	BICs = c(null_BIC, BICs, recursive=T)
	names(BICs) = c(0:(length(BICs)-1))
	if(which.min(BICs) == 1){
		best_breakpoints = NA
	}else{
		best_breakpoints = c(all_breakpoints[which.min(BICs)-1], recursive=T)
	}

	final_list = list(best_breakpoints, all_breakpoints, SSRs, BICs)
	names(final_list) = c("Breakpoints", "AllBreakpoints", "SSRs", "BICs")

	return(final_list)

}

#calling the function
source("data_for_trials.R")
test_data = test_data_2()
bp_test = bai_perron.ar(test_data[,1], test_data[,2])
