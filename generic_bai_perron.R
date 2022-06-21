#Generic Bai-Perron Test

#Key
#x_values = x variable for all observations
#y_values = y variable for all observations
#model_type = the type of model used for fitting (i.e. ar, arima, glm, lm), wrapped in quotes (arma seems to work but gets singla Hessian warnings)
#arguments = any arguments you want applied to the moddle, wrapped in quotes and seperated by semicolons (i.e. "order = c(1,0,0); method='ML'")
#p = number of parameters per regime/subsection added (i.e. 3 for linear)
#interval = minimum proportion of observations in each subsection (i.e. 0.15)
#max_breaks = maximum number of breaks to be considered
#progress = whether or not to show progress bar, TRUE/FALSE

bai_perron<-function(x_values, y_values, model_type="ar", arguments="", p=3, interval=0.15, max_breaks=3, progress=T){

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

	#Function to set up data in correct form for model type (default is formula, can also do time series)
	getForm = function(f_x_values, f_y_values, f_model_type){
		if(f_model_type == "arima" | f_model_type == "ar" | f_model_type == "arma" | f_model_type == "FitAR"){
			f_formula = ts(f_y_values, start=min(f_x_values), end=max(f_x_values))
		} else{
			f_formula = formula(f_y_values ~ f_x_values)
		}
		return(f_formula)
	}

	#Get specified model type as a function
	spec_function = match.fun(model_type)

	#Function for getting specified model with specified arguments
	getModel = function(m_spec_function, m_formula, m_arguments){

		if(m_arguments != ""){
			m_arguments = gsub(" ","",unlist(strsplit(m_arguments, ";")))
			m_length = length(m_arguments)
			m_arg = list()
			for(i in 1:m_length){
				m_arg[[i]] = eval(parse(text=m_arguments[[i]]))
			}
			arg_fill = function(m_arg){
				for(i in 1:length(m_arg)){
					return(m_arg[[i]])
				}
			}
			m_model = m_spec_function(m_formula, arg_fill(m_arg))
		}else{
			m_model = m_spec_function(m_formula)
		}

		return(m_model)
	}

	#Null model
	null_form = getForm(x_values, y_values, model_type)
	null_mod = getModel(spec_function, null_form, arguments)
	null_SSR = sum((null_mod$res[!is.na(null_mod$res)])^2)
	null_BIC = n + n*log(2*pi) + n*log(as.numeric(null_SSR)/n) + log(n)*p

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
			subsect_form = getForm(subsect_x, subsect_y, model_type)
			subsect_mod = getModel(spec_function, subsect_form, arguments)
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

		BIC = n + n*log(2*pi) + n*log(as.numeric(SSR_final[[v]][,3+v*2])/n) + log(n)*p*(v+1)
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
bp_test = bai_perron(test_data[,1], test_data[,2], "ar", "order=1", 4, 0.15, 5)
