#Generic Bai-Perron Test

#Key
#x_values = x variable for all observations
#y_values = y variable for all observations
#model_type = the type of model used for fitting (i.e. ar, arima, glm, lm), wrapped in quotes (arma seems to work but gets singla Hessian warnings)
#arguments = any arguments you want applied to the moddle, wrapped in quotes and seperated by semicolons (i.e. "order = c(1,0,0); method='ML'")
#interval = minimum proportion of observations in each subsection (i.e. 0.15)
#max_breaks = maximum number of breaks to be considered

bai_perron<-function(x_values, y_values, model_type, arguments, interval, max_breaks){

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
		if(f_model_type == "arima" | f_model_type == "ar" | f_model_type == "arma"){
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

		m_model = m_spec_function(m_formula)

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
		}

		return(m_model)
	}

	#Initializing data frame to store fit information for each subsection
	all_SSRs = data.frame()

	for(i in 1:(n-int+1)){#Select starting observation of each subsection (constrained by interval size)

		for(j in (i+int-1):n){#Select end observation of each subsection (constrained by interval size)

			subsect_x = x_values[i:j]
			subsect_y = y_values[i:j]
			subsect_form = getForm(subsect_x, subsect_y, model_type)
			subsect_mod = getModel(spec_function, subsect_form, arguments)
			subsect_SSR = sum((subsect_mod$resid[!is.na(subsect_mod$resid)])^2)
			subsect_info = cbind(subsect_x[1], max(subsect_x), subsect_SSR)
			all_SSRs = rbind(all_SSRs, subsect_info)

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

			for(z in r_int:(r_n-r_int)){
			
				first_subsect = which(r_all_SSRs[,1] == 1 & r_all_SSRs[,2] == z) #Location of subsect that starts with 1 and goes to s
				second_subsect = which(r_all_SSRs[,1] == z+1 & r_all_SSRs[,2] == r_n) #Location of subsect that starts with s and goes to end
				option = cbind(r_all_SSRs[first_subsect,1], r_all_SSRs[first_subsect,2], r_all_SSRs[second_subsect,1], r_all_SSRs[second_subsect,2], r_all_SSRs[first_subsect,3]+r_all_SSRs[second_subsect,3])
				SSR[[1]] <<- rbind(SSR[[1]], option)

				if(r_max_breaks >= 2){
					subRecurse(2, option[,3], 3, first_subsect)
				}

			}

	return(SSR)

	}

	SSR_final = recurseSSR(int, max_breaks, n, all_SSRs)

	for(x in 1:length(SSR_final)){

		SSR_final[[x]] = SSR_final[[x]][which.min(SSR_final[[x]][,3+x*2]),]

	}

	return(SSR_final)

	#compare BICs of all optimal knot sets and null

}

bp_test = bai_perron(seq(1:60), dif_means_1, "lm", "", 0.15, 5)