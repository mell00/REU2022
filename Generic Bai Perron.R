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

	#Checking to make sure interval is larger than 3 points
	if(n*interval < 3){
		print(paste("Interval is too small. Try with ", 3/n, ".", sep=""))
		return()
	}

	#Checking to make sure max breaks works with interval
	if(n*interval > n/max_breaks){
		print(paste("Max breaks is too high. Try with ",floor(n/(n*interval)),".", sep=""))
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

	for(i in 1:(n-n*interval)){#Select starting observation of each subsection (constrained by interval size)

		for(j in (i+n*interval):n){#Select end observation of each subsection (constrained by interval size)

			subsect_x = x_values[i:j]
			subsect_y = y_values[i:j]
			subsect_form = getForm(subsect_x, subsect_y, model_type)
			subsect_mod = getModel(spec_function, subsect_form, arguments)
			subsect_SSR = sum((subsect_mod$resid[!is.na(subsect_mod$resid)])^2)
			subsect_info = cbind(subsect_x[1], max(subsect_x), subsect_SSR)
			all_SSRs = rbind(all_SSRs, subsect_info)

		}

	}

	recurseSSR = function(r_max, r_interval, r_max_breaks, r_n, r_all_SSRs){

		for(y in 1:r_max_breaks){

			SSR = data.frame()

			for(z in ((r_n*r_interval)+1):(r_n-(r_n*r_interval*y)-1)){
			
				first_subsect = which(r_all_SSRs[,1] == 1 & r_all_SSRs[,2] == z) #Location of subsect that starts with 1 and goes to s
				second_subsect = which(r_all_SSRs[,1] == z+1 & r_all_SSRs[,2] == r_max) #Location of subsect that starts with s and goes to end
				option = cbind(r_all_SSRs[first_subsect,1], r_all_SSRs[first_subsect,2], r_all_SSRs[second_subsect,1], r_all_SSRs[second_subsect,2], r_all_SSRs[first_subsect,3]+r_all_SSRs[second_subsect,3])
				SSR = rbind(SSR, option)
				print(r_all_SSRs[second_subsect,1])
				print(r_all_SSRs[second_subsect,2])
				if(y > 1){

					recurseSSR(r_max, r_interval, (r_max_breaks - 1), r_n, r_all_SSRs)

				}

			}
		}

	return(SSR)

	}

	SSR_one = recurseSSR(max(x_values), interval, max_breaks, n, all_SSRs)

	return(SSR_one[which.min(SSR_one[,5]),])

#compare BICs of all optimal knot sets and null

}

bp_test = bai_perron(seq(1:60), dif_means_1, "lm", "", 0.25, 1)