#Generic Bai-Perron Test - currently known to work for arima, ar, glm, and lm

bai_perron<-function(x_values, y_values, parameters, model_type, interval, max_breaks){

	n = length(x_values)

	#Checking to make sure interval is larger than 3 points
	if(n*interval < 3){
		print(paste("Interval is too small. Try with ", 3/n, ".", sep=""))
		return()
	}

	#Checking to make sure max breaks works with specified interval
	if(n*interval > n/max_breaks){
		print(paste("Max breaks is too high. Try with ",floor(n/(n*interval)),".", sep=""))
		return()
	}

	#Setting up data in correct form for model type
	getForm = function(f_x_values, f_y_values, f_model_type){
		if(f_model_type == "lm" | f_model_type == "glm"){
			f_formula = formula(f_y_values ~ f_x_values)
			return(f_formula)
		}else if(f_model_type == "arima" | f_model_type == "ar"){
			f_formula = ts(f_y_values, start=min(f_x_values), end=max(f_x_values))
			return(f_formula)
		}
	}

	#Get specified model type as a function
	spec_function = match.fun(model_type)

	#Function for getting correct model
	getModel = function(m_spec_function, m_formula, m_parameters){

		m_model = m_spec_function(m_formula)

		if(m_parameters != ""){
			m_parameters = gsub(" ","",unlist(strsplit(m_parameters, ";")))
			m_length = length(m_parameters)
			m_param = list()
			for(i in 1:m_length){
				m_param[[i]] = eval(parse(text=m_parameters[[i]]))
			}
			param_fill = function(m_param){
				for(i in 1:length(m_param)){
					return(m_param[[i]])
				}
			}
			m_model = m_spec_function(m_formula, param_fill(m_param))
		}

		return(m_model)
	}

	all_SSRs = data.frame()

	for(i in 1:(n-n*interval)){#select starting observation of each subsect (constrained by minimum subsect size)

		for(j in (i+n*interval):n){#select end observation of each subsect (constrained by minimum subsect size)

			subsect_x = x_values[i:j]
			subsect_y = y_values[i:j]
			subsect_form = getForm(subsect_x, subsect_y, model_type)
			subsect_mod = getModel(spec_function, subsect_form, parameters)
			subsect_SSR = sum((subsect_mod$resid[!is.na(subsect_mod$resid)])^2)
			subsect_info = cbind(subsect_x[1], max(subsect_x), subsect_SSR)
			all_SSRs = rbind(all_SSRs, subsect_info)

		}

	}

	for(l in 1:max_breaks){

		#need optimal knot set for each number of breaks

	}

	return(all_SSRs)

#test all possible combinations up to max breakpoints, lowest BIC

}

bp_test = bai_perron(seq(1:60), dif_means_1, "", "lm", 0.15, 3)