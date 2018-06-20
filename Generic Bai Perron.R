#Generic Bai-Perron Test - currently known to work for arima (glm, lm, and ar need to be tested)

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
	getModel = function(spec_function, formula, parameters){

		model = spec_function(formula)

		if(parameters != ""){
			parameters = gsub(" ","",unlist(strsplit(parameters, ";")))
			length = length(parameters)
			param = list()
			for(i in 1:length){
				param[[i]] = eval(parse(text=parameters[[i]]))
			}
			param_fill = function(param){
				for(i in 1:length(param)){
					return(param[[i]])
				}
			}
			model = spec_function(formula, param_fill(param))
		}

		return(model)
	}

	final_form = getForm(x_values, y_values, model_type)
	final_model = getModel(spec_function, final_form, parameters)

	for(i in 1:(n-n*interval)){#select starting observation of each subsect (constrained by minimum subsect size)

		for(j in (i+n*interval):n){#select end observation of each subsect (constrained by minimum subsect size)

			subsect_x = x_values[i:j]
			subsect_y = y_values[i:j]
			subsect_form = getForm(subsect_x, subsect_y, model_type)
			subsect_mod = getModel(spec_function, subsect_form, parameters)
			subsect_SSR = sum(resid(subsect_mod)^2)
			print(c(subsect_x[1], max(subsect_x), subsect_SSR))

		}

	}

	for(l in 1:max_breaks){

		#need optimal knot set for each number of breaks

	}

	return(final_model)

#test all possible combinations up to max breakpoints, lowest BIC

}

bp_test = bai_perron(seq(1:30), dif_means_0, "", "lm", 0.15, 3)