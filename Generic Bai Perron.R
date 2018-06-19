#Generic Bai-Perron Test - currently works for ar, arima, lm, and glm

bai_perron<-function(x_values, y_values, parameters, model_type, percent_int, max_breaks){

	#Checking to make sure percent is larger than 3 points
	if(length(x_values) * percent_int < 3){
		return("Percent interval too small.")
	}

	#Checking to make sure max breaks works with specified percent interval
	if(length(x_values) * percent_int > length(x_values) / max_breaks){
		return("Max breaks is too high. Try with a lower value.")
	}


	#Setting up data in correct form for model type
	if(model_type == "lm" | model_type == "glm"){
		formula = formula(y_values ~ x_values)
	}else if(model_type == "arima"){
		formula = ts(y_values, start=min(x_values), end=max(x_values))
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

	final_model = getModel(spec_function, formula, parameters)

	for(i in 1:(length(x_values)-length(x_values)*percent_int)){

		for(j in (i+length(x_values)*percent_int):length(x_values)){

			subset = x_values[i]:x_values[j]

		}

	}

	return(AIC(final_model))

#test all possible combinations up to max breakpoints, lowest BIC

}

test_ts = ts(dif_means_1, start=1, end=60)

bp_test = bai_perron(seq(1:60), dif_means_1, "order=c(1,0,0)", "arima", 0.15, 5)

#ar or arma does not work with aic