#Generic Bai-Perron Test - currently works for ar, arima, lm, and glm

bai_perron<-function(x_values, y_values, parameters, model_type, percent, breaks){

	#Checking to make sure max breaks works with specified percent inerval
	if(max(x_values) * percent > max(x_values) / breaks){
		return("Max breaks is too high. Try with a lower value.")
	}


	#Setting up data in correct form for model type
	if(model_type == "lm" | model_type == "glm"){
		formula = formula(y_values ~ x_values)
	}else if(model_type == "arima" | model_type == "ar"){
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

return(final_model)

#generate fit for all possible segments

#test all possible combinations up to max breakpoints

}

test_ts = ts(dif_means_1, start=1, end=60)

bai_perron(seq(1:60), dif_means_1, "order=c(1,0,0)", "arima", 0.15, 5)