bai_perron<-function(y_values, x_values, data_type, parameters, model_type){

#check if percent works with max breakpoints

if(data_type == "time series" | data_type == "ts"){

formula = ts(y_values, start=min(x_values), end=max(x_values))

}

spec_function = match.fun(model_type)

parameters = gsub(" ","",unlist(strsplit(parameters, ";")))

length = length(parameters)

param = list()

for(i in 1:length){

param[[i]] = eval(parse(text=parameters[[i]]))

}

#generate fit for all possible segments

#test all possible combinations up to max breakpoints

model = NULL

if(length(param) == 1){

model = spec_function(formula, param[[1]])

}else if(length(param) == 2){

model = spec_function(formula, param[[1]], param[[2]])

}

return(model)

}

test_ts = ts(dif_means_1, start=1, end=60)

bai_perron(dif_means_1, seq(1:60), "time series", "order=c(1,0,0); seasonal = list(order = c(0L, 0L, 0L), period = NA)", arima)