bai_perron<-function(formula, parameters, model_type){

#how to subset formula? maybe have ys and xs inputed, then also provide formula form

#check if percent works with max breakpoints

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

bai_perron(test_ts, "order=c(1,0,0); seasonal = list(order = c(0L, 0L, 0L), period = NA)", arima)