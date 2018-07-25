#whole model 
model = ar(data)
predic_values = model$resid[-1]
value = predict(model, 1)
one = data[1] - value$pred[1]
predic_values = c(predic_values, one)
mse = mean(predic_values^2)


model_1 = ar(data[1:30])
model_2 = ar(data[31:60])
model_3 = ar(data[61:90])

predic_values_1 = model_1$resid[-1]
predic_values_2 = model_2$resid[-1]
predic_values_3 = model_3$resid[-1]

value_1 = predict(model_1, 1)
value_2 = predict(model_2, 31)
value_3 = predict(model_3, 61)

one_1 = data[1] - value_1$pred[1]
one_2 = data[31] - value_2$pred[1]
one_3 = data[61] - value_3$pred[1]

predic_values_1 = c(predic_values_1, one_1)
predic_values_2 = c(predic_values_2, one_2)
predic_values_3 = c(predic_values_3, one_3)

mse_1 = mean(predic_values_1^2)
mse_2 = mean(predic_values_2^2)
mse_3 = mean(predic_values_3^2)

avg = mean(c(mse_1,mse_2,mse_3))


mse = 0 
se = 0
for(i in 2:length(k_ends)) {
  if(i == 2){
    min = k_ends[i-1]
    x_values = full_data[c(min:k_ends[i]),1] #getting the x values in the interval
    y_values = full_data[c(min:k_ends[i]),2] #getting the y values in the interval
    data =  data.frame(x_values, y_values) #re-making this into a dataframe 
    model = ar(y_values) #running a lm on the selected interval 
    predic_values = model$resid[-1]
    value = predict(model, min) #CHECK THIS
    one = data[1,2] - value$pred[1] #CHECK THIS
    predic_values = c(predic_values, one)
    se = se + mean(predic_values^2) 
  }else if(i > 2){
    min = k_ends[i-1]
    x_values = full_data[c((min+1):k_ends[i]),1] #getting the x values in the interval
    y_values = full_data[c((min+1):k_ends[i]),2] #getting the y values in the interval
    data = data.frame(x_values, y_values) #re-making this into a dataframe 
    model = ar(y_values) #running a lm on the selected interval 
    predic_values = model$resid[-1]
    value = predict(model, min) #CHECK THIS
    one = data[1,2] - value$pred[1] #CHECK THIS
    predic_values = c(predic_values, one)
    se = se + mean(predic_values^2) #the logLik looks the log likelyhood (relates to both SSR and MLE)
  }
  
}
mse = se / (length(k_ends)-1)
mse


#Looking at the log likelihood for AR models 
fitMetrics<-function(k_ends, full_data){
  
  #create sum objects
  sum_loglik = 0
  coef_1 = 0 
  coef_2 = 0 
  #get and sum log likelihood for regressions of all intervals
  if(length(k_ends) < 3 ){
    model = arima(full_data[,2], method = "ML", order = c(1,0,0)) #AR model on the full dataset
    sum_loglik = model$loglik #finding the log likeihoods on the full dataset 
  }else{
    for(i in 2:length(k_ends)) {
      if(i == 2){
        min = k_ends[i-1]
        x_values = full_data[c(min:k_ends[i]),1] #getting the x values in the interval
        y_values = full_data[c(min:k_ends[i]),2] #getting the y values in the interval
        data = data.frame(x_values, y_values) #re-making this into a dataframe 
        model = arima(y_values, method = "ML", order = c(1,0,0)) #running a AR on the selected interval 
        sum_loglik = sum_loglik + model$loglik #the logLik looks the log likelyhood (relates to both SSR and MLE)
      }else if(i > 2){
        min = k_ends[i-1]
        x_values = full_data[c((min+1):k_ends[i]),1] #getting the x values in the interval
        y_values = full_data[c((min+1):k_ends[i]),2] #getting the y values in the interval
        data = data.frame(x_values, y_values) #re-making this into a dataframe 
        model = arima(y_values, method = "ML", order = c(1,0,0)) #running a AR on the selected interval 
        sum_loglik = sum_loglik + model$loglik #the logLik looks the log likelyhood (relates to both SSR and MLE)
      }
    }
  }
  return(sum_loglik)
}

fitMetrics(c(1,30,60,90), test_data_2) #test call








#_____________--------------______________----------

coef_finding = function(full_data) {#function to minimize to get MLE of betas

  model = arima(full_data, method = "ML", order = c(1,0,0)) #AR model on the full datase
  c = model$coef #finding the log likeihoods on the full dataset 
  coef = c[[2]]
  intercept = c[[1]]
  
  return(c(intercept, coef))
  
}
coef = coef_finding(full_data[,2])
matrix(coef,2,1)
model = arima(full_data[,2], method = "ML", order = c(1,0,0)) #AR model on the full datase
fisher = (-1 * model$var.coef)
smiley = n * fisher




