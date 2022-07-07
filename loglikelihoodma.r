

epsilon_t = function(ma, k_ends, data.ts){
  model = suppressWarnings(arima(full_data[,2], order=c(0,0,ma)))
  for (i in 1:ma){
    epsilon_list[i] = data.ts[i]-mu-epsilon_list[i-1]
  }
}

logl <- function(sigma,alpha,beta,data.ts) {
  for (i in 1:length(model))
  -dnorm(model[-1],alpha+beta*data.ts[1:length(model)-1],sigma,log=TRUE)
}

theta_list = c(0)

theta_new = mvrnorm(tail(theta_list,1),

fitMetrics<-function(k_ends, full_data){
  
  #create sum objects
  sum_loglik = 0
  coef_1 = 0 
  coef_2 = 0 
  #get and sum log likelihood for regressions of all intervals
  if(length(k_ends) < 3 ){
    model = suppressWarnings(arima(full_data[,2], order=c(0,0,ma)))
    SEE = sum(na.omit(model$res)^2)
    s2 = SEE/n
    sum_loglik = (-1*n/2)*(log(2*pi)+log(s2)+1) #finding the log likelihoods on the full dataset 
  }else{
    for(i in 2:length(k_ends)) {
      if(i == 2){
        min = k_ends[i-1]
        y_values = full_data[c(min:k_ends[i]),2] #getting the y values in the interval
        model = suppressWarnings(arima(y_values, c(0,0,ma)))
        sub_n = length(y_values)
        SEE = sum(na.omit(model$res)^2)
        s2 = SEE/sub_n
        sub_loglik = (-1*sub_n/2)*(log(2*pi)+log(s2)+1)
        sum_loglik = sum_loglik + sub_loglik #the logLik looks the log likelyhood (relates to both SSR and MLE)
      }else if(i > 2){
        min = k_ends[i-1]
        y_values = full_data[c((min+1):k_ends[i]),2] #getting the y values in the interval
        model = suppressWarnings(arima(y_values, c(0,0,ma)))
        sub_n = length(y_values)
        SEE = sum(na.omit(model$res)^2)
        s2 = SEE/sub_n
        sub_loglik = (-1*sub_n/2)*(log(2*pi)+log(s2)+1)
        sum_loglik = sum_loglik + sub_loglik #the logLik looks the log likelihood (relates to both SSR and MLE)
      }
    }
  }
  return(sum_loglik)
}
