MAvariance = function(sigma,theta){
}
loglikelihood <- toeplitz(ARMAacf(ma = x, lag.max = n - 1) * (1 + theta^2))
Qinv <- chol2inv(chol(Q))
tmp <- drop(y %*% Qinv %*% cbind(y)) / n
Sval <- tmp * (det(Q))^(1/n)



fitMetrics<-function(k_ends, full_data){
  
  #create sum objects
  sum_loglik = 0
  coef_1 = 0 
  coef_2 = 0 
  #get and sum log likelihood for regressions of all intervals
  if(length(k_ends) < 3 ){
    model = suppressWarnings(FitAR(full_data[,2], p=ma))
    SEE = sum(na.omit(model$res)^2)
    s2 = SEE/n
    sum_loglik = (-1*n/2)*(log(2*pi)+log(s2)+1) #finding the log likeihoods on the full dataset 
  }else{
    for(i in 2:length(k_ends)) {
      if(i == 2){
        min = k_ends[i-1]
        y_values = full_data[c(min:k_ends[i]),2] #getting the y values in the interval
        model = suppressWarnings(FitAR(y_values, p=ar))
        sub_n = length(y_values)
        SEE = sum(na.omit(model$res)^2)
        s2 = SEE/sub_n
        sub_loglik = (-1*sub_n/2)*(log(2*pi)+log(s2)+1)
        sum_loglik = sum_loglik + sub_loglik #the logLik looks the log likelyhood (relates to both SSR and MLE)
      }else if(i > 2){
        min = k_ends[i-1]
        y_values = full_data[c((min+1):k_ends[i]),2] #getting the y values in the interval
        model = suppressWarnings(FitAR(y_values, p=ar))
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
