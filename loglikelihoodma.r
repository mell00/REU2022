library(MASS)
 
 setwd("/Users/mellm/github/REU2022/test_Cases")
 full_data = read.csv("pacificBrownPelican.csv")
 data.ts = arima(full_data$Count_yr,order=c(0,0,ma))
 ma = 10
 n = length(full_data[,1]) #number of observations
 k = tail(full_data$NumberByPartyHours,-1)
 k_ends = suppressWarnings(as.numeric(c(min(full_data[,2]), na.omit(k), n))) #adding end points to k
 k_ends = k_ends[!is.na(k_ends)]
 mu = 1.5
 tao = abs(1)
 theta_list = data.ts$model$theta
 sigma_mtrx = diag(tao,length(theta_list))
 
 if(length(k_ends) < 3 ){
   SEE = sum(na.omit(data.ts$residuals)^2)
   s2 = SEE/n
 } else {
   for(i in 2:length(k_ends)) {
     if(i == 2){
       min = k_ends[i-1]
       y_values = full_data[c(min:k_ends[i]),2] #getting the y values in the interval
       sub_n = length(y_values)
       SEE = sum(na.omit(data.ts$residuals)^2)
       s2 = SEE/sub_n
     }
     else if(i > 2){
       min = k_ends[i-1]
       y_values = full_data[c(min:k_ends[i]),2] #getting the y values in the interval
       sub_n = length(y_values)
       SEE = sum(na.omit(data.ts$residuals)^2)
       s2 = SEE/sub_n
     }
   }
 }
   
  epsilon_list = c(0,1)
  epsilon_t = function(ma, k_ends, data.ts,mu){
     if (ma == 0){ 
       
     }
     if (ma == 1){ 
       y_values[ma] - mu
     }
     else{
       for (i in 2:ma){
         epsilon_list[i] = data.ts$residuals[i]- mu - alpha*epsilon_list[i-1]
       }
       return(epsilon_list)
     }
   }
   epsilon_t(ma,k_ends,data.ts,mu)
alpha = 1 #between -1 and 1
logl <- function(sigma,alpha,beta,data.ts) { #log likelihood calculation
  sum_loglik = 0
  for (i in 1:length(data.ts)){
    sum_loglik = sum_loglik - prod(dnorm(y_values[i],alpha+beta*data.ts[1:length(data.ts)-1],s2,log=TRUE))
  }
  return(sum_loglik)
}

theta_new = mvrnorm(n=1,mu=theta_list,Sigma=sigma_mtrx,tol = 1e-6, empirical = FALSE,EISPACK = FALSE)

r = min()





















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
    sum_loglik = sum_loglik - prod(dnorm(y_values[i],alpha+beta*data.ts[1:length(data.ts)-1],sigma,log=TRUE)) #finding the log likelihoods on the full dataset 
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
        sum_loglik = sum_loglik + sub_loglik #the logLik looks the log likelihood (relates to both SSR and MLE)
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
