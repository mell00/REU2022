library(MASS)
library(forecast)

 #THESE ARE TEST VALUES, feel free to change as needed
 setwd("/Users/mellm/github/REU2022/test_Cases")
 full_data1 = read.csv("pacificBrownPelican.csv")
 full_data = cbind(c(1:length(as.numeric(full_data1$NumberByPartyHours))), as.numeric(full_data1$Count_yr)) #binding time and count data
 
 
 data.ts = arima(full_data1$Count_yr,order=c(0,0,ma))
 data.ts_2 = auto.arima(full_data1$Count_yr)
 n = length(full_data[,1]) #number of observations
 k = tail(full_data,-1) #omitting non-numeric 1st row
 k_ends = suppressWarnings(c(min(full_data[,1]), na.omit(k), n)) #adding end points to k
 
 
 
 theta_list = data.ts$model$theta #draw existing theta values from data
 sigma_mtrx = diag(tao,length(theta_list))
 theta_new = mvrnorm(n=length(theta_list),theta_list,sigma_mtrx) #generate new theta values


  epsilon_list = c(0,1,2,3,4,5) #needs to be fixed, ignore for now
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
   epsilon_list = epsilon_t(ma,k_ends,data.ts,mu)

sum_loglik = function(){ #loglikelihood calculation
  for (i in 1:length(full_data)-1){
    sum_loglik = prod(dnorm(y_values[i],alpha+beta*full_data[i],sigma,log=TRUE))
  }
  return(sum_loglik)
}

logl <- function(sigma,alpha,beta,data.ts) { #log likelihood calculation
  sum_loglik = 0
  for (i in 1:length(data.ts)){
    if(length(k_ends) < 3 ){
      SEE = sum(na.omit(data.ts$residuals)^2)
      s2 = SEE/n
      return(sum_loglik())
    } else {
      for(i in 2:length(k_ends)) {
        if(i == 2){
          min = k_ends[i-1]
          y_values = full_data[c(min:k_ends[i]),2] #getting the y values in the interval
          sub_n = length(y_values)
          SEE = sum(na.omit(data.ts$residuals)^2)
          s2 = SEE/sub_n
          return(sum_loglik())
        }
        else if(i > 2){
          min = k_ends[i-1]
          y_values = full_data[c(min:k_ends[i]),2] #getting the y values in the interval
          sub_n = length(y_values)
          SEE = sum(na.omit(data.ts$residuals)^2)
          s2 = SEE/sub_n
          return(sum_loglik())
        }
      }
    }
  }
}

logl(s2,alpha,beta,data.ts)
theta_new = mvrnorm(n=1,mu=theta_list,Sigma=sigma_mtrx,tol = 1e-6, empirical = FALSE,EISPACK = FALSE)

r = min((logl(theta_new))/(logl(theta_list)),1)
