setwd("\\Users\\mellm\\github\\REU2022")
source("loglikelihoodma.r")

q = 5 # a positive integer
z_0 = runif(1,0,1) #1 value between 0 and 1
n = 40
theta_new = theta_new
hat_theta = theta_new #TEST
t = seq(1:10) #sequence of integers
Z_t = c(abs(rnorm(length(t-1),1))) #iid sequence of non-negative random variables

#x_sum (needs work)
i = 1:q; x_sum = sum(hat_theta[i]*Z_t[i-1])

#nonseasonal MA polynomial
x_t = function (B_theta,B_B,Z_t,q){
  B_theta * B_B[1:q]*Z_t[1:q]
}

#seasonal MA polynomial


big_theta = function(z_0){
  i = 0:q; big_theta = sum(hat_theta[i]*z_0^i)
  return(big_theta) #assumed to be non-zero
}

B_theta = big_theta(z_0)

hat_big_theta = function(q){
  i = 0:q; hat_Big_theta[i] = big_theta
}

l = function(q){ #first integer such that 2*l >= q
  l = 1
  while(2*l < q){
    l = l + 1
  }
  return(l)
}

l = l(q)

big_B = function(process){ #NEEDS WORK
  b_B = c()
  for (i in t){
    b_B[i] = (process[i])/(process[i+1])
  }
  return(b_B)
}

B_B = big_B(Z_t)

big_I = x_t(B_theta,B_B,Z_t,q)

#theta sum
i = 0:q; theta_sum = sum(hat_theta[i])

#D_n t
D_nt = seq(2*l*q + 1,n)

#D_n sum
i = 0:(2*l); D_sum = (sum((big_I[i] - B_theta*B_B)^i))*x_t

#argmax of D_n
hat_theta = list(hat_theta, theta)
outputs = sapply(hat_theta, D_n)
bestD_n = hat_theta[which.max(outputs)]

#hat_theta
hat_theta = function(bestD_n,q){
  i = 0:q; hat_theta_sum = sum(hat_theta[i])
  hat_theta = bestD_n*hat_theta_sum
  return(hat_theta)
}

#D_n

#e_t #WORK IN PROGRESS - starts at e_0
e = function(x_t,theta_new,B_theta,epsilon_list){
  e_t = list()
  q = 4
  # sample data
  mtrx <- matrix(1:q)
  #t <- 1:ncol(mtrx)
  #i <- 1:nrow(mtrx)
  for (t in 1:ncol(mtrx)){
    for (i in 1:nrow(mtrx)){
      k = 1:q; sum_q = sum(theta_new[k]*e_t[t-k])
      j = 1:q; sum_Q = sum(big_theta[j*s]*e_t[t-(j*s)])
    }
  }
  #k = 1:q; sum_q = sum(theta_new[k]*e_t[t-k])
  #j = 1:q; sum_Q = sum(big_theta[j*s]*e_t[t-(j*s)])
  for (index in t){
    e_t[index] = sum(sum_q-sum_Q-sum_q*sum_Q)
  }
  return(e_t)
}
  
  e(x_t,theta_new,B_theta,epsilon_list)
  
  if (ma == 0){}
  else if (ma == 1){
    for (t in 2:q){
      e_t[t] = x_t[t] -
    }
  }
  else if (ma >= 2){
    
  }
}

#H matrix setup #WORK IN PROGRESS
s = 3 #seasonal period
n = 10
q = 10
H = function(s,n,q){
  
  H = apply()
}


#setting up priors for theta draws (define what b_0 and B_0 are)
if(fit_storage == TRUE){
  alt_arima<-function(full_data, ma){
    tryCatch(arima(full_data[,2], method="ML", order=c(0,0,ma)), error = function(e) arima(full_data[,2], method="CSS", order=c(0,0,ma)))
  }
  model = suppressWarnings(alt_arima(full_data, ma))
  informationless = matrix(0, ncol=(ma+1), nrow=(ma+1))
  diag(informationless) = rep(1000, (ma+1))
  alt_solve<-function(model_coef){
    tryCatch(solve(model_coef), error = function(e) informationless)
  }
  fisher = suppressWarnings(alt_solve(model$var.coef)) #amount of data contained in 1 data point
  smiley = n * fisher #empirical Bayes (using data to set priors) #as n goes to inf, variance becomes unbiased
  
  coef_list = model$coef[[length(model$coef)]]
  
  for(a in 1:(length(model$coef)-1)){
    
    coef_list = c(coef_list, model$coef[[a]], recursive=T) #pulls each coefficient from model
    
  }
  
  b_0 = matrix(coef_list,(ma+1),1) #matrix of beta means for posterior draw
  B_0 = smiley #variance-covariance matrix for posterior draw
  
  #beta and sigma draw
  post_theta_list = data.frame(Empty=rep(NA,(ma+1)))
  post_sigma_list = data.frame(Empty=NA)
}

#setting up posterior

##loop through the k_ends to find the intervals
if(fit_storage == TRUE){
  fit = NULL
  squared_resids = NULL
  current_post_thetas = NULL
  current_post_sigmas = NULL
  for(m in 2:length(k_ends)) {
    len = length(k_ends)
    if(m > 2){
      min = k_ends[m-1]+1
    }else{
      min = k_ends[m-1]
    }
    y_tp_values = NULL
    for(a in 1:ma){
      current_y_tp_values = full_data[c((min+ma-a):(k_ends[[m]]-ma+(ma-a))),2]
      y_tp_length <<- length(current_y_tp_values)
      y_tp_values = c(y_tp_values, current_y_tp_values, recursive=T)
    }
    x_j = matrix(c( rep(1, each=y_tp_length), y_tp_values, recursive=T), nrow=y_tp_length, ncol=(ar+1))
    y_j = full_data[c((min+ma):k_ends[[m]]),2] #getting the y values in the interval
    sigma = sd(y_j)
    
    #bar_v
    v = solve( (1/sigma) * (t(x_j) %*% x_j )+ solve(B_0) )
    #bar_theta
    theta = v %*% ( (1/sigma) * (t(x_j) %*% y_j) + solve(B_0) %*% b_0 )
    
    #drawing a random variable from a multivariate normal pdf 
    post_theta = mvrnorm(1, theta, v)
    
    predicted_x = x_j %*% post_theta
    fit = c(fit, c(rep(NA, ma), predicted_x, recursive=T), recursive=T)
    squared_resid = (predicted_x - y_j)^2
    squared_resids = c(squared_resids, squared_resid, recursive=T)
    
    bar_v = c(bar_v, v)
    bar_theta = c(bar_theta, theta)
    
    #SIGMA: #CHECK LATER
    alpha_0 = (max(k_ends))/2 + 2
    beta_0 = sum(epsilon_list^2)
    
    sigma = rgamma(1, alpha_0, rate = beta_0)
    post_sigma = 1 / sigma
    
    current_post_thetas = cbind(current_post_thetas, post_theta)
    current_post_sigmas = cbind(current_post_sigmas, post_sigma)
    
    if(m == len ) {
      MSE = mean(squared_resids)
      all_fits = rbind(all_fits, fit)
      all_MSE = rbind(all_MSE, MSE)
      current_post_thetas = as.data.frame(current_post_thetas)
      colnames(current_post_thetas) = c(1:ncol(current_post_thetas))
      post_beta_list = cbind(post_theta_list, current_post_thetas)
      colnames(current_post_sigmas) = c(1:ncol(current_post_sigmas))
      post_sigma_list = cbind(post_sigma_list, current_post_sigmas)
    }
  }
}

if(progress == TRUE){    
  setTxtProgressBar(sample_progress, i)
}

}

if(progress == TRUE){      
  writeLines("\n")
}

#cleaning up the matrices and counts
if(length(all_k_best) != 0){
  colnames(all_k_best) = c(1:ncol(all_k_best))	
}
final.propose = c(a.count, s.count, m.count, j.count)
final.accept = c(add.accept.count, sub.accept.count, move.accept.count, jiggle.accept.count)
colnames(all_BIC) = "BIC"

#cleaning up beta/sigma draws
if(fit_storage == TRUE){
  colnames(all_MSE) = "MSE"
  colnames(all_fits) = c(1:ncol(all_fits))
  post_theta_list = post_theta_list[,-1]
  post_sigma_list = post_sigma_list[,-1]
  rownames(post_theta_list) = c(seq(0,ma,1))
  rownames(post_theta_list) = paste("T", rownames(post_theta_list), sep = "")
  rownames(post_sigma_list) = "Sigma"
  
  split_num = NULL #initializing
  
  for(i in 2:ncol(post_theta_list)){ #detecting where to split up columns in theta/sigma object
    if(startsWith(colnames(post_theta_list)[i], "1.") == TRUE){
      split_num = c(split_num, i)
    }
  }
  
  final_theta_list = list() #initializing
  
  for(i in 1:length(split_num)){ #splitting up columns in theta object
    
    if(i == 1){ #thetas from first run
      final_theta_list[[i]] = post_theta_list[,1:(split_num[i]-1)]
      if(is.atomic(final_theta_list[[i]]) == T){
        final_theta_list[[i]] = as.data.frame(final_theta_list[[i]])
        rownames(final_theta_list[[i]]) = rownames(post_theta_list)
        colnames(final_theta_list[[i]]) = 1
      }else{
        colnames(final_theta_list[[i]]) = c(1:ncol(final_theta_list[[i]]))
      }
    }else if(i < length(split_num)){# thetas from middle runs
      final_theta_list[[i]] = post_theta_list[,split_num[i-1]:(split_num[i]-1)]
      if(is.atomic(final_theta_list[[i]]) == T){
        final_theta_list[[i]] = as.data.frame(final_theta_list[[i]])
        rownames(final_theta_list[[i]]) = rownames(post_theta_list)
        colnames(final_theta_list[[i]]) = 1
      }else{
        colnames(final_theta_list[[i]]) = c(1:ncol(final_theta_list[[i]]))
      }
    }else{ #thetas from penultimate and final runs
      final_theta_list[[i]] = post_theta_list[,split_num[i-1]:(split_num[i]-1)]
      if(is.atomic(final_theta_list[[i]]) == T){
        final_theta_list[[i]] = as.data.frame(final_theta_list[[i]])
        rownames(final_theta_list[[i]]) = rownames(post_theta_list)
        colnames(final_theta_list[[i]]) = 1
      }else{
        colnames(final_theta_list[[i]]) = c(1:ncol(final_theta_list[[i]]))
      }
      final_theta_list[[i+1]] = post_theta_list[,split_num[i]:ncol(post_theta_list)]
      if(is.atomic(final_theta_list[[i+1]]) == T){
        final_theta_list[[i+1]] = as.data.frame(final_theta_list[[i+1]])
        rownames(final_theta_list[[i+1]]) = rownames(post_theta_list)
        colnames(final_theta_list[[i+1]]) = 1
      }else{
        colnames(final_theta_list[[i+1]]) = c(1:ncol(final_theta_list[[i+1]]))
      }
    } 
  }
  
  post_theta_list = final_theta_list #saving final version of theta object
  
  final_sigma_list = list() #initializing
  
  for(i in 1:length(split_num)){ #splitting up columns in sigma object
    
    if(i == 1){ #sigmas from first run
      final_sigma_list[[i]] = post_sigma_list[,1:(split_num[i]-1)]
      if(is.atomic(final_sigma_list[[i]]) == T){
        final_sigma_list[[i]] = as.data.frame(final_sigma_list[[i]])
        rownames(final_sigma_list[[i]]) = rownames(post_sigma_list)
        colnames(final_sigma_list[[i]]) = 1
      }else{
        colnames(final_sigma_list[[i]]) = c(1:ncol(final_sigma_list[[i]]))
      }
    }else if(i < length(split_num)){# sigmas from middle runs
      final_sigma_list[[i]] = post_sigma_list[,split_num[i-1]:(split_num[i]-1)]
      if(is.atomic(final_sigma_list[[i]]) == T){
        final_sigma_list[[i]] = as.data.frame(final_sigma_list[[i]])
        rownames(final_sigma_list[[i]]) = rownames(post_sigma_list)
        colnames(final_sigma_list[[i]]) = 1
      }else{
        colnames(final_sigma_list[[i]]) = c(1:ncol(final_sigma_list[[i]]))
      }
    }else{ #sigma from penultimate and final runs
      final_sigma_list[[i]] = post_sigma_list[,split_num[i-1]:(split_num[i]-1)]
      if(is.atomic(final_sigma_list[[i]]) == T){
        final_sigma_list[[i]] = as.data.frame(final_sigma_list[[i]])
        rownames(final_sigma_list[[i]]) = rownames(post_sigma_list)
        colnames(final_sigma_list[[i]]) = 1
      }else{
        colnames(final_sigma_list[[i]]) = c(1:ncol(final_sigma_list[[i]]))
      }
      final_sigma_list[[i+1]] = post_sigma_list[,split_num[i]:ncol(post_sigma_list)]
      if(is.atomic(final_sigma_list[[i+1]]) == T){
        final_sigma_list[[i+1]] = as.data.frame(final_sigma_list[[i+1]])
        rownames(final_sigma_list[[i+1]]) = rownames(post_sigma_list)
        colnames(final_sigma_list[[i+1]]) = 1
      }else{
        colnames(final_sigma_list[[i+1]]) = c(1:ncol(final_sigma_list[[i+1]]))
      }
    } 
  }
  
  post_sigma_list = final_sigma_list #saving final version of sigma object
  
}

#getting distribution of k (number of breakpoints)
num_bkpts = list()
for(i in 1:iterations){
  current_k = length(all_k_best[i,][!is.na(all_k_best[i,])])
  num_bkpts = c(num_bkpts, current_k, recursive=T)
}


if(fit_storage == TRUE){    
  final_list = list(accept_count / iterations, final.propose, final.accept, all_MSE, all_BIC, all_k_best, num_bkpts, post_beta_list, post_sigma_list, all_fits)
  names(final_list) = c("AcceptRate", "ProposedSteps", "AcceptedSteps", "MSE", "BIC", "Breakpoints", "NumBkpts", "Beta", "Sigma", "Fits")
}else{
  final_list = list(accept_count / iterations, final.propose, final.accept, all_BIC, all_k_best, num_bkpts)  
  names(final_list) = c("AcceptRate", "ProposedSteps", "AcceptedSteps", "BIC", "Breakpoints", "NumBkpts")
}

return(final_list)
}
