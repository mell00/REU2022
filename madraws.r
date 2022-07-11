source(".\\loglikelihoodma.r")







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
    #bar_beta 
    theta = v %*% ( (1/sigma) * (t(x_j) %*% y_j) + solve(B_0) %*% b_0 )
    
    #drawing a random variable from a multivariate normal pdf 
    post_theta = mvrnorm(1, theta, v)
    
    predicted_x = x_j %*% post_theta
    fit = c(fit, c(rep(NA, ma), predicted_x, recursive=T), recursive=T)
    squared_resid = (predicted_x - y_j)^2
    squared_resids = c(squared_resids, squared_resid, recursive=T)
    
    bar_v = c(bar_v, v)
    bar_theta = c(bar_theta, theta)
    
    #SIGMA:
    v0 = (max(k_ends))/2 + 2
    d0 = 0 + .5 * t(y_j - x_j %*% post_theta ) %*% (y_j - x_j %*% post_theta)
    
    sigma = rgamma(1, v0, rate = d0)
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