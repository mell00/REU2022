
#setting up posterior

##loop through the k_ends to find the intervals
if(fit_storage == TRUE){
  fit = NULL
  squared_resids = NULL
  current_post_betas = NULL
  current_post_sigmas = NULL
  for(m in 2:length(k_ends)) {
    len = length(k_ends)
    if(m > 2){
      min = k_ends[m-1]+1
    }else{
      min = k_ends[m-1]
    }
    y_tp_values = NULL
    for(a in 1:ar){
      current_y_tp_values = full_data[c((min+ar-a):(k_ends[[m]]-ar+(ar-a))),2]
      y_tp_length <<- length(current_y_tp_values)
      y_tp_values = c(y_tp_values, current_y_tp_values, recursive=T)
    }
    x_j = matrix(c( rep(1, each=y_tp_length), y_tp_values, recursive=T), nrow=y_tp_length, ncol=(ar+1))
    y_j = full_data[c((min+ar):k_ends[[m]]),2] #getting the y values in the interval
    sigma = sd(y_j)
    
    #bar_v
    v = solve( (1/sigma) * (t(x_j) %*% x_j )+ solve(B_0) )
    #bar_beta 
    beta = v %*% ( (1/sigma) * (t(x_j) %*% y_j) + solve(B_0) %*% b_0 )
    
    #drawing a random variable from a multivariate normal pdf 
    post_beta = mvrnorm(1, beta, v)
    
    predicted_x = x_j %*% post_beta
    fit = c(fit, c(rep(NA, ar), predicted_x, recursive=T), recursive=T)
    squared_resid = (predicted_x - y_j)^2
    squared_resids = c(squared_resids, squared_resid, recursive=T)
    
    bar_v = c(bar_v, v)
    bar_beta = c(bar_beta, beta)
    
    #SIGMA:
    v0 = (max(k_ends))/2 + 2
    d0 = 0 + .5 * t(y_j - x_j %*% post_beta ) %*% (y_j - x_j %*% post_beta)
    
    sigma = rgamma(1, v0, rate = d0)
    post_sigma = 1 / sigma
    
    current_post_betas = cbind(current_post_betas, post_beta)
    current_post_sigmas = cbind(current_post_sigmas, post_sigma)
    
    if(m == len ) {
      MSE = mean(squared_resids)
      all_fits = rbind(all_fits, fit)
      all_MSE = rbind(all_MSE, MSE)
      current_post_betas = as.data.frame(current_post_betas)
      colnames(current_post_betas) = c(1:ncol(current_post_betas))
      post_beta_list = cbind(post_beta_list, current_post_betas)
      colnames(current_post_sigmas) = c(1:ncol(current_post_sigmas))
      post_sigma_list = cbind(post_sigma_list, current_post_sigmas)
    }
  }
}

if(progress == TRUE){    
  setTxtProgressBar(sample_progress, i)
}


#cleaning up beta/sigma draws
if(fit_storage == TRUE){
  colnames(all_MSE) = "MSE"
  colnames(all_fits) = c(1:ncol(all_fits))
  post_beta_list = post_beta_list[,-1]
  post_sigma_list = post_sigma_list[,-1]
  rownames(post_beta_list) = c(seq(0,ar,1))
  rownames(post_beta_list) = paste("B", rownames(post_beta_list), sep = "")
  rownames(post_sigma_list) = "Sigma"
  
  split_num = NULL #initializing
  
  for(i in 2:ncol(post_beta_list)){ #detecting where to split up columns in beta/sigma object
    if(startsWith(colnames(post_beta_list)[i], "1.") == TRUE){
      split_num = c(split_num, i)
    }
  }
  
  final_beta_list = list() #initializing
  
  for(i in 1:length(split_num)){ #splitting up columns in beta object
    
    if(i == 1){ #betas from first run
      final_beta_list[[i]] = post_beta_list[,1:(split_num[i]-1)]
      if(is.atomic(final_beta_list[[i]]) == T){
        final_beta_list[[i]] = as.data.frame(final_beta_list[[i]])
        rownames(final_beta_list[[i]]) = rownames(post_beta_list)
        colnames(final_beta_list[[i]]) = 1
      }else{
        colnames(final_beta_list[[i]]) = c(1:ncol(final_beta_list[[i]]))
      }
    }else if(i < length(split_num)){# betas from middle runs
      final_beta_list[[i]] = post_beta_list[,split_num[i-1]:(split_num[i]-1)]
      if(is.atomic(final_beta_list[[i]]) == T){
        final_beta_list[[i]] = as.data.frame(final_beta_list[[i]])
        rownames(final_beta_list[[i]]) = rownames(post_beta_list)
        colnames(final_beta_list[[i]]) = 1
      }else{
        colnames(final_beta_list[[i]]) = c(1:ncol(final_beta_list[[i]]))
      }
    }else{ #betas from penultimate and final runs
      final_beta_list[[i]] = post_beta_list[,split_num[i-1]:(split_num[i]-1)]
      if(is.atomic(final_beta_list[[i]]) == T){
        final_beta_list[[i]] = as.data.frame(final_beta_list[[i]])
        rownames(final_beta_list[[i]]) = rownames(post_beta_list)
        colnames(final_beta_list[[i]]) = 1
      }else{
        colnames(final_beta_list[[i]]) = c(1:ncol(final_beta_list[[i]]))
      }
      final_beta_list[[i+1]] = post_beta_list[,split_num[i]:ncol(post_beta_list)]
      if(is.atomic(final_beta_list[[i+1]]) == T){
        final_beta_list[[i+1]] = as.data.frame(final_beta_list[[i+1]])
        rownames(final_beta_list[[i+1]]) = rownames(post_beta_list)
        colnames(final_beta_list[[i+1]]) = 1
      }else{
        colnames(final_beta_list[[i+1]]) = c(1:ncol(final_beta_list[[i+1]]))
      }
    } 
  }
  
  post_beta_list = final_beta_list #saving final version of beta object
  
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

#calling the function
#test_data = test_data_44()
#current_result = baar(NA, test_data[,1], test_data[,2], 50, 50, jump=0.25, ar=1, progress=T, fit_storage=T)