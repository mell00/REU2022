#Complete BAAR (Bayesian Adaptive Auto-Regression)

#-------Key:
# k			= x-axis values of starting breakpoints
# time		= integer x-values of the entire data set
# data		= y-values of entire data set
# interations	= number of runs for final sampling with Metropolis-Hastings
# burn_in		= number of runs for set-up sampling with Metropolis-Hastings
# make_murder_p	= the combine proportion (decimal) for make and murder steps
# note: move proportion is 1 - make_murder_p
# percent		= how much a point can jiggle
# lambda		= for Poisson distribution of breakpoint prior
# jump_p		= proportion of move steps that will be jump
# note: jiggle proprtion is 1 - jump_p
# ar			= order of AR model
# progress		= whether to show progress bars or not, TRUE/FALSE

baar = function(k, time, data, iterations, burn_in = 50, make_murder_p = 0.5, percent = 0.02, lambda = 1, jump_p = 0.25, ar = 1, progress = TRUE){
  
  ar = floor(ar)
  
  if(length(time) != length(data)){
    return("Data and time vectors must be of equal length.")
  }else if(length(data) < (6 * ar)){
    return("Data insufficient for order of AR model. Try a lower order.") 
  }else if(make_murder_p >= 1){
    return("Make/murder proportion must be less than 1.")
  }else if(percent >= 0.5){
    return("Percent for jiggle neighrborhood must be less than 0.5.")
  }else if(jump_p > 1){
    return("Jump proportion must be less than or equal to 1.")
  }
  
  library(MASS)
  library(FitAR)
  full_data = cbind(c(1:length(as.numeric(time))), as.numeric(data)) #combining time and data inputs
  n = length(full_data[,1]) #number of observations
  k_ends <<- c(min(full_data[,1]), na.omit(k), n) #adding end points to k 
  
  #function to get sum of log likelihoods
  fitMetrics<-function(k_ends, full_data){
    
    #create sum objects
    sum_loglik = 0
    coef_1 = 0 
    coef_2 = 0 
    #get and sum log likelihood for regressions of all intervals
    if(length(k_ends) < 3 ){
      model = suppressWarnings(FitAR(full_data[,2], p=ar))
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
          sum_loglik = sum_loglik + sub_loglik #the logLik looks the log likelyhood (relates to both SSR and MLE)
        }
      }
    }
    return(sum_loglik)
  }
  
  #function to find all available spaces to place new breakpoints in data set
  freeObservations<-function(k_ends, ar){
    full_set = c(1:max(k_ends))
    if(ar == 1){
      constraint = (3 - 1)
    }else{
      constraint = (2*ar-1)
    }
    exclude_set = k_ends
    for(a in 1:length(k_ends)){
      if(a == 1){
        right = k_ends[[a]] + constraint
        right_set = full_set[k_ends[[a]]:right]
        exclude_set = c(exclude_set, right_set, recursive=T)
      }else if(a == length(k_ends)){
        left = k_ends[[a]] - constraint
        left_set = full_set[left:k_ends[[a]]]
        exclude_set = c(exclude_set, left_set, recursive=T)
      }else{
        right = k_ends[[a]] + constraint
        right_set = full_set[k_ends[[a]]:right]
        left = k_ends[[a]] - constraint
        left_set = full_set[left:k_ends[[a]]]
        exclude_set = c(exclude_set, left_set, right_set, recursive=T)
      }
    }
    diff_set = setdiff(full_set,exclude_set)
    return(sort(diff_set))
  }
  
  #function to randomly add a new breakpoint
  barMake<-function(k_ends){
    
    diff_set = freeObservations(k_ends, ar)
    if(length(diff_set) < 2){
      rand_spot = diff_set
    }else{
      rand_spot = sample(diff_set, 1) #selects a random spots
    }
    k_ends_final = sort(c(k_ends, rand_spot)) #adds the random spot and sorts it 
    return(k_ends_final)
    
  }
  
  
  #function to randomly subtract a breakpoint 
  barMurder<-function(k_ends){
    
    k = k_ends[c(-1,-length(k_ends))] #removes the end points
    if(length(k) == 1){
      random_num = 1
    }else{
      random_num = sample(1:length(k), 1) #selects a random breakpoint
    }
    k_ends_final = k_ends[-(random_num+1)] #removes that selected breakpoint
    return(k_ends_final)
    
  }
  
  #function to move a breakpoint to any new location
  barMove<-function(k_ends){
    
    k_ends_less = barMurder(k_ends) #kills a point
    k_ends_final = barMake(k_ends_less) #remakes a point
    return(k_ends_final)
    
  }
  
  #function to move a breakpoint within a small user-defined neighborhood
  barJiggle<-function(percent, k_ends){
    
    k = k_ends[c(-1,-length(k_ends))]
    random_num = sample(1:length(k), 1)
    random_bkpt = k_ends[random_num+1]
    full_data = c(1:max(k_ends))
    wiggliness = max(k_ends) * percent
    prelim_neighborhood = full_data[floor(random_bkpt-wiggliness):ceiling(random_bkpt+wiggliness)]
    left_neighbor = k_ends[random_num]
    right_neighbor = k_ends[random_num+2]
    if(ar == 1){
      constraint = (3 - 1)
    }else{
      constraint = (2*ar-1)
    }
    ll_limit = left_neighbor-constraint
    lr_limit = left_neighbor+constraint
    rl_limit = right_neighbor-constraint
    rr_limit = right_neighbor+constraint
    if(ll_limit < 0){ll_limit = 0}
    if(rr_limit > max(k_ends)){rr_limit = max(k_ends)}
    exclusions = sort(c(full_data[ll_limit:lr_limit], full_data[rl_limit:rr_limit]))
    final_neighborhood = setdiff(prelim_neighborhood, exclusions)
    
    if(length(final_neighborhood) > 1){
      new_location = sample(final_neighborhood, 1)
      k_ends_less = k_ends[-(random_num+1)]
      final_k_ends = sort(c(k_ends_less, new_location))
      return(final_k_ends)
    }else if(length(final_neighborhood) == 1){
      new_location = final_neighborhood
      k_ends_less = k_ends[-(random_num+1)]
      final_k_ends = sort(c(k_ends_less, new_location))
      return(final_k_ends)
    }
    else{
      return("jiggle failure")
    }
    
  }
  
  #function to propose a new breakpoint set (also returns qs and step type)
  newEnds<-function(k_ends, make_k, murder_k){
    
    u_step = runif(1) #random number from 0 to 1 taken from a uniform distribution for selecting step
    
    if(ar == 1){
      constraint = 5
    }else{
      constraint = ar * 4
    }
    
    if(max(diff(k_ends)) >= constraint & length(k_ends) < 3 | max(diff(k_ends)) >= constraint & u_step <= make_k){
      type = "Birth"
      a.count <<- a.count + 1
      k_ends_new = barMake(k_ends) #make
      
      #setting up qs for ratio
      q1 = murder_k/(length(k_ends_new)-2)
      n_free = length(freeObservations(k_ends,ar))
      q2 = make_k/n_free
      
    } else if(u_step > make_k & u_step <= (make_k + murder_k)){
      type = "Death"
      s.count <<- s.count + 1
      k_ends_new = barMurder(k_ends) #murder
      
      #setting up qs for ratio
      n_free = length(freeObservations(k_ends_new,ar))
      q1 = make_k/n_free
      q2 = murder_k/(length(k_ends)-2)
      
    } else {
      move_u = runif(1)
      if(move_u < jump_p){
        type = "Jump"
        m.count <<- m.count + 1
        k_ends_new = barMove(k_ends) #jump
        
        #fake qs because they cancel
        q1 = 1
        q2 = 1
      }else{
        type = "Jiggle"
        j.count <<- j.count + 1
        k_ends_new = barJiggle(percent, k_ends) #jiggle
        
        if(k_ends_new[[1]] == "jiggle failure"){
          k_ends_new = k_ends
        }
        
        #fake qs because they cancel
        q1 = 1
        q2 = 1
      }
    }
    
    return(list(k_ends_new, q1, q2, type))
  }
  
  #setting up counters for burn-in Metropolis-Hastings
  type = "0"
  a.count <<- 0
  s.count <<- 0 
  m.count <<- 0
  j.count <<- 0
  add.accept.count <<- 0
  sub.accept.count <<- 0
  move.accept.count <<- 0
  jiggle.accept.count <<- 0
  
  #getting constants for qs for burn-in Metropolis-Hastings
  starting_bkpts = length(k_ends) - 1 #most probable number of breakpoints based on starting info 
  starting_nfree = length(freeObservations(k_ends,ar)) #most probable n_free based on starting info
  starting_ttl = starting_bkpts + starting_nfree #total to get percentages
  make_k = make_murder_p * (starting_nfree/starting_ttl) #proportion for make
  murder_k = make_murder_p * (starting_bkpts/starting_ttl) #proportion for murder
  
  if(progress == TRUE){
    writeLines("\nBeginning burn period.")
    burn_progress <- txtProgressBar(min = 0, max = burn_in, style = 3)
  }
  
  #Burn Metropolis Hasting
  for(i in 1:burn_in){
    
    old_loglik = fitMetrics(k_ends, full_data) #gets log likelihood for existing breakpoints
    
    k_and_q = newEnds(k_ends, make_k, murder_k)
    k_ends_new = k_and_q[[1]]
    q1 = k_and_q[[2]]
    q2 = k_and_q[[3]]
    
    new_loglik = fitMetrics(k_ends_new, full_data)
    
    delta_bic = (-2*new_loglik + log(n)*(length(k_ends_new)-1)*(3+ar)) - (-2*old_loglik + log(n)*(length(k_ends)-1)*(3+ar))
    ratio = (-1*delta_bic/2) + (log(q1*dpois(length(k_ends_new)-2,lambda)) - log(q2*dpois(length(k_ends)-2,lambda)))
    u_ratio = log(runif(1)) #random number from 0 to 1 taken from a uniform distribution and then log transformed
    
    if(abs(delta_bic) == Inf){ #safe guard against random models creating infinite ratios
      k_ends <<- k_ends #old
    } else if(ratio > u_ratio) {
      k_ends <<- k_ends_new #new
    } else {
      k_ends <<- k_ends #old
    }
    
    if(progress == TRUE){
      setTxtProgressBar(burn_progress, i)
    }
    
  }
  
  #initializing matrices/storage objects for final Metropolis-Hasting
  all_k_best = matrix(NA, nrow=1, ncol=(n/3))
  all_k_pros = matrix(NA, nrow=1, ncol=(n/3))
  bar_v = 0
  bar_beta = 0
  all_fits = data.frame()
  all_MSE = data.frame()
  all_BIC = data.frame()
  accept_count = 0
  
  #setting up counters for final Metropolis-Hasting
  type = "0"
  a.count <<- 0
  s.count <<- 0 
  m.count <<- 0
  j.count <<- 0
  add.accept.count <<- 0
  sub.accept.count <<- 0
  move.accept.count <<- 0
  jiggle.accept.count <<- 0
  
  #setting up priors for beta draws
  alt_arima<-function(full_data, ar){
    tryCatch(arima(full_data[,2], method="ML", order=c(ar,0,0)), error = function(e) arima(full_data[,2], method="CSS", order=c(ar,0,0)))
  }
  model = alt_arima(full_data, ar)
  fisher = solve(model$var.coef)
  smiley = n * fisher
  
  coef_list = model$coef[[length(model$coef)]]
  
  for(a in 1:(length(model$coef)-1)){
    
    coef_list = c(coef_list, model$coef[[a]], recursive=T)
    
  }
  
  b_0 = matrix(coef_list,(ar+1),1) #matrix of beta means for posterior draw
  B_0 = smiley #variance-covariance matrix for posterior draw
  
  #beta and sigma draw
  post_beta_list = data.frame(Empty=rep(NA,(ar+1)))
  post_sigma_list = data.frame(Empty=NA)
  
  #getting constants for qs for final Metropolis-Hasting
  starting_bkpts = length(k_ends) - 1 #most probable number of breakpoints based on starting info 
  starting_nfree = length(freeObservations(k_ends,ar))
  starting_ttl = starting_bkpts + starting_nfree #total to get percentages
  make_k = make_murder_p * (starting_nfree/starting_ttl) #proportion for make
  murder_k = make_murder_p * (starting_bkpts/starting_ttl) #proportion for murder
  
  if(progress == TRUE){
    writeLines("\nBeginning sampling period.")
    sample_progress <- txtProgressBar(min = 0, max = iterations, style = 3)
  }
  
  type_step_total = " "
  type_step = " "
  #Final Metroplis Hastings 
  for(i in 1:iterations){
    
    old_loglik = fitMetrics(k_ends, full_data) #calls fit matrix to have a function to start with
    
    k_and_q = newEnds(k_ends, make_k, murder_k)
    k_ends_new = k_and_q[[1]]
    q1 = k_and_q[[2]]
    q2 = k_and_q[[3]]
    type = k_and_q[[4]]
    
    
    new_loglik = fitMetrics(k_ends_new, full_data)
    
    delta_bic = (-2*new_loglik + log(n)*(length(k_ends_new)-1)*(3+ar)) - (-2*old_loglik + log(n)*(length(k_ends)-1)*(3+ar))
    ratio = (-1*delta_bic/2) + (log(q1*dpois(length(k_ends_new)-2,lambda)) - log(q2*dpois(length(k_ends)-2,lambda)))
    u_ratio = log(runif(1)) #random number from 0 to 1 taken from a uniform distribution and then log transformed
    
    
    
    if(abs(delta_bic) == Inf){ #safe guard against random models creating infinite ratios
      k_ends <<- k_ends #old
      bic = (-2*old_loglik + log(n)*(length(k_ends)-1)*(3+ar))
    } else if(ratio > u_ratio) {
      k_ends <<- k_ends_new #new
      bic = (-2*new_loglik + log(n)*(length(k_ends_new)-1)*(3+ar))
      accept_count = accept_count + 1
      #looking at what type of step is done and accepted
      if(type == "Birth") {
        add.accept.count <<- add.accept.count + 1
      } else if(type == "Death") {
        sub.accept.count <<- sub.accept.count + 1
      } else if(type == "Jump") {
        move.accept.count <<- move.accept.count + 1
      } else if(type == "Jiggle") {
        jiggle.accept.count <<- jiggle.accept.count + 1
      }
    } else {
      k_ends <<- k_ends #old
      bic = (-2*old_loglik + log(n)*(length(k_ends)-1)*(3+ar))
    }
    
    type_step_total = c(type_step_total, type)
    
    #condensing the data
    k_ends_best_print = c(k_ends, rep(NA, (n/3)-length(k_ends)))
    all_k_best = rbind(all_k_best, k_ends_best_print)
    
    #condensing the data
    k_ends_pros_print = c(k_ends_new, rep(NA, (n/3)-length(k_ends_new)))
    all_k_pros = rbind(all_k_pros, k_ends_pros_print)
    
    all_BIC = rbind(all_BIC, bic)
    
    #setting up posterior
    
    ##loop through the k_ends to find the intervals 
    squared_resids = NULL
    fit = NULL
    current_post_betas = NULL
    current_post_sigmas = NULL
    for(m in 2:length(k_ends)) {
      len = length(k_ends)
      if(m > 2){
        min = k_ends[m-1]+1
      }else{
        min = k_ends[m-1]
      }
      x_values = NULL
      for(a in 1:ar){
        current_x_values = full_data[c((min+ar-a):(k_ends[[m]]-ar+(ar-a))),1]
        x_length <<- length(current_x_values)
        x_values = c(x_values, current_x_values, recursive=T)
      }
      x_j = matrix(c( rep(1, each=x_length), x_values, recursive=T), nrow=x_length, ncol=(ar+1))
      y_j = full_data[c((min+ar):k_ends[[m]]),2] #getting the y values in the interval
      sigma = sd(y_j)
      
      #bar_v
      v = solve( (1/sigma) * (t(x_j) %*% x_j )+ solve(B_0) )
      #bar_beta 
      beta = v %*% ( (1/sigma) * (t(x_j) %*% y_j) + solve(B_0) %*% b_0 )
      
      predicted_x = x_j %*% beta
	fit = c(fit, c(rep(NA,ar), predicted_x, recursive=T),recursive=T)
      squared_resid = (predicted_x - y_j)^2
      squared_resids = c(squared_resids, squared_resid, recursive=T)
      
      #drawing a random variable from a multivariate normal pdf 
      post_beta = mvrnorm(1, beta, v)
      
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
	  all_fits = rbind(all_fits, fit)
        MSE = mean(squared_resids)
        all_MSE = rbind(all_MSE, MSE)
        current_post_betas = as.data.frame(current_post_betas)
        colnames(current_post_betas) = c(1:ncol(current_post_betas))
        post_beta_list = cbind(post_beta_list, current_post_betas)
        colnames(current_post_sigmas) = c(1:ncol(current_post_sigmas))
        post_sigma_list = cbind(post_sigma_list, current_post_sigmas)
      }
    }
    
    if(progress == TRUE){    
      setTxtProgressBar(sample_progress, i)
    }
    
  }
  
  if(progress == TRUE){      
    writeLines("\n")
  }
  
  #cleaning up the matrices 
  all_k_best = all_k_best[-1,colSums(is.na(all_k_best))<nrow(all_k_best)]
  clean_max = max(all_k_best[1,], na.rm=TRUE)
  all_k_best = ifelse(all_k_best == clean_max,NA,all_k_best)
  all_k_best = data.frame(all_k_best[,c(-1,-ncol(all_k_best))], row.names=NULL)
  
  #cleaning up the matrices 
  all_k_pros = all_k_pros[-1,colSums(is.na(all_k_pros))<nrow(all_k_pros)]
  clean_max = max(all_k_pros[1,], na.rm=TRUE)
  all_k_pros = ifelse(all_k_pros == clean_max,NA,all_k_pros)
  all_k_pros = data.frame(all_k_pros[,c(-1,-ncol(all_k_pros))], row.names=NULL)
  
  post_beta_list = post_beta_list[,-1]
  post_sigma_list = post_sigma_list[,-1]
  
  colnames(all_MSE) = "MSE"
  colnames(all_BIC) = "BIC"
  rownames(post_beta_list) = c(seq(0,ar,1)) #FIX HERE
  rownames(post_beta_list) = paste("B", rownames(post_beta_list), sep = "")
  rownames(post_sigma_list) = "Sigma"
  
  final.propose = c(a.count, s.count, m.count, j.count)
  final.accept = c(add.accept.count, sub.accept.count, move.accept.count, jiggle.accept.count)
  
  #getting distribution of k (number of breakpoints)
  num_bkpts = list()
  for(i in 1:iterations){
    current_k = length(all_k_best[i,][!is.na(all_k_best[i,])])
    num_bkpts = c(num_bkpts, current_k, recursive=T)
  }
  
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
      colnames(final_beta_list[[i]]) = c(1:ncol(final_beta_list[[i]]))
    }else if(i < length(split_num)){# betas from middle runs
      final_beta_list[[i]] = post_beta_list[,split_num[i-1]:(split_num[i]-1)]
      colnames(final_beta_list[[i]]) = c(1:ncol(final_beta_list[[i]]))
    }else{ #betas from penultimate and final runs
      final_beta_list[[i]] = post_beta_list[,split_num[i-1]:(split_num[i]-1)]
      colnames(final_beta_list[[i]]) = c(1:ncol(final_beta_list[[i]]))
      final_beta_list[[i+1]] = post_beta_list[,split_num[i]:ncol(post_beta_list)]
      colnames(final_beta_list[[i+1]]) = c(1:ncol(final_beta_list[[i+1]]))
    } 
  }
  
  post_beta_list = final_beta_list #saving final version of beta object
  
  final_sigma_list = list() #initializing
  
  for(i in 1:length(split_num)){ #splitting up columns in sigma object
    
    if(i == 1){ #sigmas from first run
      final_sigma_list[[i]] = post_sigma_list[,1:(split_num[i]-1)]
      colnames(final_sigma_list[[i]]) = c(1:ncol(final_sigma_list[[i]]))
    }else if(i < length(split_num)){# sigmas from middle runs
      final_sigma_list[[i]] = post_sigma_list[,split_num[i-1]:(split_num[i]-1)]
      colnames(final_sigma_list[[i]]) = c(1:ncol(final_sigma_list[[i]]))
    }else{ #sigma from penultimate and final runs
      final_sigma_list[[i]] = post_sigma_list[,split_num[i-1]:(split_num[i]-1)]
      colnames(final_sigma_list[[i]]) = c(1:ncol(final_sigma_list[[i]]))
      final_sigma_list[[i+1]] = post_sigma_list[,split_num[i]:ncol(post_sigma_list)]
      colnames(final_sigma_list[[i+1]]) = c(1:ncol(final_sigma_list[[i+1]]))
    } 
  }
  
  post_sigma_list = final_sigma_list #saving final version of sigma object
  
  type_step_total = type_step_total[-1]
  
  final_list = list(accept_count / iterations, final.propose, final.accept, all_MSE, all_BIC, all_k_best, num_bkpts, post_beta_list, post_sigma_list, all_k_pros, type_step_total, all_fits)
  names(final_list) = c("AcceptRate", "ProposedSteps", "AcceptedSteps", "MSE", "BIC", "Breakpoints", "NumBkpts", "Beta", "Sigma", "Proposed", "type_step_total", "Fits")
  
  return(final_list)
}

#calling the function
set.seed(1)
first <- arima.sim(model = list(ar = 0.01, order = c(1,0,0)), n = 45)
second <- arima.sim(model = list(ar = 0.9, order = c(1,0,0)), n = 45)
time = c(1:90)
data_11 = c(second, first)
test_data_11 = data.frame(time,data_11)
plot(test_data_11, main="Simulated Time Series Data", ylab = "Dependent Variable", xlab="Time")

current_result = baar(c(40), test_data_11[,1], test_data_11[,2], 3000, 2, jump=0.25, ar=1, progress=T)
current_result$Breakpoints[[1]]
current_result$Proposed$X1
current_result$type_step_total

#using provided fits
fits_to_use = current_result$Fits[which(current_result$Breakpoints[,1] == 45 & current_result$NumBkpts == 1),]
points(c(2:45),colMeans(fits_to_use)[2:45],col="red", pch=19)
points(c(47:90),colMeans(fits_to_use)[47:90],col="blue", pch=19)

#using arima fitted values for fit
library("forecast")

fitted_one = fitted(arima(data_11[c(1:45)],order=c(1,0,0),method="ML"))
fitted_two = fitted(arima(data_11[c(46:90)],order=c(1,0,0),method="ML"))
lines(test_data_11)
points(c(1:45), fitted_one, col="red")
points(c(46:90), fitted_two, col="blue")
lines(c(1:45), fitted_one, col="red")
lines(c(46:90), fitted_two, col="blue")

fitted_full = fitted(arima(data_11,order=c(1,0,0),method="ML"))
points(c(1:90), fitted_full, col="blue", pch=19)


current_result$Breakpoints[[1]]
hist(c(current_result$Breakpoints[[1]],current_result$Breakpoints[[2]]), breaks = 90, xlim=c(1,90), ylim=c(0,3000), xlab="Location of Breakpoint(s)", ylab = "Number of Iterations (Out of 3000)", main="Distribution of Breakpoint Location(s)", col="green3")

hist(current_result$NumBkpts, main="Distribution of Breakpoint Number", ylim=c(0,3000), xlab= "Number of Breakpoints", ylab = "Number of Iterations (Out of 3000)", col="green3", breaks=c(.5,1.5,2.5,3.5))

for(i in 1:length(current_result$Breakpoints[[1]])) {
  plot(test_data_11, main=current_result$type_step_total[i], ylab = "Dependent Variable", xlab="Time")
  points(test_data_11[current_result$Proposed$X1[i],1],test_data_11[current_result$Proposed$X1[i],2], col="blue", cex = 1.5 ,pch=16) 
  if(!is.na(current_result$Proposed$X2[i])) {
    points(test_data_11[current_result$Proposed$X2[i],1],test_data_11[current_result$Proposed$X2[i],2], col="blue", cex = 1.5 ,pch=16) 
  }
  points(test_data_11[current_result$Breakpoints[[1]][i],1],test_data_11[current_result$Breakpoints[[1]][i],2], col="green3", cex = 1.5 ,pch=16) 
}
points(test_data_11[current_result$Breakpoints[[1]][1],1],test_data_11[current_result$Breakpoints[[1]][1],2], col="green3", cex = 1.5 ,pch=16) 

par(mfrow=c(1,2))
plot(test_data_11, main="Simulated Time Series Data", ylab = "Dependent Variable", xlab="Time")
points(test_data_11[40,1],test_data_11[40,2],col="green3",pch=16, cex=1.5)
points(test_data_11[10,1],test_data_11[10,2],col="green3",pch=16, cex=1.5)


plot(test_data_11, main="Simulated Time Series Data", ylab = "Dependent Variable", xlab="Time")
points(test_data_11[40,1],test_data_11[40,2],col="green3",pch=16, cex=1.5)
points(test_data_11[47,1],test_data_11[47,2],col="blue",pch=16, cex=1.5)
points(test_data_11[10,1],test_data_11[10,2],col="green3",pch=16, cex=1.5)

