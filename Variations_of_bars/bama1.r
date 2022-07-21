#Complete BAMA (Bayesian Adaptive Moving-Average)

#-------Key:
# k			= x-axis values of starting breakpoints
# time		= integer x-values of the entire data set
# data		= y-values of entire data set
# iterations	= number of runs for final sampling with Metropolis-Hastings
# burn_in		= number of runs for set-up sampling with Metropolis-Hastings
# make_murder_p	= th e combine proportion (decimal) for make and murder steps
# note: move proportion is 1 - make_murder_p
# percent		= how much a point can jiggle
# lambda		= for Poisson distribution of breakpoint prior
# tao       = for multivariate normal distribution of new theta
# alpha     = for multivariate normal distribution of new theta
# mu        = for multivariate normal distribution of new theta
# jump_p		= proportion of move steps that will be jump
# note: jiggle proportion is 1 - jump_p
# ma			= order of MA model
# progress		= whether to show progress bars or not, TRUE/FALSE
# fit_storage	= whether or not to store betas, sigmas, and fits for each iteration, TRUE/FALSE

bama = function(k, time, data, iterations, burn_in = 50, make_murder_p = 0.5, percent = 0.02, lambda = 1, tao = abs(1), alpha = 0, mu = 1.5, jump_p = 0.25, ma = 1, progress = TRUE, fit_storage = TRUE){
  
  ma = floor(ma)
  
  if(length(time) != length(data)){
    return("Data and time vectors must be of equal length.")
  }else if(length(data) < (6 * ma)){
    return("Data insufficient for order of MA model. Try a lower order.") 
  }else if(make_murder_p >= 1){
    return("Make/murder proportion must be less than 1.")
  }else if(percent >= 0.5){
    return("Percent for jiggle neighrborhood must be less than 0.5.")
  }else if(jump_p > 1){
    return("Jump proportion must be less than or equal to 1.")
  }
  
  library(MASS)
  library(forecast)
  
  full_data = cbind(c(1:length(as.numeric(time))), as.numeric(data)) #combining time and data inputs
  data.ts = arima(full_data[1,],order=c(0,0,ma)) #fit data using ARIMA
  n = length(full_data[,1]) #number of observations
  k_ends <<- c(min(full_data[,1]), na.omit(k), n) #adding end points to k
  init_theta_list = full_data$model$theta #draw existing theta prior from data
  init_sigma2 = data.ts$sigma2
  sigma_mtrx = diag(tao,length(theta_list))
  init_epsilon_list = rnorm(n,0,init_sigma2) #needs to be fixed, ignore for now
  
  #-----------------------------------------------------------------------------------------------------
  thetaDraw<-function(){
    theta_new = mvrnorm(n=1,mu=theta_list,Sigma=sigma_mtrx,tol = 1e-6, empirical = FALSE,EISPACK = FALSE)
    return(theta_new)
  }
  
  epsilonDraw = function(ma, k_ends, y_values, mu, epsilon_list){
    if (ma == 0){
      
    }
    if (ma == 1){
      y_values[ma] - mu
    }
    else{
      for (i in 2:ma){
        epsilon_list[i] = y_values[ma] - mu - alpha*epsilon_list[i-1]
      }
      return(epsilon_list)
    }
  }
  #-----------------------------------------------------------------------------------------------------
  
  #function to get sum of log likelihoods
  fitMetrics<-function(k_ends, full_data){
    
    #create sum objects
    sum_loglik = 0
    coef_1 = 0 
    coef_2 = 0
    
    sum_loglik = function(){ #loglikelihood calculation
      for (i in 1:length(full_data)-1){
        sum_loglik = prod(dnorm(y_values[i],alpha+beta*full_data[i],sigma,log=TRUE))
      }
      return(sum_loglik)
    }
    
    #get and sum log likelihood for regressions of all intervals
    logl <- function(sigma,alpha,beta,data.ts) { #log likelihood calculation
      sum_loglik = 0
      for (i in 1:length(data.ts)){
        if(length(k_ends) < 3 ){
          SEE = sum(na.omit(data.ts$residuals)^2) #?
          s2 = SEE/n #?
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
  }
  
  #function to find all available spaces to place new breakpoints in data set
  freeObservations<-function(k_ends, ma){
    full_set = c(1:max(k_ends))
    if(ar == 1){
      constraint = (3 - 1)
    }else{
      constraint = (2*ma-1)
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
    
    diff_set = freeObservations(k_ends, ma)
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
    if(floor(random_bkpt-wiggliness) > 0){
      left_lim = floor(random_bkpt-wiggliness)
    }else{
      left_lim = 1
    }
    if(ceiling(random_bkpt+wiggliness) < max(k_ends)){
      right_lim = ceiling(random_bkpt+wiggliness)
    }else{
      right_lim = max(k_ends)
    }
    prelim_neighborhood = full_data[left_lim:right_lim]
    left_neighbor = k_ends[random_num]
    right_neighbor = k_ends[random_num+2]
    if(ar == 1){
      constraint = (3 - 1)
    }else{
      constraint = (2*ar-1)
    }
    lr_limit = left_neighbor+constraint
    rl_limit = right_neighbor-constraint
    exclusions = sort(c(full_data[1:lr_limit], full_data[rl_limit:max(k_ends)]))
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
      type = "add"
      a.count <<- a.count + 1
      k_ends_new = barMake(k_ends) #make
      
      #setting up qs for ratio
      q1 = murder_k/(length(k_ends_new)-2)
      n_free = length(freeObservations(k_ends,ma))
      q2 = make_k/n_free
      
    } else if(u_step > make_k & u_step <= (make_k + murder_k)){
      type = "sub"
      s.count <<- s.count + 1
      k_ends_new = barMurder(k_ends) #murder
      
      #setting up qs for ratio
      n_free = length(freeObservations(k_ends_new,ma))
      q1 = make_k/n_free
      q2 = murder_k/(length(k_ends)-2)
      
    } else {
      move_u = runif(1)
      if(move_u < jump_p){
        type = "move"
        m.count <<- m.count + 1
        k_ends_new = barMove(k_ends) #jump
        
        #fake qs because they cancel
        q1 = 1
        q2 = 1
      }else{
        type = "jiggle"
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
  starting_nfree = length(freeObservations(k_ends,ma)) #most probable n_free based on starting info
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
    
    theta_new = mvrnorm(n=1,mu=theta_list,Sigma=sigma_mtrx,tol = 1e-6, empirical = FALSE,EISPACK = FALSE)
    new_loglik = fitMetrics(k_ends_new, full_data)
    
    #birth and death ratios
    
    product_runs = c()
    
    old_new_product_birth = function(k){
      for (j in k){
        product_runs.append(n-(3*ar-(q1+1)(2*ar + j)))
      }
      return(prod(product_runs))
    }
    product_death = function(k){
      for (j in k){
        product_runs.append(n-3*ma-q1*2*ma + j)
      }
      return(prod(product_runs))
    }
    old_new_product_death = function(k){
      for (j in k){
        product_runs.append(n-3*ma-(q1-1)*(2*ma)+j)
      }
      return(prod(product_runs))
    }
    
    prior = NA
    
    birth_old_to_new_ratio = (factorial(num_of_bkpts+1)*q2(dpois(k_ends_new,lambda)))/(old_new_product_birth(num_of_bkpts)*prior)
    birth_new_to_old_ratio = (factorial(num_of_bkpts)*q1(dpois(k_ends,lambda)))/(product_death(num_of_bkpts)*prior)
    death_new_to_old_ratio = (factorial(num_of_bkpts)*q2*(dpois(k_ends,lambda)))/(product_death(num_of_bkpts)*prior)
    death_old_to_new_ratio = (factorial(num_of_bkpts-1)*make_k*(dpois(k_ends_new,lambda)))/(old_new_product_death(num_of_bkpts)*prior)
    #
    #end of birth and death ratios
    
    delta_bic = (-2*new_loglik + log(n)*(length(k_ends_new)-1)*(3+ma)) - (-2*old_loglik + log(n)*(length(k_ends)-1)*(3+ma))
    ratio = (-1*delta_bic/2) + (log(q1*dnorm(length(k_ends_new)-2,epsilon_list[ma],init_sigma2)) - log(q2*dnorm(length(k_ends)-2,epsilon_list[ma],init_sigma2))) #both dnorms are the same, make sure that this is correct
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
  all_k_best = data.frame(matrix(ncol=(length(k_ends)-2),nrow=0))
  if(fit_storage == TRUE){
    bar_v = 0
    bar_beta = 0
    fit = 0
    all_fits = data.frame()
    all_MSE = data.frame()
  }
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
  
  #getting constants for qs for final Metropolis-Hasting
  starting_bkpts = length(k_ends) - 1 #most probable number of breakpoints based on starting info 
  starting_nfree = length(freeObservations(k_ends,ma))
  starting_ttl = starting_bkpts + starting_nfree #total to get percentages
  make_k = make_murder_p * (starting_nfree/starting_ttl) #proportion for make
  murder_k = make_murder_p * (starting_bkpts/starting_ttl) #proportion for murder
  
  if(progress == TRUE){
    writeLines("\nBeginning sampling period.")
    sample_progress <- txtProgressBar(min = 0, max = iterations, style = 3)
  }
  
  #Final Metropolis Hastings 
  for(i in 1:iterations){
    
    old_loglik = fitMetrics(k_ends, full_data) #calls fit matrix to have a function to start with
    
    k_and_q = newEnds(k_ends, make_k, murder_k)
    k_ends_new = k_and_q[[1]]
    q1 = k_and_q[[2]]
    q2 = k_and_q[[3]]
    type = k_and_q[[4]]
    
    new_loglik = fitMetrics(k_ends_new, full_data)
    
    #birth and death ratios
    
    product_runs = c()
    
    old_new_product_birth = function(k){
      for (j in k){
        product_runs.append(n-(3*ar-(q1+1)(2*ar + j)))
      }
      return(prod(product_runs))
    }
    product_death = function(k){
      for (j in k){
        product_runs.append(n-3*ma-q1*2*ma + j)
      }
      return(prod(product_runs))
    }
    old_new_product_death = function(k){
      for (j in k){
        product_runs.append(n-3*ma-(q1-1)*(2*ma)+j)
      }
      return(prod(product_runs))
    }
    
    prior = NA
    
    birth_old_to_new_ratio = (factorial(num_of_bkpts+1)*q2(dpois(k_ends_new,lambda)))/(old_new_product_birth(num_of_bkpts)*prior)
    birth_new_to_old_ratio = (factorial(num_of_bkpts)*q1(dpois(k_ends,lambda)))/(product_death(num_of_bkpts)*prior)
    death_new_to_old_ratio = (factorial(num_of_bkpts)*q2*(dpois(k_ends,lambda)))/(product_death(num_of_bkpts)*prior)
    death_old_to_new_ratio = (factorial(num_of_bkpts-1)*make_k*(dpois(k_ends_new,lambda)))/(old_new_product_death(num_of_bkpts)*prior)
    #
    #end of birth and death ratios
    delta_bic = (-2*new_loglik + log(n)*(length(k_ends_new)-1)*(3+ma)) - (-2*old_loglik + log(n)*(length(k_ends)-1)*(3+ma))
    ratio = (-1*delta_bic/2) + (log(q1*dnorm(length(k_ends_new)-2,epsilon_list[ma],init_sigma2)) - log(q2*dnorm(length(k_ends)-2,epsilon_list[ma],init_sigma2))) #both dnorms are the same, make sure that this is correct
    u_ratio = log(runif(1)) #random number from 0 to 1 taken from a uniform distribution and then log transformed
    
    if(abs(delta_bic) == Inf){ #safe guard against random models creating infinite ratios
      k_ends <<- k_ends #old
      bic = (-2*old_loglik + log(n)*(length(k_ends)-1)*(3+ma))
    } else if(ratio > u_ratio) {
      k_ends <<- k_ends_new #new
      bic = (-2*new_loglik + log(n)*(length(k_ends_new)-1)*(3+ma))
      accept_count = accept_count + 1
      #looking at what type of step is done and accepted
      if(type == "add") {
        add.accept.count <<- add.accept.count + 1
      } else if(type == "sub") {
        sub.accept.count <<- sub.accept.count + 1
      } else if(type == "move") {
        move.accept.count <<- move.accept.count + 1
      } else if(type == "jiggle") {
        jiggle.accept.count <<- jiggle.accept.count + 1
      }
    } else {
      k_ends <<- k_ends #old
      bic = (-2*old_loglik + log(n)*(length(k_ends)-1)*(3+ma))
    }
    
    k = k_ends[c(-1,-length(k_ends))]
    if(length(k) > ncol(all_k_best)){
      all_k_best = cbind(all_k_best, rep(NA,nrow(all_k_best)))
      all_k_best = rbind(all_k_best, k)
    }else if(length(k) < ncol(all_k_best)){
      k = c(k, rep(NA, (ncol(all_k_best)-length(k))), recursive=T)
      all_k_best = rbind(all_k_best, k)
    }else{
      all_k_best = rbind(all_k_best, k)
    }
    all_BIC = rbind(all_BIC, bic)
  
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

#calling the function
#test_data = test_data_44()
#current_result = baar(NA, test_data[,1], test_data[,2], 50, 50, jump=0.25, ma=1, progress=T, fit_storage=T)