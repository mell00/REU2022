#complete BAR - Variation 0 (Random/Random/Random)

#-------Key:
# k           = breakpoint's x-axis values 
# time        = integer x-values of the entire data set 
# interations = number of runs through Metropolis hastings 
# make        = the proportion (decimal) of the make step to occuring
  #note: murder is set to same as make, move is calculated subtracting 2*make from 1 


bar0 = function(k, time, data, iterations, make){
  
  library(MASS)
  
  prob_mmm = c(make, make) #combining the two probabilties of make and murder that the user specifies 
  
  full_data = cbind(as.numeric(time), as.numeric(data)) #combing the time and data inputs from user
  
  n = max(full_data[,1]) #finding max value
  
  k_ends = c(min(full_data[,1]), na.omit(k), n) #adding in end points to k values 
  
  fitMetrics<-function(k_ends, full_data){
    
    #create sum objects
    sum_loglik = 0
    
    #get and sum log likelihood for regressions of all intervals
    if(length(k_ends) < 3 ){
      model = lm(full_data[,2]~full_data[,1])
      sum_loglik = logLik(model)[1]
    }else{
      for(i in 1:length(k_ends)) {
        if(k_ends[i] == 2){
          min = k_ends[i-1]
          x_values = full_data[c(min:k_ends[i]),1] #getting the x values in the interval
          y_values = full_data[c(min:k_ends[i]),2] #getting the y values in the interval
          data = data.frame(x_values, y_values) #re-making this into a dataframe 
          model = lm(y_values~x_values) #running a lm on the selected interval 
          sum_loglik = sum_loglik + logLik(model)[1] #the logLik looks the log likelyhood (relates to both SSR and MLE)
        }else if(k_ends[i] > 2){
          min = k_ends[i-1]
          x_values = full_data[c((min+1):k_ends[i]),1] #getting the x values in the interval
          y_values = full_data[c((min+1):k_ends[i]),2] #getting the y values in the interval
          data = data.frame(x_values, y_values) #re-making this into a dataframe 
          model = lm(y_values~x_values) #running a lm on the selected interval 
          sum_loglik = sum_loglik + logLik(model)[1] #the logLik looks the log likelyhood (relates to both SSR and MLE)
        }
      }
    }
    return(sum_loglik)
  }
  
  #random make function, this makes a random point 
  count = 0 
  barMake0<-function(k_ends){
    
    count = count + 1 #this check to make sure we do not get stuck in an infinite loop 
    if(count < 10 ) {
      rand_spot = sample(k_ends[1]:k_ends[length(k_ends)], 1) #selects a random spot
      k_ends_final = sort(c(k_ends, rand_spot)) #adds the random spot and sorts it 
      d = diff(k_ends_final) #finds the difference between all the spots 
      if(min(d) < 3) { #this make sure an additional point is not to close to a point already in existance 
        barMake0(k_ends)
      } else {
        return(k_ends_final) #the old breakpoints + the new breakpoints 
      }
    }else {
      return(k_ends)
    }
  }
  
  
  #this function kills one breakpoint randomly 
  barMurder0<-function(k_ends){
    
    k = k_ends[c(-1,-length(k_ends))] #removes the end points 
    random_num = sample(1:length(k), 1) #selects a random breakpoint
    k_ends_final = k_ends[-(random_num+1)] #removes that selected breakpoint
    return(k_ends_final)
    
  }
  
  #kills a point randomly and then adds a point randomly 
  barMove0<-function(k_ends){
    
    k_ends_less = barMurder0(k_ends) #kills a point
    k_ends_final = barMake0(k_ends_less) #remakes a point
    return(k_ends_final)
    
  }
  
  #initializing matrices 
  ratio_data = data.frame()
  all_k_new = matrix(NA, nrow=1, ncol=(n/3))
  all_k_best = matrix(NA, nrow=1, ncol=(n/3))
  
  bar_v = 0
  bar_beta = 0
  fit = 0
  #matrix_of_fits = data.frame()
  all_MSE = data.frame()
  accept_count = 0
  
  #setting up counters (these will tell us how many type it does a certain step and how many time it accept each step)
  type = "0"
  a.count = 0
  s.count = 0 
  m.count = 0
  add.accept.count = 0
  sub.accept.count = 0
  move.accept.count = 0
  
  #Metroplis Hastings 
  for(i in 1:iterations){
    
    old_loglik = fitMetrics(k_ends, full_data) #calls fit matrix to have a function to start with
    
    u_step = runif(1) #random number from 0 to 1 taken from a uniform distribution for selecting step
    
    if(length(k_ends) < 3 | u_step < prob_mmm[1]){
      type = "add"
      a.count = a.count + 1
      k_ends_new = barMake0(k_ends) #make

	#setting up qs for ratio - CHECK MY MATH HERE
	q1 = make/(length(k_ends_new)-2)
	full_set = c(k_ends, k_ends[1:length(k_ends-1)]+1, k_ends[1:length(k_ends-1)]+2, k_ends[2:length(k_ends)]-1, k_ends[2:length(k_ends)]-2) #all precluded observations
	overlap = sum(table(full_set))-length(table(full_set)) #repeated preclusions
	n_free = n - 5*(length(k_ends)-2) - 6 + overlap
	q2 = make/n_free

    } else if(u_step > prob_mmm[1] & u_step < sum(prob_mmm)){
      type = "sub"
      s.count = s.count + 1
      k_ends_new = barMurder0(k_ends) #murder

	#setting up qs for ratio - CHECK MY MATH HERE
	full_set = c(k_ends_new, k_ends_new[1:length(k_ends_new-1)]+1, k_ends_new[1:length(k_ends_new-1)]+2, k_ends_new[2:length(k_ends_new)]-1, k_ends_new[2:length(k_ends_new)]-2) #all precluded observations
	overlap = sum(table(full_set))-length(table(full_set)) #repeated preclusions
	n_free = n - 5*(length(k_ends)-2) - 6 + overlap
	q1 = make/n_free
	q2 = make/(length(k_ends)-2)

    } else{
      type = "move"
      m.count = m.count + 1
      k_ends_new = barMove0(k_ends) #move

	#fake qs because they cancel
	q1 = 1
	q2 = 1

    }
    
    new_loglik = fitMetrics(k_ends_new, full_data)

    #CHECK MY MATH HERE
    delta_bic = (-2*new_loglik + log(n)*(length(k_ends_new)-1)*(2+1)) - (-2*old_loglik + log(n)*(length(k_ends)-1)*(2+1))
    ratio = (-delta_bic/2) + log(q1) - log(q2)
    u_ratio = log(runif(1)) #random number from 0 to 1 taken from a uniform distribution and then log transformed

    ratio_data_print = c(ratio, u_ratio, delta_bic, (-delta_bic/2), log(q1), log(q2))
    
    if(abs(ratio) == Inf){ #safe guard against random models creating infinite ratios
      k_ends = k_ends #old
    } else if(ratio > u_ratio) {
      k_ends = k_ends_new #new
      accept_count = accept_count + 1
      #looking at what type of step is done and accepted
      if(type == "add") {
        add.accept.count = add.accept.count + 1
      } else if(type == "sub") {
        sub.accept.count = sub.accept.count + 1
      } else if(type == "move") {
        move.accept.count = move.accept.count + 1
      }
    } else {
      k_ends = k_ends #old
    }
    
    #condensing the data
    k_ends_new_print = c(k_ends_new, rep(NA, (n/3)-length(k_ends_new)))
    k_ends_best_print = c(k_ends, rep(NA, (n/3)-length(k_ends)))
    
    ratio_data = rbind(ratio_data, ratio_data_print)
    all_k_new = rbind(all_k_new, k_ends_new_print)
    all_k_best = rbind(all_k_best, k_ends_best_print)
    
    #setting up the posterior
    b_0 = matrix(c(0,0),2,1)
    B_0 = matrix(c(1000,0,0,1000),2,2)
    
    ##loop through the k_ends to find the intervals 
    fit = NULL
    for(m in 2:length(k_ends)) {
      len = length(k_ends)
      if(m > 2){
        min = k_ends[m-1]+1
      }else{
        min = k_ends[m-1]
      }
      x_values = full_data[c(min:k_ends[m]),1] #getting the x values in the interval
      x_j = matrix(c( rep(1, each=length(x_values)), x_values), nrow= length(x_values), ncol= 2)
      y_j = full_data[c(min:k_ends[m]),2] #getting the y values in the interval
      sigma = sd(y_j)
      
      #bar_v
      v = solve( (1/sigma) * (t(x_j) %*% x_j )+ solve(B_0) )
      #bar_beta 
      beta = v %*% ( (1/sigma) * (t(x_j) %*% y_j) + solve(B_0) %*% b_0 )
      
      predicted_x = x_j %*% beta
      fit = c(fit, predicted_x)
      
      #drawing a random variable from a multivariate normal pdf 
      post_beta = mvrnorm(1, beta, v)
      
      bar_v = c(bar_v, v)
      bar_beta = c(bar_beta, beta)
      
      if(m == len ) {
        MSE = mean((full_data[,2]-fit)^2)
        all_MSE = rbind(all_MSE, MSE)
      }
    }
  }
  
  #cleaning up the matrices 
  all_k_new = all_k_new[-1,colSums(is.na(all_k_new))<nrow(all_k_new)]
  all_k_best = all_k_best[-1,colSums(is.na(all_k_best))<nrow(all_k_best)]
  clean_max = max(all_k_new[1,], na.rm=TRUE)
  all_k_new = ifelse(all_k_new == clean_max,NA,all_k_new)
  all_k_best = ifelse(all_k_best == clean_max,NA,all_k_best)
  all_k_new = data.frame(all_k_new[,c(-1,-ncol(all_k_new))], row.names=NULL)
  all_k_best = data.frame(all_k_best[,c(-1,-ncol(all_k_best))], row.names=NULL)
  
  colnames(ratio_data) = c("Ratio", "Random", "DeltaBIC", "LikeApprox", "LogQOldNew", "LogQNewOld")
  colnames(all_MSE) = c("MSE")
  
  final.propose = c(a.count, s.count, m.count)
  final.accept = c(add.accept.count, sub.accept.count, move.accept.count)
  
  final_list = list(accept_count / iterations, final.propose, final.accept, all_MSE, all_k_best)
  names(final_list) = c("AcceptRate", "ProposedSteps","AcceptedSteps", "MSE", "Breakpoints")
  
  return(final_list)
}

#calling the function
current_result = bar0(bkpts_2$breakpoints, test_data_2[,1], test_data_2[,2], 50, 0.4)
