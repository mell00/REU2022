#Complete BALR (Bayesian Adaptive Linear Regression)

#-------Key:
# k			= x-axis values of starting breakpoints
# time		= integer x-values of the entire data set
# data		= y-values of entire data set
# interations	= number of runs for final sampling with Metropolis-Hastings
# burn_in		= number of runs for set-up sampling with Metropolis-Hastings
# make_murder_p	= the combine proportion (decimal) for make and murder steps
	#note: move proportion is 1 - make_murder_p
# percent		= how much a point can jiggle
# lambda		= for Poisson distribution of breakpoint prior
# jump_p		= proportion of move steps that will be jump
	#note: jiggle proprtion is 1 - jump_p
# progress		= whether to show progress bars or not, TRUE/FALSE
# fit_storage	= whether or not to store betas, sigmas, and fits for each iteration, TRUE/FALSE

balr = function(k, time, data, iterations, burn_in = 50, make_murder_p = 0.5, percent = 0.02, lambda = 1, jump_p = 0.25, progress = TRUE, fit_storage = TRUE){

	if(length(time) != length(data)){
		return("Data and time vectors must be of equal length.")
	}else if(make_murder_p >= 1){
		return("Make/murder proportion must be less than 1.")
	}else if(percent >= 0.5){
		return("Percent for jiggle neighrborhood must be less than 0.5.")
	}else if(jump_p > 1){
		return("Jump proportion must be less than or equal to 1.")
	}

	library(MASS)
	full_data = cbind(c(1:length(as.numeric(time))), as.numeric(data)) #combining time and data inputs
	n = length(full_data[,1]) #number of observations
	k_ends <<- c(min(full_data[,1]), na.omit(k), n) #adding end points to k 

	#function to get sum of log likelihoods
	fitMetrics<-function(k_ends, full_data){

		sum_loglik = 0 #create sum object
    
		#sum log likelihood for regressions of all subsections
		if(length(k_ends) < 3 ){ #if only 1 subsection
			model = lm(full_data[,2]~full_data[,1])
			sum_loglik = logLik(model)[1]
		}else{ #if more than 1 subsection
			for(i in 2:length(k_ends)) {
				if(i == 2){ #first subsection
					min = k_ends[i-1] #start of subsection
   					x_values = full_data[c(min:k_ends[i]),1] #subsection's x values
					y_values = full_data[c(min:k_ends[i]),2] #subsection's y values
					model = lm(y_values~x_values) #linear regression of selected subsection 
					sum_loglik = sum_loglik + logLik(model)[1] #log likelihood
				}else if(i > 2){ #all other subsections
					min = k_ends[i-1] #start of subsection
					x_values = full_data[c((min+1):k_ends[i]),1] #subsection's x values
					y_values = full_data[c((min+1):k_ends[i]),2] #getting the y values in the interval
 					model = lm(y_values~x_values) #running a lm on the selected interval 
					sum_loglik = sum_loglik + logLik(model)[1] #the logLik looks the log likelyhood (relates to both SSR and MLE)
				}
			}
		}
		return(sum_loglik)
	}
  
	#function to randomly add a new breakpoint
	barMake<-function(k_ends){

		full_set = c(1:max(k_ends))
		exclude_set = c(k_ends, k_ends[1:length(k_ends)-1]+1, k_ends[1:length(k_ends)-1]+2, k_ends[2:length(k_ends)]-1, k_ends[2:length(k_ends)]-2) #observations where a new breakpoint can't be added
		diff_set = setdiff(full_set,exclude_set)
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
		ll_limit = left_neighbor-2
		lr_limit = left_neighbor+2
		rl_limit = right_neighbor-2
		rr_limit = right_neighbor+2
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
    
		if(max(diff(k_ends)) >= 5 & length(k_ends) < 3 | max(diff(k_ends)) >= 5 & u_step <= make_k){
			type = "add"
			a.count <<- a.count + 1
			k_ends_new = barMake(k_ends) #make

			#setting up qs for ratio
			q1 = murder_k/(length(k_ends_new)-2)
			full_set = c(k_ends, k_ends[1:length(k_ends)-1]+1, k_ends[1:length(k_ends)-1]+2, k_ends[2:length(k_ends)]-1, k_ends[2:length(k_ends)]-2) #all precluded observations
			overlap = sum(table(full_set))-length(table(full_set)) #repeated preclusions
			n_free = n - 5*(length(k_ends)-2) - 6 + overlap
			q2 = make_k/n_free

		} else if(u_step > make_k & u_step <= (make_k + murder_k)){
			type = "sub"
			s.count <<- s.count + 1
			k_ends_new = barMurder(k_ends) #murder

			#setting up qs for ratio
			full_set = c(k_ends_new, k_ends_new[1:length(k_ends_new)-1]+1, k_ends_new[1:length(k_ends_new)-1]+2, k_ends_new[2:length(k_ends_new)]-1, k_ends_new[2:length(k_ends_new)]-2) #all precluded observations
			overlap = sum(table(full_set))-length(table(full_set)) #repeated preclusions
			n_free = n - 5*(length(k_ends_new)-2) - 6 + overlap
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
	full_set = c(k_ends, k_ends[1:length(k_ends)-1]+1, k_ends[1:length(k_ends)-1]+2, k_ends[2:length(k_ends)]-1, k_ends[2:length(k_ends)]-2) #observations where a new breakpoint can't be added
	overlap = sum(table(full_set))-length(table(full_set)) #any repeated values from the set above
	starting_nfree = n - 5 * (length(k_ends)-2) - 6 + overlap #most probable n_free based on starting info
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

		delta_bic = (-2*new_loglik + log(n)*(length(k_ends_new)-1)*(3+1)) - (-2*old_loglik + log(n)*(length(k_ends)-1)*(3+1))
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

	#setting up priors for beta draws
	if(fit_storage == TRUE){
		beta_lm = function(par){#function to minimize for MLE of betas

			beta0 = par[1]  #current intercept
			beta1 = par[2]  #current slope
			sigma = sd(full_data[,2]) #standard deviation
  
			#calculated likelihoods
			lik = dnorm(full_data[,2], mean = full_data[,1] * beta1 + beta0, sd = sigma)

			#convert likelihood to summary deviance score (minimizing deviance = maximizing likelihood)
			log_lik = log(lik) #log likelihood of each data point
			deviance = -2 * sum(log_lik) #calculate deviance

			return(deviance)
		}

		beta_fits = optim(par = c(0, 0), fn = beta_lm, hessian = T) #get parameter estimates for betas
		fisher = 0.5*beta_fits$hessian #if minimizing deviance, observed Fisher information is half of hessian
		smiley = n * fisher #smiley face is total number of observations times the inverse of Fisher information

		b_0 = matrix(beta_fits$par,2,1) #matrix of beta means for posterior draw
		B_0 = smiley #variance-covariance matrix for posterior draw

		#beta and sigma draw
		post_beta_list = data.frame(Empty=c(NA,NA))
		post_sigma_list = data.frame(Empty=NA)
	}

	#getting constants for qs for final Metropolis-Hasting
	starting_bkpts = length(k_ends) - 1 #most probable number of breakpoints based on starting info 
	full_set = c(k_ends, k_ends[1:length(k_ends)-1]+1, k_ends[1:length(k_ends)-1]+2, k_ends[2:length(k_ends)]-1, k_ends[2:length(k_ends)]-2) #observations where a new breakpoint can't be added
	overlap = sum(table(full_set))-length(table(full_set)) #any repeated values from the set above
	starting_nfree = n - 5 * (length(k_ends)-2) - 6 + overlap #most probable n_free based on starting info
	starting_ttl = starting_bkpts + starting_nfree #total to get percentages
	make_k = make_murder_p * (starting_nfree/starting_ttl) #proportion for make
	murder_k = make_murder_p * (starting_bkpts/starting_ttl) #proportion for murder

	if(progress == TRUE){
		writeLines("\nBeginning sampling period.")
		sample_progress <- txtProgressBar(min = 0, max = iterations, style = 3)
	}

	#Final Metroplis Hastings 
	for(i in 1:iterations){
    
		old_loglik = fitMetrics(k_ends, full_data) #calls fit matrix to have a function to start with

		k_and_q = newEnds(k_ends, make_k, murder_k)
		k_ends_new = k_and_q[[1]]
		q1 = k_and_q[[2]]
		q2 = k_and_q[[3]]
		type = k_and_q[[4]]
    
		new_loglik = fitMetrics(k_ends_new, full_data)

		delta_bic = (-2*new_loglik + log(n)*(length(k_ends_new)-1)*(3+1)) - (-2*old_loglik + log(n)*(length(k_ends)-1)*(3+1))
		ratio = (-1*delta_bic/2) + (log(q1*dpois(length(k_ends_new)-2,lambda)) - log(q2*dpois(length(k_ends)-2,lambda)))
		u_ratio = log(runif(1)) #random number from 0 to 1 taken from a uniform distribution and then log transformed
    
		if(abs(delta_bic) == Inf){ #safe guard against random models creating infinite ratios
			k_ends <<- k_ends #old
			bic = (-2*old_loglik + log(n)*(length(k_ends)-1)*(3+1))
		} else if(ratio > u_ratio) {
			k_ends <<- k_ends_new #new
			bic = (-2*new_loglik + log(n)*(length(k_ends_new)-1)*(3+1))
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
			bic = (-2*old_loglik + log(n)*(length(k_ends)-1)*(3+1))
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
    
		#setting up posterior
    
		##loop through the k_ends to find the intervals
		if(fit_storage == TRUE){
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
				x_values = full_data[c(min:k_ends[m]),1] #getting the x values in the interval
				x_j = matrix(c( rep(1, each=length(x_values)), x_values), nrow= length(x_values), ncol= 2)
				y_j = full_data[c(min:k_ends[m]),2] #getting the y values in the interval
				sigma = sd(y_j)
      
				#bar_v
				v = solve( (1/sigma) * (t(x_j) %*% x_j )+ solve(B_0) )
				#bar_beta 
				beta = v %*% ( (1/sigma) * (t(x_j) %*% y_j) + solve(B_0) %*% b_0 )
      
				#drawing a random variable from a multivariate normal pdf 
  				post_beta = mvrnorm(1, beta, v)

				predicted_x = x_j %*% post_beta
				fit = c(fit, predicted_x)
      
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
					MSE = mean((full_data[,2]-fit)^2)
					all_MSE = rbind(all_MSE, MSE)
					all_fits = rbind(all_fits, fit)
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
		rownames(post_beta_list) = c("B0", "B1")
		rownames(post_sigma_list) = "Sigma"
		post_beta_list = post_beta_list[,-1]
		post_sigma_list = post_sigma_list[,-1]

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
#test_data = test_data_11()
#current_result = balr(NA, test_data[,1], test_data[,2], 500, 100, progress=T, fit_storage=T)
