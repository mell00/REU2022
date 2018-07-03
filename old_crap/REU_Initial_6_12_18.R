#creating our data
cols1<-c('#006e82', '#8214a0', '#005ac8', '#00a0fa', '#fa78fa', '#14d2dc', '#aa0a3c', '#fa7850', '#0ab45a', '#f0f032', '#a0fa82', '#fae6be')

first = rnorm(30, mean = 5, sd = 1) #30 random data points from a normal distribution with mean of 5
second = rnorm(30, mean = 15, sd = 1) #30 random data points from a normal distribution with mean of 15
third = rnorm(30, mean = 30, sd = 1) #30 random data points from a normal distribution with mean of 30

#dif_means_0 = c(first) #setting up null set
#dif_means_1 = c(first, second) #adding two sets of data points together 
dif_means_2 = c(first, second, third) #adding three sets of data points together

#time_0 = 1:30
#time_1 = 1:60
time_2 = 1:90

#test_data_0 = data.frame(time_0, dif_means_0) #our data with x = time and y = data from 1 set
#test_data_1 = data.frame(time_1, dif_means_1) #our data with x = time and y = data from 2 sets
test_data_2 = data.frame(time_2, dif_means_2) #our data with x = time and y = data from 3 sets

#downloading the strucchange package
#install.packages("strucchange")
library("strucchange")

#Bai-Perron Method
#bkpts_0 = breakpoints(test_data_0$dif_means_0 ~ test_data_0$time_0, breaks = 5, h = 0.1) #no break points 
#bkpts_0$breakpoints #list of x-values (time) for breakpoints

#bkpts_1 = breakpoints(test_data_1$dif_means_1 ~ test_data_1$time_1, breaks = 3, h = 0.2) #no break points 
#bkpts_1$breakpoints #list of x-values (time) for breakpoints

bkpts_2 = breakpoints(test_data_2$dif_means_2 ~ test_data_2$time_2, breaks = 5, h = 0.1) #no break points 
bkpts_2$breakpoints #list of x-values (time) for breakpoints

#-----------------------------------------------------------------

#complete BAR - Variation 0 (Random/Random/Random)


#-------Key:
# k           = breakpoint's x-axis values 
# time        = integer x-values of the entire data set 
# interations = number of runs through Metropolis hastings 
# make        = the proportion (decimal) of the make step to occuring
# murder      = the proportion (decimal) of the murder step to occuring
  #note: the make and murder need to add to less then one 
#graph        = yes or no to graphing the function 


bar0 = function(k, time, data, iterations, make, murder){

  library(MASS)

  prob_mmm = c(make, murder) #combining the two probabilties of make and murder that the user specifies 

  full_data = cbind(as.numeric(time), as.numeric(data)) #combing the time and data inputs from user

  n = max(full_data[,1]) #finding max value

  k_ends = c(min(full_data[,1]), k, max(full_data[,1])) #adding in end points to k values 

  fitMetrics<-function(k_ends, full_data){

	  #create sum objects
	  sum_loglik = 0

	  #get and sum log likelihood for regressions of all intervals
	  if(length(k_ends) < 3 ){
		  model = lm(full_data[,2]~full_data[,1])
		  sum_loglik = logLik(model)[1]
	  }else{
		  for(i in 1:length(k_ends)) {
  		  if(k_ends[i] != 1){
			    min = k_ends[i-1]
			    x_values = full_data[c(min:k_ends[i]),1] #getting the x values in the interval
			    y_values = full_data[c(min:k_ends[i]),2] #getting the y values in the interval
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

  #initializing matrixes 
  ratio_data = matrix(NA, nrow=1, ncol=4)
  all_k_new = matrix(NA, nrow=1, ncol=(n/3))
  all_k_best = matrix(NA, nrow=1, ncol=(n/3))

  #Metroplis Hastings 
  for(i in 1:iterations){

    old_loglik = fitMetrics(k_ends, full_data) #calls fit matrix to have a function to start with

    u_step = runif(1) #random number from 0 to 1 taken from a uniform distribution for selecting step

    if(length(k_ends) < 3 | u_step < prob_mmm[1]){
      k_ends_new = barMake0(k_ends) #make
    } else if(u_step > prob_mmm[1] & u_step < sum(prob_mmm)){
      k_ends_new = barMurder0(k_ends) #murder
    } else{
      k_ends_new = barMove0(k_ends) #move
    }

    new_loglik = fitMetrics(k_ends_new, full_data)

    ratio = (new_loglik - log(n)*(4*(length(k_ends_new)-2)+3)) - (old_loglik - log(n)*(4*(length(k_ends)-2)+3))
    u_ratio = log(runif(1,0.25,1)) #random number from 0 to 1 taken from a uniform distribution 

    if(ratio == Inf){ #safe guard against random models creating infinite ratios
      k_ends = k_ends #old
    } else if(ratio > u_ratio) {
      k_ends = k_ends_new #new
    } else {
      k_ends = k_ends #old
    }

    #condensing the data
    ratio_data_print = c(ratio, u_ratio, (old_loglik - log(n)*(4*(length(k_ends)-2)+3)), (new_loglik - log(n)*(4*(length(k_ends_new)-2)+3)))
    k_ends_new_print = c(k_ends_new, rep(NA, (n/3)-length(k_ends_new)))
    k_ends_best_print = c(k_ends, rep(NA, (n/3)-length(k_ends)))

    ratio_data = rbind(ratio_data, ratio_data_print)
    all_k_new = rbind(all_k_new, k_ends_new_print)
    all_k_best = rbind(all_k_best, k_ends_best_print)

  }
  
  #cleaning up the matrixs 
  ratio_data = ratio_data[-1,]
  all_k_new = all_k_new[-1,colSums(is.na(all_k_new))<nrow(all_k_new)]
  all_k_best = all_k_best[-1,colSums(is.na(all_k_best))<nrow(all_k_best)]
  clean_max = max(all_k_new[1,], na.rm=TRUE)
  all_k_new = ifelse(all_k_new == clean_max,NA,all_k_new)
  all_k_best = ifelse(all_k_best == clean_max,NA,all_k_best)
  all_k_new = all_k_new[,c(-1,-ncol(all_k_new))]
  all_k_best = all_k_best[,c(-1,-ncol(all_k_best))]
  
  #prints the results
  return(list(ratio_data, all_k_new, all_k_best))
  
}

#calling the function
bar_result = bar0(bkpts_2$breakpoints, test_data_2[,1], test_data_2[,2], 50, 0.4, 0.4)
neuron_result = bar0(bkpts_neuron$breakpoints, seq(1:110), neuron[,2], 50, 0.4, 0.4)
