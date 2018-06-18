#creating our data 
first = rnorm(30, mean = 5, sd = 1) #30 random data points from a normal distribution with mean of 5
second = rnorm(30, mean = 15, sd = 1) #30 random data points from a normal distribution with mean of 15
third = rnorm(30, mean = 30, sd = 1) #30 random data points from a normal distribution with mean of 30

dif_means_0 = c(first) #setting up null set
dif_means_1 = c(first, second) #adding two sets of data points together 
dif_means_2 = c(first, second, third) #adding three sets of data points together

time_0 = 1:30
time_1 = 1:60
time_2 = 1:90

test_data_0 = data.frame(time_0, dif_means_0) #our data with x = time and y = data from 1 set
test_data_1 = data.frame(time_1, dif_means_1) #our data with x = time and y = data from 2 sets
test_data_2 = data.frame(time_2, dif_means_2) #our data with x = time and y = data from 3 sets

#downloading the strucchange package
#install.packages("strucchange")
library("strucchange")

#Bai-Perron Method
bkpts_0 = breakpoints(test_data_0$dif_means_0 ~ test_data_0$time_0, breaks = 5, h = 0.1) #no break points 
bkpts_0$breakpoints #list of x-values (time) for breakpoints

bkpts_1 = breakpoints(test_data_1$dif_means_1 ~ test_data_1$time_1, breaks = 5, h = 0.1) #no break points 
bkpts_1$breakpoints #list of x-values (time) for breakpoints

bkpts_2 = breakpoints(test_data_2$dif_means_2 ~ test_data_2$time_2, breaks = 5, h = 0.1) #no break points 
bkpts_2$breakpoints #list of x-values (time) for breakpoints

#-----------------------------------------------------------------

test_k <- c(1,30,60)
test_k_2 <- c(1,30,50,60)

fitMetrics<-function(k_ends, test_data){

	#create sum objects
	sum_sd = 0
	sum_SSE = 0

	#get and sum standard deviation and SSE for regressions of all intervals
	if(length(k_ends) < 3 ){
		sum_sd = sd(test_data[,2])
		model = lm(test_data[,2]~test_data[,1])
		SSE = sum(model$residuals^2)
		sum_SSE = SSE
	}else{
		for(i in 1:length(k_ends)) {
  			if(k_ends[i] != 1){
			min = k_ends[i-1]
			x_values = test_data[c(min:i),1] #getting the x values in the interval
			y_values = test_data[c(min:i),2] #getting the y values in the interval
			data = data.frame(x_values, y_values) #re-making this into a dataframe 
			sum_sd = sum_sd + sd(y_values) #adding up all the standard deviations
			model = lm(y_values~x_values)
			SSE = sum(model$residuals^2)
			sum_SSE = sum_SSE + SSE #adding up all the SSEs 
			}
		}
	}
	c(sum_sd,sum_SSE)

}

new_metrics = fitMetrics(test_k, test_data_1)
old_metrics = fitMetrics(test_k_2, test_data_1)

sigma_new = new_metrics[1]
sigma_old = old_metrics[1]
SSE_new = new_metrics[2]
SSE_old = old_metrics[2]

ratio = exp(-1/(2 * sigma_new) * SSE_new) + exp(-1/(2 * sigma_old) * SSE_old)
u = runif(1) #random number from 0 to 1 taken from a normal distribution 

#our temporary L
if(ratio > 1) { 
  print("new")
} else if(ratio > u) {
  print("new")
} else {
  print("old")
}

#-----------------------------------------------------------------

#complete BAR - Variation 0 (Random/Random/Random)


#-------Key:
# k           = breakpoint's x-axis values 
# time        = integer x-values of the entire data set 
# interations = number of runs through Metropolis hastings 
# make        = the proportion (decimal) of the make step to occuring
# murder      = the proportion (decimal) of the murder step to occuring
  #note: the make and murder need to add to less then one 
bar0 = function(k, time, data, iterations, make, murder){ 

  prob_mmm = c(make, murder) #combining the two probabilties of make and murder that the user specifies 

  full_data = cbind(as.numeric(time), as.numeric(data)) #combing the time and data inputs from user

  n = max(full_data[,1]) #finding max value

  k_ends = c(min(full_data[,1]), k, max(full_data[,1])) #adding in end points to k values 

  fitMetrics<-function(k_ends, test_data){

	  #create sum objects
	  sum_sd = 0
	  sum_SSE = 0

	  #get and sum standard deviation and SSE for regressions of all intervals
	  if(length(k_ends) < 3 ){
		  sum_sd = sd(test_data[,2])
		  model = lm(test_data[,2]~test_data[,1])
		  SSE = sum(model$residuals^2)
		  sum_SSE = SSE
	  }else{
		  for(i in 1:length(k_ends)) {
  			if(k_ends[i] != 1){
			    min = k_ends[i-1]
			    x_values = test_data[c(min:i),1] #getting the x values in the interval
			    y_values = test_data[c(min:i),2] #getting the y values in the interval
			    data = data.frame(x_values, y_values) #re-making this into a dataframe 
			    sum_sd = sum_sd + sd(y_values) #adding up all the standard deviations
			    model = lm(y_values~x_values)
			    SSE = sum(model$residuals^2)
			    sum_SSE = sum_SSE + SSE #adding up all the SSEs 
			  }
		  }
	  }
	  c(sum_sd,sum_SSE)
  }
  
  #random make function, this makes a random point 
  count = 0 
  barMake0<-function(k_ends){
    
    count = count + 1 #this check to make sure we do not get stuck in an infinite loop 
    if(count > 15 ) {
      rand_spot = sample(k_ends[1]:k_ends[length(k_ends)], 1) #selects a random spot
      k_ends_final = sort(c(k_ends, rand_spot)) #adds the random spot and sorts it 
      d = diff(k_ends_final) #finds the difference between all the spots 
      if(min(d) < 3) { #this make sure an additional point is not to close to a point already in existance 
        barMake0(k_ends)
      } else {
        return(k_ends_final) #the old breakpoints + the new breakpoints 
      }
    }
    
  }
  
  
  #this function kills one breakpoint randomly 
  barMurder0<-function(k_ends){

	  k = k_ends[c(-1,-length(k_ends))]
	  random_num = sample(1:length(k), 1)
	  k_ends_final = k_ends[-(random_num+1)]
	  k_ends_final

  }

  #kills a point randomly and then adds a point randomly 
  barMove0<-function(k_ends){

	  k_ends_less = barMurder0(k_ends)
	  k_ends_final = barMake0(k_ends_less)
	  k_ends_final

  }

  #initializing matrixes 
  ratio_data = matrix(NA, nrow=1, ncol=6)
  all_k_new = matrix(NA, nrow=1, ncol=(n/3))
  all_k_best = matrix(NA, nrow=1, ncol=(n/3))

  #Metroplis Hastings 
  for(i in 1:iterations){

    old_metrics = fitMetrics(k_ends, full_data)
    sigma_old = old_metrics[1]
    SSE_old = old_metrics[2]

    u_step = runif(1) #random number from 0 to 1 taken from a uniform distribution for selecting step

    if(length(k_ends) < 3 | u_step < prob_mmm[1]){
      c_step = "make"
    } else if(u_step > prob_mmm[1] & u_step < sum(prob_mmm)){
      c_step = "murder"
    } else{
      c_step = "move"
    }

    if(c_step == "make"){
      k_ends_new = barMake0(k_ends)
    } else if (c_step == "murder"){
      k_ends_new = barMurder0(k_ends)
    } else{
      k_ends_new = barMove0(k_ends)
    }

    new_metrics = fitMetrics(k_ends_new, full_data)
    sigma_new = new_metrics[1]
    SSE_new = new_metrics[2]

    ratio = exp((-1*n*log((sqrt(2*pi)*sigma_new)+0.00001)-(1/(2*sigma_new^2+0.00001))*SSE_new)+((n*log(sqrt(2*pi)*sigma_old)+0.00001)-(1/(2*sigma_old^2+0.00001))*SSE_old))
    u_ratio = runif(1) #random number from 0 to 1 taken from a uniform distribution 

    if(ratio > u_ratio) {
      choice = "new"
    } else {
      choice = "old"
    }

    if(choice == "new"){
      k_ends = k_ends_new
    }else{
      k_ends = k_ends
    }

    ratio_data_print = c(ratio, u_ratio, sigma_new, SSE_new, sigma_old, SSE_old)
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

  #plotting 
  plot(full_data, main = 1, xlab = "Time (ms)", ylab = "Number of Neurons Firing")
  points(all_k_new[1,],full_data[all_k_new[1,],2], col="blue", pch= 16, cex = 2)
  points(all_k_best[1,],full_data[all_k_best[1,],2], col="red", pch= 16, cex=2)
  for(i in 1:20) {
    n = i * 5
    plot(full_data, main = n, xlab = "Time (ms)", ylab = "Number of Neurons Firing")
    points(all_k_new[n,],full_data[all_k_new[n,],2], col="blue", pch= 16, cex = 2)
    points(all_k_best[n,],full_data[all_k_best[n,],2], col="red", pch= 16, cex = 2)
  }
  
  #prints the results
  print(ratio_data)
  print(all_k_new)
  print(all_k_best)
  
}

#calling the function
bar0(bkpts_neuron$breakpoints, rownames(neuron), neuron$V2, 100, 0.1, 0.1)

bar0(bkpts_neuron$breakpoints, rownames(neuron), neuron$V2, 20, 0.6, 0.2)
