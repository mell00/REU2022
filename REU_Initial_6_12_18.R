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

fitMetrics<-function(k_ends, test_data){

	#create sum objects
	sum_sd = 0
	sum_SSE = 0

	#get and sum standard deviation and SSE for regressions of all intervals
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

	sigma_new = sum_sd / length(k_ends)-1   
	SSE_new = sum_SSE / length(k_ends)-1
	print(sigma_new)
	print(SSE_new)

}

fitMetrics(test_k, test_data_1)

sigma_old = sd()      #need this 
SEE_old =

ratio = exp(-1/(2 * sigma_new) * SSE_new) + exp(-1/(2 * sigma_old) * SEE_old)
u = runif(1) #random number from 0 to 1 taken from a normal distrabution 

#our temporary L
if(ratio > 1) { 
  #take new
} else if(ratio > u) {
  #keep new
} else {
  #keep old
}

