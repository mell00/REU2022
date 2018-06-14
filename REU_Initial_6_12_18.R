#creating our data 
first = rnorm(30, mean = 5, sd = 1) #30 random data points from a normal distribution with means 5
second = rnorm(30, mean = 15, sd = 1) #30 random data points from a normal distribution with means 15
third = rnorm(30, mean = 30, sd = 1)

dif_means = c(first, second) #adding the two sets of data points together 
dif_means_2 = c(first, second, third)
time = c(1:60) #making time 

test_data = data.frame(time, dif_means) #our data with x = time and y = two sets of data with two means 
plot(test_data, main="Initial Data with Means Equal to 5 and 15") 

#downloading the strucchange package
#install.packages("strucchange")
library("strucchange")
?breakpoints

#Bai-Perron Method
dif_means_ts_0<-ts(first, start=1,end=30) #create time series data on only the first data 
bkpts_0<-breakpoints(dif_means_ts_0 ~ 1, breaks = 2 ) #no break points 
plot(dif_means_ts_0)
bkpts_0$breakpoints #list of x-values (time) for breakpoints

dif_means_ts_1<-ts(dif_means, start=1, end=60) #create time series data with first and second data
bkpts_1<-breakpoints(dif_means_ts_1 ~ 1, breaks = 2 , h=0.1) #one break
plot(dif_means_ts_1)
points(30, dif_means_ts_1[30], col="purple", pch= 18)
bkpts_1$breakpoints #list of x-values (time) for breakpoints

dif_means_ts_2<-ts(dif_means_2, start=1, end=90) #create time series data with first second and third data
bpts_2<-breakpoints(dif_means_ts_2 ~ 1, breaks=3, h=0.1) #two breaks 
plot(dif_means_ts_2)
points(30, dif_means_ts_2[30], col="purple", pch= 18)
points(60, dif_means_ts_2[60], col="purple", pch= 18)
bkpts_2$breakpoints #list of x-values (time) for breakpoints

#-----------------------------------------------------------------

#sd and sse
sum_sd = 0
sum_sse = 0
#install.packages("apricom")
library("apricom") #if this is not working install the package above 
for(i in k) {
  min = i-1 
  if(!(min == 0)){
    x_values = test_data[c(min:i),1] #getting the x values in the interval
    y_values = test_data[c(min:i),2] #getting the y values in the interval
    data = data.frame(x_values, y_values) #re-makning this into a dataframe 
    sum_sd = sum_sd + sd(y_values) #adding up all the standard deviations
    lm.fit = lm(y_values ~ x_values) #linear fit 
    sum_sse = sum_sse + sse(lm.fit$coefficients, y_values) #finding the SSE 
  }
}
sigma_new = sum_sd / length(k)-1   
SSE_new = sum_sse / length(k)-1     


sigma_old = sd()      #need this 
SEE_old =             #sse(b, dataset)

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

