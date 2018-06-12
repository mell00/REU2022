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
dif_means_ts_0<-ts(first, start=1, end=30) #create time series data on only the first data 
breakpoints(dif_means_ts_0 ~ 1, breaks = 2) #no break points 

dif_means_ts_1<-ts(dif_means, start=1, end=60) #create time series data with first and second data
breakpoints(dif_means_ts_1 ~ 1, breaks = 2) #one break

dif_means_ts_2<-ts(dif_means_2, start=1, end=90) #create time series data with first second and third data
breakpoints(dif_means_ts_2 ~ 1, breaks=3) #two breaks 



