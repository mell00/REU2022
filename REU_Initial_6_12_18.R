#creating our data 
first = rnorm(30, mean = 5, sd = 1) #30 random data points from a normal distribution with means 5
second = rnorm(30, mean = 15, sd = 1) #30 random data points from a normal distribution with means 15

dif_means = c(first, second) #adding the two sets of data points together 
time = c(1:60) #making time 

test_data = data.frame(time, dif_means) #our data with x = time and y = two sets of data with two means 

plot(test_data, main="Initial Data with Means Equal to 5 and 15") 
