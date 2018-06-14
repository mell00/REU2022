#addition attempt 

#setting up data 
first = rnorm(30, mean = 5, sd = 1) #30 random data points from a normal distribution with means 5
second = rnorm(30, mean = 15, sd = 1)
dif_means = c(first, second) #putting the two generated number list together 
time = c(1:60) #making time 
test_data = data.frame(time, dif_means) #making a data frame of it 

k = c(4,10,15,45,50) #pretending that these are the break points 
d = diff(k) #finding te distance between all those break points 
location = which.max(d) #finding the location of the farthest distance 
min = k[location] #lower bound 
max = k[location + 1] #upper bound
new_additon = sample(min:max, 1) #selecting a random number in the correct interval
new_additon 

plot(test_data) #plotting the original data 
points(new_additon, dif_means[new_additon], col="red", pch= 17) #plotting the new break point
points(k, dif_means[k], col="blue", pch=17) #plotting all the other break points 
