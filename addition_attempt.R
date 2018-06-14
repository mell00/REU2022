#addition attempt 

#setting up data 
first = rnorm(30, mean = 5, sd = 1) #30 random data points from a normal distribution with means 5
second = rnorm(30, mean = 15, sd = 1)
dif_means_ts_1<-ts(dif_means, start=1, end=60) #create time series data with first and second data
bkpts_1<-breakpoints(dif_means_ts_1 ~ 1, breaks = 2 , h=0.1) #one break
bkpts_1_ends<-c(1, bkpts_1$breakpoints, length(dif_means_ts_1)) #breakpoint set with end points

barBirth2<-function(currentSet){

  k_ends = currentSet #current set of breakpoints with ends
  d = diff(k_ends) #finding the distance between all those breakpoints
  locations = which(d == max(d) ) #finding the location(s) of the farthest distance 
  location = sample(locations, 1) #randomly select location in case of equal max distances
  min = k_ends[location] #lower bound 
  max = k_ends[location + 1] #upper bound
  new_addition = sample((min+1):(max-1), 1) #selecting a random number in the correct interval
  newSet = sort(c(k_ends, new_addition))
  newSet

}

barBirth2(bkpts_1_ends)
barBirth2(c(1,50,60))

plot(test_data) #plotting the original data 
points(new_addition, dif_means[new_addition], col="red", pch= 17) #plotting the new break point
points(k, dif_means[k], col="blue", pch=17) #plotting all the other break points 
