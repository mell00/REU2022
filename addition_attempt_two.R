#subtraction_attempt 

#setting up data 
first = rnorm(30, mean = 5, sd = 1) #30 random data points from a normal distribution with means 5
second = rnorm(30, mean = 15, sd = 1)
dif_means = c(first, second) #adding the two sets of data points together 

dif_means_ts_1<-ts(dif_means, start=1, end=60) #create time series data with first and second data
time = c(1:60) #making time 
test_data = data.frame(time, dif_means) #our data with x = time and y = two sets of data with two means 

bkpts_1<-breakpoints(dif_means_ts_1 ~ 1, breaks = 2 , h=0.1) #one break
bkpts_1_ends<-c(1, bkpts_1$breakpoints, length(dif_means_ts_1)) #breakpoint set with end points

#random number generator 
proposed = sample(1:length(test_data[,1]), 2)
proposed = sort(proposed) #sorting the 2 chosen numbers 

#setting up initial values 
small_1 = list()
big_1 = list()
small_2 = list()
big_2 = list()

#finding which points are closest to the propsed break points 
for(i in 1:length(k)) {
  if(proposed[1] > k[i] ) {
    small_1 = c(small_1, k[i])
  } else {
    big_1 = c(big_1, k[i])
  }
  if(proposed[2] > k[i]) {
    small_2 = c(small_2, k[i])
  } else {
    big_2 = c(big_2, k[i])
  }
}

#this is extremely conveluted and not the best way 
one = c(small_1[which.max(small_1)][[1]][1], proposed[[1]][1], big_1[which.min(big_1)][[1]][1]) 
two = c(small_2[which.max(small_2)][[1]][1], proposed[[2]][1], big_2[which.min(big_2)][[1]][1])

#finding the distance between the points 
d_one = diff(one)
d_two = diff(two)

#sorting the distance 
d_one_sort = sort(d_one)
d_two_sort = sort(d_two)

new_bp = 0 
if(d_one_sort[1] == d_two_sort[1]) { #comparing the mins, if equal, look at max 
  if(d_one_sort[2] == d_two_sort[2]) {  #if both the min length and the max length are equal, randomize 
    r = runif(1) #generating a random number 
    if(r > 0.5) {
      new_bp = proposed[2]
    } else {
      new_bp = proposed[1]
    }
  } else if(d_one_sort[2] < d_two_sort[2]) { #comparing the max to determine new point 
    new_bp = proposed[2]
  } else {
    new_bp = proposed[1]
  }
} else if(d_one_sort[1] < d_two_sort[1]) { #comparing the min to see the new break point 
  new_bp = proposed[2]
} else {
  new_bp = proposed[1]
}
new_bp #the breakpoint 
