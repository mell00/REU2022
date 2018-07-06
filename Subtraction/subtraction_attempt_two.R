#subtraction attempt two (proposes two new breakpoints and scores - bigger min bigger max, then random)

k_ends_test = c(1,20,30,44,50,60)

barMurder2 <- function(k_ends, prob_of_doing_random){
  
  #allowing for the option of doing a random addition 
  u = runif(1) #random number from 0-1 from uniform distribution
  if(length(k_ends) < 4 | u < prob_of_doing_random){
    barMurder0(k_ends)
  }else{
  
  #removing the end points 
  k<-k_ends[-c(1,length(k_ends))] 
  
  #random number generator 
  proposed = sample(1:length(k), 2, replace = FALSE) #looking at length of k 
  proposed = sort(proposed) #sorting the 2 chosen numbers 
  
  #setting up initial values 
  small_1 = list()
  big_1 = list()
  small_2 = list()
  big_2 = list()
  
  #finding which points are closest to the propsed break points 
  for(i in 1:length(k_ends)) {
    if(k[proposed[1]] > k_ends[i] ) {
      small_1 = c(small_1, k_ends[i])
    } else if (k[proposed[1]] < k_ends[i] ){
      big_1 = c(big_1, k_ends[i])
    }
    if(k[proposed[2]] > k_ends[i]) {
      small_2 = c(small_2, k_ends[i])
    } else if (k[proposed[2]] < k_ends[i]){
      big_2 = c(big_2, k_ends[i])
    }
  }
  
  one = 0
  two = 0 
  #looking at first point 
  if(length(small_1) == 0) {
    one = c(1, proposed[[1]][1], big_1[which.min(big_1)][[1]][1]) 
  } else if(length(big_1) == 0) {
    one = c(small_1[which.max(small_1)][[1]][1], proposed[[1]][1], max(k_ends))
  } else {
    one = c(small_1[which.max(small_1)][[1]][1], proposed[[1]][1], big_1[which.min(big_1)][[1]][1]) 
  }
  #looking at second point 
  if(length(small_2) == 0) {
    two = c(1, proposed[[2]][1], big_2[which.min(big_2)][[1]][1])
  } else if(length(big_2) == 0) {
    two = c(small_2[which.max(small_2)][[1]][1], proposed[[2]][1], max(k_ends))
  } else {
    two = c(small_2[which.max(small_2)][[1]][1], proposed[[2]][1], big_2[which.min(big_2)][[1]][1])
  }
 
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
    } else if(d_one_sort[2] > d_two_sort[2]) { #comparing the max to determine new point 
      new_bp = proposed[2]
    } else {
      new_bp = proposed[1]
    }
  } else if(d_one_sort[1] > d_two_sort[1]) { #comparing the min to see the new break point 
    new_bp = proposed[2]
  } else {
    new_bp = proposed[1]
  }
  
  #deleting the selecting knot 
  k_ends_final <- k_ends[-new_bp]
  return(k_ends_final)
  }
}

barMurder2(k_ends_test)
