#addition attempt one (random point in largest interval)

k_ends_test = c(1, 6, 9, 15, 18, 21, 27, 30, 33, 38, 41, 44, 47, 51, 57, 60, 63, 68, 71, 74, 77, 80, 84, 87, 90)
k_ends = c(1,10,25,60)

barMake1<-function(k_ends, count){
  
  d = diff(k_ends) #finding the distance between all those breakpoints
  location = rmultinom(1, size = 1, prob = (d^4)/sum(d^4))
  if( d[location] > 5) {
    min = k_ends[which.max(location)] #lower bound 
    max = k_ends[(which.max(location) + 1)] #upper bound
    new_bp = sample((min+3):(max-3), 1) #selecting a random number in the correct interval
    k_ends_final = sort(c(k_ends, new_bp))
    d_check = diff(k_ends_final)
    print(min(d_check))
    if(min(d_check) > 2) {
      return(k_ends_final)
    } else if (count < 10) {
      count = count + 1
      barMake1(k_ends, count)
    } else {
      return("make failure")
    }
  } else {
    if(count < 10) {
      count = count + 1
      barMake1(k_ends, count)
    } else {
      return("make failure")
    }
  }
  
}

barMake1(k_ends_test, 0)
barMake1(k_ends, 0)



