#addition attempt one (random point in largest interval)

k_ends_test = c(1,50,60)
k_ends = c(1,30,50,60)

barMake1<-function(k_ends, count ){

  d = diff(k_ends) #finding the distance between all those breakpoints
  location = rmultinom(1, size = 1, prob = (d^4)/sum(d^4))
  if( d[location] > 4) {
    min = k_ends[which.max(location)] #lower bound 
    max = k_ends[(which.max(location) + 1)] #upper bound
    new_bp = sample((min+3):(max-3), 1) #selecting a random number in the correct interval
    k_ends_final = sort(c(k_ends, new_bp))
    return(k_ends_final)
  } else {
    if(count < 11) {
      count = count + 1
      barMake1(k_ends, count)
    } else {
      return(k_ends)
    }
  }

}

barMake1(k_ends_test, 0)
