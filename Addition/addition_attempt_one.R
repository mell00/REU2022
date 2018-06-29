#addition attempt one (random point in largest interval)

k_ends_test = c(1,50,60)
k_ends = c(1,30,50,60)

barMake1<-function(k_ends){

  d = diff(k_ends) #finding the distance between all those breakpoints
  location = rmultinom(1, size = 1, prob = (d^3)/sum(d^3))
  #locations = which(d == max(d) ) #finding the location(s) of the farthest distance 
  #locations = sample(d[location], 1) #randomly select location in case of equal max distances
  min = k_ends[which.max(location)] #lower bound 
  max = k_ends[(which.max(location) + 1)] #upper bound
  new_bp = sample((min+3):(max-3), 1) #selecting a random number in the correct interval
  k_ends_final = sort(c(k_ends, new_bp))
  return(k_ends_final)

}

barMake1(k_ends_test)
