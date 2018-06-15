#addition attempt one (random point in largest interval)

k_ends_test = c(1,50,60)

barMake1<-function(k_ends){

  d = diff(k_ends) #finding the distance between all those breakpoints
  locations = which(d == max(d) ) #finding the location(s) of the farthest distance 
  location = sample(locations, 1) #randomly select location in case of equal max distances
  min = k_ends[location] #lower bound 
  max = k_ends[location + 1] #upper bound
  new_bp = sample((min+1):(max-1), 1) #selecting a random number in the correct interval
  newSet = sort(c(k_ends, new_bp))
  newSet

}

barMake1(k_ends_test)
