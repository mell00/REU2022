k_ends_test = c(1, 4, 23, 60)

count = 0

barJiggle<-function(percent, k_ends, count){

  count = count + 1
  data_length = max(k_ends)

  #determines how much the knot should jiggle
  jiggle_range = ceiling(percent*data_length)
  jiggle_neighborhood = c(1:jiggle_range)
  jiggle_spot = sample(jiggle_neighborhood,1)
  
  #"boolean" variable to make sure that we can jiggle 
  can_jiggle = "good" #default is good and we can jiggle
  
  #determines randomly if knot is jiggling to left or right
  direction = "right" #default direction is right
  u = runif(1) #random number from 0-1 from uniform distribution
  if(u < 0.5){
    direction = "left"
    jiggle_spot = (-1)*jiggle_spot
  }
  
  #determines randomly which knot is jiggling (code related to murders)
  k = k_ends[c(-1, -length(k_ends))] #removes end points
  rando_location = sample(1:length(k),1) #chooses random knot 
  rando_knot = k[rando_location]
  
  #check if we can jiggle towards an endpoint
  possible_knot = rando_knot+jiggle_spot
  if(direction == "right"){
    right_end = k_ends[length(k_ends)]
    possible_diff = (abs(possible_knot - right_end) < 3)
    if(possible_diff == TRUE){
      can_jiggle = "bad"
    }
  }else{
    left_end = k_ends[1]
    possible_diff = (abs(possible_knot - left_end) < 3)
    if(possible_diff == TRUE){
      can_jiggle = "bad"
    }
  }
  
  #check if new knot location already has a knot there 
  for(i in 1:length(k)){
    possible_diff = (abs(possible_knot - k[i]) < 3)
    if(rando_knot != k[i] & possible_diff == TRUE){
      can_jiggle = "bad"
    }
  }

  #check if we can jiggle, then jiggle!!!
  if(can_jiggle == "bad" & count < 10){
    barJiggle(percent, k_ends, count)
  }else if(can_jiggle == "bad"){
    return("jiggle failure")
  }else{
    middle_set = k_ends[-(rando_location+1)]
    final_set = sort(c(middle_set,possible_knot))
    return(final_set)
  }
}

barJiggle(.01, k_ends_test, count)

