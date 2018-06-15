k_ends_test = c(1, 10, 15, 20, 25, 30, 35, 40, 45, 50, 55, 60)

jiggle<-function(percent, data_length, k_ends){
  
  #determines how much the knot shoud jiggle
  jiggle_range = ceiling(percent*data_length)
  # print(jiggle_range ) #1
  jiggle_neighborhood = c(1:jiggle_range)
  #print(jiggle_neighborhood) #1
  jiggle_spot = sample(1:jiggle_neighborhood,1)
  # print(jiggle_spot) #1
  
  #"boolean" variable to make sure that we can jiggle 
  can_jiggle = "good" #default is good and we can jiggle
  #print(can_jiggle)
  
  #determines randomly if knot is jiggling to left or right
  direction = "right" #default direction is right
  #print(direction)
  u = runif(1) #random number from 0-1 from uniform distribution
  print(u)
  if(u < 0.5){
    direction = "left"
    # print(direction)
    jiggle_spot = (-1)*jiggle_spot
    # print(jiggle_spot)
  }
  print(direction)
  
  #determines randomly which knot is jiggling (code related to murders)
  k = k_ends[c(-1, -length(k_ends))] #removes end points
  rando_location = sample(1:length(k),1) #chooses random knot 
  print(rando_location)
  rando_knot = k[rando_location]
  print(rando_knot)
  
  
  #check if we can jiggle towards an endpoint
  if(direction == "right"){
    right_end = k_ends[length(k_ends)]
    if(right_end < rando_knot+jiggle_spot){
      can_jiggle = "bad"
    }
  }else{
    left_end = k_ends[1]
    if(left_end > rando_knot+jiggle_spot){
      can_jiggle = "bad"
    }
  }
  
  #check if new knot location already has a knot there 
  possible_knot = rando_knot+jiggle_spot
  for(i in 1:length(k)){
    if(possible_knot == k[i]){
      can_jiggle = "bad"
    }
  }
  
  #check if we can jiggle, then jiggle!!!
  if(can_jiggle == "bad"){
    return()
  }else{
    #print(rando_knot)
    middle_set = k_ends[-(rando_location+1)]
    #print(possible_knot)
    final_set = sort(c(middle_set,possible_knot))
    final_set
  }
}

jiggle(.01,60,k_ends_test)

