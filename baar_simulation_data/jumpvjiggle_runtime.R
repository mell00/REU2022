  #function to find all available spaces to place new breakpoints in data set
  freeObservations<-function(k_ends, ar){
    full_set = c(1:max(k_ends))
    if(ar == 1){
      constraint = (3 - 1)
    }else{
      constraint = (2*ar-1)
    }
    exclude_set = k_ends
    for(a in 1:length(k_ends)){
      if(a == 1){
        right = k_ends[[a]] + constraint
        right_set = full_set[k_ends[[a]]:right]
        exclude_set = c(exclude_set, right_set, recursive=T)
      }else if(a == length(k_ends)){
        left = k_ends[[a]] - constraint
        left_set = full_set[left:k_ends[[a]]]
        exclude_set = c(exclude_set, left_set, recursive=T)
      }else{
        right = k_ends[[a]] + constraint
        right_set = full_set[k_ends[[a]]:right]
        left = k_ends[[a]] - constraint
        left_set = full_set[left:k_ends[[a]]]
        exclude_set = c(exclude_set, left_set, right_set, recursive=T)
      }
    }
    diff_set = setdiff(full_set,exclude_set)
    return(sort(diff_set))
  }
  
  #function to randomly add a new breakpoint
  barMake<-function(k_ends){
    
    diff_set = freeObservations(k_ends, ar)
    if(length(diff_set) < 2){
      rand_spot = diff_set
    }else{
      rand_spot = sample(diff_set, 1) #selects a random spots
    }
    k_ends_final = sort(c(k_ends, rand_spot)) #adds the random spot and sorts it 
    return(k_ends_final)
    
  }
  
  
  #function to randomly subtract a breakpoint 
  barMurder<-function(k_ends){
    
    k = k_ends[c(-1,-length(k_ends))] #removes the end points
    if(length(k) == 1){
      random_num = 1
    }else{
      random_num = sample(1:length(k), 1) #selects a random breakpoint
    }
    k_ends_final = k_ends[-(random_num+1)] #removes that selected breakpoint
    return(k_ends_final)
    
  }
  
  #function to move a breakpoint to any new location
  barMove<-function(k_ends){
    
    k_ends_less = barMurder(k_ends) #kills a point
    k_ends_final = barMake(k_ends_less) #remakes a point
    return(k_ends_final)
    
  }
  
  #function to move a breakpoint within a small user-defined neighborhood
barJiggle<-function(percent, k_ends){
  
  k = k_ends[c(-1,-length(k_ends))]
  random_num = sample(1:length(k), 1)
  random_bkpt = k_ends[random_num+1]
  full_data = c(1:max(k_ends))
  wiggliness = max(k_ends) * percent
  if(floor(random_bkpt-wiggliness) > 0){
    left_lim = floor(random_bkpt-wiggliness)
  }else{
    left_lim = 1
  }
  if(ceiling(random_bkpt+wiggliness) < max(k_ends)){
    right_lim = ceiling(random_bkpt+wiggliness)
  }else{
    right_lim = max(k_ends)
  }
  prelim_neighborhood = full_data[left_lim:right_lim]
  left_neighbor = k_ends[random_num]
  right_neighbor = k_ends[random_num+2]
  if(ar == 1){
    constraint = (3 - 1)
  }else{
    constraint = (2*ar-1)
  }
  lr_limit = left_neighbor+constraint
  rl_limit = right_neighbor-constraint
  exclusions = sort(c(full_data[1:lr_limit], full_data[rl_limit:max(k_ends)]))
  final_neighborhood = setdiff(prelim_neighborhood, exclusions)
  
  if(length(final_neighborhood) > 1){
    new_location = sample(final_neighborhood, 1)
    k_ends_less = k_ends[-(random_num+1)]
    final_k_ends = sort(c(k_ends_less, new_location))
    return(final_k_ends)
  }else if(length(final_neighborhood) == 1){
    new_location = final_neighborhood
    k_ends_less = k_ends[-(random_num+1)]
    final_k_ends = sort(c(k_ends_less, new_location))
    return(final_k_ends)
  }
  else{
    return("jiggle failure")
  }
  
}

k_ends = c(1,15,30,45,60,75,90)
percent = 0.05
ar = 1
time_frame = data.frame()

for(i in 1:50000){

start_time = Sys.time()
barJiggle(percent, k_ends)
barJiggle(percent, k_ends)
barJiggle(percent, k_ends)
barJiggle(percent, k_ends)
barJiggle(percent, k_ends)
barJiggle(percent, k_ends)
barJiggle(percent, k_ends)
barJiggle(percent, k_ends)
barJiggle(percent, k_ends)
barJiggle(percent, k_ends)
barJiggle(percent, k_ends)
barJiggle(percent, k_ends)
barJiggle(percent, k_ends)
barJiggle(percent, k_ends)
barJiggle(percent, k_ends)
barJiggle(percent, k_ends)
barJiggle(percent, k_ends)
barJiggle(percent, k_ends)
barJiggle(percent, k_ends)
barJiggle(percent, k_ends)
end_time = Sys.time()
jiggle_time = end_time - start_time

start_time = Sys.time()
barMove(k_ends)
barMove(k_ends)
barMove(k_ends)
barMove(k_ends)
barMove(k_ends)
barMove(k_ends)
barMove(k_ends)
barMove(k_ends)
barMove(k_ends)
barMove(k_ends)
barMove(k_ends)
barMove(k_ends)
barMove(k_ends)
barMove(k_ends)
barMove(k_ends)
barMove(k_ends)
barMove(k_ends)
barMove(k_ends)
barMove(k_ends)
barMove(k_ends)
end_time = Sys.time()
jump_time = end_time - start_time

time_frame = rbind(time_frame, cbind(jiggle_time, jump_time, length(k_ends-2),max(k_ends)))

}
colMeans(time_frame)
apply(time_frame, 2, sd)

