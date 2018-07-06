
#makes a list of mins and maxes for all of the breakpoints; input is the list of breakpoints and endpoints 
list_of_scores_sub <- function(k_ends){  
  
  k_no_ends = k_ends[-c(1,length(k_ends))] #takes the ends off of k_ends so just looking at the breakpoints will be easier
  scores_list = list()
  
  #goes through each breakpoint and finds distances between it and neighbors; concatenates mins and maxes, in order, to scores_list
  for(i in 1:length(k_no_ends)){
    l = 0 
    r = 0 
    min = 0 
    max = 0 
    
    if(i == 1){ #first breakpoint has an endpoint as a neighbor
      l = k_no_ends[i] - k_ends[i] #distance between breakpoint and left endpoint
      r = k_ends[i+1] - k_no_ends[i] #distance between breakpoint and right neighbor
    }
    else if (i == length(k_no_ends)){#last breakpoint has an endpoint as a neighbor
      l = k_no_ends[i] - k_no_ends[i-1]#distance between breakpoint and left neighbor
      r = k_ends[length(k_ends)] - k_no_ends[i]#distance between breakpoint and right endpoint
    }
    else{#general case 
      l = k_no_ends[i]-k_no_ends[i-1] #distance between breakpoint and left neighbor
      r = k_no_ends[i+1]-k_no_ends[i] #distance between breakpoint and right neighbor
    }
    
    #gives the min and maxes the correct values
    if(r>l){
      max = r
      min = l
    }else{
      max = l
      min = r
    }
    scores_list = c(scores_list, min, max, recursive=T) #adds the mins and the maxes to the score_list 
  }
  return(scores_list) 
  
}


#calculates the number of wins and ties for a given breakpoint that might be subtracted 
breakpoint_win_count_sub <- function(k_ends_old, k_ends_new){
  win_counter = 0
  tie_counter = 0
  losses_counter = 0 #just to check that I can do addition
  min_bkpt = 0
  max_bkpt = 0
  dead_bkpt = setdiff(k_ends_old,k_ends_new) #finds what exactly what the delted breakpoint is 
  
  #finds location of deleted break point in the original k_ends
  dead_bkpt_grave = 0
  for(i in 1:length(k_ends_old)){
    if (k_ends_old[i] == dead_bkpt){
      dead_bkpt_grave = i
    }
  }
  
  #sets the min and max scores for the deleted breakpoint 
  right = k_ends_old[dead_bkpt_grave]-k_ends_old[dead_bkpt_grave-1]
  left = k_ends_old[dead_bkpt_grave+1]-k_ends_old[dead_bkpt_grave]
  if(right > left){
    max_bkpt = right
    min_bkpt = left
  }else{
    max_bkpt = left
    min_bkpt = right
  }
  
  scoring_list = list_of_scores_sub(k_ends_old)  #calculates a scoring list for k_ends_old
  hasWon = "no" #boolean will check to see if the point has already won match 

  #goes through the list and counts wins, ties, and losses
  for(i in 1:length(scoring_list)){
    if(i %% 2 != 0){#if i is odd, we're looking at minimums for the first time 
      hasWon = "no" #just makes sure that hasWon is set to no 
      if(min_bkpt< scoring_list[i]){ #if the breakpoint's minimum is less than the scorelist it won minimum 
        win_counter = win_counter+1 #increase the win counter by 1 
        hasWon = "yes" #the break point won so nothing else will need to happen 
      }#if(min_bkpt >= scoring_list[i]) don't do anything 
    }else if (i %% 2 == 0 && hasWon == "no"){#if i is even and it hasnt won, look at the maximum 
      if(max_bkpt < scoring_list[i]){#if the breakpoint's maximum is less than the scorelit it won maximum
        win_counter = win_counter+1 #increase the win counter by 1 
      } 
      else if(max_bkpt == scoring_list[i]){#if they're equal, then it's a tie
        tie_counter = tie_counter+0.5 #increase the tie counter by .5 
      }
      else{#the breakpoint loss =( so increaste the loss counter 
        losses_counter = losses_counter+1 #im so sorry for your loss 
      }
    }
  }
  
  tie_counter = tie_counter-0.5 #with current set up there is an occassion where the point is compared to itself; such a case will result in a tie; therefore, we need to remove one tie case 
  total_count = win_counter+tie_counter #now we need to add the total number of wins and ties 
  return(total_count)
  
}


#calculates the second part of the q algorithm for sub
#rhow --- probability of choosing a random subtraction instead of our created subtraction 
part_two_q_sub_score_sub <- function(k_ends_old, k_ends_new, rhow){
  numerator = breakpoint_win_count_sub(k_ends_old, k_ends_new) 
  denomenator = choose( (length(k_ends_old)-2) , 2)
  if(denomenator == 0 ){
    part_2 = (0)*(1-rhow) + ((rhow)*(1/(length(k_ends_old)-2)))
  } else {
    part_2 = (numerator/denomenator)*(1-rhow) + ((rhow)*(1/(length(k_ends_old)-2)))
  }
  return(part_2)
  
}

part_two_q_sub_score_sub(c(1,30,90), c(1,90), .3)
part_two_q_sub_score_sub(k_ends_old, k_ends_new, rhow)

