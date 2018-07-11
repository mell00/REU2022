
#creates a list of the scores for the n_frees 
n_free_scores_add <- function(n_free_list, k_ends_old){
  k_ends_ix = 2 #index of the closest breakpoint/endpoint in k_ends_old
  scores_list = c(0)  #establishes the list of scores, will remove 0 later 
  
  for(i in 1:length(n_free_list)){#goes through each n_free
    l = 0 
    r= 0 
    min = 0 
    max = 0 
    if(n_free_list[i]< k_ends_old[k_ends_ix]){#makes sure that the current element of the k_ends is still the upper bound 
      l = n_free_list[i]-k_ends_old[k_ends_ix-1] #distance between breakpoint and left neighbor
      r = k_ends_old[k_ends_ix]-n_free_list[i] #distance between breakpoint and right neighbor
    }
    else{#if the current element of the k_ends is not the upper bound, move the upper bound up 
      k_ends_ix = k_ends_ix+1 
      l = n_free_list[i]-k_ends_old[k_ends_ix-1] #distance between breakpoint and left neighbor
      r = k_ends_old[k_ends_ix]-n_free_list[i] #distance between breakpoint and right neighbor
    }
    #gives the min and maxes the correct values
    if(r>l){
      max = r
      min = l
    }else{
      max = l
      min = r
    }
    scores_list = c(scores_list, min, max) #adds the mins and the maxes to the score_list 
  }
  scores_list = scores_list[-1] #takes out that first zero 
  
  return(scores_list) 
}

#calculates the numbers of wins and ties for the added point
n_free_win_count_add <- function(score_list, k_ends_old, k_ends_new){
  
  win_counter = 0
  tie_counter = 0
  losses_counter = 0 #just to check that I can do addition
  min_bkpt = 0
  max_bkpt = 0
  added_bkpt = setdiff(k_ends_new, k_ends_old) #finds what exactly what the added breakpoint is 
  add_bkpt_birthplace = 0 #finds location of added break point in the original k_ends
  for(i in 1:length(k_ends_new)){
    if (k_ends_new[i] == added_bkpt){
      add_bkpt_birthplace = i
    }
  }
  
  #sets the min and max scores for the added breakpoint 
  left = k_ends_new[add_bkpt_birthplace]-k_ends_new[add_bkpt_birthplace-1]
  right = k_ends_new[add_bkpt_birthplace+1]-k_ends_new[add_bkpt_birthplace]
  if(right > left){
    max_bkpt = right
    min_bkpt = left
  }else{
    max_bkpt = left
    min_bkpt = right
  }
  
  hasWon = "no" #boolean will check to see if the point has already won match 
  
  #goes through the list and counts wins, ties, and losses
  for(i in 1:length(score_list)){
    if(i %% 2 != 0){#if i is odd, we're looking at minimums for the first time 
      hasWon = "no" #just makes sure that hasWon is set to no
      if(min_bkpt > score_list[i]){ #if the breakpoint's minimum is less than the scorelist it won minimum 
        win_counter = win_counter+1 #increase the win counter by 1 
        hasWon = "yes" #the break point won so nothing else will need to happen 
      }#if(min_bkpt >= scoring_list[i]) don't do anything 
    }else if (i %% 2 == 0 && hasWon == "no"){#if i is even and it hasnt won, look at the maximum 
      if(max_bkpt > score_list[i]){#if the breakpoint's maximum is less than the scorelit it won maximum
        win_counter = win_counter+1 #increase the win counter by 1 
      } 
      else if(max_bkpt == score_list[i]){#if they're equal, then it's a tie
        tie_counter = tie_counter+0.5 #increase the tie counter by .5 
      }
      else{#the breakpoint loss =( so increaste the loss counter 
        losses_counter = losses_counter +1 #im so sorry for your loss 
      }
    }
  }
  tie_counter = tie_counter-0.5 #tie case ; we need to remove one tie case
  total_count = win_counter+tie_counter #now we need to add the total number of wins and ties 
  return(total_count)
  
}

#calculates the second part of the q algorithm for sub
part_two_q_add_score_add <- function(n_unfree, k_ends_old, k_ends_new, rhow){
  
  data = c(1:max(k_ends_old))

  #creates the list of n_free points 
  n_free_list = setdiff(data, n_unfree)
  list_of_n_frees = sort(n_free_list)
  
  n_free_score_list = n_free_scores_add(list_of_n_frees, k_ends_old) #calculates and creates the scoring list for the n_free points 
  numerator = n_free_win_count_add(n_free_score_list, k_ends_old, k_ends_new)
  denomenator = choose(length(list_of_n_frees), 2)
  part_2 = (numerator/denomenator)*(1-rhow)+(rhow)*(1/(length(list_of_n_frees)))
  return(part_2)
  
}

K_ENDS_OLD = c(1,15,27,30)
K_ENDS_NEW = c(1,15,21,27,30) 
N_UNFREE = c(1,2,3,13,14,15,16,17,25,26,27,28,29,30)
RHOW = .5

part_two_q_add_score_add(N_UNFREE, K_ENDS_OLD, K_ENDS_NEW,  RHOW)

answer


#0.08125