#subtraction attempt one (deletes break that creates smallest interval)

k_ends_test = c(1, 20, 40, 60, 80)

barMurder1 <- function(k_ends){
  
	all_intv = diff(k_ends) #finds all of the intervals
	sum_intv = diff(cumsum(all_intv)) #finds the sums of the adjacent intervals
	small_intv_loc = which(sum_intv == min(sum_intv)) #stores indexes of smallest sum_intv

	min_intv_loc = 0 #sets the minimum sum_intv index to 0 
  
	#if there's only one small sum_intv
	#{the location in sum_intv is equal to the single value in small_intv_loc}
	if(length(small_intv_loc) == 1){
		min_intv_loc = small_intv_loc[1]
	}

	#if there are multiple small sum_intv
	#{pick a random number from 1-length of small_intv_loc
	#location od the smallest interval in sum_intv is equal to the value in small_intv_loc[random#]}
	else{
		num = sample(1:length(small_intv_loc), 1)
		min_intv_loc = small_intv_loc[num]
	}
  
	#find and delete correct break 
	k_ends_final = k_ends[-(min_intv_loc+1)]

	return(k_ends_final)
  
}

barMurder1(k_ends_test)
