#subtraction attempt one (deletes break that creates smallest interval)

k_ends_test = c(1, 20, 40, 60, 80)
k_ends = c(1,8,60)

barMurder1 <- function(k_ends){
  
	all_intv = diff(k_ends) #finds all of the intervals
	intv_1 = all_intv[-1] #takes the first number off
	intv_2 = all_intv[-length(all_intv)] #takes the last number off
	sum_intv = intv_1 + intv_2 #finds the sums of the adjacent intervals
	location = rmultinom(1, size = 1, prob = (1/sum_intv)/sum(1/sum_intv)) #choose a breakpoint based on probabilities
	k_ends_final = k_ends[-(which.max(location)+1)] #find and delete correct break 
	return(k_ends_final)
  
}

barMurder1(k_ends_test)
