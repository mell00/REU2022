#addition attempt zero - random make

k_ends_test = c(1,20,40,60)

barMake0<-function(k_ends){

	rand_spot = sample(k_ends[1]:k_ends[length(k_ends)], 1)
	k_ends_final = sort(c(k_ends, rand_spot))
	d = diff(k_ends_final)
	if(min(d) < 3) {
	  barMake0(k_ends)
	} else {
	  return(k_ends_final)
	}
	

}

barMake0(k_ends_test)

