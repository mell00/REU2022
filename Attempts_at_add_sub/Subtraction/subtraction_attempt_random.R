#subtraction attempt zero - random murder

k_ends_test = c(1,50,55,60) #test k thing 

barMurder0<-function(k_ends){

	k = k_ends[c(-1,-length(k_ends))] #removing first and last value 
	random_num = sample(1:length(k), 1) #chooses random number 
	k_ends_final = k_ends[-(random_num+1)] #removes random number 
	return(k_ends_final) #returns it 

}

barMurder0(k_ends_test) #calls test 