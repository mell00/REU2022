#subtraction attempt zero - random murder

k_ends_test = c(1,50,55,60)

barMurder0<-function(k_ends){

	k = k_ends_test[c(-1,-length(k_ends_test))]
	random_num = sample(1:length(k), 1)
	k_ends_final = k_ends[-(random_num+1)]
	k_ends_final

}

barMurder0(k_ends_test)