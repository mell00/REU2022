current_result <- current_result #EDIT HERE! define name of results you want to work with
color <- "red" #EDIT HERE! color to use for results

finbkpts<-NULL

if(is.atomic(current_result$Breakpoints) == T){

		finbkpts=na.omit(current_result$Breakpoints)

}else{
	for(i in 1:ncol(current_result$Breakpoints)){
		finbkpts<-c(finbkpts, na.omit(current_result$Breakpoints[,i]), recursive=T)
	}
}

#EDIT HERE! converts breakpoint locations from observation number to time
offset <- 0
finbkpts <- finbkpts+offset

#EDIT HERE! change  histogram of breakpoint locations
hist(finbkpts, ylim=c(0,length(current_result$BIC[,1])), right=F, xlab="Time", ylab="Number of Iterations", main="Distribution of Breakpoint Locations", col=color)

#EDIT HERE! define which k/tau for mean posterior fit
bkpt_set <- c(30,60)

if(length(bkpt_set) > 3){

	fits_to_use <- current_result$Fits[which(current_result$Breakpoints[,1] == sort(bkpt_set)[[1]]
	& current_result$Breakpoints[,2] == sort(bkpt_set)[[2]]
	& current_result$Breakpoints[,3] == sort(bkpt_set)[[3]]
	& current_result$Breakpoints[,3] == sort(bkpt_set)[[4]]
	& current_result$NumBkpts == length(bkpt_set)),]

}else if(length(bkpt_set) == 3){

	fits_to_use <- current_result$Fits[which(current_result$Breakpoints[,1] == sort(bkpt_set)[[1]]
	& current_result$Breakpoints[,2] == sort(bkpt_set)[[2]]
	& current_result$Breakpoints[,3] == sort(bkpt_set)[[3]]
	& current_result$NumBkpts == length(bkpt_set)),]

}else if(length(bkpt_set) == 2){

	fits_to_use <- current_result$Fits[which(current_result$Breakpoints[,1] == sort(bkpt_set)[[1]]
	& current_result$Breakpoints[,2] == sort(bkpt_set)[[2]]
	& current_result$NumBkpts == length(bkpt_set)),]

}else if(length(bkpt_set) == 1){

	fits_to_use <- current_result$Fits[which(current_result$Breakpoints[,1] == sort(bkpt_set)[[1]]
	& current_result$NumBkpts == length(bkpt_set)),]

}else{

	fits_to_use <- current_result$Fits[which(current_result$NumBkpts == length(bkpt_set)),]

}

plot(test_data[,2])
points(colMeans(fits_to_use), col=color)