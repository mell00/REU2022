current_result = current_result
offset = 0

finbkpts<-NULL

if(is.atomic(current_result$Breakpoints) == T){

		finbkpts=na.omit(current_result$Breakpoints)

}else{
	for(i in 1:ncol(current_result$Breakpoints)){
		finbkpts<-c(finbkpts, na.omit(current_result$Breakpoints[,i]), recursive=T)
	}
}

finbkpts = finbkpts+offset

hist(finbkpts, xlim=c(0,90), breaks=seq(1.5,89.5,1), ylim=c(0,10000), xaxp=c(0, 90, 10), right=F, xlab="Time", ylab="Number of Iterations (out of 10,000)", main="Distribution of Breakpoint Locations", col="#006e82")
