
#For results section of paper (section 3.1)
  #make sure the BAAR function is loaded as well as data 44
current_result = baar(c(100,200), test_data_44[,1], test_data_44[,2], 10000, 1500, jump=0.25, ar=1, progress=T, fit_storage=T)

#plotting for results section in paper 
plot(test_data_44, main = "Two Breaks with High Variance", xlab="Time", ylab="Dependent Variable", pch=16)

#K
hist(current_result$NumBkpts,xlim=c(0,3), breaks = c(0.5,1.5,2.5),xaxp=c(0, 3, 3), xlab="Time", ylab="Number of Iterations (out of 10,000)", main="Distribution of Number of Breakpoints", col="#006e82")

#tau
hist(finbkpts, breaks = c(1:300), xlim=c(0,300), ylim=c(0,10000), right=F, xlab="Time", ylab="Number of Iterations (out of 10,000)", main="Distribution of Breakpoint Locations", col="#006e82")

#beta and Sigma
fits_to_use = current_result$Fits[which(current_result$Breakpoints[,1] == 100 & current_result$Breakpoints[,2] == 200 & current_result$NumBkpts == 2),]
points(c(1:100),colMeans(fits_to_use)[1:100], col="#00a0fa", pch=17)
lines(c(1:100),colMeans(fits_to_use)[1:100], col="#00a0fa", lty=2)
points(c(101:200),colMeans(fits_to_use)[101:200], col="#0ab45a", pch=18)
lines(c(101:200),colMeans(fits_to_use)[101:200], col="#0ab45a", lty=2)
points(c(201:300),colMeans(fits_to_use)[201:300], col="purple", pch=18)
lines(c(201:300),colMeans(fits_to_use)[201:300], col="purple", lty=2)
mean(current_result$BIC[which(current_result$Breakpoints[,1] == 11 & current_result$NumBkpts == 1),])


#(section 3.2)
  #data set with one break
plot(test_data_300, main = "One Break with High Variances", xlab="Time", ylab="Dependent Variable", pch=16)

####for all the table information look at the file burn_in_analysis_baar

#Data for the tables
setwd("/Users/Amy/REU2018/test_Cases")
all_data3 = readRDS("data3_alljump.RData")
all_data3$AcceptRate
all_data3$AcceptedSteps / all_data3$ProposedSteps



#(section 3.3)
  #data set with 10 breaks
plot(test_data_100, main="Eight Breaks with Low Variance" , xlab="Time", ylab="Dependent Variable", pch=16)



#Case Study 
setwd("/Users/Amy/REU2018/test_Cases")
all = readRDS("pelican_alljump.RData")
all$AcceptRate
plot(all$AcceptRate)
all$AcceptedSteps / 5000







#This is for the poster (Jump and Jiggle) 
plot(test_data_300, main = "One Break with High Variances", xlab="Time", ylab="Dependent Variable", pch=16)
set.seed(1)
plot(test_data_300, main = "Jiggle Example", xlab="Time", ylab="Dependent Variable", pch=16)
points(test_data_300[100,1], test_data_300[100,2], col="#006e82", pch=16, cex=3)
points(test_data_300[99,1], test_data_300[99,2], col="red", pch=16, cex=3)
abline(v=90, col="red", lwd=3, lty=2)
abline(v=110, col="red", lwd=3, lty=2)

plot(test_data_300, main = "Jump Example", xlab="Time", ylab="Dependent Variable", pch=16)
points(test_data_300[100,1], test_data_300[100,2], col="#006e82", pch=16, cex=3)
points(test_data_300[175,1], test_data_300[175,2], col="red", pch=16, cex=3)





