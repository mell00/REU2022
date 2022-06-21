setwd("/Users/khaglich/Desktop/Edited REU Main/Variations_of_bars")
source("baar.R")

setwd("/Users/khaglich/Desktop/Edited REU Main")
source("data_for_trials.R")

#make all graphics 1028 642

#For results section of paper (section 3.1)
  #make sure the BAAR function is loaded as well as data 44
#Error in test_data_44[, 1] : object of type 'closure' is not subsettable
d <- test_data_44()
current_result = baar(c(100,200), d[,1], d[,2], 10000, 1500, jump=0.25, ar=1, progress=T, fit_storage=T)

#plotting for results section in paper 
plot(d, main = "Two Breaks with High Variance", xlab="Time", ylab="Dependent Variable", pch=16)

#K
hist(current_result$NumBkpts,xlim=c(0,3), breaks = c(0.5,1.5,2.5),xaxp=c(0, 3, 3), xlab="Time", ylab="Number of Iterations (out of 10,000)", main="Distribution of Number of Breakpoints", col="#006e82")

#tau
current_data <- d[,2] #EDIT HERE! vector of true data points
color <- "#006e82" #EDIT HERE! color to use for results
finbkpts<-NULL
if(is.atomic(current_result$Breakpoints) == T){
  finbkpts=na.omit(current_result$Breakpoints)
}else{
  for(i in 1:ncol(current_result$Breakpoints)){
    finbkpts<-c(finbkpts, na.omit(current_result$Breakpoints[,i]), recursive=T)
  }
}
offset <- 0
finbkpts <- finbkpts+offset
hist(finbkpts, breaks = c(1:300), xlim=c(0,300), ylim=c(0,10000), right=F, xlab="Time", ylab="Number of Iterations (out of 10,000)", main="Distribution of Breakpoint Locations", col="#006e82")

#beta and Sigma
beta_to_use = current_result$Beta[which(current_result$Breakpoints[,1] == 100 & current_result$Breakpoints[,2] == 200 & current_result$NumBkpts == 2)]
sigma_to_use = current_result$Sigma[which(current_result$Breakpoints[,1] == 100 & current_result$Breakpoints[,2] == 200 & current_result$NumBkpts == 2)]
fits_to_use = current_result$Fits[which(current_result$Breakpoints[,1] == 100 & current_result$Breakpoints[,2] == 200 & current_result$NumBkpts == 2),]
lower = apply(fits_to_use, 2, quantile, probs = 0.025, na.rm = T)
upper = apply(fits_to_use, 2, quantile, probs = 0.975, na.rm = T)

plot(d, main = "Simulated Data: BAAR Fits with Quantiles", xlab="Time", ylab="Dependent Variable", pch=16, cex=0.5)
points(c(1:100),colMeans(fits_to_use)[1:100], col="#00a0fa", pch=17, cex=0.5)
lines(c(1:100),colMeans(fits_to_use)[1:100], col="#00a0fa", lty=2)
points(c(101:200),colMeans(fits_to_use)[101:200], col="#0ab45a", pch=18, cex=0.5)
lines(c(101:200),colMeans(fits_to_use)[101:200], col="#0ab45a", lty=2)
points(c(201:300),colMeans(fits_to_use)[201:300], col="purple", pch=18, cex=0.5)
lines(c(201:300),colMeans(fits_to_use)[201:300], col="purple", lty=2)
lines(c(1:100),lower[1:100], col="#00a0fa", lty=3)
lines(c(101:200),lower[101:200], col="#0ab45a", lty=3)
lines(c(201:300),lower[201:300], col="purple", lty=3)
lines(c(1:100),upper[1:100], col="#00a0fa", lty=3)
lines(c(101:200),upper[101:200], col="#0ab45a", lty=3)
lines(c(201:300),upper[201:300], col="purple", lty=3)


#delta BIC 
  #armina of all data
arima_1 = arima(d[,2],order=c(1,0,0))
BIC(arima_1)
  #BIC from the BAAR model fits
min_arima = min(current_result$BIC[which(current_result$Breakpoints[,1] == 100 & current_result$Breakpoints[,2] == 200),])
min_arima
  #delta BIC
dif = BIC(arima_1) - min_arima
dif




#(section 3.2)
  #data set with one break
plot(test_data_300(), main = "One Break with High Variances", xlab="Time", ylab="Dependent Variable", pch=16)

####for all the table information look at the file burn_in_analysis_baar

#Data for the tables
setwd("/Users/khaglich/Desktop/Edited REU Main/baar_simulation_data")
all_data3 = readRDS("data3_alljump.RData")
all_data3$AcceptRate
all_data3$AcceptedSteps / all_data3$ProposedSteps



#(section 3.3)
  #data set with 8 breaks
plot(test_data_100(), main="Eight Breaks with Low Variance" , xlab="Time", ylab="Dependent Variable", pch=16)


#Case Study 
setwd("/Users/khaglich/Desktop/Edited REU Main/test_Cases/Old")
all = readRDS("pelican_alljump.RData")
all$AcceptRate
plot(all$AcceptRate)
all$AcceptedSteps / 5000




#This is for the poster (Jump and Jiggle) 
d <- test_data_300()
plot(d, main = "One Break with High Variances", xlab="Time", ylab="Dependent Variable", pch=16)
set.seed(1)
plot(d, main = "Jiggle Example", xlab="Time", ylab="Dependent Variable", pch=16)
points(d[100,1], d[100,2], col="#006e82", pch=16, cex=3)
points(d[99,1], d[99,2], col="red", pch=16, cex=3)
abline(v=90, col="red", lwd=3, lty=2)
abline(v=110, col="red", lwd=3, lty=2)

plot(d, main = "Jump Example", xlab="Time", ylab="Dependent Variable", pch=16)
points(d[100,1], d[100,2], col="#006e82", pch=16, cex=3)
points(d[175,1], d[175,2], col="red", pch=16, cex=3)





