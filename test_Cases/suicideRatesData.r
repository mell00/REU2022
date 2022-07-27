library(devtools)
#install_version("FitAR", version = "1.94", repos = "http://cran.us.r-project.org")
#library(FitAR)
library(forecast)
library(strucchange)
library(stats)
library(MASS)
par(mar=c(1,1,1,1))
setwd("/Users/mellm/github/REU2022/test_Cases")
suicide<-read.csv("suicideRatesData.csv")
attach(suicide)

setwd("/Users/mellm/github/REU2022/")
source("ar_bai_perron.r")
source("data_for_trials.r")
source("Variations_of_bars/fixedbaar1.r")

#fit data with ARIMA
single_model <- arima(suicide$X15.24.years, order=c(3,0,0))
single_fitted <- fitted(single_model)

test_data_45 = function(){
  data_45=suicide$X15.24.years
  time = c(1:37)
  test_data_45 = data.frame(time, data_45)
  return(test_data_45)
}

iterations=1000
runs=10
L=matrix(NA,nrow=iterations,ncol=runs)
A=NA
M=NA
B=NA
for(i in 1:runs){
  current_data = test_data_45()
  break_p = breakpoints(current_data[,2] ~ current_data[,1], breaks = 3, h = 0.1) 
  starting_breakpoints = break_p$breakpoints
  test1=baar(starting_breakpoints,1:37,current_data[,2],iterations, 15, jump=0.25, ar=3)
  print(i)
  L[,i]=test1$NumBkpts
  M[i]=length(starting_breakpoints[!is.na(starting_breakpoints)])
  A=c(A,(unlist(test1$Breakpoints))) #Baar location of breakpoints
  B=c(B,(starting_breakpoints)) #B-P
}
A2=A[!is.na(A)]
B2=B[!is.na(B)]
mean(L)
sd(L)
mean(M)
sd(M)
arima(test1$Breakpoints, order = c(3,0,0))

hist(A)
plot(apply(L,1,mean))
hist(A2, breaks=100, main="Distribution of BAAR Breakpoints", ylab = "Number of Iterations", xlab="Time",xaxt = "n",col="#8EDCE6")#50= 5000(iterations*runs)
axis(1, at=1:37, labels=1979:2015, tick=T)
abline(v=c(17,25,31), col="red",lwd = 3)# change v to match breakpoints
hist(B2, breaks=100, main="Bai-Perron Breakpoints", xlab="time") #(frenquency/runs)

#percentage graphs "saved graphs"
#Step1: Change the axis to match percents
#Step2: Save onto REU folder shared as in the following order BAAR(orBP)_beta1value_beta2value_sdvalue_@breakpointlocation
#example BAAR7_3_1_@45
#example BP7_3_1_@45
hist(A2, breaks=100, main="BAAR Breakpoints", ylab="Percentage of time location chosen", xlab="Time",yaxt="n")
axis(2, at=c(0,1000,2000,3000,4000),labels=c(0,1000/100000,2000/100000,3000/100000,4000/100000)) #100,000=(iterations*runs)
abline(v=45, col="red",lwd = 3)# change v to match breakpoints
hist(B2, breaks=100, main="Bai-Perron Breakpoints", ylab= "Percentage of time location chosen ", xlab="Time",yaxt="n")
axis(2, at=c(0,3.5,7,10.5,14),labels=c(0,3.5/100,7/100,10.5/100,14/100))#(frenquency/runs)
abline(v=45, col="red",lwd = 3)# change v to match breakpoints

#plot the data itself
par(mgp=c(3,1,0),mar=c(5,4,4,2)+0.1)
plot(Year,X15.24.years,main="Suicide Count Among People Ages 15-24 in the U.S.A.: 1979 to 2015", ylab = "Number of Suicides", xlim = c(1979,2015), ylim = c(0,max(X15.24.years) + 4000), pch=19, cex.main= 0.75, cex.axis=0.75, cex.lab=0.75, col="#8EDCE6")
#ARIMA fitted data
lines(c(1979:2015), single_fitted, col="#9EECF7", lty=1)
points(c(1979:2015), single_fitted, col="#9EECF7", pch=15)
#original data
lines(c(1979:2015), X15.24.years, col="#331832", lty=1)
points(c(1979:2015), X15.24.years, col="#331832", lty=1, pch=16)
#







legend(c(1979:2015), X15.24.years, list("Original Data","ARIMA AR(3) Fitted Data", "BAAR AR(3) Fitted Data"))

