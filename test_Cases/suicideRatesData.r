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


library("forecast")
setwd("/Users/mellm/github/REU2022/")
source("ar_bai_perron.r")
source("data_for_trials.r")
source("Variations_of_bars/fixedbaar1.r")


iterations=1000
runs=10
L=matrix(NA,nrow=iterations,ncol=runs)
A=NA
M=NA
B=NA
for(i in 1:runs){
  current_data = suicide
  break_p = breakpoints(current_data[,3] ~ current_data[,1], breaks = 5, h = 0.1) 
  starting_breakpoints = break_p$breakpoints
  test1=baar(starting_breakpoints,1:37,current_data[,3],iterations)
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
plot(apply(L,1,mean))
hist(A2, breaks=100, main="BAAR Breakpoints", ylab="Number of iterations (out of interations*runs)", xlab="Time")#50= 5000(iterations*runs)
abline(v=17, col="red",lwd = 3)# change v to match breakpoints
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


