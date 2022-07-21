# download RTools if issues 
# https://cran.r-project.org/bin/windows/Rtools/rtools42/rtools.html
# remember to close out of R after Rtools is downloaded 
#install.packages("devtools")
library(devtools)
#install_version("FitAR", version = "1.94", repos = "http://cran.us.r-project.org")
#library(FitAR)
library(strucchange)
library(stats)
library(MASS)
par(mfrow=c(1,1))

setwd("\\Users\\sarah\\OneDrive\\Documents\\REU\\REU2022-master\\Variations_of_bars")
source("Baar.R") #should be fixedaar updated version

#NOTES: burn-in=100 CHANGE on Fixed BAAR
        #iterations = 1000
        #runs =100

test_data_45 = function(){
  beta1 = .7
  beta2= .3
  stdev=1
  y= rnorm(1,0,1)
  for( i in 2:45){ 
    y[i] = beta1*y[i-1]+rnorm(1,0,stdev)}
  for( i in 46:90){
    y[i] = beta2*y[i-1]+rnorm(1,0,stdev)}
  data_45=y
  time = c(1:90)
  test_data_45 = data.frame(time, data_45)
  return(test_data_45)
}
iterations=1000
runs=100
L=matrix(NA,nrow=iterations,ncol=runs)
A=NA
M=NA
B=NA
for(i in 1:runs){
  y=test_data_45()
  current_data = y
  break_p = breakpoints(current_data[,2] ~ current_data[,1], breaks = 5, h = 0.1) 
  starting_breakpoints = break_p$breakpoints
  test1=baar(starting_breakpoints,1:90,y[,2],iterations)
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
abline(v=45, col="red",lwd = 3)# change v to match breakpoints
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
