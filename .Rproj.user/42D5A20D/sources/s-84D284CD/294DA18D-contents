
# download RTools if issues 
# https://cran.r-project.org/bin/windows/Rtools/rtools42/rtools.html
# remember to close out of R after Rtools is downloaded 
#install.packages("devtools")
library(devtools)
library(strucchange)
library(stats)
library(MASS)
library(autostsm)
par(mfrow=c(1,1))

setwd("\\Users\\mellm\\github\\REU2022\\Variations_of_bars")
source("bama1.R")

test_data_45 = function(){
  mu = 1
  y = rnorm(1,0,1)
  alpha = .5
  epsilon = rnorm(1,0,1)
  epsilon_t = function(t){ # t MUST be greater than or equal to 2
    epsilon_t = c(epsilon)
    for (i in 1:t){
      epsilon_t[i] = rnorm(1,0,1)
    }
    return(epsilon_t[t])
  }
  
  
  for (i in 1:90){
    y[i]= mu + epsilon + alpha*epsilon_t(2) # + .............
    
  }
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
  y[,1] = as.Date(y[,1])
  starting_breakpoints = which(!is.na(stsm_detect_breaks(stsm_estimate(data.frame(y)),y)$`10%_break`))
  test1=bama(starting_breakpoints,1:90,y[,2],iterations)
  print(i)
  L[,i]=test1
  M[i]=length(starting_breakpoints[!is.na(starting_breakpoints)])
  A=c(A,(unlist(test1$Breakpoints))) #Bama location of breakpoints
  B=c(B,(starting_breakpoints)) #B-P
}
A2=A[!is.na(A)]
B2=B[!is.na(B)]
mean(L)
sd(L)
mean(M)
sd(M)
plot(apply(L,1,mean))
hist(A2, breaks=100, main="BAMA Breakpoints", ylab="Number of iterations (out of interations*runs)", xlab="Time")#50= 5000(iterations*runs)
abline(v=45, col="red",lwd = 3)# change v to match breakpoints
hist(B2, breaks=100, main="Bai-Perron Breakpoints", xlab="time") #(frenquency/runs)