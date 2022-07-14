# download RTools if issues 
# https://cran.r-project.org/bin/windows/Rtools/rtools42/rtools.html
# remember to close out of R after Rtools is downloaded 
#install.packages("devtools")
library(devtools)
#install_version("FitAR", version = "1.94", repos = "http://cran.us.r-project.org")
library(FitAR)
library(strucchange)
library(MASS)
par(mfrow=c(2,2))

setwd("\\Users\\sarah\\OneDrive\\Documents\\REU\\REU2022-master\\Variations_of_bars")
source("baar.R")

test_data_45 = function(){
  beta = -.7
  stdev= 1
  y= rnorm(1,0,1)
  for( i in 2:32){
    y[i] = beta*y[i-1]+rnorm(1,0,stdev)}
  for(i in 33:90){
    y[i] = beta*y[i-1]+rnorm(1,0,stdev)}
  data_45=y
  time = c(1:90)
  test_data_45 = data.frame(time, data_45)
  return(test_data_45)
}
iterations=250
runs=10
L=matrix(NA,nrow=iterations,ncol=runs)
for(i in 1:runs){
  y=test_data_45()
  current_data = y
  break_p = breakpoints(current_data[,2] ~ current_data[,1], breaks = 5, h = 0.1) 
  starting_breakpoints = break_p$breakpoints
  test1=baar(starting_breakpoints,1:90,y[,2],iterations)
  print(i)
  L[,i]=test1$NumBkpts}
mean(L)
sd(L)
