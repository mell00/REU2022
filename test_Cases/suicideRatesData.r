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
single_model <- arima(suicide$X15.24.years, order=c(2,0,0))
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
current_data = ts(current_data)
A3 = sort(unique(A2))
#WORK IN PROGRESS ---------------------------------------------------
fits = new_baar_fit(current_data,A3)
new_baar_fit = function(current_data,breakpoints){
  new_fits = c() #list of new fitted values
  spliced_fitted_arima =  NA #list of arima'ed time series
  new_time = c(1) #list of time vectors for each bkpt (for spliced_data)
  new_data = c(2) #list of data_45 vectors for each bkpt (for spliced_data)
  new_start = data.frame(new_time,new_data)
  for (i in breakpoints){
    splice_data = function(current_data){ #Why does this function need to be called for each bkpt?
      for (j in 1:ncol(current_data)){
        if (j == 1){ #if j is time
          append(new_time,c(current_data[,j][1:i]))
        } else if (j==2) {
          append(new_data,c(current_data[,j][1:i]))
        }
      }
      return(new_start)
    }
    spliced_data = splice_data(current_data)
    #append to list of arima'ed time series
    append(spliced_fitted_arima, arima(new_start$new_data[[i,]],order = c(1,0,0))) #Something wrong here
    append(new_fits, fitted(spliced_fitted_arima[i]))
  }
  return(unique(new_fits))
}
#------------------------------------------------------------------------











hist(A)
plot(apply(L,1,mean))
hist(A2, breaks=100, main="Distribution of BAAR Breakpoints", ylab = "Number of Iterations", xlab="Time",xaxt = "n",col="#8EDCE6")#50= 5000(iterations*runs)
axis(1, at=1:37, labels=1979:2015, tick=T)
abline(v=c(17,23,31), col="red",lwd = 3)# change v to match breakpoints
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
#BAAR fitted data
lines(c(1979:2015), fitted_baar)







legend(c(1979:2015), X15.24.years, list("Original Data","ARIMA AR(3) Fitted Data", "BAAR AR(3) Fitted Data"))

