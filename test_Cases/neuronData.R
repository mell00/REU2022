#example of bad splines 
neuron = read.table("exampledata.txt")

library(splines)
?splines
 
mod3 = lm(neuron[,2] ~ bs(neuron[,1], df=3))
mod5 = lm(neuron[,2] ~ bs(neuron[,1], df=5))
mod9 = lm(neuron[,2] ~ bs(neuron[,1], df=9))
mod20 = lm(neuron[,2] ~ bs(neuron[,1], df=20))
mod110 = lm(neuron[,2] ~ bs(neuron[,1], df=110))


plot(neuron, xlab="Seconds", ylab="Number of Firing Events", main="Neuron Data, df = 3")
lines(neuron[,1], mod3$fitted.values, col="red", lwd=2)
plot(neuron, xlab="Seconds", ylab="Number of Firing Events", main="Neuron Data, df = 9")
lines(neuron[,1], mod9$fitted.values, col="darkgreen", lwd=2)
plot(neuron, xlab="Seconds", ylab="Number of Firing Events", main="Neuron Data, df = 110")
lines(neuron[,1], mod110$fitted.values, col="blue")


plot(neuron, xlab="Seconds", ylab="Number of Firing Events", main="Neuron Data, All fits")
lines(neuron[,1], mod3$fitted.values, col="red", lwd=2)
lines(neuron[,1], mod9$fitted.values, col="darkgreen", lwd=2)
lines(neuron[,1], mod110$fitted.values, col="blue", lwd=1)

library("strucchange")
?breakpoints

#Bai-Perron Method
dif_means_neuron<-ts(neuron[,2], start=1, end=110) #create time series data on only the first data 
bkpts_neuron<-breakpoints(dif_means_neuron ~ 1, breaks = 5, h=.05) #no break points 
plot(dif_means_neuron)
points(43,neuron[43,2], col="purple", pch= 18)
points(48,neuron[48,2], col="purple", pch= 18)
points(56,neuron[56,2], col="purple", pch= 18)
points(72,neuron[72,2], col="purple", pch= 18)
points(98,neuron[98,2], col="purple", pch= 18)






