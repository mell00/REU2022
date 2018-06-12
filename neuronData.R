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






