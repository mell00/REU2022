#new datasets

require(smooth)
ourSimulation <- sim.es("MAdM", frequency=12, obs=90, phi=0.95, persistence=c(0.01,0.5,0.01))
plot(ourSimulation)


ourSimulation <- sim.es("MNN", frequency=12, obs=90, iprob=0.2, initial=10, persistence=0.1)
plot(ourSimulation)


ourSimulation <- sim.ssarima(orders=list(ar=c(0,1),i=c(1,0),ma=c(1,2)), lags=c(1,12), constant=TRUE, obs=90)
plot(ourSimulation)


ourSimulation_1 <- sim.ces("p",B=0.2,frequency=12, obs=90, nsim=10)
plot(ourSimulation)

ourSimulation_2 <- sim.ces("p",B=0.3,frequency=12, obs=90, nsim=10)
plot(ourSimulation)


require(graphics)

ourSimulation_1 = arima.sim(n = 90, list(ar = c(0.8897, -0.4858), ma = c(0, 0)),sd = sqrt(0.1796))
plot(ourSimulation_1)

ourSimulation_1 = arima.sim(n = 90, list(ar = c(1,-.05), ma = c(0, 0)),sd = sqrt(0.5))
plot(ourSimulation_1)

ourSimulation_1 = arima.sim(n = 90, list(ar = c(0.01,-0.01), ma = c(0, 0)),sd = sqrt(0.01))
plot(ourSimulation_1)

ourSimulation_1 = arima.sim(n = 90, list(ar = c(0.5,-0.5), ma = c(0, 0)),sd = sqrt(0.5))
plot(ourSimulation_1)



sim_1 <- arima.sim(model = list(ar = 0.1, order = c(1,0,0)), n = 45)
sim_2 <- arima.sim(model = list(ar = 0.5, order = c(1,0,0)), n = 45)
sim_3 <- arima.sim(model = list(ar = 0.9999, order = c(1,0,0)), n = 45)
plot.ts(cbind(sim_1, sim_2, sim_3))

list = c(sim_3,sim_1)
x=c(1:90)
data_1 = data.frame(x,list)

plot(data_1)

set.seed(1)
first <- arima.sim(model = list(ar = 0.01, order = c(1,0,0)), n = 45)
second <- arima.sim(model = list(ar = 0.9, order = c(1,0,0)), n = 45)
time = c(1:90)
data_11 = c(second, first)
test_data_11 = data.frame(time,data_11)
plot(test_data_11, main="Simulated Time Series Data", ylab = "Dependent Variable", xlab="Time")
