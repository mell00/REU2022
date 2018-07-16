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

ourSimulation_1 = arima.sim(n = 63, list(ar = c(0.8897, -0.4858), ma = c(0, 0)),
          sd = sqrt(0.1796))
plot(ourSimulation_1)

ourSimulation_1 = arima.sim(n = 63, list(ar = c(1,-.05), ma = c(0, 0)),
                            sd = sqrt(0.1796))
plot(ourSimulation_1)
