#full set of data with plots 
xwhole = c(1,   2,   3,    4,    5,   6,   7,    8,    9,     10,   11)
ywhole = c(1.0, 0.5, 0.75, 2.75, 2.5, 13,  7,    12.5, 23,  21, 26.1 ) #6.5, 10.5, 14.5, 18.5,  22.5
plot(xwhole, ywhole, cex = 2, pch = 16)
points(5, 2.5, col="black", cex = 4, pch=16)




#five is the breakpoint so it needs to be in both regressions 
xhalf1 = c(1:5) 
xhalf2 = c(5:11)

#half sets of data for regression 
yhalf1 = c(0.5,1,1.5,2,2.5) #1-5
yhalf2 = c(2.5,6.5,10.5,14.5,18.5,22.5, 26.5)#5-10 

#does the regressions to get slopes and intercepts 
firstReg = lm(yhalf1~xhalf1) #m = .5 b=0
summary(firstReg)
secondReg = lm(yhalf2~xhalf2) # m = 4 b= -17.5
summary(secondReg)

#plots the regression lines with the 
clip(1,5,0.5,2.5) #bounded by x0=1,x1=5,y0=0.5,y1=2.5
abline(firstReg, lwd = 3)
clip(5,11,2.5,26.5)#bounded by x0=5,x1=10,y0=2.5,y1=22.5
abline(secondReg, lwd = 3)





