#data analysis for BAAR
data11_j1j0 = readRDS("baar_data11_jump1jigg0.RData")
data11_j25j75 = readRDS("baar_data11_jump25jigg75.RData")
data11_j50j50 = readRDS("baar_data11_jump50jigg50.RData")
data11_j75j25 = readRDS("baar_data11_jump75jiggle25.RData")
data11_j0j1 = readRDS("baar_data11_jump0jigg1.RData")

data1_j1j0 = readRDS("baar_data1_jump1jigg0.RData")
data1_j25j75 = readRDS("baar_data1_jump25jigg75.RData")
data1_j50j50 = readRDS("baar_data1_jump50jigg50.RData")
data1_j75j25 = readRDS("baar_data1_jump75jiggle25.RData")
data1_j0j1 = readRDS("baar_data1_jump0jigg1.RData")

data3_j1j0 = readRDS("baar_data3_jump1jigg0.RData")
data3_j25j75 = readRDS("baar_data3_jump25jigg75.RData")
data3_j50j50 = readRDS("baar_data3_jump50jigg50.RData")
data3_j75j25 = readRDS("baar_data3_jump75jiggle25.RData")
data3_j0j1 = readRDS("baar_data3_jump0jigg1.RData")

data9_j1j0 = readRDS("baar_data9_jump1jigg0.RData")
data9_j25j75 = readRDS("baar_data9_jump25jigg75.RData")
data9_j50j50 = readRDS("baar_data9_jump50jigg50.RData")
data9_j75j25 = readRDS("baar_data9_jump75jiggle25.RData")
data9_j0j1 = readRDS("baar_data9_jump0jigg1.RData")


location_of_breakpoints <- function(data) {
  one = data$Breakpoints[[1]][!is.na(data$Breakpoints[[1]])]
  points = one
  for(i in 2:30) {
    if(length(data$Breakpoints[[i]]) == 2500) {
      first =  data$Breakpoints[[i]][!is.na(data$Breakpoints[[i]])]
      points = c(points, first )#, second)
    } else if (length(data$Breakpoints[[i]]) ==  2) {
      first =  data$Breakpoints[[i]]$X1[!is.na(data$Breakpoints[[i]]$X1)]
      second = data$Breakpoints[[i]]$X2[!is.na(data$Breakpoints[[i]]$X2)]
      points = c(points, first, second)
    } else if (length(data$Breakpoints[[i]]) ==  3) {
      first =  data$Breakpoints[[i]]$X1[!is.na(data$Breakpoints[[i]]$X1)]
      second = data$Breakpoints[[i]]$X2[!is.na(data$Breakpoints[[i]]$X2)]
      third = data$Breakpoints[[i]]$X3[!is.na(data$Breakpoints[[i]]$X3)]
      points = c(points, first, second, third)
    } else if (length(data$Breakpoints[[i]]) ==  4) {
      first =  data$Breakpoints[[i]]$X1[!is.na(data$Breakpoints[[i]]$X1)]
      second = data$Breakpoints[[i]]$X2[!is.na(data$Breakpoints[[i]]$X2)]
      third = data$Breakpoints[[i]]$X3[!is.na(data$Breakpoints[[i]]$X3)]
      forth = data$Breakpoints[[i]]$X4[!is.na(data$Breakpoints[[i]]$X4)]
      points = c(points, first, second, third, forth)
    } else if (length(data$Breakpoints[[i]]) ==  5) {
      first =  data$Breakpoints[[i]]$X1[!is.na(data$Breakpoints[[i]]$X1)]
      second = data$Breakpoints[[i]]$X2[!is.na(data$Breakpoints[[i]]$X2)]
      third = data$Breakpoints[[i]]$X3[!is.na(data$Breakpoints[[i]]$X3)]
      forth = data$Breakpoints[[i]]$X4[!is.na(data$Breakpoints[[i]]$X4)]
      fifth = data$Breakpoints[[i]]$X5[!is.na(data$Breakpoints[[i]]$X5)]
      points = c(points, first, second, third, forth, fifth)
    } else if (length(data$Breakpoints[[i]]) ==  6) {
      first =  data$Breakpoints[[i]]$X1[!is.na(data$Breakpoints[[i]]$X1)]
      second = data$Breakpoints[[i]]$X2[!is.na(data$Breakpoints[[i]]$X2)]
      third = data$Breakpoints[[i]]$X3[!is.na(data$Breakpoints[[i]]$X3)]
      forth = data$Breakpoints[[i]]$X4[!is.na(data$Breakpoints[[i]]$X4)]
      fifth = data$Breakpoints[[i]]$X5[!is.na(data$Breakpoints[[i]]$X5)]
      six = data$Breakpoints[[i]]$X6[!is.na(data$Breakpoints[[i]]$X6)]
      points = c(points, first, second, third, forth, fifth, six)
    } else if (length(data$Breakpoints[[i]]) ==  7) {
      first =  data$Breakpoints[[i]]$X1
      second = data$Breakpoints[[i]]$X2[!is.na(data$Breakpoints[[i]]$X2)]
      third = data$Breakpoints[[i]]$X3[!is.na(data$Breakpoints[[i]]$X3)]
      forth = data$Breakpoints[[i]]$X4[!is.na(data$Breakpoints[[i]]$X4)]
      fifth = data$Breakpoints[[i]]$X5[!is.na(data$Breakpoints[[i]]$X5)]
      six = data$Breakpoints[[i]]$X6[!is.na(data$Breakpoints[[i]]$X6)]
      six = data$Breakpoints[[i]]$X7[!is.na(data$Breakpoints[[i]]$X7)]
      points = c(points, first, second, third, forth, fifth, six, seven)
    }
  }
  return(points)
}

#data 1
x=0
y=80000
par(mfrow=c(2,3),oma=c(0,0,2,0))
hist(location_of_breakpoints(data1_j1j0), main="Jump 1 Jiggle 0", xlab="Location of Breakpoints", xlim=c(0,90), ylim=c(x,y), breaks=90)
hist(location_of_breakpoints(data1_j75j25), main="Jump 75 Jiggle 25", xlab="Location of Breakpoints", xlim=c(0,90), ylim=c(x,y), breaks=90)
hist(location_of_breakpoints(data1_j50j50), main="Jump 50 Jiggle 50", xlab="Location of Breakpoints", xlim=c(0,90), ylim=c(x,y), breaks=90)
hist(location_of_breakpoints(data1_j25j75), main="Jump 25 Jiggle 75", xlab="Location of Breakpoints", xlim=c(0,90), ylim=c(x,y), breaks=90)
hist(location_of_breakpoints(data1_j0j1), main="Jump 0 Jiggle 1", xlab="Location of Breakpoints", xlim=c(0,90), ylim=c(x,y), breaks=90)
title("Data 1", outer = TRUE)
par(mfrow=c(1,1))

#data 3
x=0
y=60000
par(mfrow=c(2,3),oma=c(0,0,2,0))
hist(location_of_breakpoints(data3_j1j0), main="Jump 1 Jiggle 0", xlab="Location of Breakpoints", xlim=c(0,90), ylim=c(x,y), breaks=90)
hist(location_of_breakpoints(data3_j75j25), main="Jump 75 Jiggle 25", xlab="Location of Breakpoints", xlim=c(0,90), ylim=c(x,y), breaks=90)
hist(location_of_breakpoints(data3_j50j50), main="Jump 50 Jiggle 50", xlab="Location of Breakpoints", xlim=c(0,90), ylim=c(x,y), breaks=90)
hist(location_of_breakpoints(data3_j25j75), main="Jump 25 Jiggle 75", xlab="Location of Breakpoints", xlim=c(0,90), ylim=c(x,y), breaks=90)
hist(location_of_breakpoints(data3_j0j1), main="Jump 0 Jiggle 1", xlab="Location of Breakpoints", xlim=c(0,90), ylim=c(x,y), breaks=90)
title("Data 3", outer = TRUE)
par(mfrow=c(1,1))



#----------------------------trying to get a good graph------------
#data 3
x=0
y=3000
par(mfrow=c(2,1),oma=c(0,0,2,0))
hist(location_of_breakpoints(data3_j1j0), main="Jump 1 Jiggle 0", xlab="Location of Breakpoints", xlim=c(0,90), ylim=c(x,y), breaks=90)
hist(location_of_breakpoints(data3_j25j75), main="Jump 25 Jiggle 75", xlab="Location of Breakpoints", xlim=c(0,90), ylim=c(x,y), breaks=90)
title("Data 3", outer = TRUE)
par(mfrow=c(1,1))

par(mfrow=c(1,2),oma=c(0,0,2,0))
hist(c(data3_j1j0$Breakpoints[[1]]$X1[!is.na(data3_j1j0$Breakpoints[[1]]$X1)], data3_j1j0$Breakpoints[[1]]$X2[!is.na(data3_j1j0$Breakpoints[[1]]$X2)]) , main="Jump 100% Jiggle 0%", xlab="Location of Breakpoints", xlim=c(0,90), ylim=c(x,y), breaks=90, col="green")
hist(c(data3_j25j75$Breakpoints[[5]]$X1[!is.na(data3_j25j75$Breakpoints[[5]]$X1)], data3_j25j75$Breakpoints[[5]]$X2[!is.na(data3_j25j75$Breakpoints[[5]]$X2)], data3_j25j75$Breakpoints[[5]]$X3[!is.na(data3_j25j75$Breakpoints[[5]]$X3)]), main="Jump 25% Jiggle 75%", xlab="Location of Breakpoints", xlim=c(0,90), ylim=c(x,y), breaks=90, col="green")
title("Move Simulations", outer = TRUE)
par(mfrow=c(1,1))

#, data3_j25j75$Breakpoints[[1]]$X3[!is.na(data3_j25j75$Breakpoints[[1]]$X3)], data3_j25j75$Breakpoints[[1]]$X4[!is.na(data3_j25j75$Breakpoints[[1]]$X4)]
#, data3_j1j0$Breakpoints[[3]]$X3[!is.na(data3_j1j0$Breakpoints[[3]]$X3)]

#----------------------------trying to get a good graph------------

#data 9
x=0
y=60000
par(mfrow=c(2,3),oma=c(0,0,2,0))
hist(location_of_breakpoints(data9_j1j0), main="Jump 1 Jiggle 0", xlab="Location of Breakpoints", xlim=c(0,90), ylim=c(x,y), breaks=90)
hist(location_of_breakpoints(data9_j75j25), main="Jump 75 Jiggle 25", xlab="Location of Breakpoints", xlim=c(0,90), ylim=c(x,y), breaks=90)
hist(location_of_breakpoints(data9_j50j50), main="Jump 50 Jiggle 50", xlab="Location of Breakpoints", xlim=c(0,90), ylim=c(x,y), breaks=90)
hist(location_of_breakpoints(data9_j25j75), main="Jump 25 Jiggle 75", xlab="Location of Breakpoints", xlim=c(0,90), ylim=c(x,y), breaks=90)
hist(location_of_breakpoints(data9_j0j1), main="Jump 0 Jiggle 1", xlab="Location of Breakpoints", xlim=c(0,90), ylim=c(x,y), breaks=90)
title("Data 9", outer = TRUE)
par(mfrow=c(1,1))

#data 11
x=0
y=80000
par(mfrow=c(2,3),oma=c(0,0,2,0))
hist(location_of_breakpoints(data11_j1j0), main="Jump 1 Jiggle 0", xlab="Location of Breakpoints", xlim=c(0,90), ylim=c(x,y), breaks=90)
hist(location_of_breakpoints(data11_j75j25), main="Jump 75 Jiggle 25", xlab="Location of Breakpoints", xlim=c(0,90), ylim=c(x,y), breaks=90)
hist(location_of_breakpoints(data11_j50j50), main="Jump 50 Jiggle 50", xlab="Location of Breakpoints", xlim=c(0,90), ylim=c(x,y), breaks=90)
hist(location_of_breakpoints(data11_j25j75), main="Jump 25 Jiggle 75", xlab="Location of Breakpoints", xlim=c(0,90), ylim=c(x,y), breaks=90)
hist(location_of_breakpoints(data11_j0j1), main="Jump 0 Jiggle 1", xlab="Location of Breakpoints", xlim=c(0,90), ylim=c(x,y), breaks=90)
title("Data 11", outer = TRUE)
par(mfrow=c(1,1))

number_of_breakpoints <- function(data) {
  points = 0
  for(i in 1:30){
    hold = data$NumBkpts[[i]]
    points = c(points, hold)
  }
  points = points[-1]
  return(points)
}

#data 1
par(mfrow=c(2,3))
x.x = 0
x.y = 5
y.x = 0
y.y = 70000
hist(number_of_breakpoints(data1_j1j0), main="Data1 Jump 1 Jiggle 0", xlab="number of Breakpoints", xlim=c(x.x, x.y), ylim=c(y.x, y.y))
hist(number_of_breakpoints(data1_j75j25), main="Data1 Jump 75 Jiggle 25", xlab="number of Breakpoints", xlim=c(x.x, x.y), ylim=c(y.x, y.y))
hist(number_of_breakpoints(data1_j50j50), main="Data1 Jump 50 Jiggle 50", xlab="number of Breakpoints", xlim=c(x.x, x.y), ylim=c(y.x, y.y))
hist(number_of_breakpoints(data1_j25j75), main="Data1 Jump 25 Jiggle 75", xlab="number of Breakpoints", xlim=c(x.x, x.y), ylim=c(y.x, y.y))
hist(number_of_breakpoints(data1_j0j1), main="Data1 Jump 0 Jiggle 1", xlab="number of Breakpoints", xlim=c(x.x, x.y), ylim=c(y.x, y.y))
par(mfrow=c(1,1))

#data 3
par(mfrow=c(2,3))
hist(number_of_breakpoints(data3_j1j0), main="Data3 Jump 1 Jiggle 0", xlab="number of Breakpoints", xlim=c(x.x, x.y), ylim=c(y.x, y.y))
hist(number_of_breakpoints(data3_j75j25), main="Data3 Jump 75 Jiggle 25", xlab="number of Breakpoints", xlim=c(x.x, x.y), ylim=c(y.x, y.y))
hist(number_of_breakpoints(data3_j50j50), main="Data3 Jump 50 Jiggle 50", xlab="number of Breakpoints", xlim=c(x.x, x.y), ylim=c(y.x, y.y))
hist(number_of_breakpoints(data3_j25j75), main="Data3 Jump 25 Jiggle 75", xlab="number of Breakpoints", xlim=c(x.x, x.y), ylim=c(y.x, y.y))
hist(number_of_breakpoints(data3_j0j1), main="Data3 Jump 0 Jiggle 1", xlab="number of Breakpoints", xlim=c(x.x, x.y), ylim=c(y.x, y.y))
par(mfrow=c(1,1))

#data 9
par(mfrow=c(2,3))
x.y = 6
hist(number_of_breakpoints(data9_j1j0), main="Data9 Jump 1 Jiggle 0", xlab="number of Breakpoints", xlim=c(x.x, x.y), ylim=c(y.x, y.y))
hist(number_of_breakpoints(data9_j75j25), main="Data9 Jump 75 Jiggle 25", xlab="number of Breakpoints", xlim=c(x.x, x.y), ylim=c(y.x, y.y))
hist(number_of_breakpoints(data9_j50j50), main="Data9 Jump 50 Jiggle 50", xlab="number of Breakpoints", xlim=c(x.x, x.y), ylim=c(y.x, y.y))
hist(number_of_breakpoints(data9_j25j75), main="Data9 Jump 25 Jiggle 75", xlab="number of Breakpoints", xlim=c(x.x, x.y), ylim=c(y.x, y.y))
hist(number_of_breakpoints(data9_j0j1), main="Data9 Jump 0 Jiggle 1", xlab="number of Breakpoints", xlim=c(x.x, x.y), ylim=c(y.x, y.y))
par(mfrow=c(1,1))

#data 11
par(mfrow=c(2,3))
x.y = 5
y.y = 60000
hist(number_of_breakpoints(data11_j1j0), main="Data11 Jump 1 Jiggle 0", xlab="number of Breakpoints", xlim=c(x.x, x.y), ylim=c(y.x, y.y))
hist(number_of_breakpoints(data11_j75j25), main="Data11 Jump 75 Jiggle 25", xlab="number of Breakpoints", xlim=c(x.x, x.y), ylim=c(y.x, y.y))
hist(number_of_breakpoints(data11_j50j50), main="Data11 Jump 50 Jiggle 50", xlab="number of Breakpoints", xlim=c(x.x, x.y), ylim=c(y.x, y.y))
hist(number_of_breakpoints(data11_j25j75), main="Data11 Jump 25 Jiggle 75", xlab="number of Breakpoints", xlim=c(x.x, x.y), ylim=c(y.x, y.y))
hist(number_of_breakpoints(data11_j0j1), main="Data11 Jump 0 Jiggle 1", xlab="number of Breakpoints", xlim=c(x.x, x.y), ylim=c(y.x, y.y))
par(mfrow=c(1,1))





mse_plots <- function(data, title,x,y) {
  par(mfrow=c(2,3), oma=c(0,0,2,0))
  plot(data$MSE[[1]], main = "MSE 1", ylab="MSE", xlab="Time", ylim=c(x,y))
  plot(data$MSE[[5]], main = "MSE 5", ylab="MSE", xlab="Time", ylim=c(x,y))
  plot(data$MSE[[10]], main = "MSE 10", ylab="MSE", xlab="Time", ylim=c(x,y))
  plot(data$MSE[[15]], main = "MSE 15", ylab="MSE", xlab="Time", ylim=c(x,y))
  plot(data$MSE[[20]], main = "MSE 20", ylab="MSE", xlab="Time", ylim=c(x,y))
  plot(data$MSE[[25]], main = "MSE 25", ylab="MSE", xlab="Time", ylim=c(x,y))
  title(title, outer = TRUE)
}

mse_plots(data11_j1j0, "data11 Jump 1 Jiggle 0", 0.5, 3)
mse_plots(data11_j75j25, "data11 Jump 75 Jiggle 25", 0.5, 6)
mse_plots(data11_j50j50, "data11 Jump 50 Jiggle 50", 0.5, 4)
mse_plots(data11_j25j75, "data11 Jump 25 Jiggle 75", 0.5,4)
mse_plots(data11_j0j1, "data11 Jump 0 Jiggle 1",0.5,4)

mse_plots(data1_j1j0, "data1 Jump 1 Jiggle 0", 0.5, 1.5)
mse_plots(data1_j75j25, "data1 Jump 75 Jiggle 25", 0.5, 1.5)
mse_plots(data1_j50j50, "data1 Jump 50 Jiggle 50", 0.5, 1.5)
mse_plots(data1_j25j75, "data1 Jump 25 Jiggle 75", 0.5,1.5)
mse_plots(data1_j0j1, "data1 Jump 0 Jiggle 1",0.5,1.5)

mse_plots(data3_j1j0, "data3 Jump 1 Jiggle 0", 10, 40)
mse_plots(data3_j75j25, "data3 Jump 75 Jiggle 25", 10, 40)
mse_plots(data3_j50j50, "data3 Jump 50 Jiggle 50", 10, 40)
mse_plots(data3_j25j75, "data3 Jump 25 Jiggle 75", 10, 40)
mse_plots(data3_j0j1, "data3 Jump 0 Jiggle 1",10, 40)

mse_plots(data9_j1j0, "data9 Jump 1 Jiggle 0", 5, 20)
mse_plots(data9_j75j25, "data9 Jump 75 Jiggle 25", 5, 20)
mse_plots(data9_j50j50, "data9 Jump 50 Jiggle 50", 5, 20)
mse_plots(data9_j25j75, "data9 Jump 25 Jiggle 75", 5, 20)
mse_plots(data9_j0j1, "data9 Jump 0 Jiggle 1",5, 20)



bic_plots <- function(data, title,x,y) {
  par(mfrow=c(2,3), oma=c(0,0,2,0))
  plot(data$BIC[[1]], main = "BIC 1", ylab="BIC", xlab="Time", ylim=c(x,y))
  plot(data$BIC[[5]], main = "BIC 5", ylab="BIC", xlab="Time", ylim=c(x,y))
  plot(data$BIC[[10]], main = "BIC 10", ylab="BIC", xlab="Time", ylim=c(x,y))
  plot(data$BIC[[15]], main = "BIC 15", ylab="BIC", xlab="Time", ylim=c(x,y))
  plot(data$BIC[[20]], main = "BIC 20", ylab="BIC", xlab="Time", ylim=c(x,y))
  plot(data$BIC[[25]], main = "BIC 25", ylab="BIC", xlab="Time", ylim=c(x,y))
  title(title, outer = TRUE)
}

bic_plots(data11_j1j0, "data11 Jump 1 Jiggle 0", 200, 350)
bic_plots(data11_j75j25, "data11 Jump 75 Jiggle 25", 200, 350)
bic_plots(data11_j50j50, "data11 Jump 50 Jiggle 50", 200, 350)
bic_plots(data11_j25j75, "data11 Jump 25 Jiggle 75", 200, 350)
bic_plots(data11_j0j1, "data11 Jump 0 Jiggle 1", 200, 350)

bic_plots(data1_j1j0, "data1 Jump 1 Jiggle 0", 200, 350)
bic_plots(data1_j75j25, "data1 Jump 75 Jiggle 25", 200, 350)
bic_plots(data1_j50j50, "data1 Jump 50 Jiggle 50", 200, 350)
bic_plots(data1_j25j75, "data1 Jump 25 Jiggle 75", 200, 350)
bic_plots(data1_j0j1, "data1 Jump 0 Jiggle 1", 200, 350)

bic_plots(data3_j1j0, "data3 Jump 1 Jiggle 0", 400, 700)
bic_plots(data3_j75j25, "data3 Jump 75 Jiggle 25", 400, 700)
bic_plots(data3_j50j50, "data3 Jump 50 Jiggle 50", 400, 700)
bic_plots(data3_j25j75, "data3 Jump 25 Jiggle 75", 400, 700)
bic_plots(data3_j0j1, "data3 Jump 0 Jiggle 1",400, 700)

bic_plots(data9_j1j0, "data9 Jump 1 Jiggle 0", 300, 500)
bic_plots(data9_j75j25, "data9 Jump 75 Jiggle 25", 300, 500)
bic_plots(data9_j50j50, "data9 Jump 50 Jiggle 50", 300, 500)
bic_plots(data9_j25j75, "data9 Jump 25 Jiggle 75", 300, 500)
bic_plots(data9_j0j1, "data9 Jump 0 Jiggle 1", 300, 500)




mse_mean <- function(data) {
  points = 0
  for(i in 1:30){
    hold = mean(data$MSE[[i]])
    points = c(points, hold)
  }
  points = points[-1]
  return(points)
}

par(mfrow=c(2,3), oma=c(0,0,2,0))
plot(mse_mean(data11_j1j0), main="Jump 1 Jiggle 0", ylim = c(0,7), ylab = "Average MSE", xlab="Iteration")
plot(mse_mean(data11_j25j75), main="Jump 25 Jiggle 75", ylim = c(0,7), ylab = "Average MSE", xlab="Iteration")
plot(mse_mean(data11_j50j50), main="Jump 50 Jiggle 50", ylim = c(0,7), ylab = "Average MSE", xlab="Iteration")
plot(mse_mean(data11_j75j25), main="Jump 75 Jiggle 25", ylim = c(0,7), ylab = "Average MSE", xlab="Iteration")
plot(mse_mean(data11_j0j1), main="Jump 0 Jiggle 1", ylim = c(0,7), ylab = "Average MSE", xlab="Iteration")
title("Data 11", outer = TRUE)
par(mfrow=c(1,1))

par(mfrow=c(2,3), oma=c(0,0,2,0))
plot(mse_mean(data1_j1j0), main="Jump 1 Jiggle 0", ylim = c(0,3), ylab = "Average MSE", xlab="Iteration")
plot(mse_mean(data1_j25j75), main="Jump 25 Jiggle 75", ylim = c(0,3), ylab = "Average MSE", xlab="Iteration")
plot(mse_mean(data1_j50j50), main="Jump 50 Jiggle 50", ylim = c(0,3), ylab = "Average MSE", xlab="Iteration")
plot(mse_mean(data1_j75j25), main="Jump 75 Jiggle 25", ylim = c(0,3), ylab = "Average MSE", xlab="Iteration")
plot(mse_mean(data1_j0j1), main="Jump 0 Jiggle 1", ylim = c(0,3), ylab = "Average MSE", xlab="Iteration")
title("Data 1", outer = TRUE)
par(mfrow=c(1,1))

par(mfrow=c(2,3), oma=c(0,0,2,0))
plot(mse_mean(data3_j1j0), main="Jump 1 Jiggle 0", ylim = c(0,40), ylab = "Average MSE", xlab="Iteration")
plot(mse_mean(data3_j25j75), main="Jump 25 Jiggle 75", ylim = c(0,40), ylab = "Average MSE", xlab="Iteration")
plot(mse_mean(data3_j50j50), main="Jump 50 Jiggle 50", ylim = c(0,40), ylab = "Average MSE", xlab="Iteration")
plot(mse_mean(data3_j75j25), main="Jump 75 Jiggle 25", ylim = c(0,40), ylab = "Average MSE", xlab="Iteration")
plot(mse_mean(data3_j0j1), main="Jump 0 Jiggle 1", ylim = c(0,40), ylab = "Average MSE", xlab="Iteration")
title("Data 3", outer = TRUE)
par(mfrow=c(1,1))

par(mfrow=c(2,3), oma=c(0,0,2,0))
plot(mse_mean(data9_j1j0), main="Jump 1 Jiggle 0", ylim = c(0,30), ylab = "Average MSE", xlab="Iteration")
plot(mse_mean(data9_j25j75), main="Jump 25 Jiggle 75", ylim = c(0,30), ylab = "Average MSE", xlab="Iteration")
plot(mse_mean(data9_j50j50), main="Jump 50 Jiggle 50", ylim = c(0,30), ylab = "Average MSE", xlab="Iteration")
plot(mse_mean(data9_j75j25), main="Jump 75 Jiggle 25", ylim = c(0,30), ylab = "Average MSE", xlab="Iteration")
plot(mse_mean(data9_j0j1), main="Jump 0 Jiggle 1", ylim = c(0,30), ylab = "Average MSE", xlab="Iteration")
title("Data 9", outer = TRUE)
par(mfrow=c(1,1))


bic_mean <- function(data) {
  points = 0
  for(i in 1:30){
    hold = mean(data$BIC[[i]])
    points = c(points, hold)
  }
  points = points[-1]
  return(points)
}

par(mfrow=c(2,3), oma=c(0,0,2,0))
plot(bic_mean(data11_j1j0), main="Jump 1 Jiggle 0", ylim = c(0,500), ylab = "Average BIC", xlab="Iteration")
plot(bic_mean(data11_j25j75), main="Jump 25 Jiggle 75", ylim = c(0,500), ylab = "Average BIC", xlab="Iteration")
plot(bic_mean(data11_j50j50), main="Jump 50 Jiggle 50", ylim = c(0,500), ylab = "Average BIC", xlab="Iteration")
plot(bic_mean(data11_j75j25), main="Jump 75 Jiggle 25", ylim = c(0,500), ylab = "Average BIC", xlab="Iteration")
plot(bic_mean(data11_j0j1), main="Jump 0 Jiggle 1", ylim = c(0,500), ylab = "Average BIC", xlab="Iteration")
title("Data 11", outer = TRUE)
par(mfrow=c(1,1))

par(mfrow=c(2,3), oma=c(0,0,2,0))
plot(bic_mean(data1_j1j0), main="Jump 1 Jiggle 0", ylim = c(0,500), ylab = "Average BIC", xlab="Iteration")
plot(bic_mean(data1_j25j75), main="Jump 25 Jiggle 75", ylim = c(0,500), ylab = "Average BIC", xlab="Iteration")
plot(bic_mean(data1_j50j50), main="Jump 50 Jiggle 50", ylim = c(0,500), ylab = "Average BIC", xlab="Iteration")
plot(bic_mean(data1_j75j25), main="Jump 75 Jiggle 25", ylim = c(0,500), ylab = "Average BIC", xlab="Iteration")
plot(bic_mean(data1_j0j1), main="Jump 0 Jiggle 1", ylim = c(0,500), ylab = "Average BIC", xlab="Iteration")
title("Data 1", outer = TRUE)
par(mfrow=c(1,1))

par(mfrow=c(2,3), oma=c(0,0,2,0))
plot(bic_mean(data3_j1j0), main="Jump 1 Jiggle 0", ylim = c(0,700), ylab = "Average BIC", xlab="Iteration")
plot(bic_mean(data3_j25j75), main="Jump 25 Jiggle 75", ylim = c(0,700), ylab = "Average BIC", xlab="Iteration")
plot(bic_mean(data3_j50j50), main="Jump 50 Jiggle 50", ylim = c(0,700), ylab = "Average BIC", xlab="Iteration")
plot(bic_mean(data3_j75j25), main="Jump 75 Jiggle 25", ylim = c(0,700), ylab = "Average BIC", xlab="Iteration")
plot(bic_mean(data3_j0j1), main="Jump 0 Jiggle 1", ylim = c(0,700), ylab = "Average BIC", xlab="Iteration")
title("Data 3", outer = TRUE)
par(mfrow=c(1,1))

par(mfrow=c(2,3), oma=c(0,0,2,0))
plot(bic_mean(data9_j1j0), main="Jump 1 Jiggle 0", ylim = c(0,600), ylab = "Average BIC", xlab="Iteration")
plot(bic_mean(data9_j25j75), main="Jump 25 Jiggle 75", ylim = c(0,600), ylab = "Average BIC", xlab="Iteration")
plot(bic_mean(data9_j50j50), main="Jump 50 Jiggle 50", ylim = c(0,600), ylab = "Average BIC", xlab="Iteration")
plot(bic_mean(data9_j75j25), main="Jump 75 Jiggle 25", ylim = c(0,600), ylab = "Average BIC", xlab="Iteration")
plot(bic_mean(data9_j0j1), main="Jump 0 Jiggle 1", ylim = c(0,600), ylab = "Average BIC", xlab="Iteration")
title("Data 9", outer = TRUE)
par(mfrow=c(1,1))





#-------------------------------------run time -----------------------------$
