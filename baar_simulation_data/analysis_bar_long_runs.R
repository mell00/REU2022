#data analysis for BAAR
j1j0 = readRDS("baarlr_data3_jump1jigg0.RData")
j25j75 = readRDS("baarlr_data3_jump25jigg75.RData")
j50j50 = readRDS("baarlr_data3_jump50jigg50.RData")
j75j25 = readRDS("baarlr_data3_jump75jiggle25.RData")
j0j1 = readRDS("baarlr_data3_jump0jigg1.RData")




location_of_breakpoints <- function(data) {
  one = data$Breakpoints[[1]][!is.na(data$Breakpoints[[1]])]
  points = one
  for(i in 2:3) {
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


#data 3
x=0
y=300000
par(mfrow=c(2,3),oma=c(0,0,2,0))
hist(location_of_breakpoints(j1j0), main="Jump 1 Jiggle 0", xlab="Location of Breakpoints", xlim=c(0,90), ylim=c(x,y), breaks=90)
hist(location_of_breakpoints(j75j25), main="Jump 75 Jiggle 25", xlab="Location of Breakpoints", xlim=c(0,90), ylim=c(x,y), breaks=90)
hist(location_of_breakpoints(j50j50), main="Jump 50 Jiggle 50", xlab="Location of Breakpoints", xlim=c(0,90), ylim=c(x,y), breaks=90)
hist(location_of_breakpoints(j25j75), main="Jump 25 Jiggle 75", xlab="Location of Breakpoints", xlim=c(0,90), ylim=c(x,y), breaks=90)
hist(location_of_breakpoints(j0j1), main="Jump 0 Jiggle 1", xlab="Location of Breakpoints", xlim=c(0,90), ylim=c(x,y), breaks=90)
title("Data 3 Long Runs", outer = TRUE)
par(mfrow=c(1,1))


number_of_breakpoints <- function(data) {
  points = 0
  for(i in 1:3){
    hold = data$NumBkpts[[i]]
    points = c(points, hold)
  }
  points = points[-1]
  return(points)
}

par(mfrow=c(2,3),oma=c(0,0,2,0))
x.x = 0
x.y = 5
y.x = 0
y.y = 300000
hist(number_of_breakpoints(j1j0), main="Jump 1 Jiggle 0", xlab="number of Breakpoints", xlim=c(x.x, x.y), ylim=c(y.x, y.y))
hist(number_of_breakpoints(j75j25), main="Jump 75 Jiggle 25", xlab="number of Breakpoints", xlim=c(x.x, x.y), ylim=c(y.x, y.y))
hist(number_of_breakpoints(j50j50), main="Jump 50 Jiggle 50", xlab="number of Breakpoints", xlim=c(x.x, x.y), ylim=c(y.x, y.y))
hist(number_of_breakpoints(j25j75), main="Jump 25 Jiggle 75", xlab="number of Breakpoints", xlim=c(x.x, x.y), ylim=c(y.x, y.y))
hist(number_of_breakpoints(j0j1), main="Jump 0 Jiggle 1", xlab="number of Breakpoints", xlim=c(x.x, x.y), ylim=c(y.x, y.y))
title("Data 3 Long Runs", outer = TRUE)
par(mfrow=c(1,1))


mse_plots <- function(data, title,x,y) {
  par(mfrow=c(3,1), oma=c(0,0,2,0))
  plot(data$MSE[[1]], main = "MSE 1", ylab="MSE", xlab="Time", ylim=c(x,y))
  plot(data$MSE[[2]], main = "MSE 2", ylab="MSE", xlab="Time", ylim=c(x,y))
  plot(data$MSE[[3]], main = "MSE 3", ylab="MSE", xlab="Time", ylim=c(x,y))
  title(title, outer = TRUE)
}

mse_plots(j1j0, "Long Runs: Jump 1 Jiggle 0", 15, 40)
mse_plots(j75j25, "Long Runs: Jump 75 Jiggle 25", 15, 40)
mse_plots(j50j50, "Long Runs: Jump 50 Jiggle 50", 15, 40)
mse_plots(j25j75, "Long Runs: Jump 25 Jiggle 75", 15, 40)
mse_plots(j0j1, "Long Runs: Jump 0 Jiggle 1", 15, 40)



bic_plots <- function(data, title,x,y) {
  par(mfrow=c(3,1), oma=c(0,0,2,0))
  plot(data$BIC[[1]][1:200], main = "BIC 1", ylab="BIC", xlab="Time", ylim=c(x,y))
  plot(data$BIC[[2]][1:200], main = "BIC 2", ylab="BIC", xlab="Time", ylim=c(x,y))
  plot(data$BIC[[3]][1:200], main = "BIC 3", ylab="BIC", xlab="Time", ylim=c(x,y))
  title(title, outer = TRUE)
}

bic_plots(j1j0, "Long Runs: Jump 1 Jiggle 0", 500, 700)
bic_plots(j75j25, "Long Runs: Jump 75 Jiggle 25", 500, 700)
bic_plots(j50j50, "Long Runs: Jump 50 Jiggle 50", 500, 700)
bic_plots(j25j75, "Long Runs: Jump 25 Jiggle 75", 500, 700)
bic_plots(j0j1, "Long Runs: Jump 0 Jiggle 1", 500, 700)

mse_mean <- function(data) {
  points = 0
  for(i in 1:3){
    hold = mean(data$MSE[[i]])
    points = c(points, hold)
  }
  points = points[-1]
  return(points)
}



mse_sd <- function(data) {
  sd_points = 0 
  for(i in 1:3){
    sd_hold = sd(data$MSE[[i]])
    sd_points = c(sd_points, sd_hold)
  }
  sd_points = sd_points[-1]
  return(sd_points)
}

par(mfrow=c(2,3), oma=c(0,0,2,0))
plot(mse_mean(j1j0), main="Jump 1 Jiggle 0", ylim = c(15,40), ylab = "Average MSE", xlab="Iteration",cex = 1.5)
points(c(1:3), mse_mean(j1j0) + (2*mse_sd(j1j0)), cex = 1, pch=16)
points(c(1:3), mse_mean(j1j0) - (2*mse_sd(j1j0)), cex = 1, pch=16)
plot(mse_mean(j25j75), main="Jump 25 Jiggle 75", ylim = c(15,40), ylab = "Average MSE", xlab="Iteration",cex = 1.5)
points(c(1:3), mse_mean(j25j75) + (2*mse_sd(j25j75)), cex = 1, pch=16)
points(c(1:3), mse_mean(j25j75) - (2*mse_sd(j25j75)), cex = 1, pch=16)
plot(mse_mean(j50j50), main="Jump 50 Jiggle 50", ylim = c(15,40), ylab = "Average MSE", xlab="Iteration",cex = 1.5)
points(c(1:3), mse_mean(j50j50) + (2*mse_sd(j50j50)), cex = 1, pch=16)
points(c(1:3), mse_mean(j50j50) - (2*mse_sd(j50j50)), cex = 1, pch=16)
plot(mse_mean(j75j25), main="Jump 75 Jiggle 25", ylim = c(15,40), ylab = "Average MSE", xlab="Iteration",cex = 1.5)
points(c(1:3), mse_mean(j75j25) + (2*mse_sd(j75j25)), cex = 1, pch=16)
points(c(1:3), mse_mean(j75j25) - (2*mse_sd(j75j25)), cex = 1, pch=16)
plot(mse_mean(j0j1), main="Jump 0 Jiggle 1", ylim = c(15,40), ylab = "Average MSE", xlab="Iteration",cex = 1.5)
points(c(1:3), mse_mean(j0j1) + (2*mse_sd(j0j1)), cex = 1, pch=16)
points(c(1:3), mse_mean(j0j1) - (2*mse_sd(j0j1)), cex = 1, pch=16)
title("Data 3", outer = TRUE)
par(mfrow=c(1,1))


m_1 = c(mse_mean(j1j0)[1], mse_mean(j25j75)[1], mse_mean(j50j50)[1], mse_mean(j75j25)[1], mse_mean(j0j1)[1])
m_2 = c(mse_mean(j1j0)[2], mse_mean(j25j75)[2], mse_mean(j50j50)[2], mse_mean(j75j25)[2], mse_mean(j0j1)[2])
m_3 = c(mse_mean(j1j0)[3], mse_mean(j25j75)[3], mse_mean(j50j50)[3], mse_mean(j75j25)[3], mse_mean(j0j1)[3])

sd_1_up = c(mse_mean(j1j0)[1] + (2*mse_sd(j1j0)[1]), mse_mean(j25j75)[1] + (2*mse_sd(j25j75)[1]), mse_mean(j50j50)[1] + (2*mse_sd(j50j50)[1]), mse_mean(j75j25)[1] + (2*mse_sd(j75j25)[1]), mse_mean(j0j1)[1] + (2*mse_sd(j0j1)[1]) )
sd_2_up = c(mse_mean(j1j0)[2] + (2*mse_sd(j1j0)[2]), mse_mean(j25j75)[2] + (2*mse_sd(j25j75)[2]), mse_mean(j50j50)[2] + (2*mse_sd(j50j50)[2]), mse_mean(j75j25)[2] + (2*mse_sd(j75j25)[2]), mse_mean(j0j1)[2] + (2*mse_sd(j0j1)[2]) )
sd_3_up = c(mse_mean(j1j0)[3] + (2*mse_sd(j1j0)[3]), mse_mean(j25j75)[3] + (2*mse_sd(j25j75)[3]), mse_mean(j50j50)[3] + (2*mse_sd(j50j50)[3]), mse_mean(j75j25)[3] + (2*mse_sd(j75j25)[3]), mse_mean(j0j1)[3] + (2*mse_sd(j0j1)[3]) )

sd_1_down = c(mse_mean(j1j0)[1] - (2*mse_sd(j1j0)[1]), mse_mean(j25j75)[1] - (2*mse_sd(j25j75)[1]), mse_mean(j50j50)[1] - (2*mse_sd(j50j50)[1]), mse_mean(j75j25)[1] - (2*mse_sd(j75j25)[1]), mse_mean(j0j1)[1] - (2*mse_sd(j0j1)[1]) )
sd_2_down = c(mse_mean(j1j0)[2] - (2*mse_sd(j1j0)[2]), mse_mean(j25j75)[2] - (2*mse_sd(j25j75)[2]), mse_mean(j50j50)[2] - (2*mse_sd(j50j50)[2]), mse_mean(j75j25)[2] - (2*mse_sd(j75j25)[2]), mse_mean(j0j1)[2] - (2*mse_sd(j0j1)[2]) )
sd_3_down = c(mse_mean(j1j0)[3] - (2*mse_sd(j1j0)[3]), mse_mean(j25j75)[3] - (2*mse_sd(j25j75)[3]), mse_mean(j50j50)[3] - (2*mse_sd(j50j50)[3]), mse_mean(j75j25)[3] - (2*mse_sd(j75j25)[3]), mse_mean(j0j1)[3] - (2*mse_sd(j0j1)[3]) )


par(mfrow=c(1,3), oma=c(0,0,2,0))
plot(m_1, main="Average MSE first Iteration", ylim = c(15,40), ylab = "Average MSE", xlab="Jump Jiggle Combination",cex = 1.5)
points(c(1:5), sd_1_up, cex = 1, pch=16)
points(c(1:5), sd_1_down, cex = 1, pch=16)
plot(m_2, main="Average MSE Second Iteration", ylim = c(15,40), ylab = "Average MSE", xlab="Jump Jiggle Combination",cex = 1.5)
points(c(1:5), sd_2_up, cex = 1, pch=16)
points(c(1:5), sd_2_down, cex = 1, pch=16)
plot(m_3, main="Average MSE Third Iteration", ylim = c(15,40), ylab = "Average MSE", xlab="Jump Jiggle Combination",cex = 1.5)
points(c(1:5), sd_3_up, cex = 1, pch=16)
points(c(1:5), sd_3_down, cex = 1, pch=16)
title("Long Runs", outer = TRUE)
par(mfrow=c(1,1))

mse_mean_total <- function(data) {
  points = 0
  for(i in 1:3){
    hold = (data$MSE[[i]])
    points = c(points, hold)
  }
  points = points[-1]
  m = mean(points)
  return(m)
}
mse_sd_total <- function(data) {
  sd_points = 0 
  for(i in 1:3){
    sd_hold = (data$MSE[[i]])
    sd_points = c(sd_points, sd_hold)
  }
  sd_points = sd_points[-1]
  s = sd(sd_points)
  return(s)
}

m = c(mse_mean_total(j1j0), mse_mean_total(j25j75), mse_mean_total(j50j50), mse_mean_total(j75j25), mse_mean_total(j0j1))
sd_up = c(mse_mean_total(j1j0) + (2*mse_sd_total(j1j0)), mse_mean_total(j25j75) + (2*mse_sd_total(j25j75)), mse_mean_total(j50j50) + (2*mse_sd_total(j50j50)), mse_mean_total(j75j25) + (2*mse_sd_total(j75j25)), mse_mean_total(j0j1) + (2*mse_sd_total(j0j1)) )
sd_down = c(mse_mean_total(j1j0) - (2*mse_sd_total(j1j0)), mse_mean_total(j25j75) - (2*mse_sd_total(j25j75)), mse_mean_total(j50j50) - (2*mse_sd_total(j50j50)), mse_mean_total(j75j25) - (2*mse_sd_total(j75j25)), mse_mean_total(j0j1) - (2*mse_sd_total(j0j1)) )

plot(m, main="Average MSE Over All Iterations", ylim = c(15,40), ylab = "Average MSE", xlab="Jump Jiggle Combination",cex = 1.5)
points(c(1:5), sd_up, cex = 1, pch=16)
points(c(1:5), sd_down, cex = 1, pch=16)



