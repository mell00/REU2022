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
par(mfrow=c(2,3))
hist(location_of_breakpoints(data1_j1j0), main="Data1 Jump 1 Jiggle 0", xlab="Location of Breakpoints")
hist(location_of_breakpoints(data1_j75j25), main="Data1 Jump 75 Jiggle 25", xlab="Location of Breakpoints")
hist(location_of_breakpoints(data1_j50j50), main="Data1 Jump 50 Jiggle 50", xlab="Location of Breakpoints")
hist(location_of_breakpoints(data1_j25j75), main="Data1 Jump 25 Jiggle 75", xlab="Location of Breakpoints")
hist(location_of_breakpoints(data1_j0j1), main="Data1 Jump 0 Jiggle 1", xlab="Location of Breakpoints")
par(mfrow=c(1,1))

#data 3
par(mfrow=c(2,3))
hist(location_of_breakpoints(data3_j1j0), main="Data3 Jump 1 Jiggle 0", xlab="Location of Breakpoints")
hist(location_of_breakpoints(data3_j75j25), main="Data3 Jump 75 Jiggle 25", xlab="Location of Breakpoints")
hist(location_of_breakpoints(data3_j50j50), main="Data3 Jump 50 Jiggle 50", xlab="Location of Breakpoints")
hist(location_of_breakpoints(data3_j25j75), main="Data3 Jump 25 Jiggle 75", xlab="Location of Breakpoints")
hist(location_of_breakpoints(data3_j0j1), main="Data3 Jump 0 Jiggle 1", xlab="Location of Breakpoints")
par(mfrow=c(1,1))

#data 9
par(mfrow=c(2,3))
hist(location_of_breakpoints(data9_j1j0), main="Data9 Jump 1 Jiggle 0", xlab="Location of Breakpoints")
hist(location_of_breakpoints(data9_j75j25), main="Data9 Jump 75 Jiggle 25", xlab="Location of Breakpoints")
hist(location_of_breakpoints(data9_j50j50), main="Data9 Jump 50 Jiggle 50", xlab="Location of Breakpoints")
hist(location_of_breakpoints(data9_j25j75), main="Data9 Jump 25 Jiggle 75", xlab="Location of Breakpoints")
hist(location_of_breakpoints(data9_j0j1), main="Data9 Jump 0 Jiggle 1", xlab="Location of Breakpoints")
par(mfrow=c(1,1))

#data 11
par(mfrow=c(2,3))
hist(location_of_breakpoints(data11_j1j0), main="Data11 Jump 1 Jiggle 0", xlab="Location of Breakpoints")
hist(location_of_breakpoints(data11_j75j25), main="Data11 Jump 75 Jiggle 25", xlab="Location of Breakpoints")
hist(location_of_breakpoints(data11_j50j50), main="Data11 Jump 50 Jiggle 50", xlab="Location of Breakpoints")
hist(location_of_breakpoints(data11_j25j75), main="Data11 Jump 25 Jiggle 75", xlab="Location of Breakpoints")
hist(location_of_breakpoints(data11_j0j1), main="Data11 Jump 0 Jiggle 1", xlab="Location of Breakpoints")
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
hist(number_of_breakpoints(data11_j1j0), main="Data11 Jump 1 Jiggle 0", xlab="number of Breakpoints", xlim=c(x.x, x.y), ylim=c(y.x, y.y))
hist(number_of_breakpoints(data11_j75j25), main="Data11 Jump 75 Jiggle 25", xlab="number of Breakpoints", xlim=c(x.x, x.y), ylim=c(y.x, y.y))
hist(number_of_breakpoints(data11_j50j50), main="Data11 Jump 50 Jiggle 50", xlab="number of Breakpoints", xlim=c(x.x, x.y), ylim=c(y.x, y.y))
hist(number_of_breakpoints(data11_j25j75), main="Data11 Jump 25 Jiggle 75", xlab="number of Breakpoints", xlim=c(x.x, x.y), ylim=c(y.x, y.y))
hist(number_of_breakpoints(data11_j0j1), main="Data11 Jump 0 Jiggle 1", xlab="number of Breakpoints", xlim=c(x.x, x.y), ylim=c(y.x, y.y))
par(mfrow=c(1,1))