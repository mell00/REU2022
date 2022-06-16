#creating data 
library("strucchange")
require(graphics)
time = 1:90


#----------------ZERO A---------------- (no breaks)
test_data_0_a = function(){
  data_0_a = rnorm(90, mean = 10, sd = 1)
  test_data_0_a = data.frame(time, data_0_a)
  return(test_data_0_a)
}

#----------------ZERO B---------------- (no breaks)
test_data_0_b = function(){
  data_0_b = rnorm(90, mean = 10, sd = 5)
  test_data_0_b = data.frame(time, data_0_b)
  return(test_data_0_b)
}

#----------------ONE---------------- (clean sd 1 break)
test_data_1 = function(){
  first = rnorm(45, mean = 10, sd = 1) 
  second = rnorm(45, mean = 20, sd = 1) 
  data_1 = c(first, second) 
  test_data_1 = data.frame(time, data_1)
  return(test_data_1)
}

#----------------TWO---------------- (clean sd 2 breaks)
test_data_2 = function(){
  first = rnorm(30, mean = 5, sd = 1) 
  second = rnorm(30, mean = 15, sd = 1) 
  third = rnorm(30, mean = 30, sd = 1) 
  data_2 = c(first, second, third) 
  test_data_2 = data.frame(time, data_2)
  return(test_data_2)
}

#----------------THREE---------------- (messy sd 1 break)
test_data_3 = function(){
  first = rnorm(45, mean = 10, sd = 5) 
  second = rnorm(45, mean = 20, sd = 5) 
  data_3 = c(first, second) 
  test_data_3 = data.frame(time, data_3)
  return(test_data_3)
}

#----------------FOUR---------------- (messy sd 2 breaks)
test_data_4 = function(){
  first = rnorm(30, mean = 5, sd = 6) 
  second = rnorm(30, mean = 20, sd = 6) 
  third = rnorm(30, mean = 35, sd = 6) 
  data_4 = c(first, second, third) 
  test_data_4 = data.frame(time, data_4) 
  return(test_data_4)
}

#----------------FIVE---------------- (slopes one and 4 )
test_data_5 = function(){
  first = c(1:45)
  second = c(seq(46, 224, by = 4))
  first = rnorm(length(first), mean=first, sd=1)
  second = rnorm(length(second), mean=second, sd=1)
  data_5 = c(first, second)
  test_data_5 = data.frame(time, data_5)
  return(test_data_5)
}

#----------------SIX---------------- (slopes, 1 ,4 ,and 8)
test_data_6 = function(){
  first = c(1:30)
  second = c(seq(31,148, by = 4))
  third = c(seq(149, 382, by = 8))
  first = rnorm(length(first), mean=first, sd=1)
  second = rnorm(length(second), mean=second, sd=1)
  third = rnorm(length(third), mean=third, sd=1)
  data_6 = c(first, second, third)
  test_data_6 = data.frame(time, data_6)
  return(test_data_6)
}

#----------------SEVEN----------------(one break with small slopes )
test_data_7 = function(){
  first = c(seq(1,23, by = .5))
  second = c(24:68)
  first = rnorm(length(first), mean=first, sd=1)
  second = rnorm(length(second), mean=second, sd=1)
  data_7 = c(first, second)
  test_data_7 = data.frame(time, data_7)
  return(test_data_7)
}

#----------------EIGHT----------------(two breaks small slopes)
test_data_8 = function(){
  first = c(seq(1,8.25, by = .25))
  second = c(seq(8.5,23, by = .5))
  third = c(seq(24, 46, by = .75))
  first = rnorm(length(first), mean=first, sd=1)
  second = rnorm(length(second), mean=second, sd=1)
  third = rnorm(length(third), mean=third, sd=1)
  data_8 = c(first, second, third)
  test_data_8 = data.frame(time, data_8)
  return(test_data_8)
}

#----------------Nine---------------- (one break, variance change )
test_data_9 = function(){
  first = rnorm(45, mean = 10, sd = 1)
  second = rnorm(45, mean = 10, sd = 5)
  data_9 = c(first, second)
  test_data_9 = data.frame(time, data_9)
  return(test_data_9)
}

#----------------Ten---------------- (Two breaks and high variance change)
test_data_10 = function(){
  first = rnorm(30, mean = 10, sd = 1)
  second = rnorm(30, mean = 10, sd = 5)
  third = rnorm(30, mean = 10, sd = 1)
  data_10 = c(first, second, third)
  test_data_10 = data.frame(time, data_10)
  return(test_data_10)
}


#----------------Eleven---------------- AR Simulation
test_data_11 = function(){
  first = arima.sim(model = list(ar = 0.1, order = c(1,0,0)), n = 45)
  second = arima.sim(model = list(ar = 0.9999, order = c(1,0,0)), n = 45)
  data_11 = c(second, first)
  test_data_11 = data.frame(time,data_11)
  return(test_data_11)
}


#------------------100---------------------(8 breaks with mean changes)
test_data_100 = function(){
  first = rnorm(100, mean = 5, sd = 1) 
  second = rnorm(100, mean = 10, sd = 1) 
  third = rnorm(100, mean = 15, sd = 1) 
  fourth= rnorm(100, mean = 10, sd = 1) 
  fifth = rnorm(100, mean = 5, sd = 1) 
  six = rnorm(100, mean = 10, sd = 1) 
  seven = rnorm(100, mean = 15, sd = 1) 
  eight = rnorm(100, mean = 10, sd = 1) 
  nine = rnorm(100, mean = 5, sd = 1) 
  time = c(1:900)
  data_100 = c(first, second, third, fourth, fifth, six, seven, eight, nine) 
  test_data_100 = data.frame(time, data_100)
  return(test_data_100)
}

#------------------200---------------------(8 breaks with variance changes)
test_data_200 = function(){
  first = rnorm(100, mean = 10, sd = 10) 
  second = rnorm(100, mean = 10, sd = 5) 
  third = rnorm(100, mean = 10, sd = 1) 
  fourth= rnorm(100, mean = 10, sd = 10) 
  fifth = rnorm(100, mean = 10, sd = 5) 
  six = rnorm(100, mean = 10, sd = 1) 
  seven = rnorm(100, mean = 10, sd = 10) 
  eight = rnorm(100, mean = 10, sd = 5) 
  nine = rnorm(100, mean = 10, sd = 1) 
  time = c(1:900)
  data_200 = c(first, second, third, fourth, fifth, six, seven, eight, nine) 
  test_data_200 = data.frame(time, data_200)
  return(test_data_200)
}

#------------------300---------------------(1 breaks with mean and 200 data points)
test_data_300 = function(){
  first = rnorm(100, mean = 10, sd = 5) 
  second = rnorm(100, mean = 20, sd = 5) 
  data_300 = c(first, second) 
  time = c(1:200)
  test_data_300 = data.frame(time, data_300)
  return(test_data_300)
}

#------------------44---------------------(2 breaks with mean changes and 300 data points )
test_data_44 = function(){
  first = rnorm(100, mean = 10, sd = 5) 
  second = rnorm(100, mean = 20, sd = 5) 
  third = rnorm(100, mean = 30, sd = 5) 
  data_44 = c(first, second,third) 
  time = c(1:300)
  test_data_44 = data.frame(time, data_44)
  return(test_data_44)
}




#----------------plots of 10 datasets---------------- 
par(mfrow=c(3,4))
plot(test_data_0_a(), main = "0 Breaks, Low Variance", xlab="Time", ylab="Dependent Variable")
plot(test_data_0_b(), main = "0 Breaks, High Variance", xlab="Time", ylab="Dependent Variable")
plot(test_data_1(), main = "1 Break, Low Variance", xlab="Time", ylab="Dependent Variable")
plot(test_data_2(), main = "2 Breaks, Low Variance", xlab="Time", ylab="Dependent Variable")
plot(test_data_3(), main = "1 Break, High Variance", xlab="Time", ylab="Dependent Variable")
plot(test_data_4(), main = "2 Breaks, High Variance", xlab="Time", ylab="Dependent Variable")
plot(test_data_5(), main = "1 Break, Big Slopes", xlab="Time", ylab="Dependent Variable")
plot(test_data_6(), main = "2 Breaks, Big Slopes", xlab="Time", ylab="Dependent Variable")
plot(test_data_7(), main = "1 Break, Small Slopes", xlab="Time", ylab="Dependent Variable")
plot(test_data_8(), main = "2 Breaks, Small Slopes", xlab="Time", ylab="Dependent Variable")
plot(test_data_9(), main = "1 Breaks, Variance Change", xlab="Time", ylab="Dependent Variable")
plot(test_data_10(), main = "2 Breaks, Variance Change", xlab="Time", ylab="Dependent Variable")
par(mfrow=c(1,1))


#-------------------plots of climate data ------------------
#plot(temp_data[,2]~temp_data[,1], main="Global Tempature Anomaly from 1880-2018", ylab="Anomaly in degree Celsius", xlab="Time (years)")
#lines(c(1880,2020), c(0,0))

#-------------------plots of pelican data-----------------------
#plot(pelican$Count_y,pelican$NumberByPartyHours, ylab="Individuals per Party Hour", xlab = "Years (Since 1900)", main= "Pacific Brown Pelican Population: 1939-2017", pch=20, col="brown")




