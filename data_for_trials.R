#creating data 
library("strucchange")


#----------------ZERO---------------- (no breaks)
data_0 = rnorm(90, mean = 10, sd = 1)
time = 1:90
test_data_0 = data.frame(time, data_0)
bkpts_0 = breakpoints(test_data_0$data_0 ~ test_data_0$time, breaks = 5, h = 0.1) 

#----------------ONE---------------- (clean sd 1 break)
first = rnorm(45, mean = 10, sd = 1) 
second = rnorm(45, mean = 20, sd = 1) 
data_1 = c(first, second) 
time = 1:90
test_data_1 = data.frame(time, dif_means_1) 
bkpts_1 = breakpoints(test_data_1$data_1 ~ test_data_1$time, breaks = 5, h = 0.1) 

#----------------TWO---------------- (clean sd 2 breaks)
first = rnorm(30, mean = 5, sd = 1) 
second = rnorm(30, mean = 15, sd = 1) 
third = rnorm(30, mean = 30, sd = 1) 
data_2 = c(first, second, third) 
time = 1:90
test_data_2 = data.frame(time, data_2) 
bkpts_2 = breakpoints(test_data_0$data_2 ~ test_data_2$time, breaks = 5, h = 0.1) 

#----------------THREE---------------- (messy sd 1 break)
first = rnorm(45, mean = 10, sd = 5) 
second = rnorm(45, mean = 20, sd = 5) 
data_3 = c(first, second) 
time = 1:90
test_data_3 = data.frame(time, data_3) 
bkpts_3 = breakpoints(test_data_0$data_3 ~ test_data_3$time, breaks = 5, h = 0.1) 

#----------------FOUR---------------- (messy sd 2 breaks)
first = rnorm(30, mean = 5, sd = 6) 
second = rnorm(30, mean = 20, sd = 6) 
third = rnorm(30, mean = 35, sd = 6) 
data_4 = c(first, second, third) 
time = 1:90
test_data_4 = data.frame(time, data_4) 
bkpts_4 = breakpoints(test_data_4$data_4 ~ test_data_4$time, breaks = 5, h = 0.1)

#----------------FIVE---------------- (slopes one and 4 )
first = c(1:45)
second = c(seq(46, 224, by = 4))
data_5 = c(first, second)
test_data_5 = data.frame(time, data_5)
bkpts_5 = breakpoints(test_data_5$data_5 ~ test_data_5$time, breaks = 5, h = 0.1)

#----------------SIX---------------- (slopes, 1 ,4 ,and 8)
first = c(1:30)
second = c(seq(31,148, by = 4))
third = c(seq(149, 382, by = 8))
data_6 = c(first, second, third)
test_data_6 = data.frame(time, data_6)
bkpts_6 = breakpoints(test_data_6$data_6 ~ test_data_6$time, breaks = 5, h = 0.1)
#----------------SEVEN----------------
first = c(seq(1,23, by = .5))
second = c(24:68)
data_7 = c(first, second)
test_data_7 = data.frame(time, data_7)
bkpts_7 = breakpoints(test_data_7$data_7 ~ test_data_7$time, breaks = 5, h = 0.1)

#----------------EIGHT----------------







