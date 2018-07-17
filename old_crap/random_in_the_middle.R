#Places initial knot in the middle of the data set 

#Key
#x_values = x variable for all observations

random_in_the_middle<-function(x_values){
  n = length(x_values) #Number of observations
  middle = ceiling(n/2)
  x_values = 1:n #Turn x values into observations (is this needed for this?)
  my_first_breakpoint = c(x_values[middle])
  
 # print(my_first_breakpoint)
  
  return(my_first_breakpoint)
}

data_set = c(1:103)
random_in_the_middle(data_set)