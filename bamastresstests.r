
mu = 1
y = rnorm(1,0,1)
alpha = .5
epsilon = rnorm(1,0,1)
epsilon_t = function(t){ # t MUST be greater than or equal to 2
  epsilon_t = c(epsilon)
  for (i in 1:t){
    epsilon_t[i] = rnorm(1,0,1)
  }
  return(epsilon_t[t])
}


for (i in 1:90){
  y[i]= mu + epsilon + alpha*epsilon_t(2) # + .............
  
}

