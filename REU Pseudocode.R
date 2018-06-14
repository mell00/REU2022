linear
	- 2 normal draws of 30 each w/ differing means
	- 2 normal draws of 30 each w/ differing standard deviations
	- 1 normal draw of 60 w/o any differences
AR
	- AR0 and AR1 with different coeffecients

library(strucchange)
breakpoints() for Bai-Perron breakpoints #/location, needs max breakpoints (w/ 1 or 15% or log)

barMake<-function(current knot set){
OPTION 0 (null model)
random
OPTION 1
scoring (vs. probability) w/ higher = better and lower = worse
proposes 2+ breakpoints
x1-x2 for both proposed breakpoints, minimize
OPTION 1A
add in possibility of worst distance
OPTION 2
only put in largest gap between existing knots
}

barMurder<-function(current knot set){
OPTION 0
random
OPTION 1
weighting scheme
most likely takes one with smallest sum of distance between two knots
OPTION 1A
ratio of sum vs. most extreme
OPTION 1B
minimize distances on either size
OPTION 2
pick 2 randomly, compare scores
}

barMove{
OPTION 1
murder and make
OPTION 2
jiggle
}

bar<-function(# of iterations, initial breakpoint set, probability of birth/death/move){
select type of action - birth/death/move (random based on inputted probabilities)

if(birth selected){
barBirth
}
else if(death selected){
barDeath
}
else{
barDeath
barBirth
}

calculate MLE, save fit/K/tau in distribution
ratio<-function(e^[-1/2sigma_n(SSE_n) + 1/2sigma_o(SSE_o)]
if(ratio > 1){take new}else if(ratio < 1){take new w/ probability
u<-runif(1,0,1)
if(u < prob){take new}else{take old}}

}