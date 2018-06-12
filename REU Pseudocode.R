linear
	- 2 normal draws of 30 each w/ differing means
	- 2 normal draws of 30 each w/ differing standard deviations
	- 1 normal draw of 60 w/o any differences
AR
	- AR0 and AR1 with different coeffecients

library(strucchange)
breakpoints() for Bai-Perron breakpoints #/location, needs max breakpoints (w/ 1 or 15% or log)

bararBirth<-function(current knot set){
OPTION 1
scoring (vs. probability) w/ higher = better and lower = worse
proposes 2+ breakpoints
x1-x2 for both proposed breakpoints, minimize
OPTION 1A
add in possibility of worst distance
OPTION 2
only put in largest gap between existing knots
}

bararDeath<-function(current knot set){
random
}

barar<-function(# of iterations, initial breakpoint set, probability of birth/death/move){
select type of action - birth/death/move (random based on inputted probabilities)

if(birth selected){
bararBirth
}
else if(death selected){
bararDeath
}
else{
bararDeath
bararBirth
}

calculate MLE, save MLE/K/tau in distribution
if MLE terrible, reiterate with previous breakpoint set; if not terrible, reiterate with new breakpoint set
}