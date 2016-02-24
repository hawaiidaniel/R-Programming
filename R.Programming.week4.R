#str function

#Generating Random Numbers
#rnorm:generate random Normal variates with given mean and sd
#dnorm:evaluate the Normal probability density with a given mean/sd at a point
#pnorm:evaluate the cumulative distribution function for a Normal distribution
#qnorm:quantile function
#rpois:generate random Poisson variates with a given rate
#ppois

#setting the random number with set.seed ensures reproducibility

#Simulating a Linear Model y=b0*+b1*x+e
set.seed(20)
x=rnorm(100)
e=rnorm(100,0,2)
y=0.5+2*x+e
plot(x,y)
#x is binary
set.seed(10)
x=rbinom(100,1,0.5)
e=rnorm(100,0,2)
y=0.5+2*x+e
plot(x,y)
#Poisson Model
set.seed(1)
x=rnorm(100)
log.mu=0.5+0.3*x
y=rpois(100,exp(log.mu))
plot(x,y)

#Random Sampling
#sample function draws randomly from a specified set of objects to sample from 
#arbitrary distributions
set.seed(1)
sample(1:10,4)
sample(letters,5)
sample(1:10,replace = T)

#R Profiler: DO NOT use system.time and Rprof together!
system.time(sample(1:10,replace = T))
summary(Rprof())
summaryRprof()














