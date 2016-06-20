############# Overwiev of Discret Probability Distributions in R ##########
# As we know Probability Distribution is collection of outcomes and
# associated with them probabilities. E.g: {(W1,p1),(w2,p2),...,(Wn,Pn)} 
# where P1+P2+...+Pn=1. 

# "d"	returns the height of the probability density functions. PMF
# "p" returns the cumulative density functions. CDF
# "q"	returns the inverse cumulative density functions (quantiles). inverse CDF
# "r"	returns randomly generated numberss. 


# Bernoulli Distribution
# First most simple Bernoulli distribution with values={0,1} where 
# each of values have prbability P(for 1) and (1-P)(for 0). As input 
# this distribution function receive variable and as output we get probability
# of that variable. For Bernoulli distribution in R don't exist separate
# functions because Bernoulli Distribution is case of Binomial Distribution
# with N=1. In case of N=1 we have sequence with length one(1).
# Expectation E(X)= p or (1-q), Varience Var(X)=p*q

# PMF(prob mass function) or PDF dbinom(prob density fucntion)
dbinom(x=1, size =1 , prob = 0.4, log = FALSE) # return 0.4
dbinom(x=0, size =1 , prob = 0.4, log = FALSE) # return 0.6

# CDF(culmutative distribution function)
pbinom(q=1, size=1, prob=0.4, lower.tail = TRUE, log.p = FALSE)

# As input get probability and return possible value.
qbinom(p=0.61, size = 1, prob = 0.4) #return 1 
qbinom(p=0.59, size = 1, prob = 0.4) # return 0


# Generate random numbers 

rbinom(n=20, size = 1, prob=0.5)


# Discret Uniform Distribution.
# In probability theory and statistics, the discrete uniform 
# distribution is a symmetric probability distribution whereby
# a finite number of values are equally likely to be observed;
# every one of n values has equal probability 1/n. 
# Expectation E(X)= (n+1)/2 , Varience Var(X)=((n^2)-1)/12
# where n is number of possible value.

# Generation random number
sample(6, size = 3000, replace = TRUE)  # where p=1/6
sample(30:70, size = 27, replace= TRUE) # where p=1/40
sample(c("H","T"), size = 1000, replace =TRUE) # where p=1/2



# Binomial Distribution
# In probability theory and statistics, the binomial 
# distribution with parameters n and p is the discrete probability 
# distribution of the number of successes in a sequence of 
# n independent yes/no experiments, each of which yields
# success with probability p. A success/failure experiment is also 
# called a Bernoulli experiment or Bernoulli trial; when n = 1,
# the binomial distribution is a Bernoulli distribution. 
# Expectation E(X)= n*p , Varience Var(X)=n*p*q
# where n is number of trials.


# PMF(prob mass function) or PDF dbinom(prob density fucntion)
dbinom(x=5, size =15 , prob = 0.5, log = FALSE) # return 0.4
dbinom(x= 5 , prob = 0.5, log = FALSE) # return 0.6

# CDF(culmutative distribution function)
pbinom(q=5, size=15, prob=0.5, lower.tail = TRUE, log.p = FALSE)
pbinom(q=5, size=15, prob=0.5, lower.tail = TRUE, log.p = FALSE) # return 1

# As input get probability and return possible value.
qbinom(p=0.09164429, size = 15, prob = 0.5) #return 5


# Generate random numbers 
rbinom(n=200, size = 16, prob=0.5)

# Also for Binomial Distribution we can use package('distrEx')
library(distrEx)
X <- Binom(size = 3, prob = 0.45)



# Geometric Distribution
# The number of Bernoulli trials needed to get the first success
# has Geometric Distribution.
# Expectation E(X)= 1/p , Varience Var(X)=(1-p)/(p^2)

#x, q	vector of quantiles representing the number of failures in a
# sequence of Bernoulli trials before success occurs.

# PMF(prob mass function) or PDF dbinom(prob density fucntion)
dgeom(x=3, prob = 0.5)

# CDF(culmutative distribution function)
pgeom(q=8,prob = 0.5)

# As input get probability and return possible value.
qgeom(0.995117,prob = 0.5) # return 7

# Generate random numbers 
rgeom(200, prob = 0.5)


# Negative Binomial Distribution
# Expectation E(X)= k/p, Varience Var(X)=k*(1-p)/(p^2)
# where k is number of success wich we should obtain
# PMF(prob mass function) or PDF dbinom(prob density fucntion)
dnbinom(3,4,0.5)

# CDF(culmutative distribution function)
pnbinom(3,4,0.5)

# As input get probability and return possible value.
qnbinom(0.5,4,prob = 0.5) # return 7

# Generate random numbers
rnbinom(200, 4, 0.5)

# Poisson Distribution
# Is a discrete probability distribution that expresses the probability 
# of a given number of events occurring in a fixed interval of time
# and/or space if these events occur with a known average rate and 
# independently of the time since the last event.

# Lambda is average frequency of event in that interval.
# Expectation E(X)= lambda, Varience Var(X)=lambda

# PMF(prob mass function) or PDF dbinom(prob density fucntion)
dpois(x=3, lambda = 5)

# CDF(culmutative distribution function)
ppois(4, lambda=5, lower.tail = TRUE)

# As input get probability and return possible value.
qpois(p = 0.6159607, lambda = 5) # return 6

# Generate random numbers 
rpois(200, lambda = 5)


# Poisson approximation of Binomial Distribution.
# When the number of trials is large and probability is very small 
# we can approximate Binamial Distribution through Poisson Distribution

# If n is very large
# and P->0
# and np->lambda
# approximately it is n>=30 and p<=0.05 then we use Poisson formulas for 
# Binomial Distribution, and use n*p as Lambda for Poisson pmf and cdf. 








