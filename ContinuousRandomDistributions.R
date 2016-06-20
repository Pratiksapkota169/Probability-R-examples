############# Overwiev of Continuous Probability Distributions in R ##########
# Continuous is probability distribution where we use X with endless number of
# possible value. It conversely to Discret P.D where X has finite or countable
# set of values. C.P.D - represent things such human heigh or weight, computer
# software installation time, where possible values are infinite. In this case
# if you try to get specific value probabilities(PMF) we get 0. E.g:P{X=x}=0 Always.
# Because we have infinite count of possible values therefore
# P{X=x} = x/infinite->0 => P{X=x}=0. However you can get values for specific 
# intervals PDF(a,b)=P{a<=X<=b}, you can ask "which probability why number from interval [a,b]. As
# answer you get percentage of observation which fall in that range.



# First we introduce general case and real world example. As observed
# data we get randomly selected numbers from Gaussian(Normal) distribution.
# And try to ESTIMATE PDF(Probabiltiy Density Distribution).(You can not 
# obtain analitic view PDF for observed data, but we can estimate that
# function in digital form.)

# Supose we try estimate weather in [-4,4], we randomly generate 1000
# observation.

weatherObserv<-rnorm(1000, mean = 2, sd = 1)

# Print histogram of observed data with default range break.
hist(weatherObserv,probability = TRUE, # In stead of frequency
     breaks = "FD",      # For more breaks than the default
     col = "BLUE", border = "seashell3")

# We estimate PDF with density() function for observed data
# and print in our graph as red curve.
weatherPDF <- density(weatherObserv)
lines(weatherPDF,   # Add the kernel density estimate (-.5 fix for the bins)
      col = "RED", lwd = 3)

# Now we try some PDF Feature
# install.packages("sfsmisc")
# library(sfsmisc)  // from univesite of zurits
# 1. PDF(-inf,+inf)==1 
integrate.xy(weatherPDF$x,weatherPDF$y) # =1.000974 which is almost 1.:D
# Now we try to get PDF(a,b) values in specific [a,b]
integrate.xy(weatherPDF$x,weatherPDF$y,1,3)
# we can't compute f(x) in any point because we estimate f(x) through density() from
# observed data, we don't have f(x) at any point, we have that only on observed points.


# Continuous Uniform Distribution(Standard Uniform is also Uniform where a=0 and b=1)
# Standard Uniform(Expectation: E(x)=1/2, Varience: Var(X)=1/12)
# Uniform Distribution(Expectation: E(X)=(a+b)/2, Varience: Var(X)=((b-a)^2)/12 )

# PDF 
# case of discret we specify x as integer
dunif(x=4,min = 1, max = 5, log = FALSE) # values of f(x) in point X.

# CDF 
punif(q=3.5, min = 1, max = 5, lower.tail = TRUE, log.p = FALSE)

# reverse CDF
qunif(p=0.65, min = 1, max = 5, lower.tail = TRUE, log.p = FALSE)

# Generate random number with equaly chance in interval
genUnifData<-runif(n=1000, min = 1, max = 5)

# plot hist and density function

hist(genUnifData,probability = TRUE, # In stead of frequency
     breaks = "FD",      # For more breaks than the default
     col = "BLUE", border = "seashell3")
lines(density(genUnifData),   # Add the kernel density estimate (-.5 fix for the bins)
      col = "RED", lwd = 3)

# Exponential distribution - is often used to model time: waiting time,
# interrival time, hardware laftime, failure time e.t.c. When the number
# of rare events is Poisson(Discret D), the time between events is Exponential.
# Exp. distr. has "lambda" rate, if time measured in minutes then lambda is frequency
# Expectaion E(X)= 1/lambda, Varience Var(X) = 1/(lambda^2).
# rate == lambda.
# PDF 
dexp(0.2, rate = 4) # values of f(x) in point X.

# CDF 
pexp(0.1, rate = 4) # You shall mulitply this by time unit value.E.g: 0.2*60m=12m. 0.1*1y=36day.

# reverse CDF
qexp(0.9999939, rate = 4)

# Generate random number with equaly chance in interval

genExpData<- rexp(n=1000, rate = 5)

# plot hist and density function


hist(genExpData,probability = TRUE, # In stead of frequency
     breaks = "FD",      # For more breaks than the default
     col = "BLUE", border = "seashell3")
lines(density(genExpData),   # Add the kernel density estimate (-.5 fix for the bins)
      col = "RED", lwd = 3)

# 1. PDF(-inf,+inf)==1 
integrate.xy(density(genExpData)$x,density(genExpData)$y) # =1.000974 which is almost 1.:D
# Now we try to get PDF(a,b) values in specific [a,b]
integrate.xy(density(genExpData)$x,density(genExpData)$y,0.9,1.2)



# Gamma Distribution Gamma(alpha,lambda): is widely used for the total time of a
# multistage scheme, with "alpha" steps and "lambda" Exponential amount of time,
# between them.
# Gamma(alpha=1,lambda) == Exponential(lambda), for every "lambda"
# Gamma(alpha, lambda=1/2) == Chi-square(2*alpha), for every "alpha"
# Expectation E(X)=alpha/lambda, Var(X)=alpha/(lambda^2)
# shape=alpha, rate = lambda, scale = 1/lambda

#PDF
dgamma(x = 0.2,shape = 6, rate = 12 )
#CDF as probability
pgamma(q = 0.2,shape = 6, rate = 12 )

# reverse CDF
qgamma(0.490, shape = 6, rate = 12)

# Generate random number with equaly chance in interval
genGammaData<-rgamma(n=1000, shape = 6, rate = 12)

# plot hist and density function
hist(genGammaData,probability = TRUE, # In stead of frequency
     breaks = "FD",      # For more breaks than the default
     col = "BLUE", border = "seashell3")
lines(density(genGammaData),   # Add the kernel density estimate (-.5 fix for the bins)
      col = "RED", lwd = 3)




# Normal Distribution (Most usable distribution in real life(heigh,weigh,time,weather e.t.c))
# In correlation with Central Limit Theorem, its became most powerfull distribuion.
# Normal Distribution also know as Guassian Distribution. 
# Expectation E(X)=mean(observed value), Varience Var(X)=Var(observed Value)

# PDF 
dnorm(2.2, mean = 2, sd = 1, log = FALSE) # values of f(x) in point X.

# CDF
pnorm(q = 2, mean = 2, sd = 1, lower.tail = TRUE, log.p = FALSE)

# reverse CDF
qnorm(0.8413447, mean = 2, sd = 1, lower.tail = TRUE, log.p = FALSE)

# Generate random number with equaly chance in interval
genNormData<-rnorm(n=1000,mean = 2,sd = 1)

# plot hist and density function
hist(genNormData,probability = TRUE, # In stead of frequency
     breaks = "FD",      # For more breaks than the default
     col = "BLUE", border = "seashell3")
lines(density(genNormData),   # Add the kernel density estimate (-.5 fix for the bins)
      col = "RED", lwd = 3)



