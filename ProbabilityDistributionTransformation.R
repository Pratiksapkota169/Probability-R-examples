############ Distribution Transformation ################

############### By DISCRET Methods ###################
############## From Uniform to other ####################

# Random Variable = R.V.

# 1.
# From Standart Uniform R.V. getting Bernoulli R.V 
# with probability  p = x
x=0.5
runif(1,min = 0, max = 1)<x

# 2.
# From Standart Uniform R.V. getting Binomial R.V
# We obtain Binomial as sum of Bernoulli R.V. where
# "n" is number of trial, p=y is probability.
n=20
y=0.68
sum(runif(n,min = 0, max = 1)<y)


# 3.
# From Standart Uniform R.V. getting Geometric R.V
# p=z is probability.
z=0.28
Xg = 1
while (runif(1,min = 0, max = 1)>z) {
  Xg=Xg+1
}
Xg


# 4.
# From Standart Uniform R.V. getting Binomial R.V
# We obtain Binomial as sum of Bernoulli R.V. where
# "k" is number of success, p=y is probability.
# we obtain by sum of K indipendent Geomentric R.V

NB=1
k=5
i=0
while (i<k) {
  
  z=0.28
  Xg = 1
  while (runif(1,min = 0, max = 1)>z) {
    Xg=Xg+1
  }
  
  NB=NB+Xg
  i=i+1
}
NB

 

############### By INVERSE TRANSFORM Method with F(X) ################
# Continuous case
# From Standard Uniform R.V to Exponentional(lmbd) with lambda=lmbd
# f(x)=lmbd*exp(-lmbd*x),f'(x)=F(X)=1-exp(-lmbd*x)
# getting X=F^(-1)(x), solve F(X)=U => X= (-1/lmbd)*log(1-U)[11]
# First we should generate U random variable and after use [11] to get X.
lmbd=5
(-1/lmbd)*log(runif(1), base = exp(1))


# Discret case
# From Standard Uniform R.V to Geometric with lambda=lmbd
# In this case we generate U and get smallest x from fininte
# set of possible values, for which F(x)>U
# X>= log(1-U)/log(1-p)

p=0.5
ceiling(log( 1- runif(200), base = exp(1))/log(1-p, base = exp(1)))

############### By REJECTION Method with f(x) ###################

# Generate Gamma R.V from Uniform U and V R.V.s

alpha=5.5
beta=3.1
a=0
b=1
c=2.5

X=0
Y=c

while( Y >  gamma( ((alpha+beta)/(gamma(alpha)*gamma(beta))))*(X^(alpha-1))*((1-X)^(beta - 1)) ){
  U=runif(1)
  V=runif(1)
  X=a+((b-a)*U)
  Y=c*V
}
X
