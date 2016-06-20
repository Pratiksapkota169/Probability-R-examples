# Basic Probability 



############### Permutation And Combination ############
# When you compute a permutation or combination we use planty
# as set(but not multiset), if you want to interpret it as 
# multiset(numeriacal,or characterisic) simply remove duplicates.
# E.g: c('red', 'blue', 'black','black'), for P(n,k) and C(n,k) this 
# is set where each "black" is unique "black". If you want interpret
# "black"s as same simple remove duplicates. 

install.packages("gtools")
install.packages("iterpc")

install.packages("prob")
# nsamp() for permutation and combination

library('gtools')
library('iterpc')

x <- c('red', 'blue', 'black','black')

################# Library "gtools" ##################
# Permutation:
# Pr(n,k). Here we try all possible permutation with repetition(replacement),
# that mean, every time when we select new element for set, we return that 
# element in the initial set. And we construct all possible set in that way.
permutations(n=4,r=2,v=x,set=FALSE,repeats.allowed=T)
# P(n,k). After getting element we don't return it into
# initial set.
permutations(n=4,r=2,v=x,set=FALSE,repeats.allowed=F)

# Combination:
# Cr(n,k) 
combinations(n=3,r=2,v=x,set=FALSE,repeats.allowed=T)
# C(n,k) = P(n,k)/P(k,k)=P(n,K)/k!, where P(k,k) the 
# number of same object in massive with length k. Its
# mean we remove same objects, and left only one from those.
combinations(n=3,r=2,v=x,set=FALSE,repeats.allowed=F) 


################# Library "iterpc" ##################
# Permutation
# Pr(n,k)
I = iterpc(3,2,x, ordered = TRUE, replace = TRUE)
getall(I)

# P(n,k)
I = iterpc(3,2,x, ordered = TRUE, replace = FALSE)
getall(I)

# Combination:
# Cr(n,k) if replace exist in new set we also get sets
# with all same element.E.g (3,3,3)
I = iterpc(3,2,x, ordered = FALSE, replace = TRUE)
getall(I)
# C(n,k)
I = iterpc(3,2,x, ordered = FALSE, replace = FALSE)
getall(I)


####### Random Variable ########
N=1000

XLabels<-c("Coffee","Coca-Cola","Natural Juice", "Fresh","Vine","Cocktail")
YLabels<-c("SMALL","MEDIUM","LARGE","EXTRA LARGE")

# XVector is random vector ov Drink Types. Where XLabels->1:6(enumerate drink types)
# Its function from anyType to real for compute Expectation(mean).
XVector<-sample(1:6, N, replace=TRUE)
# YVector is random vector ov Size Types. Where YLabels->1:4(enumerate size types)
# Its function from anyType to real for compute Expectation(mean).
YVector<-sample(1:4, N, replace=TRUE)

OutcomeX<-function(x){
  
  return(XLabels[x])
  
}

OutcomeY<-function(y){
  
  return(YLabels[y])
  
}

# Count probability of Drink Types

outcomeCountX <- table(XVector)
outcomeVectorX<-cbind(as.numeric(names(outcomeCountX)), as.vector(outcomeCountX))
probabilityValuesX<-cbind(as.numeric(names(outcomeCountX)),apply(outcomeVectorX, 2, function(x) x/length(XVector) )[,2])

# Count probability of Size Types
outcomeCountY <- table(YVector)
outcomeVectorY<-cbind(as.numeric(names(outcomeCountY)), as.vector(outcomeCountY))
probabilityValuesY<-cbind(as.numeric(names(outcomeCountY)),apply(outcomeVectorY, 2, function(x) x/length(YVector) )[,2])


# Probability Mass Function definition for X(Drink Types) call it pmfX.
pmfX<-function(x){
  
  return( probabilityValuesX[probabilityValuesX[,1]==x][2])
  
}
# Probability Mass Function definition for Y(Size Types) call it pmfX.
pmfY<-function(y){
  
  return( probabilityValuesY[probabilityValuesY[,1]==y][2])
  
}

# Cumulative(Integreal) Distribution Function for X(Drink Types). cdfX
cdfX<-function(x){

  return(sum(probabilityValuesX[1:x,2]))
  
}
# Cumulative(Integreal) Distribution Function for Y(Size Types). cdfY
cdfY<-function(y){
  
  return(sum(probabilityValuesY[1:y,2]))
  
}


# Compute Expectation of Random Variable. X, ExpX
ExpX<-function(){
  
  return(t(probabilityValuesX[,1])%*%probabilityValuesX[,2])
  
}

# Compute Expectation of Random Variable. Y, ExpY
ExpY<-function(){
  
  return(t(probabilityValuesY[,1])%*%probabilityValuesY[,2])
  
}

# Compute varience for X.
VarX<-function(){
  
  return(t( (probabilityValuesX[,1] - ExpX())^2 )%*%probabilityValuesX[,2])
  
}
# Compute varience for Y.
VarY<-function(){
  
  return(t( (probabilityValuesY[,1] - ExpY())^2 )%*%probabilityValuesY[,2])
  
}
# Stadard deviation for X.
sdX<-function(){
  
  return(sqrt(VarX()))
  
}
# Stadard deviation for Y.
sdY<-function(){
  
  return(sqrt(VarY()))
  
}

# Compute Expectation of X and Y random vector(X,Y) which equal,
# E((X,Y))=E(XY)= SUMbyX(SUMbyY((x,y)P(x,y))). P(x,y)=P{X=x intersect Y=y}


XY<-cbind(XVector,YVector)
concatCol <- apply(XY, 1, paste, collapse = " ")
df<-data.frame((table(concatCol)))

df$Freq = by(df$Freq, 1:nrow(df), function(x) x/N )[1:nrow(df)]


ExpXY<-function(){
  expXY<-0
  for(iX in 1:length(XLabels)){
    for(iY in 1:length(YLabels)){
      ind<-paste(c(iX, iY), collapse = " ")
      if(sum(df$concatCol==ind ) != 0){
        expXY<- expXY + (iX*iY)*df$Freq[df$concatCol==ind]
      }
    }
  }
  
  return(expXY)
  
}

# Compute varience for XY.
VarXY<-function(){
  varXY<-0
  for(iX in 1:length(XLabels)){
    for(iY in 1:length(YLabels)){
      ind<-paste(c(iX, iY), collapse = " ")
      if(sum(df$concatCol==ind ) != 0){
        varXY<- varXY + ((iX*iY)-ExpXY())^2*df$Freq[df$concatCol==ind]
      }
    }
  }
  
  return(varXY)
  
}
# Stadard deviation for XY.
sdXY<-function(){
  
  return(sqrt(VarXY()))
  
}

# Compute covarience of X and Y. Cov(X,Y) as covXY()
CovXY<-function(){
  
  return(ExpXY() - ExpX()*ExpY())
  
}

# Compute corelation between X and Y its same as Cov(X,Y) mapped to [-1,1].
# cor(X,Y):Cov(X,Y)->[-1,1]
CorelXY<-function(){
  
  return(CovXY()/sdX()*sdY())
  
}

# Chebishev's inequality: for compute probability(risk) of Expectation.
# it means 
Pcheb<- function(Exp,sd,eps){
  
  return( (sd/eps)^2 )
  
}

 

