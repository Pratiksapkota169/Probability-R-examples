C<-250
p<-0.3
q<-0.15
beta<-10
lambda<-3
a=0.99
eps<-0.01

N = 0.25*(((qnorm((1-a)/2))/eps)^2) #calculate the number of experiments

TotalTimes<-c()

for(k in 1:N){
  
  X<-rbinom(1,250,0.3)
  #X<-sum(runif(250)<0.3)
  
  Y<-rgeom(X, prob = q)
  TotalTasksCount<-sum(Y)
  T<- sum(rgamma(TotalTasksCount, shape = 10, rate = 3))
  
  Y<-ceiling(log( 1 - runif(X) , base = exp(1))/ log( 1-q , base = exp(1)) )
  TotalTasksCount<-sum(Y)
  #T<-sum(log(matrix(runif(beta*TotalTasksCount), ncol=beta), base = exp(1))*(-1/lambda))
  TotalTimes[k]=T
}

pest<-mean((TotalTimes<1440))


N<-1000
Time<-c()
Nerrors<-c()

Last3<-c(28,22,18)
for( k in 1:N){
  
  DE<-sum(Last3)
  T<-0
 
    lambda = min(Last3)
    X<-rpois(1,lambda = lambda)
    DE=DE+X
    Last3<- c(Last3[2:3],X)
    T<-T+1
    Time[k]<-T
    Nerrors[k]<-DE
  
}
DE
mean(Time)
mean(Nerrors)

 