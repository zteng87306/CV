library(rstan)
x=faithful$eruptions
n.iteration=100
mu=c(2,4.5)

sigma=0.64
alpha=c(0.3,0.7)
lambda=0.5

#Gibbs sampler
Gibbs_sampler=function(k){
  MCMC=matrix(NA,nrow=n.iteration,ncol=272)
  for(i in 1:n.iteration){
    z=c()
    for(j in 1:272){
      a=alpha[1]*dnorm(x[j],mu[1],sigma)
      b=alpha[2]*dnorm(x[j],mu[2],sigma)
      z[j]=sample(c(1,2),size=1,prob=c(a/(a+b),b/(a+b)))
    }
    
    a1=1/(sum(z==1)/sigma^2+1/lambda^2)
    mu[1]=rnorm(1,sum(z==1)/sigma^2*a1*mean(x[z==1]),sqrt(a1))
  
    a2=1/(sum(z==2)/sigma^2+1/lambda^2)
    mu[2]=rnorm(1,sum(z==2)/sigma^2*a2*mean(x[z==2]),sqrt(a2))
    
    MCMC[i,]=z
  }

  mean_20=c()
  for(i in 1:272){
    mean_20[i]=mean(MCMC[,i])
  }
  
  return(list(MCMC,mean_20))
}


#collapsed gibbs sampler
collapsed_Gibbs_sampler=function(k){
  z=c()
  for(j in 1:272){
    a=alpha[1]*dnorm(x[j],mu[1],sigma)
    b=alpha[2]*dnorm(x[j],mu[2],sigma)
    z[j]=sample(c(1,2),size=1,prob=c(a/(a+b),b/(a+b)))
  }
  
  MCMC1=matrix(NA,nrow=n.iteration,ncol=272)
  for(k in 1:n.iteration){
    for(i in 1:272){
      n1=sum(z[-i]==1)
      s1=sum(x[-i][z[-i]==1])
    
      p1=dnorm(x[i],(n1/sigma^2)/(n1/sigma^2+1/lambda^2)*(s1/n1),sqrt(1/(n1/sigma^2+1/lambda^2)+sigma^2))
    
      n2=sum(z[-i]==2)
      s2=sum(x[-i][z[-i]==2])
    
      p2=dnorm(x[i],(n2/sigma^2)/(n2/sigma^2+1/lambda^2)*(s2/n2),sqrt(1/(n2/sigma^2+1/lambda^2)+sigma^2))

      z[i]=sample(c(1,2),size=1,prob=c(p1*alpha[1]/(p1*alpha[1]+p2*alpha[2]),p2*alpha[2]/(p1*alpha[1]+p2*alpha[2])))
    }
    MCMC1[k,]=z
  }
  mean_201=c()
  for(i in 1:272){
    mean_201[i]=mean(MCMC1[,i])
  }
  return(list(MCMC1,mean_201))
   
}

a=Gibbs_sampler(1)
b=Gibbs_sampler(2)
c=Gibbs_sampler(3)
d1=cbind(a[[1]][,1],b[[1]][,1],c[[1]][,1])
d2=cbind(a[[1]][,2],b[[1]][,2],c[[1]][,2])
d3=cbind(a[[1]][,3],b[[1]][,3],c[[1]][,3])
d4=cbind(a[[1]][,4],b[[1]][,4],c[[1]][,4])
d5=cbind(a[[1]][,5],b[[1]][,5],c[[1]][,5])
d6=cbind(a[[1]][,6],b[[1]][,6],c[[1]][,6])
d7=cbind(a[[1]][,7],b[[1]][,7],c[[1]][,7])
d8=cbind(a[[1]][,8],b[[1]][,8],c[[1]][,8])
array1=array(NA,c(n.iteration,3,8))
array1[,,1]=d1;array1[,,2]=d2;array1[,,3]=d3;array1[,,4]=d4
array1[,,5]=d5;array1[,,6]=d6;array1[,,7]=d7;array1[,,8]=d8
cluster1_1=x[a[[2]]<1.5]
cluster1_2=x[a[[2]]>=1.5]

f=collapsed_Gibbs_sampler(1)
g=collapsed_Gibbs_sampler(2)
h=collapsed_Gibbs_sampler(3)
i1=cbind(f[[1]][,1],g[[1]][,1],h[[1]][,1])
i2=cbind(f[[1]][,2],g[[1]][,2],h[[1]][,2])
i3=cbind(f[[1]][,3],g[[1]][,3],h[[1]][,3])
i4=cbind(f[[1]][,4],g[[1]][,4],h[[1]][,4])
i5=cbind(f[[1]][,5],g[[1]][,5],h[[1]][,5])
i6=cbind(f[[1]][,6],g[[1]][,6],h[[1]][,6])
i7=cbind(f[[1]][,7],g[[1]][,7],h[[1]][,7])
i8=cbind(f[[1]][,8],g[[1]][,8],h[[1]][,8])
array2=array(NA,c(n.iteration,3,8))
array2[,,1]=i1;array2[,,2]=i2;array2[,,3]=i3;array2[,,4]=i4
array2[,,5]=i5;array2[,,6]=i6;array2[,,7]=i7;array2[,,8]=i8
cluster2_1=x[f[[2]]<1.5]
cluster2_2=x[f[[2]]>=1.5]




#Rhat
monitor(array1)
monitor(array2)


#classification
plot(cluster1_1,runif(length(cluster1_1),-0.15,0.15),col="blue",ylim=c(-0.4,0.4),xlim=c(1,5.5),xlab="x",ylab="",main="Gibbs sampling")
points(cluster1_2,runif(length(cluster1_2),-0.15,0.15),col="green")
abline(v=3)
plot(cluster2_1,runif(length(cluster2_1),-0.15,0.15),col="blue",ylim=c(-0.4,0.4),xlim=c(1,5.5),xlab="x",ylab="",main="Collapsed Gibbs sampling")
points(cluster2_2,runif(length(cluster2_2),-0.15,0.15),col="green")
abline(v=3)


#time series
ts.plot(a[[1]][,6],ylab='Z6')
ts.plot(f[[1]][,6],ylab='Z6')


#ACF
acf(a[[1]][,6],main='')
acf(f[[1]][,6],main='')











