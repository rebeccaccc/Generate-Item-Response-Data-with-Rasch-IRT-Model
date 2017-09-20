###########################################################
####  Simulate item responses using a Rasch IRT model  ####
####  Author: Chen Li                                  ####
###########################################################

### IRT model: Rasch
### number of examinees: 1000
### number of items: 20

##set working directory. 
path ="XXXXX" 
setwd(path)

### Simulate true abilities for examinees from a standard normal distribution
theta <- rnorm(1000,0,1)

### Scale the randomly generated abilities to have a mean of 0, standard deviation of 1. 
### (scale either the ability or the item difficulty to have a mean of 0, standard deviation of 1 
###  to set the scale of the relative standing of theta and b)
theta<-(theta-mean(thera))/sd(theta)
### save the simulated ability parameters in a csv file 
write.table(theta,"theta.csv",sep = ",",row.names = FALSE,col.names = FALSE)

### Simulate item difficulty parameters from a standard normal distribution
b <- rnorm(20,0,1)
### save the generated item parameters 
write.table(b,"b.csv", sep =",",row.names = FALSE,col.names = FALSE)

### number of examinees
num.examinee<-length(theta)

### number of items
num.item<-length(b)

### replicate the simulated dataset for 100 times
replication<-100

#### Method 1: generating data using loops
for (r in 1:replication){
  res <- matrix(rep(NA, num.item*num.examinee), num.examinee, num.item)
  for(j in 1:num.examinee){
    for (i in 1:num.item){
      prob<-1/(1 + exp(-(theta[j]- b[i])))
      rini<-runif(1)
      if(rini>prob){res[j,i]<-0}
      if(rini<prob){res[j,i]<-1}     
    }
  }
  ### save the generated response dataset
  filename <- paste("Rasch_rep",r,".csv",sep="")
  write.table(res,filename,sep=",",row.names=F,col.names=F,na=" ",quote=F) 
}

#### Method 2 generating data using matrix computation 
theta.mat<-matrix(theta,num.examinee,num.item)
b.mat<-mat<-matrix(b,num.examinee,num.item, byrow=T)
for (r in 1:replication){
  logit <-(theta-b)
  P <- 1/(1 + exp(-logit))
  ### generate numbers from a uniform distribution and form a matrix with the same dimensions with the response data
  
  rand<- matrix(runif(num.examinee*num.item),num.examinee,num.item)
  ### compare the generated probabilities with the randomly generated numbers from a uniform distribution with a minmum of 0 and a maximum of 1
  res<- ifelse(P > rand,1,0)
  ### save the generated response dataset
  filename <- paste("Rasch_rep",r,".csv",sep="")
  write.table(res,filename,sep=",",row.names=F,col.names=F,na=" ",quote=F) 
}
