library(devtools)
install_github("hazimehh/L0Learn")
library(L0Learn)

## an easy example
set.seed(1)
X<-matrix(rnorm(500*1000), nrow=500, ncol=1000)
B<-c(rep(1,10),rep(0,990))
e<-rnorm(500)
y<-X%*%B+e  #B is the true support
## fitting L0 Models
fit<-L0Learn.fit(X,y,penalty="L0",maxSuppSize=30)
fit$lambda
fit$lambda[[1]][7]  #a certain \lambda
# result is solutions generate by a sequence of \lambda values(chosen automatically by the algorithm)
print(fit)
# extract the estimated B for particular values of \lambda and \gamma
a<-coef(fit, lambda=0.0325142, gamma=0)
a[1] #first row is the intercept
coef(fit,lambda=fit$lambda[[1]][7], gamma=0) #same
#make prediction
predict(fit, newx=X, lambda=0.0325142, gamma=0) #newX is the testing sample
#visualize the regularization path by plotting the coefficients of the estimated B versus the support size
plot(fit, gamma=0, showLines = TRUE) #ggplot object, can modify

##L0L2
#with a maximum of 20 nonzeros and a sequence of 5 \gamma values ranging between 0.0001 and 10
fit<-L0Learn.fit(X,y,penalty="L0L2",nGamma=5, gammaMin=0.0001, gammaMax = 10, maxSuppSize = 20)
print(fit)
fit$gamma
coef(fit,lambda=0.0011539, gamma=10)
#make prediction
predict(fit, newx=X, lambda=0.0011539, gamma=10)

#CD-PSI
fit0<-L0Learn.fit(X,y,penalty="L0",maxSuppSize=20,algorithm = "CDPSI")
fit0$lambda
fit0$lambda[[1]][7] 

#Cross Validation
#5 fold CV using L0L2 penalty
cvfit<-L0Learn.cvfit(X,y,nFolds = 5,seed=1, penalty = "L0L2", nGamma = 5,
                     gammaMin=0.0001, gammaMax = 0.1, maxSuppSize = 50)
cvfit$cvMeans # is a list where the ith element
cvfit$cvMeans[[i]] #stores the cross-validation errors for the ith value of gamma
cvfit$fit$gamma[i]

lapply(cvfit$cvMeans,min)

plot(cvfit, gamma=cvfit$fit$gamma[4])

#To extract the optimal λ(i.e., the one with minimum CV error) in this plot
optimalGammaIndex = 4 # index of the optimal gamma identified previously
optimalLambdaIndex = which.min(cvfit$cvMeans[[optimalGammaIndex]])
optimalLambda = cvfit$fit$lambda[[optimalGammaIndex]][optimalLambdaIndex]
optimalLambda
#To print the solution corresponding to the optimal gamma/lambda pair:
coef(cvfit, lambda=optimalLambda, gamma=cvfit$fit$gamma[4])

# To do subsect selection
# always include first 3 variables
fit <- L0Learn.fit(X, y, penalty="L0", maxSuppSize=20, excludeFirstK=3)
plot(fit, gamma=0)

# user-specified lambda grids
userLambda <- list()
userLambda[[1]] <- c(1, 1e-1, 1e-2, 1e-3, 1e-4) #decreasing order
fit <- L0Learn.fit(X, y, penalty="L0", autoLambda=FALSE, lambdaGrid=userLambda, maxSuppSize=1000)
print(fit)


####to do test
X<-matrix(rnorm(50*100), nrow=50, ncol=100)
B<-c(rep(1,10),rep(0,90))
e<-rnorm(50)
y<-X%*%B+e  #B is the true support

cvfit<-L0Learn.cvfit(X,y,nFolds = 5,seed=1, penalty = "L0L2", nGamma = 5,
                     gammaMin=0.0001, gammaMax = 0.1, maxSuppSize = 50,algorithm = "CDPSI")
cvfit$cvMeans # is a list where the ith element
cvfit$cvMeans[[i]] #stores the cross-validation errors for the ith value of gamma
cvfit$fit$gamma[i]

lapply(cvfit$cvMeans,min)
optimalGammaIndex = 4 # index of the optimal gamma identified previously
optimalLambdaIndex = which.min(cvfit$cvMeans[[optimalGammaIndex]])
optimalLambda = cvfit$fit$lambda[[optimalGammaIndex]][optimalLambdaIndex]
optimalLambda
#To print the solution corresponding to the optimal gamma/lambda pair:
test<-coef(cvfit, lambda=optimalLambda, gamma=cvfit$fit$gamma[4])
test[1:50]



library(glmnet)
x=matrix(rnorm(100*20),100,20)
y=rnorm(100)
g2=sample(1:2,100,replace=TRUE)
g4=sample(1:4,100,replace=TRUE)
fit1=glmnet(x,y)
predict(fit1,newx=x[1:5,],s=c(0.01,0.005))
predict(fit1,type="coef")
plot(fit1,xvar="lambda")
fit2=glmnet(x,g2,family="binomial")
predict(fit2,type="response",newx=x[2:5,])
predict(fit2,type="nonzero")
fit3=glmnet(x,g4,family="multinomial")
predict(fit3,newx=x[1:3,],type="response",s=0.01)



set.seed(1010)
n=1000;p=100
nzc=trunc(p/10)
x=matrix(rnorm(n*p),n,p)
beta=rnorm(nzc)
fx= x[,seq(nzc)] %*% beta
eps=rnorm(n)*5
y=drop(fx+eps)
px=exp(fx)
px=px/(1+px)
ly=rbinom(n=length(px),prob=px,size=1)
set.seed(1011)
cvob1=cv.glmnet(x,y)
plot(cvob1)
coef(cvob1)
predict(cvob1,newx=x[1:5,], s="lambda.min")
title("Gaussian Family",line=2.5)
set.seed(1011)
cvob1a=cv.glmnet(x,y,type.measure="mae")
plot(cvob1a)
title("Gaussian Family",line=2.5)
set.seed(1011)
par(mfrow=c(2,2),mar=c(4.5,4.5,4,1))
cvob2=cv.glmnet(x,ly,family="binomial")
plot(cvob2)
title("Binomial Family",line=2.5)
frame()
set.seed(1011)
cvob3=cv.glmnet(x,ly,family="binomial",type.measure="class")
plot(cvob3)
title("Binomial Family",line=2.5)
## Not run:
set.seed(1011)
cvob3a=cv.glmnet(x,ly,family="binomial",type.measure="auc")
plot(cvob3a)
title("Binomial Family",line=2.5)
set.seed(1011)
mu=exp(fx/10)
y=rpois(n,mu)
cvob4=cv.glmnet(x,y,family="poisson")
plot(cvob4)
title("Poisson Family",line=2.5)


x=matrix(rnorm(100*20),100,20)
y=rnorm(100)
g2=sample(1:2,100,replace=TRUE)
g4=sample(1:4,100,replace=TRUE)
fit1=glmnet(x,y)
plot(fit1)
plot(fit1,xvar="lambda",label=TRUE)
fit3=glmnet(x,g4,family="multinomial")
plot(fit3,pch=19)


arcene.d <- read.csv(url("https://archive.ics.uci.edu/ml/machine-learning-databases/arcene/ARCENE/arcene_test.data"),
                     header=FALSE)


(system.time(lad_newton (y=y2, X=X2, beta=beta0, epsilon=0.25,lambda=10, 
                         naive=TRUE, max_iter=1e2, tol=1e-3))[3])


###
X<-matrix(rnorm(500*10000), nrow=500, ncol=10000)
B<-c(rep(1,10),rep(0,9990))
e<-rnorm(500)




set.seed(1)
## L0 number of nonzero
y<-X%*%B+e  #B is the true support
cvfit = L0Learn.cvfit(X, y, nFolds=5, seed=1, penalty="L0", 
                      nGamma=5, gammaMin=0.0001, gammaMax=0.1, maxSuppSize=500)
lapply(cvfit$cvMeans, min)
a<-lapply(cvfit$cvMeans, min)
index<-which.min(unlist(a)) 
optimalGammaIndex = index # index of the optimal gamma identified previously
optimalLambdaIndex = which.min(cvfit$cvMeans[[optimalGammaIndex]])
optimalLambda = cvfit$fit$lambda[[optimalGammaIndex]][optimalLambdaIndex]
optimalLambda
coli<-coef(cvfit, lambda=optimalLambda, gamma=cvfit$fit$gamma[index])
colSums(coli != 0)
#L0 degree of freedom
pred<-predict(fit, newx=X, lambda=optimalLambda, gamma=cvfit$fit$gamma[index])
cov<-cor(as.vector(pred),as.vector(y))
dof<-cov/sig2


## L0L2 number of nonzero
y<-X%*%B+e  #B is the true support
cvfit = L0Learn.cvfit(X, y, nFolds=5, seed=1, penalty="L0L2", 
                      nGamma=5, gammaMin=0.0001, gammaMax=0.1, maxSuppSize=500)
lapply(cvfit$cvMeans, min)
a<-lapply(cvfit$cvMeans, min)
index<-which.min(unlist(a)) 
optimalGammaIndex = index # index of the optimal gamma identified previously
optimalLambdaIndex = which.min(cvfit$cvMeans[[optimalGammaIndex]])
optimalLambda = cvfit$fit$lambda[[optimalGammaIndex]][optimalLambdaIndex]
optimalLambda
coli<-coef(cvfit, lambda=optimalLambda, gamma=cvfit$fit$gamma[index])
colSums(coli != 0)
#L0L2 degree of freedom
pred<-predict(fit, newx=X, lambda=optimalLambda, gamma=cvfit$fit$gamma[index])
cov<-cor(as.vector(pred),as.vector(y))
dof<-cov/sig2


## L0L1 number of nonzero
y<-X%*%B+e  #B is the true support
cvfit = L0Learn.cvfit(X, y, nFolds=5, seed=1, penalty="L0L1", 
                      nGamma=5, gammaMin=0.0001, gammaMax=0.1, maxSuppSize=500)
lapply(cvfit$cvMeans, min)
a<-lapply(cvfit$cvMeans, min)
index<-which.min(unlist(a)) 
optimalGammaIndex = index # index of the optimal gamma identified previously
optimalLambdaIndex = which.min(cvfit$cvMeans[[optimalGammaIndex]])
optimalLambda = cvfit$fit$lambda[[optimalGammaIndex]][optimalLambdaIndex]
optimalLambda
coli<-coef(cvfit, lambda=optimalLambda, gamma=cvfit$fit$gamma[index])
colSums(coli != 0)
#L0L1 degree of freedom
pred<-predict(fit, newx=X, lambda=optimalLambda, gamma=cvfit$fit$gamma[index])
cov<-cor(as.vector(pred),as.vector(y))
dof<-cov/sig2


## 
set.seed(1)
X<-matrix(rnorm(500*1000), nrow=500, ncol=1000)
B<-c(rep(1,10),rep(0,990))
e<-rnorm(500)
y<-X%*%B+e  #B is the true support
cvfit = L0Learn.cvfit(X, y, nFolds=5, seed=1, penalty="L0L2", 
                      nGamma=5, gammaMin=0.0001, gammaMax=0.1, maxSuppSize=50)
a<-lapply(cvfit$cvMeans, min)
index<-which.min(unlist(a)) 
optimalGammaIndex = index # index of the optimal gamma identified previously
optimalLambdaIndex = which.min(cvfit$cvMeans[[optimalGammaIndex]])
optimalLambda = cvfit$fit$lambda[[optimalGammaIndex]][optimalLambdaIndex]
optimalLambda
coli<-coef(cvfit, lambda=optimalLambda, gamma=cvfit$fit$gamma[index])
colSums(coli != 0)




