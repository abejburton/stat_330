library(MASS) #stdres
library(car) ## addedvariable plots
library(normtest) #jb.norm.test
library(lmtest) #bptest

knitr::opts_knit$set(root.dir ="/Users/abrahamburton/Downloads" )
## Read in Supervisor Data
super = read.table("supervisor.txt",header=TRUE)
names(super)

## Explore the Data
plot(super,cex=.5,pch=20) #pairs and plot do the same thing. Pairs can be good for non quantitative vars.
pairs(super)

pairs(super[,c(1,2:4)]) #subset to only get some of the graphs
cor(super) # corr matrix
cov(super) # cov var matrix
cor(super)[1,] #subset
round(cov(super),2) #round for cleaner look

## Fit a MLR Model
super.lm = lm(Rating~Complaints+Privileges+Learn+Raises+Critical+Advance,data=super)
super.lm = lm(Rating~.,data=super) # the period takes all other variables as covariate
summary(super.lm)

## Check model assumptions

## Added variable plot to check linearity assumption
avPlots(super.lm) ##each of these should be linear. residuals :Y axis . y~ all x but x_p (part of response not explained by another covariate) x axis. xp~all x but x_pPlots residuals (part of x not explaned by other covariates)

## equal variance

plot(super.lm$fitted.values,super.lm$residuals,pch=19)
abline(a=0,b=0)

#you can also plot residuals against specific vars.
plot(super$Critical,super.lm$residuals)
abline(a=0,b=0)

bptest(super.lm)

## Normality



hist(stdres(super.lm))

qqnorm(stdres(super.lm))
abline(0,1)


jb.norm.test(stdres(super.lm),nrepl = 1e4)
ks.test(stdres(super.lm),"pnorm")


## Predict for a new supervisor where 
##Complaints=100,Privileges=0,Learn=100,Raises=100,Critical=100,Advance=0. Extrapolating bc values are outside of the range of the data.

predict.lm(super.lm,newdata=data.frame(Complaints=100,Privileges=0,Learn=100,
                                       Raises=100,Critical=100,Advance=0))
apply(super,2,range) #find range of dataframe super for all columns (2) versus rows (1)

## Perform a a series of cross-validation studies
n.test = 4
n.cv = 1e4
bias = numeric(n.cv)
rpmse = numeric(n.cv)
coverage = numeric(n.cv)
width = numeric(n.cv)

set.seed(3)

for(i in 1:n.cv){
  test.obs = sample(1:30,n.test)
  super.test = super[test.obs,]
  super.train = super[-test.obs,]
  train.lm = lm(Rating~.,data=super.train)
  preds = predict.lm(train.lm,newdata=super.test,interval = "prediction")
  bias[i] = mean(preds[,1]-super.test$Rating)
  rpmse[i] = sqrt(mean((preds[,1]-super.test$Rating)^2))
  width[i] = mean(preds[,3]-preds[,2])
  coverage[i] = mean(preds[,2] < super.test$Rating & preds[,3] > super.test$Rating)
  
}



mean(bias)
mean(rpmse)
mean(coverage)
mean(width)

range(super$Rating)
sd(super$Rating)
