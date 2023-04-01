################################
## R Code for Climate Dataset ##
################################

## Load libraries and data
library(MASS)
knitr::opts_knit$set(root.dir ="/Users/abrahamburton/Downloads" )
climate <- read.table("/Users/abrahamburton/Downloads/Climate.txt",header=TRUE)

n = nrow(climate)

plot(climate$co2,climate$globaltemp,pch=19)
plot(climate$year,climate$co2,pch=19)

plot(climate$year,climate$globaltemp,pch=19)


plot(climate$co2,climate$globaltemp,pch=19)

## Fit a Linear model

climate.lm <- lm(globaltemp~co2,data=climate)
summary(climate.lm)

plot(climate.lm$fitted.values,climate.lm$residuals,pch=19)
abline(0,0,col = "red",lwd = 2,lty = 2)

#time trend
plot(climate$year,climate.lm$residuals,pch=19)
abline(0,0,col = "red",lwd = 2,lty = 2)


hist(stdres(climate.lm),freq = FALSE,breaks = 20)
curve(dnorm,from = -3,to = 3,col = "cornflowerblue",lwd = 2,
      lty = 2,add = TRUE)

qqnorm(stdres(climate.lm))
abline(0,1,col = "red",lwd =2,lty = 2)

#acf(climate$globaltemp)
#acf(climate.lm$residuals)

## Build a Conf. Int for the slope
confint(climate.lm,level=0.90,parm="co2")
confint(climate.lm,level=0.95,parm="co2")
confint(climate.lm,level=0.99,parm="co2")
confint(climate.lm,level=0.9999,parm="co2")


confint(climate.lm) # automatically gives 95% for all parameters

#manual confidence interval
coef(climate.lm)[2]+c(-1,1)*qt(0.975, df=n-2)*summary(climate.lm)$coef[2,2]

## Conduct tests of hypothesis on slope and intercept
summary(climate.lm) ## Two-sided
t.stat <- (climate.lm$coef[2]-0)/summary(climate.lm)$coef[2,2]
p.value <- 2 * (1 - pt(t.stat,df=nrow(climate)-2))

t.stat
p.value


## Calculate confidence interval for mean (fitted line)
pred.conf <- predict.lm(climate.lm,newdata=data.frame(co2=360),
                   interval="confidence",level=0.95)

pred.conf

## Calculate prediction intervals  - one observation
pred.pred <- predict.lm(climate.lm,newdata=data.frame(co2=360),
                   interval="prediction",level=0.95)
pred.pred
## Calculate prediction intervals  - for a lot of possible values. difference is in the standard error formula. predicted adds a 1
all.preds <- predict.lm(climate.lm,
                        newdata=data.frame(co2=seq(315,400,length=100)),
                        interval="prediction",level=0.95)
head(cbind(seq(315,400,length=100),all.preds))


plot(climate$co2,climate$globaltemp,pch=19)

abline(a=climate.lm$coef[1],b=climate.lm$coef[2],
       col="red",lwd=3)
lines(seq(315,400,length=100),all.preds[,2],
      col="green",lty=2) ##Plot Lower bound
lines(seq(315,400,length=100),all.preds[,3],
      col="green",lty=2) ##Plot Upper bound

## Cross Validation Excercise (calculate coverage)

n.test <- round(0.2 * n)
cv.reps = 1000

coverage = numeric(cv.reps)
width = numeric(cv.reps)
PRMSE = numeric(cv.reps)
bias = numeric(cv.reps)

for(i in 1:cv.reps){
  
  test.obs <- sample(1:n,n.test)
  test.data <- climate[test.obs,]
  train.data <- climate[-test.obs,]
  train.lm <- lm(globaltemp~co2,data=train.data)
  test.preds <- predict.lm(train.lm,newdata=test.data,
                           interval="prediction",level = 0.95)
  coverage[i] <- mean(test.data$globaltemp > test.preds[,2] & 
                        test.data$globaltemp < test.preds[,3])
  width[i] = mean(test.preds[,3] - test.preds[,2])
  bias[i] = mean(test.preds[,1] - test.data$globaltemp)
  PRMSE[i] = sqrt(mean((test.preds[,1] - test.data$globaltemp)^2))
  
}

mean(coverage)   ### compare with 0.95
mean(width)
mean(bias)       
mean(PRMSE)

range(climate$globaltemp)
sd(climate$globaltemp)




## Fit a centered LM
co2.cntr <- climate$co2-mean(climate$co2)

## the scale function (by default) subtracts off the mean and divides by standard deviation
co2.cntr2 <- scale(climate$co2,scale = FALSE)  

climate.lm.cntr <- lm(globaltemp~co2.cntr,data=climate)
summary(climate.lm.cntr)
confint(climate.lm.cntr)

## Predict using the centered model
climate.lm.cntr$coef[1]+climate.lm.cntr$coef[2]*(360-mean(climate$co2))


### original model
predict.lm(climate.lm,newdata=data.frame(co2=360))




