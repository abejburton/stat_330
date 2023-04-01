##
## Run an Analysis on Advertising Data
##
## Load in some libraries needed
#install.packages(c("ggplot2","lmtest","normtest"))

knitr::opts_knit$set(root.dir ="/Users/abrahamburton/Downloads" )
ad <- read.csv("/Users/abrahamburton/Downloads/Advertising.csv", header = T, stringsAsFactors = F)

library(ggplot2) 
library(MASS)
library(lmtest)
library(normtest)


## Exploratory Analyses

plot(ad$P,ad$R,pch=19,xlab="Pages",ylab="Revenue")
with(ad,plot(P,R,pch=19,xlab="Pages",ylab="Revenue"))
scatter.smooth(ad$P,ad$R)

ggplot(data = ad) + geom_point(aes(x = P, y = R),col ="red") + 
  geom_smooth(aes(P,R))

cor(ad$P,ad$R)

## Fit a Linear Model to Data
ad_lm = lm(R~P,data=ad)
summary(ad_lm)

## Check Linear & Equal Variance Assumption 

plot(ad_lm$fitted.values,ad_lm$residuals,
     pch=20,ylim=c(-30,30))

abline(a=0,b=0,lwd=2,col = "red",lty = 3)
abline(h = 0,lwd=2,col = "red",lty = 2)
# basically visually checks zero conditional mean assumption. checks against y hat ^ and below checks against covariates

plot(ad$P,ad_lm$residuals,
     pch=20,ylim=c(-30,30))
abline(a=0,b=0,lwd=2,col = "red",lty = 3)
abline(h = 0,lwd=2,col = "red",lty = 2)

lmtest::bptest(ad_lm) ## Breusch-Pagan test for heteroskedasticity the lmtest:: calls the package then the function. ut just clarifies where the fn comes from if you need

## Check Normality Assumption

std_res = MASS::stdres(ad_lm) ## This is accounting for more than just sigma. standardized residuals
std_res

hist(std_res)

#calculates porportion of data rather than counts on the y axis. this lets you put a density curve on the plot
hist(std_res,freq = FALSE)
curve(dnorm,from = -4,to = 4,add = TRUE,
      col = "green")
#add bins

hist(std_res,freq = FALSE,breaks = 15)
curve(dnorm,from = -4,to = 4,add = TRUE,
      col = "green")

ggplot() + geom_density(aes(x=std_res))
ggplot() + geom_histogram(aes(x=std_res))

#qq plot. better tool to assess normality than histogram. observed residuals/standard errors compared to the theoretical line from a normal distr.
qqnorm(std_res,pch=20)
abline(a=0,b=1)

### not normal (fake data) from a t distribution but with low df so lots of variance

set.seed(2)
exp.vars = scale(rt(37,df = 3))
qqnorm(exp.vars,pch=20,main = "Not Normal Q-Q Plot")
abline(a=0,b=1)

### actually normal (fake data)

exp.vars = scale(rnorm(37))
qqnorm(exp.vars,pch=20,main = "Not Normal Q-Q Plot")
abline(a=0,b=1)


ks.test(std_res,"pnorm") # Kolmogorov-Smirnov testj= works for any distr. but is week. ho normal
normtest::jb.norm.test(std_res)  #Jarque-Bera test h0 data skewness= normal skew and kurtosis = normal kurtosis, ha at least one isnt =. stricter

## Check to see if there are outliers

cd = cooks.distance(ad_lm)
plot(cd,type="h")
outliers = which(cd>(4/nrow(ad)))
ad[outliers,]

plot(ad_lm)
## Plot log-transformed data

plot(log(ad$P),log(ad$R),pch=19,xlab="log(Pages)",ylab="log(Revenue")
ggplot(ad,aes(x=log(P),y=log(R)))+geom_point()


## Fit a log-transformed SLR Model
trans_lm = lm(log(R)~log(P),data=ad)
summary(ad_lm)$r.squared. ## R^2 of original model
summary(trans_lm) ## be careful in interpreting these coefficients since we transformed the data
std_res = stdres(trans_lm)
ggplot() + geom_density(aes(x=std_res))
ks.test(std_res,"pnorm")
normtest::jb.norm.test(std_res)
bptest(trans_lm)

## Plot Fitted Regression line on transformed scale
plot(log(ad$P),log(ad$R),pch=19,xlab="log(Pages)",ylab="log(Rev)")
abline(a=trans_lm$coef[1],b=trans_lm$coef[2],lwd=3,col="red")

## Plot the transformed regression model on original scale of the data#######
plot(ad$P,ad$R,pch=19,xlab="Pages",ylab="Revenue")
pred_pages = seq(min(ad$P),max(ad$P),length=100) ## Sequence (seq) of Pages of Advertising that I'm interested in predicting revenue    
preds_trans = trans_lm$coef[1]+trans_lm$coef[2]*log(pred.pages) ## Prediction of log(Rev)
preds_orig = exp(preds_trans) ## Predictions of Revenue
lines(pred_pages,preds_orig,lwd=3,col="red") ## Draw "line" on original scale of data

preds = data.frame(P=pred_pages,R=preds_orig)
ggplot(data=ad,aes(x=P,y=R))+geom_point()+
  geom_line(data=preds,aes(x=P,y=R),color="red",inherit.aes=FALSE)




## Another way to plot the line on the original scale of the data

my.model = function(x){
  exp(trans_lm$coef[1]+trans_lm$coef[2]*log(x))
}
plot(my.model,from=0,to=25,lwd=3,col="red")
points(ad$P,ad$R,pch=19)

## Check assumptions of transformed model
hist(stdres(trans_lm),freq = FALSE) #Normality OK
curve(dnorm,from = -4,to = 4,add = TRUE)

plot(trans_lm$fitted.values,trans_lm$residuals,pch=19) ## Equal Variacne OK
abline(a=0,b=0)

## Assess the fit (via R^2)
summary(trans_lm)$r.squared ## R^2 is bigger than untransformed model
summary(ad_lm)$r.squared




## Assess predictive ability of the model (via cross validation) #######


set.seed(1)
n_test = 4
n_cv = 1000
bias_log = numeric(n_cv)
rpmse_log = numeric(n_cv)

for(i in 1:n_cv){
  test_obs = sample(1:nrow(ad),n_test) #randomly select without replacement
  test_ad = ad[test_obs,] #subset data by the observations you chose
  train_ad = ad[-test_obs,] #subsets all data not in sample
  train_lm = lm(log(R)~log(P),data=train_ad) #predict R based on P with the test data
  test_preds = exp(predict.lm(train_lm,newdata=test_ad))
  bias_log[i] = mean((test_preds-test_ad$R))
  rpmse_log[i] = sqrt(mean((test_preds-test_ad$R)^2))
}

mean(bias_log)
mean(rpmse_log)
range(ad$R)
sd(ad$R)

## Original scale
set.seed(1)
n_test = 4
n_cv = 1000
bias = rep(NA,n_cv)
rpmse = rep(NA,n_cv)

for(cv in 1:n_cv){
  test_obs = sample(1:nrow(ad), n_test)
  test_ad = ad[test_obs,]
  train_ad = ad[-test_obs,]
  train_lm = lm(R ~ P, data=train_ad)
  test_preds = predict.lm(train_lm,newdata=test_ad)
  bias[cv] = mean(test_preds-test_ad$R)
  rpmse[cv] = sqrt(mean((test_preds-test_ad$R)^2))
}


mean(bias_log)
mean(rpmse_log)
mean(bias)
mean(rpmse)
range(ad$R)
sd(ad$R)







