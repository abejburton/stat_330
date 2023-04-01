library(ISLR)
library(ggplot2)
library(splines)
library(xtable)

data("Wage")
#

data_wage = Wage[,c(1,2,4,5,7,11)]


head(data_wage)

attach(data_wage)
summary(data_wage)

hist(wage)
hist(log(wage))


# box plot for race

ggplot(data_wage) + geom_boxplot(aes(x = race,y = wage,col = race))

# box plot for education

ggplot(data_wage) + geom_boxplot(aes(x = education,y = wage,col = education))

# box plot for job class

ggplot(data_wage) + geom_boxplot(aes(x = jobclass,y = wage,col = jobclass))

# scatter age and wage

ggplot(data_wage) + geom_point(aes(x = age,y = wage)) + 
  geom_smooth(aes(x = age,y = wage))


ggplot(data_wage) + geom_point(aes(x = age,y = wage)) + 
  geom_smooth(aes(x = age,y = wage,group = education, col = education),size = 1.5)

# scatter age with linear model fit

ggplot(data_wage) + geom_point(aes(x = age,y = wage)) + 
  geom_smooth(aes(x = age,y = wage),method = "lm")


lm_prelim = lm(wage ~.,data = data_wage)
summary(lm_prelim)

######## prediction data

dat_new = data_wage
dat_new$year = median(data_wage$year)
dat_new$race = "1. White"
dat_new$education = "2. HS Grad"
dat_new$jobclass = "2. Information"


idx_plot = which(data_wage$race == "1. White" &
                   data_wage$education == "2. HS Grad" &
                   data_wage$jobclass == "2. Information")

dat_sub = data_wage[idx_plot,]

################# Step-wise functions models


lm_step = lm(wage ~ year + race + education + jobclass + 
               I(age >= 25 & age < 35) + 
               I(age >= 35 & age < 55) + 
               I(age >= 55 & age < 65) + 
               I(age >= 65) ,data = data_wage)

summary(lm_step)

anova(lm_prelim,lm_step)

pred = predict(lm_step,newdata = dat_new,interval = "confidence")

age_pred = data.frame(age = sort(unique(data_wage$age)), pred = 
                        tapply(pred[,1],data_wage$age,mean),
                      upper = tapply(pred[,2],data_wage$age,mean),
                      lower = tapply(pred[,3],data_wage$age,mean))

ggplot(dat_sub) + geom_point(dat = dat_sub, aes(x = age,y = wage)) + 
  geom_line(data = age_pred,aes(x = age,y = pred),size = 1, color = "blue") +
  geom_line(data = age_pred,aes(x = age,y = upper),size = 1, color = "red") + 
  geom_line(data = age_pred,aes(x = age,y = lower),size = 1, color = "red")


################# polynomial models

lm_poly2 = lm(wage ~ year + race + education + jobclass + 
                poly(age,2),data = data_wage)

lm_poly3 = lm(wage ~ year + race + education + jobclass + 
                poly(age,3),data = data_wage)

lm_poly4 = lm(wage ~ year + race + education + jobclass + 
                poly(age,4),data = data_wage)

anova(lm_poly2,lm_poly3,lm_poly4)

summary(lm_poly3)

pred = predict(lm_poly4,newdata = dat_new,interval = "confidence")

age_pred = data.frame(age = sort(unique(data_wage$age)), pred = 
                        tapply(pred[,1],data_wage$age,mean),
                      upper = tapply(pred[,2],data_wage$age,mean),
                      lower = tapply(pred[,3],data_wage$age,mean))

ggplot(dat_sub) + geom_point(dat = dat_sub, aes(x = age,y = wage)) + 
  geom_line(data = age_pred,aes(x = age,y = pred),size = 1, color = "blue") +
  geom_line(data = age_pred,aes(x = age,y = upper),size = 1, color = "red") + 
  geom_line(data = age_pred,aes(x = age,y = lower),size = 1, color = "red")

################# spline models

# ns() -- natural spline
# bs() -- b-spline


age_use = 1:100
wage_use = 1:100

X = model.matrix(wage_use ~ 0 + bs(age_use,knots = c(30,60),degree = 1))

head(X)


lm_spline_linear = lm(wage ~ year + race + education + jobclass + 
                        bs(age,knots = quantile(age,c(0.5)), degree = 1),
                      data = data_wage)

lm_spline_quadratic = lm(wage ~ year + race + education + jobclass + 
                           bs(age,knots = quantile(age,c(0.5)), degree = 2),
                         data = data_wage)


lm_spline_cubic = lm(wage ~ year + race + education + jobclass + 
                       bs(age,knots = quantile(age,c(0.5)), degree = 3),
                     data = data_wage)


AIC(lm_poly)
AIC(lm_spline_linear)
AIC(lm_spline_quadratic)
AIC(lm_spline_cubic)


pred = predict(lm_spline_cubic,newdata = dat_new,interval = "confidence")

age_pred = data.frame(age = sort(unique(data_wage$age)), 
                      pred = tapply(pred[,1],data_wage$age,mean),
                      upper = tapply(pred[,2],data_wage$age,mean) ,
                      lower = tapply(pred[,3],data_wage$age,mean) )

ggplot(dat_sub) + geom_point(dat = dat_sub, aes(x = age,y = wage)) + 
  geom_line(data = age_pred,aes(x = age,y = pred),size = 1, color = "blue") +
  geom_line(data = age_pred,aes(x = age,y = upper),size = 1, color = "red") + 
  geom_line(data = age_pred,aes(x = age,y = lower),size = 1, color = "red")


interaction_poly = lm(wage ~ year + race + jobclass + 
                        education * poly(age,degree = 3),
                      data = data_wage)

interaction_spline = lm(wage ~ year + race + jobclass + 
                          education * bs(age,knots = quantile(age,c(0.5)), degree = 2),
                        data = data_wage)


AIC(lm_spline_quadratic)
AIC(interaction_poly)
AIC(interaction_spline)


#create data for prediction

data_wage_pred = data_wage
data_wage_pred$race = "1. White"
data_wage_pred$jobclass = "2. Information"
data_wage_pred$year = mean(data_wage_pred$year)

pred = predict(interaction_spline,newdata = data_wage_pred)

library(reshape2)

#what does this do? something with predictions
age_pred_block = melt(tapply(pred,list(data_wage$age,data_wage$education),mean))
age_pred_block = age_pred_block[complete.cases(age_pred_block),]
colnames(age_pred_block) = c("age","eduction","wage")


ggplot(data_wage) + geom_point(dat = data_wage, aes(x = age,y = wage)) + 
  geom_line(data = age_pred_block,aes(x = age,y = wage,group = eduction,color = eduction),size = 1.5) 
  
  
  
  
  
  ################# spline with fake data 
  
  x = sort(c(0,runif(798,0.1,0.9),1))
X_mat = bs(x,knots = 0.5, degree = 3)
beta0 = 0
beta = c(-5,4,-7,5)
sig = 0.5

y = beta0 + X_mat  %*% beta + rnorm(200,0,sig)

plot(x,y,pch = 20)


spline_mod = lm(y ~ X_mat)

predict(spline_mod,interval = "confidence")

pred = as.data.frame(predict(spline_mod,interval = "confidence"))
pred$x = x
dat_fake = data.frame(x = x,y = y)

ggplot(dat_fake) + geom_point(dat = dat_fake, aes(x = x,y = y)) + 
  geom_line(data = pred,aes(x = x,y = fit),size = 1, color = "blue") +
  geom_line(data = pred,aes(x = x,y = lwr),size = 1, color = "red") + 
  geom_line(data = pred,aes(x = x,y =  upr),size = 1, color = "red")

