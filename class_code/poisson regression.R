library(bestglm)
library(ggplot2)
library(tidyverse)
library(car)

dat = read.csv("/Users/abrahamburton/Downloads/visits_data.csv", stringsAsFactors = TRUE)
dat$health = factor(dat$health,levels = c("poor","average","excellent"))
dat$region = relevel(dat$region,ref = "other")
hist(dat$visits)

#create data where you create mean visits bins and then plot them against covariates.
dat_plot = dat %>% 
  group_by(chronic) %>% 
  summarize(mean_visits = mean(visits),n =n())

ggplot(dat_plot,aes(x = chronic,y = log(mean_visits) )) + 
  geom_point(aes(size = n))


dat_plot = dat %>% 
  group_by(age) %>% 
  summarize(mean_visits = mean(visits),n =n())

ggplot(dat_plot,aes(x = age,y = log(mean_visits) )) +
  geom_point(aes(size = n))

quantile(dat$age)

dat_plot = dat %>% 
  group_by(school) %>% 
  summarize(mean_visits = mean(visits),n =n())

ggplot(dat_plot,aes(x = school,y = log(mean_visits) )) +
  geom_point(aes(size = n))


#+1 because log of 0 doesn't work
ggplot(dat,aes(x = health,y = log(visits+1) )) + geom_boxplot() 
ggplot(dat,aes(x = gender,y = log(visits+1))) + geom_boxplot() 
ggplot(dat,aes(x = insurance,y = log(visits+1))) + geom_boxplot() 
ggplot(dat,aes(x = medicaid,y = log(visits+1))) + geom_boxplot() 
ggplot(dat,aes(x = adl,y = log(visits+1))) + geom_boxplot() 
ggplot(dat,aes(x = region,y = log(visits+1))) + geom_boxplot() 



###############
# variable selection includes all covariates. 
#to load rdata do load("name.RData")
all_mod = bestglm(dat,family = poisson,method = "seqrep",IC = "AIC")
all_mod = bestglm(dat,family = poisson,method = "seqrep",IC = "BIC")
all_mod = bestglm(dat,family = poisson,method = "forward",IC = "LOOCV")
# CV Doesn't work for data with more than two levels. This is because every variable is a yes or no decision, so that's why it doesn't work.


pois_mod = glm(visits ~.,data = dat,family = poisson)
summary(pois_mod)
avPlots(pois_mod,terms = ~ chronic)
avPlots(pois_mod,terms = ~ health)

beta_hat = coef(pois_mod)
beta_hat
exp(beta_hat) #in terms of mu
100 * (exp(beta_hat)-1) #percent change

CI = confint(pois_mod)

CI
exp(CI)

100* (exp(CI) -1)

100*(exp(coef(pois_mod))-1)


############# ############# ############# ############# 
############# Let's get confidence intervals
############# ############# ############# ############# 

dat_pred = dat[4406,]

############# ############# ############# Best approach



pred = predict.glm(pois_mod, newdata=dat_pred,se.fit=TRUE)
pred$fit
CI_log = pred$fit + c(-1,1) * qnorm(1 - 0.05 / 2) * pred$se.fit #95% confident that the mean of people with shared attributes of this individual is in this interval
exp(CI_log)

exp(CI_log)


############# ############# ############# worse approach because the CI is calculated at response level, not log mean

pred2 = predict.glm(pois_mod, newdata=dat_pred,
                   type="response", se.fit=TRUE) #type = respose exponentiates results automatically
pred2$fit
pred2$fit + c(-1,1) * qnorm(1 - 0.05 / 2) * pred2$se.fit 


##### we have a lot of data, so it doesn't matter.

#monte carlo simulation ci
nsim = 1e5
log_mu_sim = rnorm(nsim,pred$fit,pred$se.fit)
sim_counts = rpois(nsim,exp(log_mu_sim))
quantile(sim_counts,c(.025,.975))
hist(sim_counts)

