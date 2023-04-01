library(forecast)
library(car)
## Read in the Data

windspeed = read.csv("/Users/abrahamburton/Downloads/wind_speed_use.csv")
attach(windspeed)
head(windspeed)

## Plot the Data with markers every 24 hours

par(mar = c(5,5,1,1))
plot(hour,winds,type = "l",
     ylab = "Wind speed (m/s)",xlab = "hour",cex.lab = 1.4)

abline(v = (1:15) * 24 - 11, col = "red",lwd = 1,lty = 2)


## Plot ACF of the data


forecast::Acf(winds,main = "",cex.lab = 1.4,
              lag.max = 50,ylab = "ACF")

## Plot partial ACF of the data

#conditional dependence. shows additional unexplained correlation at 2 hours that wasn't explained in hour 1. Looks at conditional independence.
forecast::Acf(winds,main = "",cex.lab = 1.4,
              lag.max = 50,ylab = "PACF",
              type = "partial")

## ## ## ## ## ## 
## Get residuals accounting for several covariates
## ## ## ## ## ## 

resid_use = resid(lm(winds ~ solar + rh_max + airt_max))

## Plot residuals as a function of hour. Looks like there is a correlation with hour

par(mar = c(5,6,1,1))
plot(0:23,tapply(resid_use,1:(15*24) %% 24, mean),
     ylab = "avg residual",xlab = "hour of day",
     cex.lab = 1.4)

## Plot residuals as a function of wind direction

par(mar = c(5,6,1,1))
plot(seq(50,330,by  = 20),tapply(resid_use,cut(windd,breaks = seq(40,340,by = 20)),mean),
     ylab = "avg residual",xlab = "wind direction (relative to north)",
     cex.lab = 1.4)

#so time of day and wind direction should be included in the model

############################
####### Fit a model that has wind direction (converted to radians)
########################


lm_mod_simp = lm(winds ~ solar + rh_max + airt_max+ 
                   sin(windd * 2 * pi/360) + cos(windd * 2 * pi/360)) #this gives the x and y radians of wind direction

X_t_simp = model.matrix(lm_mod_simp)[,-1] #this matrix will be used for time series models

summary(lm_mod_simp)
AIC(lm_mod_simp)

## Plot fit

par(mar = c(5,5,1,1))
plot(hour,winds,type = "l",lwd = 2,
     ylab = "Wind speed (m/s)",xlab = "hour",cex.lab = 1.4)

lines(hour,predict(lm_mod_simp),col = "red",lty = 1,lwd = 2)
## Plot residuals -- If there is a pattern, we might d
## doubt the independence assumption

par(mar = c(5,5,1,1))
plot(hour,resid(lm_mod_simp),type = "l",lwd = 2,
     ylab = "Residuals (m/s)",xlab = "hour",cex.lab = 1.4)
abline(h = 0,lwd =2,col = "red",lty = 2)


## Plot ACF  -- If there is a pattern, we might d
## doubt the independence assumption. If it were ind., everything should be within blue bounds
forecast::Acf(resid(lm_mod_simp),main = "",cex.lab = 1.4,
              lag.max = 50,ylab = "Residual ACF")


############################
####################
############################
####### Fit a model that has wind direction and seasonal time
########################

lm_mod = lm(winds ~ solar + rh_max + airt_max + hour + 
              sin(hour * 2 * pi/24) + cos(hour * 2 * pi/24) + 
              sin(windd * 2 * pi/360) + cos(windd * 2 * pi/360)) #has a time of day effect and radian form

summary(lm_mod)
X_t_season = model.matrix(lm_mod)[,-1]

## Plot fit

par(mar = c(5,5,1,1))
plot(hour,winds,type = "l",lwd = 2,
     ylab = "Wind speed (m/s)",xlab = "hour",cex.lab = 1.4)

lines(hour,predict(lm_mod),col = "red",lty = 1,lwd = 2)

## Plot residuals -- If there is a pattern, we might d
## doubt the independence assumption

par(mar = c(5,5,1,1))
plot(hour,resid(lm_mod),type = "l",lwd = 2,
     ylab = "Residuals (m/s)",xlab = "hour",cex.lab = 1.4)
abline(h = 0,lwd =2,col = "red",lty = 2)


## Plot ACF  -- If there is a pattern, we might d
## doubt the independence assumption
forecast::Acf(resid(lm_mod),main = "",cex.lab = 1.4,
              lag.max = 50,ylab = "Residual ACF")

############################
#################### Fit Arima models
########################

# forecast::Arima(winds_ts,order = c(p, d, q),
#                 seasonal = list(order = c(P,D,Q),period = ???),
#                 xreg = X) X is model matrix like created above with covariate coefficients. remember to drop column of ones. period = number of time periods in the set (7 for days, 12 for months, etc.)
# p = order of AR model
# d = order of differences. If d = 0, then we have ARMA models
# q = order of MA model
####### don't use the seasonal part if you aren't using seasonal terms
#  P = order of seasonal AR, if 0, no AR terms
#  D = order of seasonal differences. if 0, no differencing (no I)
#  Q = order of seasonal AR, if zero, no MA terms
#  X = model.matrix(lm_mod)[,-1] --- don't use the column of ones
#  You won't use xreg in the homework. If you do, you must be very careful 
#  about adjusting the sine and cosine terms. Specifically, you 

winds_ts = ts(winds,frequency = 24) # says make a time series out of data.

###### ###### ###### ###### ###### 
###### AR(1)
###### ###### ###### ###### ###### 


ar1_simp = forecast::Arima(winds_ts,order = c(1, 0, 0),
                           xreg = X_t_season) # AR(1) from the (1,0,0)

###### plot fit

par(mar = c(5,5,1,1))
plot(hour,winds,type = "l",lwd = 2,
     ylab = "Wind speed (m/s)",xlab = "hour",cex.lab = 1.4)

lines(hour,ar1_simp$fitted,col = "red",lty = 1,lwd = 2)

###### plot ACF

Acf(ar1_simp$residuals,ylab = "ACF of Residuals",cex.lab = 1.4)



###### ###### ###### ###### ###### 
###### MA(1)
###### ###### ###### ###### ###### 

ma1_simp = forecast::Arima(winds_ts,order = c(0, 0, 1),
                           xreg = X_t_season)

par(mar = c(5,5,1,1))
plot(hour,winds,type = "l",lwd = 2,
     ylab = "Wind speed (m/s)",xlab = "hour",cex.lab = 1.4)

lines(hour,ma1_simp$fitted,col = "red",lty = 1,lwd = 2)

Acf(ma1_simp$residuals,ylab = "ACF of Residuals",cex.lab = 1.4)


###### ###### ###### ###### ###### 
##### ARMA(1,1)
###### ###### ###### ###### ###### 

arma1_simp = forecast::Arima(winds_ts,order = c(1, 0, 1),
                             xreg = X_t_season)



### plot the fit

par(mar = c(5,5,1,1))
plot(hour,winds,type = "l",lwd = 2,
     ylab = "Wind speed (m/s)",xlab = "hour",cex.lab = 1.4)

lines(hour,arma1_simp$fitted,col = "red",lty = 1,lwd = 2)

### ACF of residuals

Acf(arma1_simp$residuals,ylab = "ACF of Residuals",cex.lab = 1.4)



###### ###### ###### ###### ###### 
##### ARIMA(1,1,1)
###### ###### ###### ###### ###### 

arima1_simp = forecast::Arima(winds_ts,order = c(1, 1, 1),
                              xreg = X_t_season)

### plot the fit

par(mar = c(5,5,1,1))
plot(hour,winds,type = "l",lwd = 2,
     ylab = "Wind speed (m/s)",xlab = "hour",cex.lab = 1.4)

lines(hour,arima1_simp$fitted,col = "red",lty = 1,lwd = 2)

### ACF of residuals

Acf(arima1_simp$residuals,ylab = "ACF of Residuals",cex.lab = 1.4)


##### SARIMA(1,1,1)

# arima1_simp = forecast::Arima(winds_ts,order = c(1, 1, 1),
#                                 seasonal = list(order = c(1,1,1),period = 24),
#                               xreg = X_t_season)

# We drop the trig covariates for time. 

sarima1_simp = forecast::Arima(winds_ts,order = c(1, 1, 1),
                              seasonal = list(order = c(1,1,1),period = 24),
                              xreg = X_t_simp)

### plot the fit.

par(mar = c(5,5,1,1))
plot(hour,winds,type = "l",lwd = 2,
     ylab = "Wind speed (m/s)",xlab = "hour",cex.lab = 1.4)

lines(hour,sarima1_simp$fitted,col = "red",lty = 1,lwd = 2)

### ACF of residuals

Acf(sarima1_simp$residuals,ylab = "ACF of Residuals",cex.lab = 1.4)


##### ##### ##### ##### ##### ##### ##### ##### 
##### ##### ##### ##### ##### ##### 

## Find the best model BIC
BIC(lm_mod)
ar1_simp$bic
ma1_simp$bic
arma1_simp$bic
arima1_simp$bic
sarima1_simp$bic

## Find the best model AIC
AIC(lm_mod)
ar1_simp$aic
ma1_simp$aic
arma1_simp$aic
arima1_simp$aic
sarima1_simp$aic

## ## ## ## ## ## ## ## 
## Check Assumptions
## ## ## ## ## ## ## ## 

## ## ## ## ## independence
Acf(sarima1_simp$residuals)

## ## ## ## ## Normality of residuals

hist(sarima1_simp$residuals/sd(sarima1_simp$residuals),
     freq = FALSE,breaks = 20)
curve(dnorm(x),from = -4,to = 4,add = TRUE)

qqnorm(sarima1_simp$residuals[-c(1:24)]/sd(sarima1_simp$residuals[-c(1:24)]))
abline(0,1,col = "red",lwd = 2,lty = 2)


## ## ## ## ## ## ## ## 
## Run look at forecasts for the last day (24 hours)
## Forcast Forward several hours
## ## ## ## ## ## ## ## 


## Subset the data

winds.test = winds_ts[-(1:(14 * 24))]
winds.train = winds_ts[1:(14 * 24)]

## Fit model to subset of the data

train.mod = forecast::Arima(winds.train,
                            order = c(1, 1, 1),
                            seasonal = list(order = c(1,1,1),period = 24),
                            xreg = X_t_simp[1:(14 * 24),])

## Forecast
#h = steps ahead

pred = forecast::forecast(train.mod,h = 24,
                          xreg = X_t_simp[-(1:(14 * 24)),])

## plot predictions. These predictions cheat because you wouldn't know covariate values 24 hours into the future. Which is whay lots of TS models are done w/o covariates

par(mar = c(4,4,1,1))
plot(pred,xlab = "Hour",ylab = "Wind Speed",cex.lab = 1.4,
     ylim = c(-3,15),xlim = c(275,360))

## calculate errors/bias

sqrt(mean((winds.test - pred$mean)^2)) ### RPMSE
mean(pred$mean - winds.test)           ### bias


