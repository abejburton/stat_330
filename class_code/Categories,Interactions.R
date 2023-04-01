library(ggplot2)
library(car)
library(lmtest)
library(normtest)
library(MASS)

## Read in Salary Data
knitr::opts_knit$set(root.dir ="/Users/abrahamburton/Downloads" )
salary = read.csv("Salary.csv",header=TRUE, stringsAsFactors = TRUE)
names(salary)
head(salary)

## Relabel the Education factor levels so HS is baseline
levels(salary$Education)
salary$Education = relevel(salary$Education,ref = 'HS') #This line and the one below do the same thing
salary$Education = factor(salary$Education,levels=c("HS","BS","BS+")) # this reorders the levels to make HS the reference group. Fit is the same
levels(salary$Education)

levels(salary$Manager)

factor(salary$Experience)

## Boxplots

boxplot(Salary~Education,data=salary)

boxplot(Salary~Manager,data=salary)

plot(Salary~Experience,data=salary,pch=19)

with(salary,tapply(Salary,Manager,mean)) #look at salary by manager and calculate the mean

ggplot(salary,aes(y=Salary,x=Education,fill = Education)) +
  geom_boxplot()

## Exploratory Plots of Education with Colored Dots

ed.colors = c("red","blue","black")

plot(salary$Experience,salary$Salary,
     col=ed.colors[salary$Education],pch=19,
     xlab="Experience",ylab="Salary")

legend("bottomright",
       legend=levels(salary$Education),
       col=ed.colors,pch=19,cex = 0.8)

#gives all combinations of education and manager. remove one of those to only color code one var.

ggplot(salary,aes(y=Salary,x=Experience,color=Education:Manager))+
  geom_point() +
  geom_smooth(method="lm",se=FALSE)

## Exploratory Plots of Manager with Colored Dots

man.colors = c("firebrick","blue","black",
               "green","cyan","purple")

plot(salary$Experience,salary$Salary,
     col=man.colors[salary$Manager:salary$Education],pch=19,
     xlab="Experience",ylab="Salary")

legend("bottomright",
       legend=levels(salary$Manager:salary$Education),
       col=man.colors,pch=19,ncol = 2)

## Fit a Model with Education/Manager Interaction

sal.lm = lm(formula = Salary~Experience+Education+Manager+Education:Manager,
            data=salary)

#r automatically will turn ed and manager into dummy variables 

summary(sal.lm)

sal.lm = lm(formula = Salary~Experience+Education*Manager,data=salary) # this * creates a main effect for both variables and the interaction

summary(sal.lm)


################ ################ ################ 
################ Look at assumptions
################ ################ ################ 

######### linearity

avPlots(sal.lm)

######### linearity, indep, and equal variance

plot(sal.lm$fitted.values,sal.lm$residuals)
abline(h = 0,col = "red",lty = 2)


bptest(sal.lm)

######### normality

hist(stdres(sal.lm),freq = FALSE,ylim = c(0,.4))
curve(dnorm,from = -3,to = 3,add = TRUE)

qqnorm(stdres(sal.lm))
abline(a = 0,b=1,col = "red",lty = 2)

jb.norm.test(stdres(sal.lm))
ks.test(stdres(sal.lm),"pnorm")


#################### Design Matrix


X.mat = model.matrix(sal.lm)

head(X.mat)

#this gives the X matrix of the regression in the lm


# head(X.mat[,colnames(X.mat) != "EducationBS:ManagerYes"])
# 
# 
# d = X.mat[,colnames(X.mat) != "EducationBS:ManagerYes"]
# lm(salary$Salary ~ d[,-1])


## F-test for overall regression
summary(sal.lm)

## F-test for test of interaction terms

noint.lm = lm(Salary~Experience+Education+Manager,data=salary)

anova(noint.lm,sal.lm)

## F-test for education x experience interations

exp.edu.int.lm = lm(formula = Salary~Education*Experience+Education*Manager,data=salary)

summary(exp.edu.int.lm)

anova(sal.lm,exp.edu.int.lm)



noman.lm = lm(Salary~Experience+Education,data=salary)

anova(noman.lm,noint.lm,sal.lm)


anova(sal.lm,noint.lm)

## T-tests for individual effects
summary(sal.lm)

## Confidence Intervals
confint(sal.lm)

## Plot Fitted Regression lines. GGPLOT does this way easier (code at the beginning)
plot(Salary~Experience,ylim = c(1e4,4e4),
     data=salary,pch=19)
abline(a=sal.lm$coef[1],b=sal.lm$coef[2],col="black") #HS, No
abline(a=sal.lm$coef[1]+sal.lm$coef[3]+sal.lm$coef[5]+
         sal.lm$coef[6],b=sal.lm$coef[2],col="green") #BS, Yes

abline(a=sal.lm$coef[1]+sal.lm$coef[4],b=sal.lm$coef[2],col="purple") #BS+, No
abline(a=sal.lm$coef[1]+sal.lm$coef[3],b=sal.lm$coef[2],col="red") #BS, No
abline(a=sal.lm$coef[1]+sal.lm$coef[5],b=sal.lm$coef[2],col="orange") #HS, Yes
abline(a=sal.lm$coef[1]+sal.lm$coef[4]+sal.lm$coef[5]+sal.lm$coef[7],
       b=sal.lm$coef[2],col="cyan") #BS+, Yes
legend("topleft",c("HS,NO","HS,Yes","BS,NO","BS,Yes","BS+,NO","BS+,Yes"),
       col = c("black","orange","red","green","purple","cyan"),
       lwd = 4,lty = 1)




