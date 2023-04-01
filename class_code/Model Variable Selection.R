library(car)
library(MuMIn)
library(bestglm)
library(bestglm) ##You can also use regsubsets() in library(leaps) but I like this one


#########################################
## Read in EI Data and Make Transforms ##
#########################################
knitr::opts_knit$set(root.dir ="/Users/abrahamburton/Downloads" )
ei = read.table("/Users/abrahamburton/Downloads/envimpact.txt",header=TRUE)
log.ei = ei

log.ei[,12:14] = log(log.ei[,12:14])
colnames(log.ei)[12:14] = c("log.Hydro","log.Nit","log.SO2")
head(log.ei)



round(cor(log.ei),2)

### eigenvalues

X = as.matrix(log.ei[,-16])

ei.eigen = eigen(t(cbind(scale(X))) %*% cbind(scale(X)))

ei.eigen$values

# proportion of variance explained by each eigenvector

ei.eigen$values / sum(ei.eigen$values)



eigen(cor(log.ei[,-1]))$values / sum(eigen(cor(log.ei[,-1]))$values)


##################
## Look at VIFs ##
##################

lm.model = lm(AAMort~.,data = log.ei)

car::vif(lm(AAMort~.,data=log.ei))

vif <- car::vif(lm(AAMort~.,data=log.ei))
vif[1]
with(log.ei,plot(log.Hydro,log.Nit,pch=19))


###################################
## Perform Best Subset Selection ##
###################################

# library(MuMIn)
# full.model = lm(AAMort~., data=log.ei, na.action="na.fail")
# var.selection = dredge(full.model, rank="BIC") #Quite slow

#############################################
## Perform Best Subset Selection using BIC ## when you do this the Y needs to be last var. in data set

### XY matrix is a matrix with all covariates and y as the last var
### IC (information criterion) - "AIC", "BIC", "CV","LOOCV" - leave one out cv. faster than CV but has weaknesses. if cv t=100,1000
### method = "exhaustive", "forward","backward", "seqrep"
### topmodels = 10 how many of the best models should be saved?
#############################################

var.selection = bestglm(log.ei,IC="BIC",method="exhaustive",TopModels=10) # Y MUST BE THE LAST COLUMN!!!!
plot(var.selection$Subsets$BIC,type="b",pch=19,xlab="# of Vars",ylab="BIC")
summary(var.selection$BestModel)
confint(var.selection$BestModel)


var.selection2 = bestglm(log.ei,IC="BIC",method="forward",TopModels=10)
plot(var.selection2$Subsets$BIC,type="b",pch=19,xlab="# of Vars",ylab="BIC")
summary(var.selection$BestModel)
summary(var.selection2$BestModel)

kable(confint(var.selection$BestModel))
##########################################
## Perform Forward Selection using AIC  ##
##########################################

var.selection = bestglm(log.ei,IC="AIC",method="forward",TopModels=10)
plot(var.selection$Subsets$AIC,type="b",pch=19,xlab="# of Vars",ylab="AIC")
summary(var.selection$BestModel)

#####################################################
## Perform Backward Variable Selection using PMSE ##
#####################################################

var.selection = bestglm(log.ei,IC="CV",method="backward",TopModels=10,t=100)

plot(0:15,var.selection$Subsets$CV,type="b",pch=19,xlab="# of Vars",ylab="CV")
summary(var.selection$BestModel)





    
    
