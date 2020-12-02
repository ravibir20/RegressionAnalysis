getwd()
setwd("/Users/Bir/Documents/Regression Analysis")
options(digits=100)

install.packages("TH.data")
library("TH.data")
data("bodyfat")
library(MASS)
library(car)




BFM <- read.table(("kv.txt"), header = T)
View(BFM)
n <- nrow(BFM)
plot(BFM)

lm.fit <- lm(DEXfat ~ age + waistcirc + hipcirc + elbowbreadth + kneebreadth + anthro3a + anthro3b + anthro3c + anthro4 , data = BFM)


summary(lm.fit)
anova(lm.fit)

attach(BFM)
par(mfrow=c(3,3))
plot(age, DEXfat, main = "Scatterplot") 
plot(waistcirc, DEXfat, main = "Scatterplot")
plot(hipcirc, DEXfat, main = "Scatterplot")
plot(elbowbreadth, DEXfat, main = "Scatterplot")
plot(kneebreadth, DEXfat, main = "Scatterplot")
plot(anthro3a, DEXfat, main = "Scatterplot")
plot(anthro3b, DEXfat, main = "Scatterplot")
plot(anthro3c, DEXfat, main = "Scatterplot")
plot(anthro4, DEXfat, main = "Scatterplot")

par(mfrow=c(3,2))

rstd <- rstandard(lm.fit) #Standardized Residual
rstud <- rstudent(lm.fit) #R-student residual plot
sres <- studres(lm.fit) #studentized residual plot



qqnorm(lm.fit$residuals, main="Normal Residual QQ plot") #Normal Residual QQ plot
qqnorm(rstd, main="Normal Standardized Residual QQ plot") #Normal Standardized Residual QQ plot
plot(lm.fit$residuals, main = "Residual Plot") #residual Plot
plot(rstd, main = "Standardized Resiudal Plot") #standardized resiudal plot
plot(sres, main = "Studentized Residual Plot") #studentized residual plot
plot(rstud, main = "R-Student Residual Plot") #R-student residual plot


par(mfrow=c(1,1))
attach(BFM)
plot(DEXfat, lm.fit$residuals, main = "DEXfat vs Resiudal") #DEXfat vs resiudal Plot


par(mfrow=c(1,1))
cd <- cooks.distance(lm.fit) #cooks distance 
plot(cd, type = "h", ylim = range(cd)*c(1, 1.1), main = "Cook's Distance") #stretch y-axis to make space for labels
cd

#---------------------------------------------
#OBSERVATION: NOW REMOVE INDEX 41 AND INDEX 48



newdata <- BFM[-c(41, 48),] 
newdata


lm.fitNEW <- lm(DEXfat ~ age + waistcirc + hipcirc + elbowbreadth + kneebreadth + anthro3a + anthro3b + anthro3c + anthro4 , data = newdata)
sresNEW <- studres(lm.fitNEW) #studentized residual plot






#Transformations
attach(newdata)
par(mfrow=c(3,3))
plot(age, sresNEW, main = "Scatterplot") 
plot(waistcirc, sresNEW, main = "Scatterplot")
plot(hipcirc, sresNEW, main = "Scatterplot")
plot(elbowbreadth, sresNEW, main = "Scatterplot")
plot(kneebreadth, sresNEW, main = "Scatterplot")
plot(anthro3a, sresNEW, main = "Scatterplot")
plot(anthro3b, sresNEW, main = "Scatterplot")
plot(anthro3c, sresNEW, main = "Scatterplot")
plot(anthro4, sresNEW, main = "Scatterplot")

#correlation matrix and scatter plot too see correlation between predictors
cor(newdata)
cor(newdata[,c(1, 3, 4, 5, 6, 7, 8, 9, 10)])
plot(newdata[,c(1, 3, 4, 5, 6, 7, 8, 9, 10)])



library(car)


#vif
vif(lm(DEXfat ~ age + waistcirc + hipcirc + elbowbreadth + kneebreadth + anthro3a + anthro3b + anthro3c + anthro4 , data = newdata))

#delete anthro4

vif(lm(DEXfat ~ age + waistcirc + hipcirc + elbowbreadth + kneebreadth + anthro3a + anthro3b + anthro3c , data = newdata))

#delete anthro3b


vif(lm(DEXfat ~ age + waistcirc + hipcirc + elbowbreadth + kneebreadth + anthro3a + anthro3c , data = newdata))

#All possible regression

library(leaps)
library(glmnet)
library(ISLR)


regfit.all1 <- regsubsets(DEXfat ~ age +  waistcirc + hipcirc + elbowbreadth + kneebreadth + anthro3a +  anthro3c , data = newdata)
reg.summary1 <- summary(regfit.all1)
reg.summary1



par(mfrow = c(3,1))

plot(reg.summary1$adjr2, xlab = "Number of Variables", ylab = "adjr2", type = "l")
adj_r2_max = which.max(reg.summary1$adjr2) 
points(adj_r2_max, reg.summary1$adjr2[adj_r2_max], col ="red", cex = 2, pch = 20)

plot(reg.summary1$cp, xlab = "Number of Variables", ylab = "cp", type = "l")
cp_min = which.min(reg.summary1$cp) 
points(cp_min, reg.summary1$cp[cp_min], col = "red", cex = 2, pch = 20)

plot(reg.summary1$bic, xlab = "Number of Variables", ylab = "bic", type = "l")
bic_min = which.min(reg.summary1$bic)
points(bic_min, reg.summary1$bic[bic_min], col = "red", cex = 2, pch = 20)



#Bootstrap

#these are the packages you need
install.packages("boot")
install.packages("complmrob")
install.packages("robustbase")

library("boot")
library("complmrob")
library("robustbase")

#use lmrob to fit a linear model to use with bootstrap. Here is a few that I tested
lm4 <- lmrob(DEXfat ~ waistcirc + hipcirc + anthro3a + anthro3c , data = newdata)
lm6 <- lmrob(DEXfat ~ age + waistcirc + hipcirc + kneebreadth + anthro3a + anthro3c , data = newdata)



#this performs thebootstrapping. The R parameter is number of replications
bc <- bootcoefs(lm4, R = 100)

#use this to get the bootstrap confidence intervals
summary(bc)

#-----------------------------

install.packages("boot")
library("boot")

#4 variables
lm.fit <- lm(DEXfat ~ waistcirc + hipcirc + anthro3a + anthro3c, data = newdata)
anova(lm.fit)
summary(lm.fit)
sum <- summary(lm.fit)
ms <- c(100) #500)
#par(mfrow = c(2, 3))
for (m in ms) {
  coefs <- c()
  for (i in seq(m)) {
    n <- nrow(BFM)
    indices <- sample(n, n, replace = TRUE)
    residual.boot <- sum$residuals[indices]
    DEXfat.boot <- lm.fit$fitted.values + residual.boot # New bootstrap samples, see (eq. 5.19)
    lm.fit.boot <- lm(DEXfat.boot ~ BFM$waistcirc + BFM$hipcirc + BFM$anthro3a + BFM$anthro3c)
    coefs <- rbind(coefs, coef(lm.fit.boot))
  }
  
  param.sd.boot <- apply(coefs, 2, sd)
  print(param.sd.boot)
  # Also create confidence intervals accoring to the percentile method
  # presented in Section 15.4.2, Montgomery
  conf.ints <- c()
  
  #  for (k in seq( length(lm.fit$coefficients))) {
  for (k in seq(5)) {
    #hist(coefs[, k])
    quants <- quantile(coefs[, k], probs = c(0.025, 0.975))
    beta.est <- coef(lm.fit)[k]
#    print("test2")
    D1 <- beta.est - quants[1]
    D2 <- quants[2] - beta.est
    conf.ints <- rbind(conf.ints, c(beta.est - D2, beta.est + D1, beta.est))
  }
  
  colnames(conf.ints) <- c( names(quants), "beta est")
  rownames(conf.ints) <- names( coef(lm.fit))
  conf.ints
  confint(lm.fit)
}

# 6 variables
lm.fit <- lm(DEXfat ~ age + waistcirc + hipcirc + kneebreadth + anthro3a + anthro3c, data = newdata)
sum <- summary(lm.fit)
ms <- c(100) #500)
#par(mfrow = c(2, 3))
for (m in ms) {
  coefs <- c()
  for (i in seq(m)) {
    n <- nrow(BFM)
    indices <- sample(n, n, replace = TRUE)
    residual.boot <- sum$residuals[indices]
    DEXfat.boot <- lm.fit$fitted.values + residual.boot # New bootstrap samples, see (eq. 5.19)
    lm.fit.boot <- lm(DEXfat.boot ~ BFM$age + BFM$waistcirc + BFM$hipcirc + BFM$kneebreadth + BFM$anthro3a + BFM$anthro3c)
    coefs <- rbind(coefs, coef(lm.fit.boot))
  }
  
  param.sd.boot <- apply(coefs, 2, sd)
  print(param.sd.boot)
  # Also create confidence intervals accoring to the percentile method
  # presented in Section 15.4.2, Montgomery
  conf.ints <- c()
  
  #  for (k in seq( length(lm.fit$coefficients))) {
  for (k in seq(7)) {
    #hist(coefs[, k])
    quants <- quantile(coefs[, k], probs = c(0.025, 0.975))
    beta.est <- coef(lm.fit)[k]
    print("test2")
    D1 <- beta.est - quants[1]
    D2 <- quants[2] - beta.est
    conf.ints <- rbind(conf.ints, c(beta.est - D2, beta.est + D1, beta.est))
  }
  
  colnames(conf.ints) <- c( names(quants), "beta est")
  rownames(conf.ints) <- names( coef(lm.fit))
  conf.ints
  confint(lm.fit)
}





