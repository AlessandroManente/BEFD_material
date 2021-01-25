# -------------------- EXERCISES FROM CASE STUDIES LAB 1 AND LAB 2 --------------------

# All the libraries
library("readxl")
library(DIMORA)
source("D:\\Desktop\\University (Data Science)\\II YEAR - I SEMESTER\\Business economic and financial data\\Lab 1\\DIMORA1.0.0.R")
library(forecast)
library(fpp2) 
library(lmtest) 
library(sm)
library(KernSmooth)
library(splines)
library(gam)
library (gbm)

#Directory (to complete)
dir <- "D:\\Desktop\\University (Data Science)\\II YEAR - I SEMESTER\\Business economic and financial data\\..."



# -------------------- Exercise 1 --------------------
# Exercise 1: how could we model the entire series?

#Data overview
beer<- ausbeer
plot(beer)
tsdisplay(beer)



### OPTION 1 (Smoothing spline + GGM + ARIMA)

#Application of smoothing splines
tt <- 1:length(beer)
beer_vec <- as.numeric(beer)
sm.mod <-  smooth.spline(tt,beer_vec,  lambda=0.001) 
plot(tt, beer_vec, xlab="time", ylab="beer", type='l')
lines(sm.mod, col=2)
sm.mod

#Now we fit the values obtained with the smoothing plines with GGM
GGM <- GG.model(sm.mod$y, display = T)
GGM
pred_GGM <- fitted(GGM)
pred_GGMinst <- make.instantaneous(pred_GGM)

#Plot comparison
plot(beer_vec, type= "l")
lines(sm.mod, col=2)
lines(pred_GGMinst,col=3)
legend(130,350,legend= c("observed t.s.", "smooth", "GGM on smooth"), col=c(1,2,3), lty=1)

#Residuals (we can't use the function "residuals()" because GGM is fitted on a smooth spline)
res_GGM <- beer_vec - pred_GGMinst
tsdisplay(res_GGM)

#We model the residuals with two different ARIMA models
arima_res <- auto.arima(res_GGM)
arima_res
#We try to add a differencing term to the previous automatic model
arima_res2 <- Arima(res_GGM, order = c(3,1,2))
arima_res2
#AIC values 
AIC(arima_res)
AIC(arima_res2)

#Plot the results
plot(beer_vec, type='l')
lines(pred_GGMinst + fitted(arima_res), col=2)
lines(pred_GGMinst + fitted(arima_res2), col=3)
legend(130,350,legend= c("observed t.s.", "auto.arima", "manual.arima"), col=c(1,2,3), lty=1)



### OPTION 2 (Linear model (degree=2 + ARIMA))

#Linear model (polynomial version) + ARIMA on residuals (attempt: quadratic modeling of the trend)
m.beer.pol<- tslm(beer~ poly(trend, 2, raw=TRUE) + season)
summary(m.beer.pol)

#Fitting
fit.lm<- fitted(m.beer.pol)
plot(beer)
lines(fit.lm, col=2)
legend(1995,335,legend= c("observed","Lin"), col=c(1,2), lty=1)

#Analysis of residuals
res.lm<- residuals(m.beer.pol)
tsdisplay(res.lm)

#ARIMA on residuals
arimaResiduals <- auto.arima(res.lm)
summary(arimaResiduals)

#Fitting on residuals and plot comparison
fit.ar <- fitted(arimaResiduals)
plot(fit.ar, lty=1)
lines(res.lm, lty=1,col=2)
legend(1990,63,legend= c("arima forecast", "observed t.s."), col=c(1,2), lty=2)

#Combination of tslm and ARIMA
fittbeer <- fit.lm + fit.ar
plot(beer, type='l', main = "Beer")
lines(fittbeer, col=2)
lines(fit.lm,col=3)
legend(1995,335,legend= c("observed","Lin+ARIMA","Lin"), col=c(1,2,3), lty=1)

#New residuals
new.res<-fittbeer-beer
tsdisplay(new.res)



### OPTION 3 (Direct ARIMA)

#ARIMA model (parameters of the best model evaluated using AIC)
arima.beer <- Arima(beer, order = c(2,1,1), seasonal = list(order=c(2,1,2), period=4))
arima.beer

#Fitting
plot(beer)
lines(fitted(arima.beer), col=2)
legend(1995,335,legend= c("observed","ARIMA"), col=c(1,2), lty=1)

#Analysis of residuals
res.arima<- residuals(arima.beer)
tsdisplay(res.arima)

#Forecasts
forcast.arima <- forecast(arima.beer)
plot(forcast.arima)



### OPTION 4 (Like option 1 but with LOESS)

#Application of loess (to evaluate the loess on all the points add "evaluation", default = 50)
lo.mod <- loess.smooth(tt, beer_vec, evaluation = length(tt)) 
plot(tt, beer_vec, xlab="time", ylab="beer", type='l')
lines(lo.mod, col=2)
legend(160,335,legend= c("observed","LOESS"), col=c(1,2), lty=1)
lo.mod

#Now we fit the values obtained with the loess with GGM
GGM <- GG.model(lo.mod$y, display = T)
GGM
pred_GGM <- fitted(GGM)
pred_GGM
pred_GGMinst <- make.instantaneous(pred_GGM)

#Plot comparison
plot(beer_vec, type= "l")
lines(lo.mod, col=2)
lines(pred_GGMinst,col=3)
legend(160,335,legend= c("observed","LOESS", "GGM"), col=c(1,2,3), lty=1)


#Residuals (we can't use the function "residuals()" because GGM is fitted on a smooth spline)
res_GGM <- beer_vec - pred_GGMinst
tsdisplay(res_GGM)

#We model the residuals with an ARIMA model
arima_res3 <- auto.arima(res_GGM)
arima_res3
AIC(arima_res3)

plot(beer_vec, type='l')
lines(pred_GGMinst + fitted(arima_res3), col=2)
legend(140,335,legend= c("observed","GGM + ARIMA"), col=c(1,2), lty=1)





# -------------------- Exercise 2 --------------------
###Exercise 2: try to study the behavior of these series together
#Using a multiple regression model, for example studying the consumption against the other variables
#in this way we can see if there are significant relations between these variables.
#model<-tslm( uschange[,1]~ uschange[,2] +  uschange[,3] +  uschange[,4] +  uschange[,5])

#All the data
uschange
str(uschange)
plot(uschange)
#Unique visualization of the series, to find possible relations
autoplot(uschange)

#Variable renaming
consumption <- uschange[,1]
income <- uschange[,2]
production <- uschange[,3] 
savings <- uschange[,4]
unempl <- uschange[,5]

#---Total model
mod<-tslm(consumption ~ season + income + production + savings + unempl)
summary(mod)

#Fitting and residuals
fit <- fitted(mod)
res <- residuals(mod)

#Evaluations
#Description
plot(consumption)
lines(fit, col=2)
legend(2005.5,2.5,legend= c("observed","Tot. mod"), col=c(1,2), lty=1)
#Residuals
tsdisplay(res)
dwtest(mod, alt="two.sided")
#AIC
AIC(mod)
#R^2
#summary(mod)

#---Nested model
mod.nest<-tslm(consumption ~ income + savings)
summary(mod.nest)

#Fitting and residuals
fit.nest <- fitted(mod.nest)
res.nest <- residuals(mod.nest)

#Evaluations
#Description
plot(consumption)
lines(fit.nest, col=2)
legend(2005.5,2.5,legend= c("observed","Nest. mod"), col=c(1,2), lty=1)
#Residuals
tsdisplay(res.nest)
dwtest(mod.nest, alt="two.sided")
#AIC
AIC(mod.nest)
#R^2
#summary(mod)

#---GAM model (with smoothing)
g.consumption <- as.numeric(consumption)
g.income <- as.numeric(income)
g.production <- as.numeric(production)
g.savings <- as.numeric(savings)
g.unempl<- as.numeric(unempl)

g1 <- gam(g.consumption ~ s(g.income) + s(g.production) + s(g.savings) + s(g.unempl),arg=c("df=2","df=3","df=4"))
summary(g1)
AIC(g1)
g2 <- gam(g.consumption ~ s(g.income) + s(g.production) + s(g.savings) + g.unempl,arg=c("df=2","df=3","df=4"))
summary(g2)
AIC(g2)
#g2 is better in terms of significance of predictors and AIC

#Fitting and residuals
fit.g2 <- fitted(g2)
res.g2 <- residuals(g2)

#Evaluations
#Description
plot(g2, se=T, ask=T)
plot(as.numeric(consumption), type='l')
lines(as.numeric(fit.g2), col=2)
legend(140,-1,legend= c("observed","Gam2"), col=c(1,2), lty=1)
#Residuals
tsdisplay(res.g2)
dwtest(g2, alt="two.sided")
#AIC
AIC(g2)
#R^2
#summary(mod)

#Final comment: all the models have a good fit on the data, the residuals have a good behavior,
#the best model is choosen considering the AIC: g2 is the best one





# -------------------- Exercise 3 --------------------
###Exercise 3: try the same analysis with the other series in the dataset

#consumption <- uschange[,1]: seen in case study lab 1
income <- uschange[,2]
production <- uschange[,3] 
savings <- uschange[,4]
unempl <- uschange[,5]


#INCOME
tsdisplay(income)
arima.inc <- Arima(income, order=c(3,0,2))
arima.inc

#Fitting and residuals
fit.inc <- fitted(arima.inc)
res.inc <- residuals(arima.inc)

#Evaluations
#Description
plot(income)
lines(fit.inc, col=2)
legend(2005,4.6,legend= c("observed","Arima"), col=c(1,2), lty=1)
#Residuals
tsdisplay(res.inc)
#AIC
AIC(arima.inc)

#Forecast
for.inc <- forecast(arima.inc)
plot(for.inc)


#PRODUCTION
tsdisplay(production)
arima.pr <- Arima(production, order=c(1,0,1))
arima.pr

#Fitting and residuals
fit.pr <- fitted(arima.pr)
res.pr <- residuals(arima.pr)

#Evaluations
#Description
plot(production)
lines(fit.pr, col=2)
legend(2005,4.6,legend= c("observed","Arima"), col=c(1,2), lty=1)
#Residuals
tsdisplay(res.pr)
#AIC
AIC(arima.pr)

#Forecast
for.pr <- forecast(arima.pr)
plot(for.pr)


#SAVINGS
tsdisplay(savings)
arima.sa <- Arima(savings, order=c(1,0,1))
arima.sa

#Fitting and residuals
fit.sa <- fitted(arima.sa)
res.sa <- residuals(arima.sa)

#Evaluations
#Description
plot(savings)
lines(fit.sa, col=2)
legend(2005,-42,legend= c("observed","Arima"), col=c(1,2), lty=1)
#Residuals
tsdisplay(res.sa)
#AIC
AIC(arima.sa)

#Forecast
for.sa <- forecast(arima.sa)
plot(for.sa)


#UNEMPLOYMENT
tsdisplay(unempl)
arima.em <- Arima(unempl, order=c(2,0,0))
arima.em

#Fitting and residuals
fit.em <- fitted(arima.em)
res.em <- residuals(arima.em)

#Evaluations
#Description
plot(unempl)
lines(fit.em, col=2)
legend(2005,1.45,legend= c("observed","Arima"), col=c(1,2), lty=1)
#Residuals
tsdisplay(res.em)
#AIC
AIC(arima.em)

#Forecast
for.em <- forecast(arima.em)
plot(for.em)

#Final graphical evaluation
var_fit <- data.frame(income, production, savings, unempl, fit.inc, fit.pr, fit.sa, fit.em)
par(mfrow=c(2,2))
for(i in c(1:8)){
  plot(as.numeric(var_fit[,i]), type='l', xlab="Time", ylab=colnames(var_fit[i]))
  lines(var_fit[i+4], col=2)
  if (i==4) {
    break
  }
}
par(mfrow=c(1,1))





# -------------------- Exercise 4 --------------------
###Exercise 4: try other solutions for modelling this series

plot(euretail, ylab="retail index",xlab="year")
tsdisplay(euretail)

#Redefinition od time and seasonal variables
tt<- (1:length(euretail))
#seas factorizes the observations
seas <- factor(c(rep(1:4,length(euretail)/4)))

#Model
g1 <- gam(euretail ~ s(tt)+ seas,arg=c("df=2","df=3","df=4"))
summary(g1)
g2 <- gam(euretail ~ s(tt),arg=c("df=2","df=3","df=4"))
summary(g2)
AIC(g1)
AIC(g2)

#Fitting and residuals
fit.g2 <- fitted(g2)
res.g2 <- residuals(g2)

#Evaluations
#Description
plot(g2, se=T, ask=T)
plot(as.numeric(euretail), type='l')
lines(as.numeric(fit.g2), col=2)
legend(48,93,legend= c("observed","Gam2"), col=c(1,2), lty=1)
#Residuals
tsdisplay(res.g2)
dwtest(g2, alt="two.sided")
#AIC
AIC(g2)

#The residuals behavior is not satisfactory, so we model them with an ARIMA
arima.gm.res<- auto.arima(res.g2)
arima.gm.res
fit.ar.g2 <- fitted(arima.gm.res)
plot(as.numeric(euretail), type='l')
#Combination of g2 on data and Arima on residuals
lines(fit.ar.g2 + fit.g2, col=2)
legend(43,93,legend= c("observed","Gam2 + Arima"), col=c(1,2), lty=1)

#Residuals of the new model
new.res <- (fit.ar.g2 + fit.g2) - as.numeric(euretail)
tsdisplay(new.res)



# -------------------- Exercise 5 --------------------
###Exercise 5: model Japan variable with BASS model and use its forecasts to make predictions with
#our initial model
arrivals
Japan<- arrivals[,1]
NZ<- arrivals[,2]
autoplot(arrivals[,c(1,2)])

BMs<-BASS.standard(as.numeric(Japan), display = T)

#Cumulative forecasting
cum.fit.BM <- fitted(BMs)
#Instantaneous forecasting
fit.BM <- make.instantaneous(cum.fit.BM)
#Bass Model estimates (single par.: BMs$Estimate[1,1])
BMs$Estimate

#Plot
plot(as.numeric(Japan), type='l')
lines(fit.BM, col=2)

#Cumulative residuals
cum.res.BM <- residuals(BMs)
#Instantaneous residuals
res.BM <- make.instantaneous(cum.res.BM) 
tsdisplay(res.BM)

#Forecasting towards the future
#NON ho idea di come fare, sorry. Il BASS model non ha proprio la funzionalità per predire
#fitted non serve. Buona fortuna





# -------------------- Exercise 6 --------------------
###Exercise 6: try to find a good model for this series

autoplot(goog)
tsdisplay(goog)

#---ARIMA
#It's clear that we need a first order differencing to stabilize the data (stationarity)
plot(diff(goog))
arima.model <- Arima(goog, order=c(2,1,2))
#arima.model <- auto.arima(goog)
arima.model

#Fitting and residuals
fit.ar <- fitted(arima.model)
res.ar <- residuals(arima.model)

#Evaluations
#Description
plot(goog)
lines(fit.ar, col=2)
legend(720,500,legend= c("observed","Arima"), col=c(1,2), lty=1)
#Residuals
tsdisplay(res.ar)
#AIC
AIC(arima.model)

#Forecast
for.ar <- forecast(arima.model)
plot(for.ar)


#---LINEAR MODEL + ARIMA on residuals
goog.ts <- ts(goog) 
lm <- tslm(goog.ts ~ trend)
summary(lm)

#Fitting and residuals
fit.lm <- fitted(lm)
res.lm <- residuals(lm)

#Evaluations
#Description
plot(goog)
lines(fit.lm, col=2)
legend(720,500,legend= c("observed","Lin. mod"), col=c(1,2), lty=1)
#Residuals
tsdisplay(res.lm)
#AIC
AIC(lm)

arima.res <- auto.arima(res.lm, seasonal=FALSE)
arima.res
fit.ar <- fitted(arima.res)
plot(goog, type='l')
#Combination of lm on data and Arima on residuals
lines(fit.ar + fit.lm, col=2)
legend(720,500,legend= c("observed","LM + ARIMA"), col=c(1,2), lty=1)

#Residuals of the new model
new.res <- (fit.ar + fit.lm) - goog
tsdisplay(new.res)


#---REGRESSION SPLINES
tt <- 1:1000
reg <- lm(goog~bs(tt, df=7, degree=3))
summary(reg)

#Fitting and residuals
fit.reg <- fitted(reg)
res.reg <- residuals(reg)

#Evaluations
#Description
plot(goog)
lines(fit.reg, col=2)
legend(720,500,legend= c("observed","Lin. mod"), col=c(1,2), lty=1)
#Residuals
tsdisplay(res.reg)
#AIC
AIC(reg)





# -------------------- Exercise 7 --------------------
###Exercise 7: try to fit another GB with shrinkage=0.01 (DA RIVEDERE IN DETTAGLIO)
#Model (we fit directly on the whole training set to find the best number of trees)
boost.movies.2=gbm(vote_average ~ .-vote_classes ,data=train.1, 
                   distribution="gaussian", n.trees=5000, interaction.depth=4, shrinkage=0.01)
summary(boost.movies.2)

#Predictions (on training set to find the best number of trees)
yhat.boost.2=predict(boost.movies, newdata=train.2, n.trees=1:5000)

#Errors and best number of trees
err.2 = apply(yhat.boost.2, 2, function(pred) mean((train.2$vote_average - pred)^2))
best2=which.min(err.2)
min(err.2)

#Final model with best number of trees on entire training set (dati.train)
boost.movies.2=gbm(vote_average ~ .-vote_classes, data=dati.train, 
                   distribution="gaussian", n.trees=best2, interaction.depth=4)
summary(boost.movies.2)

#Prediction on test set
p.boost=predict(boost.movies.2, newdata=dati.test, n.trees=best2)
dev.boost.2 <- sum((p.boost-dati.test$vote_average)^2)
dev.boost.2





# -------------------- Exercise 8 --------------------
###Exercise 8: try to model a10 dataset of Lab2 (try to apply different type of models, not only
#linear models or ARIMA) and US and UK time series in arrivals dataset in CS_Lab1 


#Antidiabetic drugs example
a10
plot(a10)
tsdisplay(a10)

#REGRESSION SPLINES
#We select and identify the knots 'equispaced' (we will consider like "true knots" the internal ones,
#the others are "boundary knots")
tt <- 1:204
xi<-seq(min(tt), max(tt), length=5)
#Model (3 internal knots, from 2 to 4. Note: "knots=" define the POSITIONS of knots, not the number)
m1<-lm(a10 ~ bs(tt, knots=xi[2:(length(xi)-1)], degree=3))
#For graphical reasons select 200 points where to evaluate the model
#Make predictions by using the 'xxx' points
fit1<-predict(m1)
#Plot
plot(tt,a10,xlab="engine size", ylab="distance", type='l')
lines(tt,fit1,col=2)
#Vertical plots to indicate the knots
abline(v=xi[2], lty=3)
abline(v=xi[3], lty=3)
abline(v=xi[4], lty=3)



#Quarterly international arrivals (in thousands) to Australia from Japan, New Zealand, UK 
#and the US. 1981Q1 - 2012Q3
autoplot(arrivals)
autoplot(arrivals[,c(3,4)])

#Variables definition
UK<- arrivals[,3]
US<- arrivals[,4]

#US series
BM.US<-BASS.standard(US, display = T)
#Cumulative forecasting
fitted(BM.US)
#Instantaneous forecasting
make.instantaneous(fitted(BM.US))
#Bass Model estimates (single par.: BM.US$Estimate[1,1])
BM.US$Estimate
#Cumulative residuals
residuals(BM.US)
#Instantaneous residuals
make.instantaneous(residuals(BM.US))
tsdisplay(make.instantaneous(residuals(BM.US)))
plot(as.numeric(US), type='l')
lines(make.instantaneous(fitted(BM.US)), col=2)


#UK series
BM.UK<-BASS.standard(UK, display = T)
#Cumulative forecasting
fitted(BM.UK)
#Instantaneous forecasting
make.instantaneous(fitted(BM.UK))
#Bass Model estimates (single par.: BM.US$Estimate[1,1])
BM.UK$Estimate
#Cumulative residuals
residuals(BM.UK)
#Instantaneous residuals
make.instantaneous(residuals(BM.UK)) 
#It requires a modeling of seasonality with a SARIMA model
tsdisplay(make.instantaneous(residuals(BM.UK)))
plot(as.numeric(UK), type='l')
lines(make.instantaneous(fitted(BM.UK)), col=2)

#Final plot
plot(as.numeric(US), type='l', ylim=c(20, 270))
lines(as.numeric(UK), type='l', col=2)
lines(make.instantaneous(fitted(BM.US)), col=3)
lines(make.instantaneous(fitted(BM.UK)), col=4)
legend(0,250,legend= c("observed US","observed UK", "BASS US", "BASS UK"), col=c(1,2,3,4), lty=1)



#Is US a good predictor of UK?
armax.mod <- auto.arima(UK, xreg=US)
armax.mod

#Fitting and residuals
fit.ar <- fitted(armax.mod)
res.ar <- residuals(armax.mod)

#Evaluations
#Description
plot(UK)
lines(fit.ar, col=2)
legend(1983,250,legend= c("observed","Armax"), col=c(1,2), lty=1)
#Residuals
tsdisplay(res.ar)
#AIC
AIC(armax.mod)
