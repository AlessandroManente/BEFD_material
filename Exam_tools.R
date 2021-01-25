############
# EXAM TOOLS
############

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
setwd("...")
data <- read.csv("...")




# -------------------- PLOTS --------------------

par(mfcol=c(1,1))
par(mfrow=c(1,1))
plot(data, type="l", xlim=c(0,500), ylim=c(3,100), xlab="", ylab="")
#Diagnostic tool
plot(linear_model)
lines(data,col=2)
legend(35,20,legend= c("observed","BM"), col=c(1,2), lty=1)
abline(model, col=3)
#Vertical line
abline(v=xi[2], lty=3)
#To explore response variable
boxplot(y, col="orange", ylim=c(0,10), main="Movies", ylab="Rating")
hist(dy, col="orange", main="Movies", xlab="Rating")
#Multicollinearity (FOR A BETTER GRAPH SEE R SCRIPT "MULTICOLLINEARITY")
pairs(variables)

#FOR TIME SERIES
tsdisplay(data)
Acf(data)
Pacf(data)
#MULTIPLE TIME SERIES. Unique visualization of the series, to find possible relations
autoplot(data)

#FOR GRADIENT BOOSTING
summary(boost.model)

#IMPROVE READABILITY. Increase space on the left to fit the name of variables.
#Default parameters
mai.old<-par()$mai
mai.old
#New parameters equal to old parameters
mai.new<-mai.old
#Substitute parameter relative to left space
mai.new[2] <- 2.1 
mai.new
#Modify graphical parameters
par(mai=mai.new)
#las=1 horizontal names on y 
summary(boost.movies.1, las=1) 
#cBar sets how many variables to draw
summary(boost.movies.1, las=1, cBar=10)
#Back to old parameters
par(mai=mai.old)

#PARTIAL DEPENDENCE PLOT to explore better the relation between our response and explanatory var.
#Univariate
plot(boost.movies.1, i.var=1, n.trees = best)
#Bivariate
plot(boost.movies.1, i.var=c(1,5), n.trees = best) 





# -------------------- LINEAL MODELS (ALSO FOR TIME SERIES) --------------------

#To transform data: from vector to time series:
#data (vector) -> ts_data = ts(data, frequency=4) -> linear model: tslm(ts_data ~ trend+season)
#Time variable "tt" for a linear model 
tt<- 1:NROW(apple)
#Linear model
fitlm <- lm(data~tt)
summary(fitlm)
#Data transformed as time series for quartely data
data.ts<-ts(data, frequency=4)
#Model with trend and seasonality
#"trend" and "seasonality" are two variables inside the tslm function
fitts <- tslm(data.ts~ trend+season)

#We exclude the "class version" of our response variable (specify "data=dati.train" for train. set)
m1 <- lm(vote_average~.-vote_classes)
#Model selection with stepwise regression (the best one has the minimum AIC value. We can have 
#also non-significant variables)
m2 <- step(m1, direction="both")

#Prediction
p.lm <- predict(m2, newdata=dati.test)
#This is the deviance of our selected model. This value can be compared with the deviance of other
#model (GAM and GB)
dev.lm <- sum((p.lm-dati.test$vote_average)^2)
AIC(m2)

#Another way of performing the same linear regression, factoring seasonality and using tt
#tt: time for modelling the trend
tt<- (1:length(y))
#seas factorizes the observations
#1:3: adds the last 3 observations (without this, "rep" deletes them)
seas <- factor(c(rep(1:4,length(y)/4),1:3)) 
#All the parameter are linear
mod <- lm(y~ tt+seas+x)





# -------------------- DIFFUSION PROCESS MODEL (BM, GBM, GGM) --------------------

BMs<-BASS.standard(data, display = T)
#Cumulative forecasting
fitted(BMs)
#Instantaneous forecasting
make.instantaneous(fitted(BMs))
#Bass Model estimates (single par.: BMs$Estimate[1,1])
BMs$Estimate
#Cumulative residuals
residuals(BMs)
#Instantaneous residuals
make.instantaneous(residuals(BMs)) 

#prelimestimates: use the estimations of BM
GBMe1<-BASS.generalized(iphone, shock = "exp", nshock = 1, prelimestimates = c(m, p , q, 17, -0.1, 0.1), display=F)
fitted(GBMe1)
make.instantaneous(fitted(GBMe1))

GGM <- GG.model(iphone, prlimestimates = c(m, 0.01, 0.1, p, q), display = F)
fitted(GGM)
make.instantaneous(fitted(GGM))

#Combination with arima on residuals
arimaResiduals <- Arima(residuals, order = c(1,0,1), seasonal = list(order = c(1,0,0), period = 4))
#Cumulative forecasting on cumulative residuals
fitted(arimaResiduals)
make.instantaneous(fitted(arimaResiduals))
forecast <- forecast_on_data_BM_GB_GGM + forecast_on_res_ARIMA





# -------------------- ARIMA AND ARMAX --------------------

#Differencing
diff1<- diff(data)

# Use the argument seasonal=FALSE to prevent it searching for seasonal ARIMA models
auto.arima(data)
arima.mod<- Arima(data, order=c(0,1,1))
fitted(arima.mod)
residuals(arima.mod)
forecast(arima.mod)
arima_with_seas<- Arima(a10, order=c(0,1,1), seasonal=list(order=c(0,2,1), period=12))

#ARMAX model
armax1<- Arima(y, xreg=x1, order=c(1,0,1))
auto.armax<- auto.arima(NZ, xreg=Japan)
#armax: the regression term appears significant (0.1825/0.0456 > 2)
#Residuals
res1<- residuals(armax1)
tsdisplay(res1)
#Fitting, behavior in description
fitted(armax1)
plot(uschange[,1])
lines(fitted(armax1), col=2)
#We don't make any forecast because it is available only if we have future values of 'x1'





# -------------------- LOCAL REGRESSION AND LOESS --------------------

sm.regression(x,y, h = 10, add = T, ngrid=200, col=2, display="se")
locpoly(x, y, degree=1, bandwidth=30)

#LOESS
loess.smooth(x,y, span=0.9)
#'evaluation': parameter we obtain a smoother line (default 50)
scatter.smooth(x,y, span=0.3, evaluation=200)





# -------------------- REGRESSION SPLINES --------------------

#We select and identify the knots 'equispaced' (we will consider like "true knots" the internal ones,
#the others are "boundary knots")
xi<-seq(min(x), max(x), length=4)
#Model (2 internal knots, from 2 to 3. Note: "knots=" define the POSITIONS of knots, not the number)
m1<-lm(y ~ bs(x, knots=xi[2:(length(xi)-1)], degree=3))
#For graphical reasons select 200 points where to evaluate the model
xxx<-seq(min(x),max(x),length=200)
#Make predictions by using the 'xxx' points
fit1<-predict(m1, data.frame(x=xxx))
#Plot
plot(x,y,xlab="engine size", ylab="distance")
lines(xxx,fit1,col=2)
#Vertical plots to indicate the knots
abline(v=xi[2], lty=3)
abline(v=xi[3], lty=3)

#First model with 2 internal knots (5 (degrees of freedom) - 3 (degrees) = 2 knots)
m1bis<-lm(y~bs(x, df=5, degree=3))
fit1<-predict(m1bis, data.frame(x=xxx))
lines(xxx,fit1,col=3)





# -------------------- SMOOTHING SPLINES --------------------

#lambda=1 means straight line, the model doesn't allow a lambda value equals to 0
s <- smooth.spline(x,y, lambda=0.0001)
p <- predict(s1, x=xxx)
lines(p, col=2)





# -------------------- GAM --------------------

#Are the paramenters all linear? To see if tt and Japan has a NON-linear role we use GAM
#s() stands for smoothing spline. Values for df should be greater than 1, with df=1 implying 
#a linear fit. In our model we say that the df could be 2,3 or 4 (increasingly linear structure).
#More high is the df more jumpier is the function.
g1 <- gam(NZ~s(tt)+seas+s(Japan),arg=c("df=2","df=3","df=4"))
#We have two ANOVA: for parametric and NON-parametric effects. For seas we don't have a non-par.
#part because we don't model it with splines (it is pointless molling the seasonality with
#smoothing splines because is a factorial variable).
summary(g1)
#Try another option with loess (lo) (LOESS)
g2<- gam(NZ~lo(tt)+seas+lo(Japan))
AIC(g1)
AIC(g2)
#Perform an analysis of residuals
tsdisplay(residuals(g1))
#Model residuals with ARIMA
ar<- auto.arima(residuals(g1))
plot(as.numeric(NZ), type="l")
#Combination of g1 on data and Arima on residuals
lines(fitted(ar)+ fitted(g1), col=2)
#We don't make any forecast because it is available only if we have future values of 'Japan', 
#general problem of this approach. A possible solution is model the t.s. of Japan (for example
#with a BASS model) and make some forecasts and use them to make forecast on NZ with our initial 
#model. We can use a BASS model beacuse the data are turistic destinations, so they are a product 
#and follow a cycle product pattern

#GAM with splines. 
#Considering that we want use some variables with smoothing splines, and this
#is a problem with factorial variables, we create a formula to recognize variables that allow the
#use of splines and variables that don't allow it. We will use splines on all the var. that
#allow it (only numericals, not factors)
fg1 <- as.formula(paste(paste("dati.train[,8] ~s(", paste(names(dati.train[c(1,2,4,5,6)]),collapse=") + s("),")"), paste(names(dati.train)[c(3,7, 10:25)], collapse="+ "), sep=" + "))
#Here we don't specify any df, it uses the default value 4: for each var.with smoothing 1 for 
#the par. part and 3 for the non-par. part (see summary)
g1<-gam(fg1,data=dati.train)
summary(g1)
plot(g1, se=T, ask=T)

#To perform a STEPWISE MODEL SELECTION with GAM, we have to start with a linear model (df=1).
#This below is a linear model (no splines, no loess and so on).
g2 <- gam(vote_average~.-vote_classes, data=dati.train)
#We have only the parametric part
summary(g2)
#Show the linear effects (in fact we have only straight lines)
plot(g2, se=T, ask=T) 
#Perform stepwise selection using "gam.scope". We specify our data, the position of our response
#var. and the df that we want evaluate, the function scope() looks for the best variables 
#considering non-linearity with 2,3 and 4 df in this case.
#Values for df should be greater than 1, with df=1 implying a linear fit
sc <- gam.scope(dati.train[,-8], response=8, arg=c("df=2","df=3","df=4"))
#This function doesn't return always the same model
g3<- step.Gam(g2, scope=sc, trace=T)
#The selected model is choosen considering AIC. An other important point is that this function 
#assigns the best number of df (2,3 or 4) to those variables that are modeled with smoothing 
#splines. g3 is the best model found. Close to the variable we can see the total number of df,
#for the var. with smoothing splines we have the par. and non-par. part
summary(g3)
AIC(g3)
plot(g3, se=T, ask=T)
#If we want to see better some plot
#par(mfrow=c(3,5))
#plot(g3, se=T)

#Prediction
dati.test[,c(3,7, 10:24)]= lapply(dati.test[,c(3,7, 10:24)],factor)
p.gam <- predict(g3, newdata=dati.test)     
#The deviance is lower than linear model. The GAM predict in a better way and is also more 
#interpretable
dev.gam <- sum((p.gam-dati.test$vote_average)^2)





# -------------------- GRADIENT BOOSTING --------------------

#Divide the training set into 2 parts (70 (train) and 30 (validation)) to select the best 
#depth (with or without interactions) and the best number of trees (out two hyperparameters)
set.seed(999)
tt = sample (1:nrow(dati.train), 0.7*nrow(dati.train))
#In order to estimate the model
train.1=dati.train[tt ,]
#In order to select the best model: dept and number of trees
train.2=dati.train[-tt ,]

#First boosting for regression (stump: not interactions, depth=1)
boost.movies=gbm(vote_average ~ .-vote_classes, data=train.1, 
                 distribution="gaussian", n.trees=5000, interaction.depth=1)
summary(boost.movies)
#Second boosting: increase the depth of trees (4) (We allow interactions)
boost.movies.1=gbm(vote_average ~ .-vote_classes, data=train.1, 
                   distribution="gaussian", n.trees=5000, interaction.depth=4)
summary(boost.movies)

#Predictions for our two models, to select the best one
#yhat.boost: matrix with 678 rows (samples) and 5000 columns (trees)
yhat.boost=predict(boost.movies, newdata=train.2, n.trees=1:5000)
yhat.boost.1=predict(boost.movies.1, newdata=train.2, n.trees=1:5000)

#Calculate the error for each iteration (5000).
err = apply(yhat.boost, 2, function(pred) mean((train.2$vote_average - pred)^2))
err.1 = apply(yhat.boost.1, 2, function(pred) mean((train.2$vote_average - pred)^2))

#Best error for each model
best0=which.min(err)
#Best error with 814 trees
best0
min(err)
#The second model has a minor prediction error
best1=which.min(err.1)
#Best error with 313 trees
best1
min(err.1) 
#Since min(err.1) is smaller than min(err), depth=4 is better than depth=1, we select best1
best1 
best<- best1

#We fit our final model with best depth and number of trees on entire training set (dati.train)
boost.movies.1=gbm(vote_average ~ .-vote_classes, data=dati.train, 
                   distribution="gaussian", n.trees=best, interaction.depth=4)
summary(boost.movies.1)

#Prediction on test set
p.boost=predict(boost.movies.1, newdata=dati.test, n.trees=best)
dev.boost <- sum((p.boost-dati.test$vote_average)^2)

#I CAN ADD THE ARGUMENT shrinkage=0.01 (learning rate)





# -------------------- VARIOUS --------------------

# -------------------- CLEANING --------------------

#Clean data (remove Nan values)
sum(is.na(data))
new.data<-data[!is.na(data)]
str(data)

#Erase columns 1 and 2 of indicator variables (useless)
data<-data[,-c(1,2)]

#Transform variable release_date in format "data"
dati$release_date <- as.Date(dati$release_date, "%d/%m/%Y")

#Make some variables factors (columns=variables=c(3,7, 10:24))
dati.train[,c(3,7, 10:24)]= lapply(dati.train[,c(3,7, 10:24)],factor)
dati.test[,c(3,7, 10:24)]= lapply(dati.test[,c(3,7, 10:24)],factor)
str(dati.train)

#From time series to vector or from string to number
as.numeric(data)


# -------------------- EXPLORING --------------------

#Explore explanatory variables with histograms
summary(dati)
#We consider the plots of a subset of variables
summary(dati[,c(1,2,5,6)])
par(mfrow=c(2,2))
#We can see a right skewed behavior for three of them, so we need a log transformation
for(i in c(1,2,5,6)){
  hist(dati[,i], col="grey", main=paste(colnames(dati)[i]), xlab="")
}
#Transform quantitative variables in log scale
dati$budget <- log(dati$budget)
dati$popularity <- log(dati$popularity)
dati$revenue <- log(dati$revenue)
summary(dati[,c(1,2,5,6)])
for(i in c(1,2,5,6)){
  hist(dati[,i], col="grey", main=paste(colnames(dati)[i]), xlab="")
}


# -------------------- EVALUATION --------------------

#Divide the dataset, training and test set (TIME SERIES)
train.set<- window(data, start=1992, end=2006 -.1)
#or
train.set<- window(data, end=2005)
test.set<- window(data, start=2005)
#Divide the dataset, training and test set
set.seed(1)
train = sample (1:nrow(dati), 0.7*nrow(dati))
dati.train=dati[train ,]
dati.test=dati[-train ,]

#Deviance (to compare multiple lin. regression, GAM and GB)
dev.mod <- sum((predicted - dati.test$y)^2)

#Not work for Gradient Boosting
AIC(model)

#Durbin-Watson test
dwtest(model, alt="two.sided")

#Normalized R2
#if R2_tilde > 0.3, the more complex model is significant (in this case GBM is better than BM)
#it makes sense to use a more complex model
R2_tilde<-(GBMe1$Rsquared - BMs$Rsquared)/(1 - BMs$Rsquared)