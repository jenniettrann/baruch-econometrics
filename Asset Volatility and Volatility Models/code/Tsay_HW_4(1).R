

# Tsay Homework 4

# Install packages if necessary.

# options("install.lock"=FALSE)

# install.packages("quantmod")
# install.packages("fBasics")
# install.packages("fUnitRoots")
# install.packages("fracdiff")

setwd("C:/Users/tpisk/Desktop/")

######################################################################################
######################################################################################
######################################################################################
######################################################################################

# Tsay Chapter 4, Exercise 1

da_ex1=read.table("d-spy-0111.txt",header=T)

head(da_ex1)

attach(da_ex1)

LNrtn=log(rtn+1)

# (a)

t.test(LNrtn)

Box.test(LNrtn,lag=10,type="Ljung")

acf(LNrtn)

m1=arima(LNrtn,order=c(0,0,2))

m1

tsdiag(m1)

Box.test(m1$residuals,lag=10,type="Ljung")

Box.test(m1$residuals^2,lag=10,type="Ljung")

# (b)

install.packages("fGarch")

library(fGarch)

m2=garchFit(~arma(0,2)+garch(2,1),data=LNrtn,trace=F)

summary(m2)

m2=garchFit(~arma(0,1)+garch(2,1),data=LNrtn,trace=F)

summary(m2)

##############################################################################
#
# Must un-comment the plot command below, then
# must manually choose the option to plot, and then choose 0 to exit.
#
##############################################################################

# plot(m2)

# (c)

m3=garchFit(~arma(0,1)+garch(2,1),data=LNrtn,trace=F,cond.dist="std")

summary(m3)

##############################################################################
#
# Must un-comment the plot command below, then
# must manually choose the option to plot, and then choose 0 to exit.
#
##############################################################################

# plot(m3)

# Tsay Chapter 4, Exercise 2

### percentage log returns

PCrtn=LNrtn*100

# (a)

m4=garchFit(~arma(0,1)+aparch(2,1),data=PCrtn,delta=2,include.delta=F,trace=F)

summary(m4)

# (b)

m5=garchFit(~arma(0,1)+aparch(2,1),data=PCrtn,delta=2,include.delta=F,trace=F,cond.dist="std")

summary(m5)

predict(m5,5)
###############################################################################
# Tsay Chapter 4, Exercise 3

detach(da_ex1)

da_ex3=read.table("m-ko-6111.txt",header=T)

head(da_ex3)

attach(da_ex3)

KOLNrtn=log(ko+1)

# (a)

t.test(KOLNrtn)

Box.test(KOLNrtn,lag=10,type="Ljung")

Box.test((KOLNrtn-mean(KOLNrtn))^2,lag=10,type="Ljung")

Box.test(KOLNrtn^2,lag=10,type="Ljung")

# (b)

m1=garchFit(~garch(1,1),data=KOLNrtn,trace=F)

summary(m1)

##############################################################################
#
# Must un-comment the plot command below, then
# must manually choose the option to plot, and then choose 0 to exit.
#
##############################################################################

# plot(m1)

# (c)

m2=garchFit(~garch(1,1),data=KOLNrtn,trace=F,cond.dist="std")

summary(m2)

##############################################################################
#
# Must un-comment the plot command below, then
# must manually choose the option to plot, and then choose 0 to exit.
#
##############################################################################

# plot(m2)

predict(m2,5)

# Tsay Chapter 4, Exercise 4

KOPCrtn=KOLNrtn*100

source("Tgarch11.R")

# (a)

m3=Tgarch11(KOPCrtn)

sresi=m3$residuals/m3$volatility

t.test(sresi)

Box.test(sresi,lag=10,type="Ljung")

Box.test(sresi^2,lag=10,type="Ljung")

qqnorm(sresi)

# (b)

source("Ngarch.R")

m4=Ngarch(KOPCrtn)

sresi=m4$residuals/m4$volatility

t.test(sresi)

Box.test(sresi,lag=10,type="Ljung")

Box.test(sresi^2,lag=10,type="Ljung")

qqnorm(sresi)

