###  The weekly cardiovascular mortality rate in Los Angeles County Data ###

## Library: 

library(astsa)
library(ggplot2)
library(ggthemes)


dir(path = "~/Desktop/Machine Learning & Programing/Time Series & Forecasting")

x = scan("cmort.dat")
print(x)
x = ts(x)



## Plot the time series: 

plot(x, type = "b", main = "Time series of Mortality Rate")


### Plot the autocorrelation: 

acf(x, xlim = c(1,18))


### Plot the xt versus x(t-1): 

lag1.plot(x)


## Define lag1

xlag1 = lag(x, -1)

y = cbind(x, xlag1)
y = as.data.frame(y)
print(y)



## Fit the Autoregression order =1: 

ar1Fit = lm(x ~ xlag1, data = y)


## Print the result: 

summary(ar1Fit)


## Plot of residual and fit values: 

residuals = ar1Fit$residuals
fitvals = ar1Fit$fitted.values

DataFile = cbind(residuals, fitvals)
DataFile = as.data.frame(DataFile)


ggplot(data = DataFile, aes(x = fitvals, y = residuals))+geom_point(shape = 5, col = "black", size = 1) + geom_hline(yintercept = 0, col = "red", linetype = "dashed") + ggtitle(label  = "Residual versus Fitted Values") + xlab(label = "Fitted Values") + ylab(label = "Residuals")



### Plot autocorrelation of residuals: 

acf(residuals, xlim = c(1,20))




