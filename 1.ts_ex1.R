### Time Series Exercises ### 

## Library ##

library(astsa)
library(ggplot2)
library(ggthemes)

## Read the data file: 
x = read.table(file = "quakes.dat")
print(x)


## Specialize x as time series: 

x = ts(x) 

plot(x, type = "b", main = "Time Series Plot of Quakes Data")
lag1.plot(series = x, max.lag = 1)


### Plot of autocorrelation function: 

## acf for x from lag1 to lag20: 
acf(x, xlim = c(1,20))

### Lag1 of x: 
xlag1 = lag(x, -1)
y = cbind(x, xlag1)
print(y)

### Fit the AR(1) model: 
ar1fit = lm(y[,1]~y[,2])

### Print out thhe summary results: 

summary(ar1fit)

## Slope is -0.003721 and intercept is 0.116742

Resval = ar1fit$residuals
Fitval = ar1fit$fitted.values

###ACF of the residuals values: 

acf(Resval)


## Plot of fit vals versus residuals values: 

DF_assumption = cbind(Resval, Fitval)
DF_assumption = as.data.frame(DF_assumption)

Plot1 = ggplot(data = DF_assumption, 
               mapping = aes(x = Fitval, y = Resval)) + geom_point(col = 'blue', size = 1, shape = 5) + ggtitle(label = "Plot of Residuals versus Fitted values") + xlab(label = "Fitted Values") + ylab(label = "Residual Values") + theme_base() + geom_hline(yintercept = 0, col = "red", size = 0.5, linetype = "dashed")
print(Plot1)
