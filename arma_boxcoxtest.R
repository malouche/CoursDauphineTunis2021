

### X_t=.5+X_{t-1}+Z_t+2 Z_{t-1}

library(sarima)
x=sim_sarima(n=200, model = list(ar=-.5,ma=2))
x[1:4]

f=function(h) 5*.5^h
f(1:5)
ARMAtoMA(ar=.5,ma=2,lag.max=5)
TacvfARMA(phi=.5,theta =2,lag.max=5)


### Box text


library(caschrono)
set.seed(123)
y1=arima.sim(n=100,list(ma=c(-.7,.3)),sd=sqrt(4))
library(forecast)
library(ggplot2)
ggtsdisplay(y1, plot.type='scatter',theme = theme_bw())


a1=Box.test.2(y1,nlag=1:10,type="Ljung-Box")
a1

y2<-rnorm(100)
a2=Box.test.2(y2,nlag=1:10,type="Ljung-Box")
a2


## X_t=-.3X_{t-1}+.1X_{t-2}+Z_t

y3=arima.sim(n=100,list(ar=c(-.3,.1)),sd=sqrt(4))
ggtsdisplay(y3, plot.type='scatter',theme = theme_bw())
library(ZIM)
z=y3+.3*bshift(y3,1)-.1*bshift(y3,2)
b1=Box.test.2(y3,nlag=1:10,type="Ljung-Box")


