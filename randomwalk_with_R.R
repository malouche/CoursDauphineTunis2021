################################################
### Random walk with drift lambda=0 ############
################################################

w = rnorm(200,0,1) 
x = cumsum(w) 
y=ts(x,start=c(1960,1),frequency = 12)
y


autoplot(y)+labs(title="Random walk",
                 subtitle="with zero drift",
                 caption="")+
  xlab("Date")+ylab("")+ggcharts::theme_ggcharts(axis = "x", grid = "X")



################################################
### Random walk with drift lambda>0 ############
################################################


wd = w +.2
xd = cumsum(wd)


yd=ts(xd,start=c(1960,1),frequency = 12)
yd


autoplot(yd)+labs(title="Random walk",
                 subtitle="with  drift",
                 caption="")+
  xlab("Date")+ylab("")+ggcharts::theme_ggcharts(axis = "x", grid = "X")


################################################
## Both TS in the same plot     ################
################################################


dt=cbind(y,yd)
dt%>%head()

autoplot(dt,facets = F)+ylab("")+xlab("")+
  ggtitle("Random walks")+
  xlab("Date")+ylab("")+ggcharts::theme_ggcharts(axis = "x", grid = "X")
