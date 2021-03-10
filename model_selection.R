library(forecast)
library(caschrono)
library(ggplot2)
data("m30")
z=supsmu(x = 1:length(m30),y=m30)  ### Smoothing method to obtain the trend  of the TS 
library(ggfortify)
ts1 <- m30
ts2 <- z$y
autoplot(ts( cbind(ts1, ts2)  , start = c(1973,7), frequency = 12 ),
         facets = FALSE,xlab = "",ylab = "")+theme_bw()+theme(legend.position = 'none')



### Detect outliers 

x=tsoutliers(m30,lambda = NULL)
x$index
m30[x$index] 
x$replacements

#### Data without outliers 

m30b=m30
m30b[x$index]=x$replacements

#### Plot with/without outliers


autoplot(ts( cbind(m30, m30b)  , start = c(1973,7), frequency = 12 ),
         facets = FALSE,xlab = "",ylab = "")+theme_bw()+theme(legend.position='top')


#### Box Cox transformation

## loglik
lam1=BoxCox.lambda(m30b,method = "loglik",lower = 0,upper = 10)
lam1

## Guerrero
lam2=BoxCox.lambda(m30b,method = "guerrero",lower = 0,upper = 10)
lam2


### Auto Arima with Box Cox transformation

lmA=auto.arima(m30b,lambda = lam1)
lmA

lmB=auto.arima(m30b,lambda = 0)  # since the Box Cox transformation provides negative AIC, this model can't
                                 # be approved
lmB


#### Residual Analysis/Standard residuals

residuals=lmA$residuals

stdres <- residuals/sqrt(lmA$sigma2)
ggtsdisplay(stdres,lag.max=30,theme = theme_bw())

### No trend, no drift

library(urca)
t1<-ur.df(residuals,type = "none")
summary(t1)

library(tseries)
t2=adf.test(residuals,k = 0)
t2

### What kind of ARMA model fits our resuduals

armaselect(residuals,nbmod = 10)  #p=q=0

### Box-Test (WN residuals)

dt=data.frame(lag=1:30,
              pvalues=sapply(1:30,
                             function(i)
                               Box.test(stdres,lag = i,type = "Ljung-Box")$p.value))
### Use Bonferoni adjustment

dt$padj=p.adjust(dt$pvalues, 
                 method = "bonferroni")

dt$padj  ### all the pvalues are greater than 5%

### Selected model with the 1st method

lmA

t_stat(lmA)


#### Second method

### Estimate p,d,q,P,D,Q,S

m30b%>%BoxCox(lambda = lam1)%>%diff(lag=12)%>%autoplot()+theme_bw()+xlab("Year")+ylab("")


### Stationary test

t3<-m30b%>%BoxCox(lambda = lam1)%>%ur.df(type="none",selectlags = 'AIC')
t3%>%summary


t4<-m30b%>%BoxCox(lambda = lam1)%>%diff(lag=12)%>%ur.df(type="none",selectlags = 'AIC')
t4%>%summary


### Determinig the orders (graphicaly)


m30b%>%BoxCox(lambda = lam1)%>%diff(lag=12)%>%ggtsdisplay(smooth = T,theme=theme_bw())

### We cab conclude 

## AR=0,1,2
## SAR=0,1,2,3
## MA=0,1,2,3
## SAM=0,1
## d=0
## D=1
## S=12

m30b%>%BoxCox(lambda = lam1)%>%diff(lag=12)%>%t.test()  ## With drift



### Orders of the set of models

orders=list(p=c(0,1,2),d=0,q=c(0,1,2,3),
               P=c(0,1,2,3),D=1,Q=c(0,1),
                        S=12)
all_orders=expand.grid(orders$p,orders$d,orders$q,
                         orders$P,orders$D,orders$Q,
                         orders$S)

all_orders
dim(all_orders)
class(all_orders)
all_orders=as.matrix(all_orders,96,7)
colnames(all_orders)=c("p","d","q","P","D","Q","T")



models=vector('list',nrow(all_orders))
p=nrow(all_orders)

for(i in 1:p){
    cat("\r",i,"/",p)
    od=as.vector(all_orders[i,1:3])
    od_s=as.vector(all_orders[i,4:6])
   models[[i]]=try(Arima(m30b,order=od,seasonal=list(order=od_s,period=12),
                                  lambda=lam1,
                                  include.drift = T))


}


### Extract AIC

 aic=rep(NA,nrow(all_orders))
  for(i in 1:nrow(all_orders)){
        aic[i]=try(as.numeric(models[[i]]$aic))
    }
 aic=as.numeric(aic)

 mod_names=unlist(lapply(models,function(x)as.character(x)))
 
 i=grep(pattern = "ARIMA",x = mod_names)
 mod_names[-i]=NA
 dt=data.frame(model=mod_names,aic=aic)
 i=order(dt$aic,decreasing = F)[1:5]
 dt[i,]
dim(dt)

### Best selected model is 

dt[1,]

### T-test on the coefficients

t_stat(models[[89]])

### Extract the models with only significant coefficients


 x=rep(NA,nrow(all_orders))
 for(i in 1:nrow(all_orders)){
     x[i]=try(prod(t_stat(models[[i]])[2,]<=0.05))
   }
 x=as.numeric(x)
 xtabs(~x)
x
dt=dt[which(x==1),]
dim(dt)

dt[dt$aic,]

#### Extract the models with WN residuals
## Among the best 4 arma models fitting the residuals we should have at least one WN

 j=as.numeric(rownames(dt))
 x=rep(NA,nrow(dt))
 for(i in 1:nrow(dt)){
   cat("\r",i,"/",nrow(dt))
     m=models[[j[i]]]
     tt=armaselect(m$residuals,nbmod = 4)
     x[i]=sum(rowSums(tt[,1:2])==0)
 }
 xtabs(~x)
 dt=dt[which(x==1),]
 dim(dt)
 dt[order(dt$aic,decreasing = F),]

 ### The best selected model is models[[53]]

 
models[[53]]
t_stat(models[[53]]) 
armaselect(models[[53]]$residuals,nbmod = 4)

### Comparing with the one in the 1st method

lmA
models[[53]]


### Forecasting using the model 1

models[[53]]%>%forecast(h=10,level=95,lambda=NULL)%>%
  autoplot()+theme_bw()

### Estimating prediction error

library(DMwR)

## 

m30b_tr<-window(m30b, end = c(2006))

length(m30b_tr)
mod1<-Arima(m30b_tr,order = c(1,0,1),
           seasonal = list(order=c(2,1,1),period=12),lambda = lam1)
mod1
t_stat(mod1)

f1<-forecast(mod1,h=length(m30b)-length(m30b_tr),level=95,lambda=lam1,biasadj = T)
f1
error_1=DMwR::regr.eval(window(m30b, start = c(2006,2)), f1$mean)

error_1

all_orders[53,]

models[[53]]

mod2<-Arima(m30b_tr,order = c(1,0,1),
            seasonal = list(order=c(0,1,1),period=12),lambda = lam1)
mod2
t_stat(mod2)



f2<-forecast(mod2,h=length(m30b)-length(m30b_tr),level=95,lambda=lam1,biasadj = T)
f2
error_2=DMwR::regr.eval(window(m30b, start = c(2006,2)), f2$mean)
error_2

error_1-error_2

mod3<-Arima(m30b_tr,order = c(2,0,2),
            seasonal = list(order=c(4,1,1),period=12),lambda = lam1)

f3<-forecast(mod3,h=length(m30b)-length(m30b_tr),level=95,lambda=lam1,biasadj = T)
f3
error_3=DMwR::regr.eval(window(m30b, start = c(2006,2)), f3$mean)
error_3

error_3-error_2  ### The prediction error is lower when we plug more parameters in the model 


mod4<-Arima(m30b_tr,order = c(5,0,2),
            seasonal = list(order=c(4,1,1),period=12),lambda = lam1)


f4<-forecast(mod4,h=length(m30b)-length(m30b_tr),level=95,lambda=lam1,biasadj = T)

error_4=DMwR::regr.eval(window(m30b, start = c(2006,2)), f3$mean)
error_4
error_4-error_2


### Cross validation

## Forecast function
## two arguments
## x is the TS
## h is the horizon of the predictions

forefunction=function(x,h,lambda=lam1){
  modx<-Arima(x,order = c(1,0,1),
              seasonal = list(order=c(0,1,1),
                              period=12),
              lambda = lambda)
  forecast(modx,h=h)
  }


er<- tsCV(m30b,forefunction,h=1)
er

## How it works?


 h=1
 TT=length(m30b)
 tt=20
 x=m30b[1:(TT-h)]
 mod<-Arima(x[1:tt],order = c(1,0,1),
                         seasonal = list(order=c(0,1,1),period=12),
                         lambda = lam1)
 xhat=forecast(mod,h=h)
 xhat
 x[tt+h]-xhat$mean[1]
er[tt]


### Selecting the best model with CV

## Selecting the best 5 models

bestmodels=vector('list',5)
dt=dt[order(dt$aic,decreasing = F),]
dt=dt[1:5,]
j=as.numeric(rownames(dt))
for(k in 1:5) bestmodels[[k]]=models[[j[k]]] 
 
### Estimating the errors by Cross validation
err_cv=vector('list',5)
 for(k in 1:5){
     cat("\r",k)
     zz=bestmodels[[k]]$arma
     od=c(zz[1],zz[6],zz[2])
     od_s=c(zz[3],zz[7],zz[4])
     forefunction=function(x,h){
         modx<-Arima(x,order = od,seasonal = list(order=od_s,lambda = lam1))
         forecast(modx,h=h)
       }
     err_cv[[k]]=tsCV(m30b,forefunction,h=3)
 }


### 


 for(k in 1:5) names(err_cv)[k]=as.character(dt[k,1])
 
 err_cv_d=plyr::ldply(err_cv)
 
 err_cv_dw=reshape2::melt(err_cv_d,measure.vars = 2:4)
 err_cv_dw=na.omit(err_cv_dw)
 
 ### Boxplot 
 colnames(err_cv_dw)=c("Model","h","Error")
 p<-ggplot(err_cv_dw,aes(y=Error,x=Model))+geom_boxplot(aes(fill=h),alpha=.4)+theme_bw()
 p+coord_flip()  ### but we can't see a huge difference 
 
 

### Compute RMSE
 
  
rmse=err_cv_dw%>%group_by(Model,h)%>%
   summarise(rmse=sqrt(sum(Error^2)))
  
p<-ggplot(rmse,aes(y=rmse,x=h,group=Model))+
   geom_line(aes(color=Model))+
  geom_point(aes(color=Model),size=3)+theme_bw()
p+ylab("RMSE")

