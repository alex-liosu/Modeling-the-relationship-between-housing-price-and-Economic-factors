
## Codes for the linear model with auto regressive errors

library(readxl)

## read data into R

shtnames = readxl::excel_sheets('...//osu2022fall//6950//project//Home Price Prediction Dataset.xlsx')
for (i in 1:length(shtnames)) {
  a = readxl::read_xlsx('...//osu2022fall//6950//project//Home Price Prediction Dataset.xlsx',sheet = shtnames[i],na="NA")
  assign(shtnames[i],a)
}

##extract monthly data set 

library(readxl)
X_monthly=read_xlsx('...//osu2022fall//6950//project//Home Price Prediction Dataset.xlsx',sheet = shtnames[1])


summary(X_monthly)


## data cleaning and clean NAs

X_monthly=X_monthly[-1,]
Y=Y[-1,]


X_monthly_sub<-data.frame(NULL)
for (i in Y$Period){
  index<-which(X_monthly$Period==i)
  X_monthly_sub<-rbind(X_monthly_sub,X_monthly[index,])
}


nas<-is.na(X_monthly_sub)
apply(nas,2,sum)
data<-X_monthly_sub

summary(data)

## add response varaiables into data


data<-cbind(data,Y$HPI)


colnames(data)[15]<-"HPI"


data$CONST<-as.numeric(data$CONST)
data$`Permits-Number`<-as.integer(data$`Permits-Number`)
data$Consumption<-as.numeric(data$Consumption)
data$`Disposable Income`<-as.numeric(data$`Disposable Income`)

## divide data into trainging and test set
n=length(data$HPI)

ntrain=floor(0.8*n)
ntest=n-ntrain


train=1:ntrain
test=(ntrain+1):n

## fit the full model

model=lm(HPI~.,data=data[train,-1])
summary(model)


plot(model)

## Boxcox transformation

library(MASS)
boxcox(model)

## Model with transformation

model=lm(HPI^(3/2)~.,data=data[train,-1])
summary(model)

## backward stepwise procedure to select variables

library(MASS)
model_aic<-stepAIC(model,direction="both",k=log(ntrain),trace=FALSE)

summary(model_aic)

## plot predicted values

y_test=predict(model_aic,data[test,-1])
plot(data$Period[test], data$HPI[test]^(3/2))
points(data$Period[test],y_test,col="red")

## check equal variance
plot(model_aic$residuals,model_aic$fitted.values, ylab="residuals",xlab="fitted values",main="residuals v.s. fitted values")

## Normality check
qqnorm(model_aic$residuals)
qqline(model_aic$residuals)

## check autoregression 
plot(data$Period[train],model_aic$residuals, ylab="residuals",xlab="period",main="resdiuals v.s. periods")

## Test residuals' stationary
library(aTSA)
adf.test(model_aic$residuals)

## Plot ACF and PACF of residuals

acf(model_aic$residuals,main="Series residuals")
pacf(model_aic$residuals,main="Series residuals")

## AR(2) model

model2<-ar(model_aic$residuals,dmean=FALSE)
plot(model2$resid)



rho=model2$ar
model2$order

## Plot prediction of house prices with autoregressive errors

predict_new<-function(e,ypre,arm){
  e1<-predict(arm,e)
  return (ypre+e1$pred)
}



err=model_aic$residuals[ntrain]
y_pre=c()

for (i in (ntrain+1):n){
  y_m<-predict(model_aic,data[i,-1])
  y_hat<-predict_new(err,y_m,model2)
  err=data$HPI[i]^(3/2)-y_m
  y_pre=c(y_pre,y_hat)
}



plot(data$Period[test],y_test^(2/3),col="red",type = "l",lwd=2,ylim=c(170,250),ylab="HPI Index",xlab = "period",main="prediction of data in test set")
points(data$Period[test],data$HPI[test],pch=16,cex=0.5)
lines(data$Period[test],y_pre^(2/3),col="blue",lwd=2)
legend(x = "topright",          # Position
       legend = c("sample","prediction", "prediction with autoregressive error"), 
       lty=c(NA,1,1),
       pch=c(16,NA,NA),
       lwd=2,
       cex=0.65,
       col = c("black","red","blue"))      




########################################################################################################################################################################

#Codes for models with two separate periods

library(readxl)

shtnames = readxl::excel_sheets('/Users/aphmao/Desktop/STAT 6950/Project/Home Price Prediction Dataset (1).xlsx')

for (i in 1:length(shtnames)) {
  a = readxl::read_xlsx('/Users/aphmao/Desktop/STAT 6950/Project/Home Price Prediction Dataset (1).xlsx',sheet = shtnames[i],na="NA")
  assign(shtnames[i],a)
}

X_monthly=read_xlsx('/Users/aphmao/Desktop/STAT 6950/Project/Home Price Prediction Dataset.xlsx',sheet = shtnames[1])
summary(X_monthly)

X_monthly=X_monthly[-252,]
X_monthly=X_monthly[-251,]
Y=Y[-1,]
X_monthly$Period=Y$Period
Y=Y[,2]
data = cbind(X_monthly,Y)
summary(data)

data$CONST<-as.numeric(data$CONST)
data$`Permits-Number`<-as.integer(data$`Permits-Number`)
data$Consumption<-as.numeric(data$Consumption)
data$`Disposable Income`<-as.numeric(data$`Disposable Income`)

##EDA

hist(data$HPI)
summary(data$HPI)
boxplot(data$HPI,main="Boxplot of HPI")
##From the histogram, boxplot, and the summary statistics of HPI, we find that the median of HPI is around 160, and the variance is not very large. 

cormat <- round(cor(data[,-1]),2)
head(cormat)
##install.packages('reshape2')
library(reshape2)
melted_cormat <- melt(cormat)
head(melted_cormat)

# Get lower triangle of the correlation matrix
get_lower_tri<-function(cormat){
  cormat[upper.tri(cormat)] <- NA
  return(cormat)
}
# Get upper triangle of the correlation matrix
get_upper_tri <- function(cormat){
  cormat[lower.tri(cormat)]<- NA
  return(cormat)
}

upper_tri <- get_upper_tri(cormat)
upper_tri

# Melt the correlation matrix
library(reshape2)
melted_cormat <- melt(upper_tri, na.rm = TRUE)
# Heatmap
library(ggplot2)
ggplot(data = melted_cormat, aes(Var2, Var1, fill = value))+
  geom_tile(color = "white")+
  scale_fill_gradient2(low = "blue", high = "red", mid = "white", 
                       midpoint = 0, limit = c(-1,1), space = "Lab", 
                       name="Pearson\nCorrelation") +
  theme_minimal()+ 
  theme(axis.text.x = element_text(angle = 45, vjust = 1, 
                                   size = 12, hjust = 1))+
  coord_fixed()

##We find that Disposable income and consumption are positively highly correlated with HPI.
## Housing starts and permits number are positively highly correlated with homes sold.
##Permits valuation is positively highly correlated with construction spending.
##mortgage rate is negatively highly correlated with consumption and Disposable income.

plot(data$HPI,data$`Disposable Income`,xlab = 'Disposable Income', ylab = 'HPI')
plot(data$HPI,data$Consumption, xlab = 'Consumption', ylab = 'HPI')
plot(data$`Permits-Number`,data$`Housing Starts`,xlab = 'Permits Number', ylab = 'Housing Starts')
plot(data$`Permits-Valuation`,data$CONST,xlab = 'Permits Valuation', ylab = 'CONST')
plot(data$`Mortgage Rate`,data$`Disposable Income`)
plot(data$`Mortgage Rate`,data$Consumption)

plot(data$Period,data$HPI, xlab = 'Period', ylab = 'HPI')

##We find that there are nonlinear pattern between HPI, Disposable Income, and Consumption.
##There may be collinearity between housing starts and permits number, and construction spending and permits valuation.


##Preliminary model fitting
model=lm(HPI~.,data=data)
summary(model)

plot(data$Period,model$residuals)
plot(data$Period,data$`Months of Supply`)

##We find that the residuals seem to have a seasonal patterns.
attach(data)
library(broom)
augment(model)

##We use the month plot to check whether there is a seasonal patterns over the years. 
library(lubridate)
library(tidyverse)
augment(model) %>% mutate(Period = ymd(Period))

tmp <- augment(model) %>% mutate(Period = ymd(Period))

augment(model) %>% mutate(Period = ymd(Period))

augment(model) %>% mutate(Period = ymd(Period)) %>% summarize(max(Period))
(augment(model) %>% mutate(Period = ymd(Period)))$.resid

tmp <- ts((augment(model) %>% mutate(Period = ymd(Period)))$.resid,
          freq = 12,
          start = c(2000, 1), end = c(2020, 10))
tmp

monthplot(tmp)
##We find that for each month over the years, the patterns look similar. This means that there is no seasonal effects. We then use autocorrelation function and partial auto correlation function to examine autocorrelation.
acf(tmp)
pacf(tmp)

##Based on the results of acf and pacf, we find that it would be good to use first order auto correlation structure. 
plot(model)

augment(model) %>% mutate(Period = ymd(Period)) %>% 
  ggplot(aes(x=Period, y =.resid)) + geom_point()
augment(model) %>% mutate(Period = ymd(Period)) %>% 
  ggplot(aes(x=Period, y =.resid)) + geom_point() +
  geom_smooth(se=FALSE)


augment(model) %>% mutate(Period = ymd(Period)) %>% 
  ggplot(aes(x=Period, y =.resid)) + geom_point() +
  geom_smooth(se=FALSE, span=0.3)

augment(model) %>% mutate(Period = ymd(Period)) %>% 
  ggplot(aes(x=Period, y =.resid)) + geom_point() +
  geom_smooth(se=FALSE, span=0.1)

model=lm(HPI~.,data=data %>% mutate(Period = ymd(Period)))
summary(model)

plot(data$Period, data$HPI)
plot(data$Period, data$`Fed Funds Rate`)

model=lm(HPI~.-Period,data=data %>% mutate(Period = ymd(Period)))
summary(model)


plot(data$Period, resid(model))


library(GGally)
head(data)
library(GGally)
ggpairs(data)
plot(data$Period, data$HPI, xlab='Period', ylab='HPI')
##From th scatter plot of period  and HPI, it shows that before 2006 and after 2014 seem to be linear. Therefore, we decide to use the data before 2006 and after 2014 to model two linear regressions.




##We now use first order autocorrelation structure to fit a linear regression.

library("nlme")
names(data) = c("period", 'unemp', 'const', 'month_supply','mortgage','permit_num','permit_valuation','housing_starts','consumption','disposable_income','savings','fed_funds_rate','homes_for_sale','home_sold','HPI')
data_early = data[1:72,]
data_later = data[169:250,]

mod.gls <- gls(HPI~.-period,
               data=data_early, correlation=corAR1(), method="ML")
summary(mod.gls)
plot(mod.gls)

##Refit the model using only significant terms under 0.05
mod.gls.early_final <- gls(HPI~const+mortgage+consumption+disposable_income+savings+homes_for_sale,
                           data=data_early, correlation=corAR1(), method="ML")
summary(mod.gls.early_final)
plot(mod.gls.early_final)
qqnorm(mod.gls.early_final, abline = c(0,1))

mod.gls.early_final <- gls(HPI~const+consumption+disposable_income+savings+homes_for_sale,
                           data=data_early, correlation=corAR1(), method="ML")
summary(mod.gls.early_final)
plot(mod.gls.early_final)
qqnorm(mod.gls.early_final, abline = c(0,1))



later=as.data.frame(data_later)[,-1]
mod.gls.later <- gls(HPI~.,data=later, correlation = corAR1(),method="ML")
summary(mod.gls.later)
plot(mod.gls.later)
##Refit the model using only significant terms under 0.05
mod.gls.later_final <- gls(HPI~const+consumption+disposable_income+savings+fed_funds_rate+homes_for_sale,
                           data=data_later, correlation=corAR1(), method="ML")
summary(mod.gls.later_final)
plot(mod.gls.later_final)
qqnorm(mod.gls.later_final, abline = c(0,1))

##Refit the model using only significant terms under 0.05
mod.gls.later_final <- gls(HPI~const+consumption+disposable_income+savings,
                           data=data_later, correlation=corAR1(), method="ML")
summary(mod.gls.later_final)
plot(fitted(mod.gls.later_final),resid(mod.gls.later_final))
qqnorm(mod.gls.later_final, abline = c(0,1))
plot(mod.gls.later_final$fitted,mod.gls.later_final$residuals, ylab='residuals',xlab='fitted values')

plot(data_later$period,mod.gls.later$residuals)

model.later$residuals



library(MASS)
boxcox(lm(HPI~.,data = data_early[,-1]))

mod.gls.early <- gls(log(HPI)~.,
                     data=data_early[,-1], correlation=corAR1(), method="ML")
summary(mod.gls.early)
plot(mod.gls.early)

##Refit the model using only significant terms
mod.gls.early_refiit <- gls(log(HPI)~const+mortgage+consumption+disposable_income+savings,
                            data=data_early[,-1], correlation=corAR1(), method="ML")
summary(mod.gls.early_refiit)
plot(mod.gls.early_refiit)


mod.gls.early_final <- lm(log(HPI)~const+mortgage+consumption+disposable_income+savings,
                          data=data_early[,-1])
summary(mod.gls.early_final)
plot(mod.gls.early_final$fitted.values,mod.gls.early_final$residuals, ylab = 'residuals', xlab = 'fitted values')

plot(mod.gls.early_final)


