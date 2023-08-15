library(quantmod)
library(xts)

library(tseries)

##Chosen traded asset: Apple stock##

##Download apple stock prices
tickr <- "AAPL"
sdate <- '2008-01-01'
edate <- '2023-03-31'

#Download daily stock price and remove NA values
getSymbols(tickr, src = "yahoo", from = sdate, to = edate)
appl_d <- Ad(AAPL)
appl_d <- na.omit(appl_d)

#Download monthly stock price and remove NA values
getSymbols(tickr, src = "yahoo", from = sdate, to = Sys.Date(), 
           periodicity = 'monthly')
appl_m <- Ad(AAPL)
appl_m <- na.omit(appl_m)

#Calculate log returns for daily and monthly prices
appl_d_lr <- diff(log(appl_d))
appl_d_lr <- na.omit(appl_d_lr)

appl_m_lr <- diff(log(appl_m))
appl_m_lr <- na.omit(appl_m_lr)

#Converting the price and returns into time series
price_d <- ts(appl_d)
price_m <- ts(appl_m, frequency=12, start=c(2008,1))

ret_d <- ts(appl_d_lr)
ret_m <- ts(appl_m_lr, frequency=12, start=c(2008,1))

#Basic statistical examination of prices and returns
library(fBasics)

basicStats(appl_d)
basicStats(appl_d_lr)

###Visualization###
library(ggplot2)

##Time series chart of prices
appl_df <- data.frame(date = index(AAPL), price = Ad(AAPL))

ggplot(appl_df, aes(x = date, y = AAPL.Adjusted)) + geom_line(color = "red") +
  geom_ribbon(aes(ymin = 0, ymax = AAPL.Adjusted), alpha = 0.1, fill = "red") +
  labs(x = "Date", y = "Price ($)", 
       title = "Time-series chart of Apple stock price") + theme_bw() 

##Histogram of returns
ggplot(data = appl_d_lr, aes(x = appl_d_lr)) +
  geom_histogram(binwidth = 0.01, fill = "red", color = "white", alpha = 0.6) +
  labs(x = "Log Returns", y = "", title = "Histogram of Apple Log Returns") + 
  theme(panel.grid.major = element_line(color = "grey", size = 0.1),
        panel.grid.minor = element_blank(), panel.border = element_blank(),
        axis.line = element_line(color = "black")) + theme_bw()


##ACF and PACF of prices and returns
par(mfrow=c(2,2))
acf(price_m, main = "ACF of prices")
pacf(price_m, main = "PACF of prices")
acf(ret_d, main = "ACF of returns")
pacf(ret_d, main = "PACF of returns")

# #Perform skewness and kurtosis tests on prices
# sk_p = skewness(price_d);
# T <- length(price_d);
# tst = abs(sk_p/sqrt(6/T))
# pv <- 2*(1-pnorm(tst))
# pv
# 
# kt_p <- kurtosis(price_m)
# tst <- abs(kt_p/sqrt(24/T))
# pv <- 2*(1-pnorm(tst))
# pv

#Perform skewness and kurtosis tests on log returns
sk_r = skewness(ret_d);
T <- length(ret_d); 
tst = abs(sk_r/sqrt(6/T))
pv <- 2*(1-pnorm(tst))
pv

kt_r <- kurtosis(ret_d)
tst <- abs(kt_r/sqrt(24/T))
pv <- 2*(1-pnorm(tst))
pv

# ##Unit root test on prices
# #Perform test for mean price being zero
# t.test(price_m)
# 
# #Perform normality test using the Jaque-Bera method
# normalTest(price_m, method="jb")
# 
# #Ljung box test
# Box.test(price_m, lag = 5, type = "Ljung")
# 
# #ACF and PACF of prices to determine order of models
# par(mfrow=c(1,2))
# acf(price_m)
# pacf(price_m)


##Unit root test on Returns
#Perform test for mean return being zero
t.test(ret_d)
#p<0.05, reject null, mean does not equal zero

#Perform normality test using the Jarque-Bera method
normalTest(ret_d, method="jb")
#p<0.05, reject null, distribution is not normally distributed

#Ljung box test
log(T)
Box.test(ret_d, lag = 8, type = "Ljung")
#p<0.05, reject null, autocorrelation exists in the return series

#Perform dickey-fuller test on log returns
adf.test(ret_d)
#p<0.05, reject null, log returns are stationary

#ACF and PACF of returns to determine order of models
par(mfrow=c(1,2))
acf(ret_d)
pacf(ret_d)
#ACF suggests lag 1, lag 4, lag 6
#PACF suggests lag 1, lag 4, lag 6
#Therefore, we can try AR(1), AR(4) models

##EACF
library(forecast)
library(TSA)

#For prices
#Price with differencing (to remove trend)
par(mfrow=c(1,2))
acf(diff(price_m))
pacf(diff(price_m))

adf.test(diff(price_m))

eacf(diff(price_m), ar.max = 7, ma.max = 13)
#(0,2), (0,3), (1,2), (0,4), (3,5)

##Price models##
auto.arima(price_m)
m1 = Arima(price_m, order = c(0,1,2))
m2 = Arima(price_m, order = c(0,1,3))
m3 = Arima(price_m, order = c(1,1,2))
m4 = Arima(price_m, order = c(0,1,4))
m5 = Arima(price_m, order = c(3,1,5)) #Chosen model based on AIC

#dataframe of AIC
aic_p <- list(m1$aic, m2$aic, m3$aic, m4$aic, m5$aic)
print(aic_p)

#Residual Checking
#Check Residuals

# checkresiduals(m1, lag=10)
# checkresiduals(m2, lag=10)
# checkresiduals(m3, lag=10)
# checkresiduals(m4, lag=10)
checkresiduals(m5, lag=10)

#Ljung box test for residuals
Box.test(m5$residuals, lag = 10, type = "Ljung")
#Box.test(m1$residuals, lag = 10, type = "Ljung")
#Box.test(m2$residuals, lag = 10, type = "Ljung")
#Box.test(m3$residuals, lag = 10, type = "Ljung")
#Box.test(m4$residuals, lag = 10, type = "Ljung")

qqnorm(m5$residuals)

#Forecasting prices
forecast_p <- forecast(m5, h=10,level=c(90,95))
plot(forecast_p)

#####Model fitting on returns#####

#EACF for returns
eacf(ret_d, ar.max = 7, ma.max = 13)
#(0,1), (0,2), (0,4), (1,1), (1,2), (1,4), (2,1), (2,2), (2,3), (2,4)

#Fitting different models
auto.arima(ret_d)
#auto arima suggests (2,0,0)

m6 = Arima(ret_d, order = c(2,0,0)) #auto arima
m7 = Arima(ret_d, order = c(0,0,1)) 
m8 = Arima(ret_d, order = c(0,0,2)) 
m9 = Arima(ret_d, order = c(0,0,4)) #Chosen model based on lowest AIC
m10 = Arima(ret_d, order = c(1,0,1))
m11 = Arima(ret_d, order = c(1,0,2))
m12 = Arima(ret_d, order = c(1,0,4))
m13 = Arima(ret_d, order = c(2,0,1))
m14 = Arima(ret_d, order = c(2,0,2))
m15 = Arima(ret_d, order = c(2,0,3))
m16 = Arima(ret_d, order = c(2,0,4))

#dataframe of AIC
aic_r <- list(m6$aic, m7$aic, m8$aic, m9$aic, m10$aic, m11$aic, m12$aic, m13$aic,
              m14$aic, m15$aic, m16$aic)
print(aic_r)

#Check residuals

checkresiduals(m9, lag=10)

#Ljung box test for residuals
Box.test(m9$residuals, lag = 8, type = "Ljung")
#p>0.05, fail to reject null, no autocorrelation observed in residuals

qqnorm(m9$residuals)

#Forecasting
forecast_r <- forecast(m9, h=10,level=c(90,95))
plot(forecast_r)

#VAR MODEL
library(quantmod)
library(ggplot2)

# Set the start and end dates as Date type
start_date <- as.Date("2008-01-01")
end_date <- as.Date('2023-03-31')

#Download daily prices for Apple and S&P 500 using quantmod
getSymbols(c("AAPL", "^GSPC"), src = "yahoo", from = start_date, to = end_date)

# Extract the 'Close' column from the data for Apple and S&P 500
apple_prices <- data.frame(Date = index(AAPL), Close = as.numeric(AAPL$AAPL.Close))
sp500_prices <- data.frame(Date = index(GSPC), Close = as.numeric(GSPC$GSPC.Close))

#Analysis-
#We have considered the daily price data of apple stock and compared its movement with 
#S&P 500.
#We considered S&P 500 price movement with the price of Apple.
#Reasons for considering the apple stock price is- 
#Apple stock prices and the S&P 500 moves in the same direction. 
#By analyzing both together, you can identify patterns and trends in the broader 
#market. and how they relate to Apple specifically.
#S&P 500 is used as a benchmark for performance of the stock market, so analyzing 
#Apple's performance relative to it can give you a sense of how well the company is 
#doing compared to its peers. 


#Merge the data into a single data frame
merged_prices <- merge(apple_prices, sp500_prices, by = "Date")
apple_prices$log_price <- log(apple_prices$Close)
sp500_prices$log_price <- log(sp500_prices$Close)
#Analysis
#Prices are not stationary, hence we cannot apply VAR method directly.
#Took Log of Prices and the movement happens to be the same direction again.

# Merge the data into a single data frame
merged_prices <- merge(apple_prices, sp500_prices, by = "Date")

# Create a line plot with two lines, one for each stock
ggplot(merged_prices, aes(x = Date)) +
  geom_line(aes(y = log_price.x), color = 'blue') +
  geom_line(aes(y = log_price.y), color = 'red') +
  
  # Set the title and y-axis label
  labs(title = "Log Prices of Apple and S&P 500", y = "Log Price ($)") +
  
  # Adjust x-axis labels
  scale_x_date(date_breaks = "1 year", date_labels = "%Y")
#Analysis-
#Looking at the graph of the log prices of Apple and S&P 500 from 2008-01-01 
#to 2023-03-31, we can see that both Apple and S&P 500 have had significant growth 
#over the years, but with some fluctuations and periods of decline.Starting with 
#Apple, we can see that the log prices of the company's stocks have steadily 
#increased over time. However, there are some noticeable periods of decline, such as 
#in 2008-2009 during the financial crisis, and again in 2015-2016. In both cases, 
#the log prices eventually recovered and continued to grow.Looking at the S&P 500, 
#we can also see that the log prices have grown over the years, but with some 
#significant fluctuations. There were notable dips in 2008-2009 during the financial 
#crisis, and again in 2020 during the COVID-19 pandemic. However, in both cases, 
#the index eventually recovered and continued to grow.Comparing the two graphs, we 
#can see that there are some periods where Apple outperforms the S&P 500, and other 
#periods where the opposite is true. For example, from 2008-2012, Apple's log prices 
#grew at a much faster rate than the S&P 500, while from 2013-2016, the S&P 500 
#outperformed Apple. More recently, from 2017-2023, Apple has once again outperformed 
#the S&P 500.Overall, the graphs suggest that both Apple and the S&P 500 have 
#experienced significant growth over the past decade and a half, despite some 
#periods of decline and volatility. Investors in both Apple and the broader market h
#have seen positive returns over this time period, but with some variations in 
#performance over different periods.

library(MTS)
head(apple_prices)
nrow(apple_prices)
head(sp500_prices)
nrow(sp500_prices)
da1=cbind(apple_prices$log_price,sp500_prices$log_price)
nrow(da1)
head(da1)
xt1=da1[,1:2]
ccm(xt1)
#All the lags look significant.
#Taking the first difference.

rt1=diffM(log(xt1)) ### diffrencing all series
ccm(rt1) 
#after we difference the matrices of log of price, we see that the CCM looks
#better with less significant lags, meaning less dependence. 
#CCM is still not the most informative method for determining 
#the lag.
#Analysis- Plot the cross-correlation matrices, we saw that they are all significant. 
#Difference the matrices of log of price, we see that the CCM looks better with less 
#significant lags, meaning less dependence. 
#Reasons why differencing has improved the results is-
#It might would have removed the trend component of the data.
#It can reduce the impact of autocorrelation on the results.

#Order specification
#Order determination is the process of selecting the appropriate number of lags to 
#include in a VAR model.Determined the order by examining the autocorrelation and 
#partial autocorrelation functions of the variables in the system, and selecting the 
#lag order that best captures the relationship between the variables while minimizing 
#any remaining autocorrelation in the model residuals.Decision criteria for 
#VAR was based on Akaike Information Criterion (AIC) or Bayesian Information Criterion (BIC).

VARorder(rt1) 
m1=VAR(rt1,p = 13) ### Fit a VAR(13) model
m1$coef
m2=VAR(rt1,p = 1) ### Fit a VAR(1) model
m2$coef
m3=VAR(rt1,p = 9) ### Fit a VAR(9) model
m3$coef

### Model checking

MTSdiag(m1, gof = 24, adj = 0, level = F)
MTSdiag(m2, gof = 24, adj = 0, level = F)
MTSdiag(m3, gof = 24, adj = 0, level = F)
#Analysis-
#Looking at p-values of Ljung-Box Statistics, we have considered model1 be the 
#most suitable model as the p values of Ljung Box statistics looked least significant.

### Model_Forecasting
library(vars)
library(forecast)
forecasts <- predict(m3,n.ahead = 260, ci=0.95)
plot(forecasts)


#####ARCH_GARCH#####

###For DAILY returns###

#Plot the returns time series, ACF and PACF
plot(ts(ret_d))

#ACF and PACF of returns
par(mfrow=c(1,2))
acf(ret_d)
pacf(ret_d)

#Chosen ARMA model for returns
m9 = Arima(ret_d, order = c(0,0,4))
m9

checkresiduals(m9)

ret_r = resid(m9) #Taken for further analysis

#ACF & PACF of absolute and Squared Residuals
ret_rr=ret_r^2
ret_r_abs = abs(ret_r)

par(mfrow=c(1,2))
acf(as.vector(ret_rr),main="ACF of Squared Residuals"); 
pacf(as.vector(ret_rr),main="PACF of Squared Residuals") # homoscedasticity check

par(mfrow=c(1,2))
acf(as.vector(ret_r_abs),main="ACF of Absolute Residuals"); 
pacf(as.vector(ret_r_abs),main="PACF of Absolute Residuals") # homoscedasticity check
#Heteroscedasticity detected, doing ARCH effect test below to confirm
#Cannot determine GARCH order

#ARCH test on returns 

#Lagrange multiplier test
at = ret_r - mean(ret_r)
Box.test(at^2, lag = 8, type = 'Ljung')
#p<0.05, reject Ho, presence of ARCH effect

library(MTS)
archTest(ret_r)
#p<0.05, Confirmed: Reject Ho =  Presence of ARCH effect

##EACF to determine GARCH order
#Squared residuals
eacf(ret_rr)
#Not clear

#Absolute residuals
eacf(ret_r_abs)
#(1,1), (1,2), (2,2), (2,3), (3,2), (3,3)
#Going forward with absolute residuals as squared residuals is not clear

#GARCH Model fitting on returns
library(rugarch)

#Selecting GARCH order
g1 = garch(ret_r, order = c(1,1))
g2 = garch(ret_r, order = c(1,2)) #Chosen model based on lowest AIC
g3 = garch(ret_r, order = c(2,2))
g4 = garch(ret_r, order = c(2,3))
g5 = garch(ret_r, order = c(3,2))
g6 = garch(ret_r, order = c(3,3))

aic_res <- list(AIC(g1), AIC(g2), AIC(g3), AIC(g4), AIC(g5), AIC(g6))
print(aic_res)

summary(g2)
#Ljung box test p>0.05, suggests no autocorrelation

#Checking normality assumption 
qqnorm(residuals(g2)); qqline(residuals(g2))
#plot looks normal

##Fitting our chosen GARCH(1,2) model with chosen ARMA(1,4) mean model
#Specify the mean and garch models
spec <- ugarchspec(mean.model = list(armaOrder = c(0,4)), 
                   variance.model = list(model = "sGARCH", garchOrder = c(1,2)))

def.fit = ugarchfit(spec = spec, data = ret_d)
print(def.fit)
#No sign bias, joint effect exists
#Moving to EGARCH and TGARCH to remove joint effect

##Fitting different GARCH models

#EGARCH
spec1 = ugarchspec(variance.model=list(model="eGARCH", garchOrder=c(1,2)), 
                   mean.model=list(armaOrder=c(0,4), include.mean=TRUE),  
                   distribution.model="norm") 

dft.fit1 = ugarchfit(spec = spec1, data = ret_d)
print(dft.fit1)
#Joint effect not seen

#TGARCH
spec2 = ugarchspec(variance.model=list(model="fGARCH",submodel = "TGARCH",
                                       garchOrder=c(1,2)), 
                   mean.model=list(armaOrder=c(0,4), include.mean=TRUE), 
                   distribution.model="norm") 

dft.fit2 = ugarchfit(spec = spec2, data = ret_d)
print(dft.fit2)

#Plots to check volatility and distribution
plot(dft.fit1,which=9)
plot(dft.fit2,which=9)

#QQ plot suggests student t distribution

#EGARCH with t-distribution
spec3 = ugarchspec(variance.model=list(model="eGARCH", garchOrder=c(1,2)), 
                   mean.model=list(armaOrder=c(0,4), include.mean=TRUE),  
                   distribution.model="std") 

dft.fit3 = ugarchfit(spec = spec3, data = ret_d)
print(dft.fit3)

#TGARCH with t-distribution
spec4 = ugarchspec(variance.model=list(model="fGARCH",submodel = "TGARCH", 
                                       garchOrder=c(1,2)), 
                   mean.model=list(armaOrder=c(0,4), include.mean=TRUE),  
                   distribution.model="std") 

dft.fit4 = ugarchfit(spec = spec4, data = ret_d)
print(dft.fit4)
#Lowest AIC

#GARCH-M
spec5 = ugarchspec(variance.model=list(model="eGARCH",
                                       garchOrder=c(1,2)), 
                   mean.model=list(armaOrder=c(0,4), 
                                   include.mean=TRUE, archm = TRUE), 
                   distribution.model="std") 

dft.fit5 = ugarchfit(spec = spec5, data = ret_d)
print(dft.fit5)
#archm coefficient is not significant

plot(dft.fit4,which=3)
plot(dft.fit4,which=9)
#QQ plot looks normal

#Forecasting
bootp1 = ugarchboot(dft.fit4, method=c("Partial","Full")[1], n.ahead = 12,
                    n.bootpred=1000,n.bootfit=1000)
bootp1

plot(bootp1,which=3) #Volatility forecast
plot(bootp1,which=2) #Returns forecast





library(quantmod)
library(xts)

library(tseries)


##Download apple stock prices
tickr <- "AAPL"
sdate <- '2008-01-01'
edate <- '2023-03-31'

#Download daily stock price and remove NA values
getSymbols(tickr, src = "yahoo", from = sdate, to = edate)
appl_d <- Ad(AAPL)
appl_d <- na.omit(appl_d)

#Download monthly stock price and remove NA values
getSymbols(tickr, src = "yahoo", from = sdate, to = Sys.Date(), 
           periodicity = 'monthly')
appl_m <- Ad(AAPL)
appl_m <- na.omit(appl_m)

#Calculate log returns for daily and monthly prices
appl_d_lr <- diff(log(appl_d))
appl_d_lr <- na.omit(appl_d_lr)

appl_m_lr <- diff(log(appl_m))
appl_m_lr <- na.omit(appl_m_lr)

#Converting the price and returns into time series
price_d <- ts(appl_d)
price_m <- ts(appl_m, frequency=12, start=c(2008,1))

ret_d <- ts(appl_d_lr)
ret_m <- ts(appl_m_lr, frequency=12, start=c(2008,1))



aapl=log(ret_d+1)
naapl=-aapl    ## loss value
head(naapl)


##RISK MODELLING

### RiskMetrics #########
source("RMfit.R")
RMfit(naapl)
### One can use default parameter beta = 0.96 wihtout estimation with the command
RMfit(naapl,estim=F)


### Econometric modeling
library(fGarch)
#Student-t distribution
m1=garchFit(~garch(1,2),data=naapl,trace=F,cond.dist="std")
summary(m1)
pm1=predict(m1,10)
pm1
source("RMeasure.R")
RMeasure(-.001491,.0139,cond.dist="std",df=5.0659)
library("rugarch")
plot(m1, which = c(1, 3))

#### 10-day VaR
names(pm1)
v1=sqrt(sum(pm1$standardDeviation^2))
RMeasure(-.001491,v1,cond.dist="std",df=5.0659)

library(ggplot2)
# Create a data frame of the forecast values
df <- data.frame(
  step = 1:10,
  mean = pm1$meanForecast,
  sd = pm1$standardDeviation)

# Create a plot of the mean forecast with error bars representing the standard deviation
ggplot(df, aes(x = step, y = mean)) +
  geom_errorbar(aes(ymin = mean - sd, ymax = mean + sd), width = 0.2) +
  geom_line() +
  xlab("Forecast Step") +
  ylab("Forecast Value") +
  ggtitle("GARCH Model 10-Step Ahead Forecasts")



### Empirical quantile and quantile regression
quantile(naapl,c(0.95,0.99,0.999))
library(quantmod)
library(tidyverse)
start_date <- as.Date("2008-01-01")
end_date <- as.Date("2023-03-31")
tickers <- c("AAPL", "^GSPC")
getSymbols(tickers, from = start_date, to = end_date)
head(AAPL)
head(GSPC)
head(naapl)
nrow(GSPC)
nrow(AAPL)
nrow(naapl)
colnames(AAPL)
colnames(GSPC)

# Remove the last row of the GSPC and AAPL
GSPC <- GSPC[-nrow(GSPC), ]
AAPL <- AAPL[-nrow(AAPL), ]

nrow(GSPC)
nrow(AAPL)
nrow(naapl)

aapl_data <- as.data.frame(cbind(naapl, AAPL$AAPL.Volume))
GSPC_data <- as.data.frame(`GSPC`)
head(GSPC_data)

df <- cbind(aapl_data, GSPC_data$GSPC.Adjusted)
head(df)
colnames(df) <- c("naapl", "vol", "GSPC")
head(df)

library(quantreg)
require(quantreg)

# fit quantile regression model
# a) at 95%

m3=rq(naapl~vol+GSPC,data=df,tau=0.95)
summary(m3)
ts.plot(naapl)

vol_last <- as.numeric(tail(df$vol, n = 1))
GSPC_last <- as.numeric(tail(df$GSPC, n = 1))
VaR_quant <- -0.00321 + 0*vol_last + 0.00001*GSPC_last
lines(m3$fitted.values,col="red")
VaR_quant

# b) at 99%

m4=rq(naapl~vol+GSPC,data=df,tau=0.99)
summary(m4)
ts.plot(naapl)

vol_last <- as.numeric(tail(df$vol, n = 1))
GSPC_last <- as.numeric(tail(df$GSPC, n = 1))
VaR_quant <- -0.00083 + 0*vol_last + 0.00001*GSPC_last
lines(m4$fitted.values,col="red")
VaR_quant

# c) at 99.9%

m5=rq(naapl~vol+GSPC,data=df,tau=0.999)
summary(m5)
ts.plot(naapl)

vol_last <- as.numeric(tail(df$vol, n = 1))
GSPC_last <- as.numeric(tail(df$GSPC, n = 1))
VaR_quant <- -0.05413 + 0*vol_last + 0.00004*GSPC_last
lines(m5$fitted.values,col="red")
VaR_quant

#Comparison between the models at different levels

library(ggplot2)
library(tidyverse)

df <- data.frame(
  Confidence_Level = c("95%", "99%", "99.9%"),
  Risk_Metric_Model = c(0.02485893, 0.03515846, 0.04670317),
  Econometric_Model_1DayVaR = c(0.02023623, 0.03469852, 0.06161141),
  Econometric_Model_10DayVaR = c(0.07150791, 0.12009818, 0.21051981),
  Empirical_and_Quantile_Regression_Model = c(0.0370681, 0.0394481, 0.1069824)
)

df_long <- df %>% 
  pivot_longer(-Confidence_Level, names_to = "Model", values_to = "VaR")

ggplot(df_long, aes(x = Confidence_Level, y = VaR, fill = Model)) + 
  geom_bar(stat = "identity", position = position_dodge()) + 
  labs(title = "Comparison of VaR Models", x = "Confidence Level", y = "VaR") + 
  scale_fill_brewer(palette = "Set1") + 
  theme_minimal()
