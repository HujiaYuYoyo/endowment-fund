#library(fitdistrplus)
library(quantmod)
library(mvtnorm)
library(MASS)

# clear workspace
rm(list = ls())

# stocks we're using
stocks = c("^GSPC")

# bonds we're using
bonds = c("QQQ")

# alternative investments
alternative = c()

# cash
cash = c()

assets = c(stocks, bonds, alternative, cash)

# gets the return of an asset denoted by its symbol from Yahoo Finance
get_return <- function(symbol, from_date, to_date) {
  s = getSymbols(symbol, from=from_date, auto.assign = F)
  s_adj_close = data.frame(s)[,6]
  
  periodReturn(s, period='yearly',from=from_date, to=to_state)  # change to monthly, if desired, later
}

returns = c()
from_date = "2000-01-01"
to_date = "2018-01-01"

# iterate through all assets and do following:
# 1. get returns
# 2. add to vector of returns
# then:
# 3. fit to multivariate distribution
# 4. print parameters
for (i in 1:length(assets)) { 
  asset <- assets[i]
  return <- as.numeric(get_return(asset, from_date, to_date))
  returns <- rbind(returns, return)
}

log_returns <- log(returns + 1)
df <- data.frame(log_returns)
row.names(df) <- assets

means = rowMeans(df)
sd = diag(sqrt(cov(t(df))))
corrs = cor(t(df))

# use qq-plot to check for goodness-of-fit
qqnorm(returns)

# the idea is that return follows a normal distribution and implies
# that stock price follows a lognormal distribution, where S(t) = S(0)exp(r)

# alternatively, we might want to try autoregressive (AR) models
library('ggplot2')
library('forecast')
library('tseries')

sp500 <- as.numeric(get_return("^GSPC", from_date, to_date))
sp500_train <-sp500[1:(0.9 * length(sp500))] 
sp500_test <- sp500[(0.9 * length(sp500) + 1):length(sp500)] 

ar.fit <- arima(sp500_train, order = c(2, 0, 0))
ar.preds <- predict(ar.fit, n.ahead = (length(sp500) - (0.9 * length(sp500))))$pred
ar.forecast <- forecast(ar.fit, h = 100)

plot(ar.forecast, main = "AR forecasts for asset returns")

accuracy(ar.preds, sp500_test)[2] 
