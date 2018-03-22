library(readxl)
library(quantmod)

# setup environment
setwd("...")
rm(list = ls())

# indexes_data.xlsx provided
index_data = read_excel("indexes_data.xlsx")
hedge_data = read_excel("hedgefund.xlsx")
am_hedge_data = read_excel("hedgefund_amurica.xlsx")

hedge_data_price = hedge_data[100:221, 3]
am_hedge_data_price = am_hedge_data[100:221, 3]

hedge_data = hedge_data[100:221, 2]
am_hedge_data = am_hedge_data[100:221, 2]


hedge_data = as.numeric(unlist(hedge_data)) / 100
am_hedge_data = as.numeric(unlist(am_hedge_data)) / 100

# stocks we're using
stocks = c("^GSPC")

# bonds we're using
bonds = c("LQD", "IEF")

# alternative investments
alternative = c()

# cash
cash = c("BIL")

# all assets
assets = c(stocks, bonds, alternative, cash)

# gets the return of an asset denoted by its symbol from Yahoo Finance
get_return <- function(symbol, from_date, to_date) {
  s = getSymbols(symbol, from=from_date, to=to_date, auto.assign = F)
  s_adj_close = data.frame(s)[,6]
  periodReturn(s, period='monthly',from=from_date, to=to_date)  
}

# define params
returns = c()
prices = c()
from_date = "2008-01-01"
to_date = "2018-03-01"

years = as.matrix(get_return("^GSPC", from_date, to_date))

# build up vector of returns from assets
for (i in 1:length(assets)) { 
  asset <- assets[i]
  return <- as.numeric(get_return(asset, from_date, to_date))
  returns <- cbind(returns, return)
  
  price <- as.numeric(data.frame(getSymbols(asset, from=from_date, to=to_date, auto.assign = F))[,6])
  prices <- cbind(prices, price)
}

# add hedge fund index
returns <- returns[1:122, 1:4]
returns <- cbind(returns, hedge_data)

prices <- prices[1:122, 1:4]
prices <- cbind(prices, hedge_data_price)
prices_with_date <- cbind(seq(1:122), prices)

# plot returns
colnames(prices_with_date) <- c("MONTH", "STOCK", "CORPBOND", "GOVBOND", "CASH", "HEDGEFUND")
ggplot(prices_with_date, aes(MONTH)) + 
  geom_line(aes(y = STOCK, colour = "STOCK")) 

# take log of returns
log_returns <- log(returns + 1)
df <- data.frame(log_returns)
assets = c(assets, "HEDGE")
colnames(df) <- assets

# fit distribution
df.mu = colMeans(df)
df.Sigma = cov(df)
df.corrs = cor(df)

# convert to yearly params
df.mu = 12 * df.mu
df.mu
df.Sigma = 12 * df.Sigma
df.Sigma
df.sd = sqrt(diag(df.Sigma))
df.sd

# compute cholesky fact
df.chol = chol(df.Sigma)
df.chol

# output into file

############################################
##        IGNORE BELOW FOR NOW            ##
############################################

# use qq-plot to check for goodness-of-fit
qqnorm(returns)

# the idea is that return follows a normal distribution and implies
# that stock price follows a lognormal distribution, where S(t) = S(0)exp(r)

# alternatively, we might want to try autoregressive (AR) models
library('ggplot2')
library('forecast')
library('tseries')

#sp500 <- as.numeric(get_return("^GSPC", from_date, to_date))
#sp500_train <-sp500[1:(0.9 * length(sp500))] 
#sp500_test <- sp500[(0.9 * length(sp500) + 1):length(sp500)] 

#ar.fit <- arima(sp500_train, order = c(2, 0, 0))
#ar.preds <- predict(ar.fit, n.ahead = (length(sp500) - (0.9 * length(sp500))))$pred
#ar.forecast <- forecast(ar.fit, h = 100)

#plot(ar.forecast, main = "AR forecasts for asset returns")

#accuracy(ar.preds, sp500_test)[2] 
