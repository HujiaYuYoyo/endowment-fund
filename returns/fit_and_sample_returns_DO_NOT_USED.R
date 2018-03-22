library(readxl)
library(quantmod)

# setup environment
setwd("...")
rm(list = ls())

# indexes_data.xlsx provided up to 1/31
index_data = read_excel("indexes_data.xlsx")
# hedge fund data is from 2000 onwards
hedge_data = read_excel("hedgefund.xlsx")  
am_hedge_data = read_excel("hedgefund_amurica.xlsx")  

# goes up to 1-31-2018
gov_bond_data = read.table("govbond.txt", header=FALSE)
corp_bond_data = read.table("corpbond.txt", header=FALSE)

gov_bond_data = gov_bond_data[,1]
corp_bond_data = corp_bond_data[,1]
cash_data = cash_data[,1]

hedge_data = hedge_data[4:220, 2]
am_hedge_data = am_hedge_data[4:220, 2]

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
get_return <- function(symbol, to_date, from_date) {
  s = getSymbols(symbol, to=to_date, from=from_date, auto.assign = F)
  s_adj_close = data.frame(s)[,6]
  x = periodReturn(s, period='monthly')
}

# define params
returns = c()
to_date = "2018-02-01"
from_date = "1995-01-01"

## build up vector of returns from assets
#for (i in 1:length(assets)) { 
#  asset <- assets[i]
#  return <- as.numeric(get_return(asset, to_date))
#  returns <- cbind(returns, return)
#}

cash <- as.numeric(get_return("BIL", to_date, from_date))

sp500 <- as.numeric(get_return("^GSPC", to_date, from_date))
sp500 <- sp500[149:277]

gov_bond_data <- gov_bond_data[102:230]
corp_bond_data <- corp_bond_data[330:458]

hedge_data <- hedge_data[89:217]
am_hedge_data <- am_hedge_data[89:217]

## add hedge fund index
#returns <- returns[1:122, 1:4]
#returns <- cbind(returns, hedge_data)

# create return matrix
returns = cbind(sp500, corp_bond_data, gov_bond_data, hedge_data, cash)

# take log of returns
log_returns <- log(returns + 1)
df <- data.frame(log_returns)
assets = c("stock", "cbond", "gbond", "hedgefund", "cash")
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
