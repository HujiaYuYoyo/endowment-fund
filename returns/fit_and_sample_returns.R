library(readxl)
library(quantmod)

# setup environment
setwd("/Users/michellezhang/Desktop/endowment_fund/returns")
rm(list = ls())

# indexes_data.xlsx provided
index_data = read_excel("indexes_data.xlsx")

hedge_data = read_excel("hedgefund.xlsx")
hedge_data_price = hedge_data[100:221, 3]
hedge_data = hedge_data[100:221, 2]
hedge_data = as.numeric(unlist(hedge_data)) / 100

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


years = as.matrix(get_return("^GSPC", from_date, to_date))

x <- seq(as.Date("2008-02-01"), by="1 month", length.out=121)
x_breaks <- seq(as.Date("2008-02-01"), as.Date("2018-02-01"), by="1 year")
x_labels <- as.character(x_breaks, format="%h-%y")

returns_with_index <- returns[2:122, 1:5]
df <- data.frame(TIME=x, RETURNS=returns_with_index)

# plot returns
library(ggplot2)
colnames(df) <- c("TIME", "STOCK", "CORPBOND", "GOVBOND", "CASH", "HEDGEFUND")
ggplot(df, aes(TIME)) + 
  geom_line(aes(y = STOCK, colour = "S&P 500 Index")) +
  geom_line(aes(y = CORPBOND, colour = "iShares iBoxx $ Invmt Grade Corp Bd ETF")) +
  geom_line(aes(y = GOVBOND, colour = "iShares 7-10 Year Treasury Bd ETF")) +
  geom_line(aes(y = HEDGEFUND, colour = "Eurekahedge Hedge Fund Index")) +
  geom_line(aes(y = CASH, colour = "SPDR Blmbg Barclays 1-3 Mth T-Bill ETF")) +
  ggtitle("10-Year Return for Assets") +
  labs(y="Return", x="Time") +
  theme(legend.position = c(0.75, 0.25)) +
  labs(colour="") +
  scale_color_manual(values=c("#9933ff", "#cc0000", "#666699", "#0066ff", "#33cc33")) +
  ylim(-0.3, 0.2) +
  scale_x_date(breaks=x_breaks, labels=x_labels, minor_breaks=df$TIME)

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
qqnorm(log_returns)

# alternatively, we might want to try autoregressive (AR) models
#library('forecast')
#library('tseries')

#sp500 <- as.numeric(get_return("^GSPC", from_date, to_date))
#sp500_train <-sp500[1:(0.9 * length(sp500))] 
#sp500_test <- sp500[(0.9 * length(sp500) + 1):length(sp500)] 

#ar.fit <- arima(sp500_train, order = c(2, 0, 0))
#ar.preds <- predict(ar.fit, n.ahead = (length(sp500) - (0.9 * length(sp500))))$pred
#ar.forecast <- forecast(ar.fit, h = 100)

#plot(ar.forecast, main = "AR forecasts for asset returns")

#accuracy(ar.preds, sp500_test)[2] 
