#library(fitdistrplus)
library(quantmod)
library(MASS)

assets = c("^GSPC")

# define functions here gets the yearly return of an asset denoted by its symbol from Yahoo Finance
get_return <- function(symbol, from_date, to_date) {
  s = getSymbols(symbol, from=from_date, auto.assign = F)
  s_adj_close = data.frame(s)[,6]
  
  periodReturn(s, period='weekly',from=from_date, to=to_state)
}


from_date = "1990-01-01"
to_date = "2018-01-01"

# iterate through all assets and do following:
# 1. get weekly (daily) return 
# 2. fit lognormal distr. to return
# 3. convert to yearly return
#      - mean should be 52 * E[weekly return]
#      - std dev should be sqrt(52) * Var(weekly return)
# 4. print params for distr.
for (i in 1:length(assets)) { 
  asset <- assets[i]
  returns <- as.numeric(get_return(asset, from_date, to_date))
  fit <- fitdistr(returns, "normal")
  yearly_mean <- fit$estimate['mean'] * 52
  yearly_sd <- fit$estimate['sd'] * 52
  
  print(fit)
  cat("Asset:", asset, "\n")
  cat("Fitted mean:", yearly_mean, "\tFitted sd:", yearly_sd, "\n\n")
}

# use qq-plot to check for goodness-of-fit
qqnorm(returns)

# the idea is that return follows a normal distribution and implies
# that stock price follows a lognormal distribution, where S(t) = S(0)exp(r)
