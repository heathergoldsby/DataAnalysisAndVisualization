# Risk
library(quantmod)
library(PerformanceAnalytics)
library(ggplot2)
library(reshape2)


### Get the data

# tickers to get data for
symbols = c("VFINX","FCNTX", "QQQ", "FB", "AMZN", "NFLX", "GOOG", "AAPL")

# start date
start_date = "2001-01-01"


getSymbols(symbols, from=start_date)

# change to monthly
# Credit:  https://www.r-bloggers.com/aggregate-portfolio-contributions-through-time/ - this is where the pull code is from
x.Price <- do.call(merge, lapply(symbols, function(x) {
  
  Cl(to.monthly(Ad(get(x)), drop.time = TRUE,
                indexAt='endof'))
}))

# make the column names more reasonable
colnames(x.Price) = symbols
# compute returns
x.Returns <- na.omit(Return.calculate(x.Price)) 


# compute yearly data
x.Price.yearly <- do.call(merge, lapply(symbols, function(x) {
  
  Cl(to.yearly(Ad(get(x)), drop.time = TRUE,
               indexAt='endof'))
}))
colnames(x.Price.yearly) = symbols
# compute yearly returns
x.Returns.yearly <- na.omit(Return.calculate(x.Price.yearly)) 




# Drawdown - 
apple_returns <- chart.CumReturns(x.Returns$AAPL, main = "Growth of $1 over time", colorset=rich6equal, legend.loc = "topleft", wealth.index=TRUE, auto.grid=FALSE)
# png(file="apple_returns.png",width=1000,height=500)
# apple_returns
# dev.off()
apple_drawdown <- chart.Drawdown(x.Returns$AAPL)
# png(file="apple_drawdown.png",width=1000,height=500)
# apple_drawdown
# dev.off()

all_drawdown <- chart.Drawdown(x.Returns)

# 8 is the index of AAPL
table.Drawdowns(x.Returns[,8,drop=FALSE])


# Pain Index - 

PainIndex(x.Returns)

# Alpha, Beta, R-squared, and many more! 
t <- table.SFM(x.Returns[,1:ncol(x.Returns)], x.Returns[,1])
colnames(t) <- symbols
t

# Sharpe & Sortino
# looking at cummulative returns
cumRet <- chart.CumReturns(x.Returns, main = "", colorset=rich6equal, legend.loc = "topleft", wealth.index=TRUE, auto.grid=FALSE)
# png(file="cummulative_ret.png",width=1000,height=500)
# cumRet
# dev.off()

# set your risk free rate
Rf = 0
MAR = 0
# Rf=.04/12 
# MAR =.05/12
t2 <- rbind(Return.cumulative(x.Returns), Return.annualized(x.Returns), SharpeRatio(x.Returns, Rf=Rf), SharpeRatio.annualized(x.Returns, Rf=Rf), SortinoRatio(x.Returns, MAR=MAR), StdDev(x.Returns))
t2  


# More to investigate!
UlcerIndex(x.Returns)

