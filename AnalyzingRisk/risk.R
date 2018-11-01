# Risk
library(quantmod)
library(PerformanceAnalytics)
library(ggplot2)
library(reshape2)


### Get the data

# tickers to get data for
symbols = c("VFINX","FB", "AAPL", "AMZN", "NFLX", "GOOG")

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
png(file="apple_returns.png",width=1000,height=500)
apple_returns
dev.off()
apple_drawdown <- chart.Drawdown(x.Returns$AAPL)
png(file="apple_drawdown.png",width=1000,height=500)
apple_drawdown
dev.off()
table.Drawdowns(x.Returns[,5,drop=FALSE])


# Pain Index - 

PainIndex(x.Returns)
