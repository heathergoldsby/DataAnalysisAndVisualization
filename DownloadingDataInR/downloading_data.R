library(quantmod)
library(PerformanceAnalytics)


### Get the data


# tickers to get data for
symbols = c("VFINX","FCNTX", "QQQ")
#symbols = c("FB", "AMZN", "NFLX", "GOOG")

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
