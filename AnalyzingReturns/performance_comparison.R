
library(quantmod)
library(PerformanceAnalytics)
library(ggplot2)
library(reshape2)

### Get the data
  

# tickers to get data for
symbols = c("VFINX","FCNTX", "QQQ")

# start date
start_date = "2001-01-01"

# second start date
second_start_date = "2015/"

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

# create the returns for the shorter time period
x.Returns.short <- x.Returns[second_start_date]

# compute yearly data
x.Price.yearly <- do.call(merge, lapply(symbols, function(x) {
  
  Cl(to.yearly(Ad(get(x)), drop.time = TRUE,
               indexAt='endof'))
}))
colnames(x.Price.yearly) = symbols
# compute yearly returns
x.Returns.yearly <- na.omit(Return.calculate(x.Price.yearly))


  ## Growth of $1 from 1.1.2000 to now.

growthChart.long <- chart.CumReturns(x.Returns, main = "Growth of $1 over time", colorset=rich6equal, legend.loc = "topleft", wealth.index=TRUE, auto.grid=FALSE)
growthChart.long

growthChart.short <- chart.CumReturns(x.Returns.short, main = "Growth of $1 over time", colorset=rich6equal, legend.loc = "topleft", wealth.index=TRUE, auto.grid=FALSE)
growthChart.short


# Create a start date -> end returns plot. 
price.df <- data.frame(date=index(x.Price), coredata(x.Price))

for (i in 2:(ncol(price.df))) { 
  aRet <- ((price.df[nrow(price.df),i] - price.df[,i]) / price.df[,i]) + 1
  if (i == 2) {
    return.df <- aRet
  } else {
    return.df <- data.frame(return.df, aRet)
  }
}

return.df <- data.frame(as.Date(price.df[,1]),return.df)
colnames(return.df)<- colnames(price.df)

melted.return.df <- melt(return.df ,  id.vars = 'date', variable.name = 'Security')

growthFromDateTillNowChart <- ggplot(data = melted.return.df, aes(x = date, y = value)) + geom_line(aes(colour = Security)) + scale_color_manual(values=rich6equal) + theme(legend.position = c(0.9, 0.8),  legend.title=element_text(size=8), legend.text=element_text(size=8)) + scale_x_date(name ="Start Date") + scale_y_continuous(name ="")  + ggtitle("Growth of $1 from Start Date Until Present (10.2018)")
growthFromDateTillNowChart


##Comparison of Annual Returns
annual.returns.df <- data.frame(date=index(x.Returns.yearly), coredata(x.Returns.yearly))
melted.annual.returns.df <- melt(annual.returns.df ,  id.vars = 'date', variable.name = 'Security')

annualReturnsChart <- ggplot(data = melted.annual.returns.df, aes(x = date, y = value*100)) + geom_line(aes(colour = Security)) + scale_color_manual(values=rich6equal) + theme(legend.position = c(0.9, 0.2),  legend.title=element_text(size=8), legend.text=element_text(size=8)) + scale_x_date(name ="Year") + scale_y_continuous(name ="Annual Return Percentage")  + ggtitle("Annual Return")
annualReturnsChart


##Rolling Performance
rollingPerformanceChart <- chart.RollingPerformance(x.Returns, main = "12 Month Rolling Performance", colorset=rich6equal)
rollingPerformanceChart


