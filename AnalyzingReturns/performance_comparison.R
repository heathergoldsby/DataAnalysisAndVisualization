
library(quantmod)
library(PerformanceAnalytics)
library(ggplot2)
require(reshape2)

### Get the data
  

# tickers to get data for
symbols = c("VFINX","FCNTX", "QQQ")

# start date
start_date = "2001-01-01"

# second start date
second_start_date = "2015/"

symbols2 <- getSymbols(symbols, from=start_date)

# change to monthly
# Credit:  https://www.r-bloggers.com/aggregate-portfolio-contributions-through-time/ - this is where the pull code is from
x.P <- do.call(merge, lapply(symbols2, function(x) {
  
  Cl(to.monthly(Ad(get(x)), drop.time = TRUE,
                indexAt='endof'))
}))
colnames(x.P) = symbols
# compute returns
x.R <- na.omit(Return.calculate(x.P)) 

# create the returns for the shorter time period
x.Rshort <- x.R[second_start_date]

# compute yearly data
x.P1 <- do.call(merge, lapply(symbols2, function(x) {
  
  Cl(to.yearly(Ad(get(x)), drop.time = TRUE,
               indexAt='endof'))
}))
colnames(x.P1) = symbols
# compute yearly returns
x.R1 <- na.omit(Return.calculate(x.P1)) * 100


  ## Growth of $1 from 1.1.2000 - 9.7.2018

q1 <- chart.CumReturns(x.R, main = "", colorset=rich6equal, legend.loc = "topleft", wealth.index=TRUE, auto.grid=FALSE)
# png(file="Growth_all.png",width=500,height=250)
# q1
# dev.off()



q2 <- chart.CumReturns(x.Rshort, main = "", colorset=rich6equal, legend.loc = "topleft", wealth.index=TRUE, auto.grid=FALSE, xlab = "Time", ylab="Growth of $1" )
# png(file="Growth_short.png",width=500,height=250)
# q2
# dev.off()

# Create a start date -> end returns plot. 
mydf3 <- data.frame(date=index(x.P), coredata(x.P))

for (i in 2:(ncol(mydf3))) { 
  c <- (mydf3[nrow(mydf3),i] - mydf3[,i]) /mydf3[,i] 
  if (i == 2) {
    mynewdf <- c
  } else {
    mynewdf <- data.frame(mynewdf, c)
  }
}

mynewdf <- data.frame(as.Date(mydf3[,1]),mynewdf)
colnames(mynewdf)<- colnames(mydf3)

mydf2 <- melt(mynewdf ,  id.vars = 'date', variable.name = 'Security')

q3 <- ggplot(data = mydf2, aes(x = date, y = value)) + geom_line(aes(colour = Security)) + scale_color_manual(values=rich6equal) + theme(legend.position = c(0.9, 0.8),  legend.title=element_text(size=8), legend.text=element_text(size=8)) + scale_x_date(name ="Start Date") + scale_y_continuous(name ="Growth of $1 from\n Start Date Until End") 

# png(file="Returns_from_start_date.png",width=500,height=250)
# q3
# dev.off()


##Comparison of Annual Returns


mydf <- data.frame(date=index(x.R1), coredata(x.R1))
mydf2 <- melt(mydf ,  id.vars = 'date', variable.name = 'Security')

q4<- ggplot(data = mydf2, aes(x = date, y = value)) + geom_line(aes(colour = Security)) + scale_color_manual(values=rich6equal) + theme(legend.position = c(0.9, 0.2),  legend.title=element_text(size=8), legend.text=element_text(size=8)) + scale_x_date(name ="Year") + scale_y_continuous(name ="Annual Return Percentage") 
# png(file="Annual_returns.png",width=500,height=250)
# q4 
# dev.off()




##Rolling Performance


q5 <- chart.RollingPerformance(x.R, main = "", colorset=rich6equal)
# png(file="Rolling_returns.png",width=500,height=250)
# q5 
# dev.off()


