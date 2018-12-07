# Load packages ----
library(shiny)
library(quantmod)
library(PerformanceAnalytics)
library(ggplot2)
require(reshape2)



# User interface ----
ui <- fluidPage(
  titlePanel("Security Analysis"),
  
  sidebarLayout(
    sidebarPanel(

      dateRangeInput("dates", "Date range:", start = "2000-01-01", min = "2000-01-01", startview="year"), 
      textInput("tickers", "Tickers (comma separated list):", value = "VFINX,FCNTX,QQQ"),
      selectInput("plotType", "Plot type:", choices = list("Capital Growth" = 1, "Captial Growth from Starting Point" = 2,
                                                           "Annualized Return" = 3, "Rolling Return" = 4, "Drawdown" = 5)),
      
      br(), 
      numericInput('rf', 'Risk Free Rate:', 0.03,
                   min = 0, max = 1, step=0.01), 
      numericInput('mar', 'Minimum Acceptable Return:', 0.04, min = 0, max = 1, step=0.01),
      uiOutput("ddTickerSelection")
    ),

    mainPanel(
      tabsetPanel(
        tabPanel("Plots", plotOutput("plot")), 
        tabPanel("Statistics", tableOutput("table"),h5("*These ratios use the first security listed as the benchmark."),
 tableOutput("ratioTable")),
        tabPanel("Drawdown", plotOutput("drawdownPlot"), tableOutput("drawdown"))
      )
      
      )
  )
)

# Server logic
server <- function(input, output) {
  tickerList <- reactive({
    # list of tickers
    tickerList <- strsplit(input$tickers, ",")
    if (length(tickerList[[1]]) > 10) { 
      tickerList[[1]] <- tickerList[[1]][1:10]
    }
    tickerList <- lapply(tickerList,trimws)
    unlist(tickerList)
  })
  
  output$ddTickerSelection <- renderUI({
    selectInput("ddTicker", "Drawdown Ticker:", choices = tickerList())
  })
  
  securityPriceData <- reactive({    
    req(tickerList())
    data.env <- new.env()
    getSymbols(tickerList(), data.env, from=input$dates[1], to = input$dates[2])
    data.env
  })
  
  monthlySecurityPriceData <- reactive({
    req(securityPriceData())
    ### https://www.r-bloggers.com/aggregate-portfolio-contributions-through-time/ - this is where the pull code is from
    securityPriceData.monthly <- do.call(merge, lapply(tickerList(), function(x) {
      Cl(to.monthly(Ad(get(x,envir = securityPriceData())), drop.time = TRUE,
                    indexAt='endof'))
    }))
    colnames(securityPriceData.monthly) = tickerList()
    print(tickerList())
    securityPriceData.monthly
  })
  
  monthlyReturnData <- reactive({
    na.omit(Return.calculate(monthlySecurityPriceData()))
  })
  
  yearlySecurityPriceData <- reactive({
    req(securityPriceData())
    ### https://www.r-bloggers.com/aggregate-portfolio-contributions-through-time/ - this is where the pull code is from
    securityPriceData.yearly <- do.call(merge, lapply(tickerList(), function(x) {
      Cl(to.yearly(Ad(get(x,envir = securityPriceData())), drop.time = TRUE,
                   indexAt='endof'))
    }))
    colnames(securityPriceData.yearly) = tickerList()
        securityPriceData.yearly
  })
  
  yearlyReturnData <- reactive({
    yrRet <- na.omit(Return.calculate(yearlySecurityPriceData()))
    yrRet
  })
  
  
  returnsFromStartDate <- reactive({
    # Create a start date -> end returns plot. 
    price.df <- data.frame(date=index(monthlySecurityPriceData()), coredata(monthlySecurityPriceData()))
    
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
    
    melt(return.df ,  id.vars = 'date', variable.name = 'Security')

  })
  
  meltedYearlyReturns <- reactive({
    df <- data.frame(date=index(yearlyReturnData()), coredata(yearlyReturnData()))
    melt(df ,  id.vars = 'date', variable.name = 'Security')
  })
  
  output$plot <- renderPlot({
    if (input$plotType == 1) { 
      chart.CumReturns(monthlyReturnData(), main = "Growth of $1 over time", colorset=rich6equal, legend.loc = "topleft", wealth.index=TRUE, auto.grid=FALSE)
    } else if (input$plotType == 2) {
      ggplot(data = returnsFromStartDate(), aes(x = date, y = value)) + geom_line(aes(colour = Security)) + scale_color_manual(values=rich6equal) + theme(legend.position = c(0.9, 0.8),  legend.title=element_text(size=8), legend.text=element_text(size=8)) + scale_x_date(name ="Start Date") + scale_y_continuous(name ="Growth of $1 from\n Start Date Until End") 
    } else if (input$plotType == 3) {
      ggplot(data = meltedYearlyReturns(), aes(x = date, y = value*100)) + geom_line(aes(colour = Security)) + scale_color_manual(values=rich6equal) + theme(legend.position = c(0.9, 0.2),  legend.title=element_text(size=8), legend.text=element_text(size=8)) + scale_x_date(name ="Year") + scale_y_continuous(name ="Annual Return Percentage") 
    } else if (input$plotType == 4) { 
      chart.RollingPerformance(monthlyReturnData(), main = "", colorset=rich6equal, legend.loc = "topleft")
    } else if (input$plotType == 5) { 
      chart.Drawdown(monthlyReturnData(), colorset=rich6equal, legend.loc = "bottomright")
    }
  })
  
  output$table <- renderTable({
    t <- table.SFM(monthlyReturnData()[,1:ncol(monthlyReturnData())], monthlyReturnData()[,1])
    colnames(t) <- tickerList()
    t
  },  rownames = TRUE)
  
  
  output$ratioTable <- renderTable({
    
    # set your risk free rate
    t2 <- rbind(Return.cumulative(monthlyReturnData()), Return.annualized(monthlyReturnData()), SharpeRatio(monthlyReturnData(), Rf=input$rf), SharpeRatio.annualized(monthlyReturnData(), Rf=input$rf), SortinoRatio(monthlyReturnData(), MAR=input$mar), StdDev(monthlyReturnData()), PainIndex(monthlyReturnData()), UlcerIndex(monthlyReturnData()))
    t2  
    
  }, rownames = TRUE)
  
  output$drawdown <- renderTable({
    i <- match(input$ddTicker, tickerList())
    table.Drawdowns(monthlyReturnData()[,i,drop=FALSE])
    
  }, rownames = TRUE)
  
  output$drawdownPlot <- renderPlot({
    i <- match(input$ddTicker, tickerList())
    chart.Drawdown(monthlyReturnData()[,i], colorset=rich6equal, legend.loc = "bottomright")
  })
}

# Run the app
shinyApp(ui, server)
