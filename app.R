require(shinydashboard)
require(DT)
require(shiny)
require(tidyverse)
require(timeDate)
require(googlesheets4)
require(forecast)
require(xts)
require(scales)

sheet <- "Market Data Backup"
raw_df <- read_sheet("https://docs.google.com/spreadsheets/d/1ig3mbe43WIVZbHNuTI70AYLih4WryOqm6KujYc4bUeM/edit#gid=555070615", sheet=sheet)
raw_df[,2:length(raw_df)] <- sapply(raw_df[,2:length(raw_df)], str_remove_all, ",")
raw_df[,2:length(raw_df)] <- sapply(raw_df[,2:length(raw_df)], as.numeric)

dates <- unique(as.Date(raw_df$time))
stocks <- colnames(raw_df[,2:ncol(raw_df)])

header <- dashboardHeader(
    title = "Thesis Project"
)

body <- dashboardBody(
    includeCSS("https://raw.githubusercontent.com/willoutcault/608_final/master/styles.css"),
    fluidRow(
        column(width = 4,
               box(width = NULL, status = "warning",
                   selectInput("date", "Date: ", dates, selected = Sys.Date())
               )
        ),
        column(width = 4,
               box(width = NULL, status = "warning",
                   selectInput("stock", "stock: ", stocks, selected = stocks[1])
               )
        ),
        column(width = 4,
               box(width = NULL, status = "warning",
                   selectInput("timestep", "Time-Step: ",
                               c("1 min", "5 min", "15 min", "30 min", "1 hour"),
                               selected = "1 min")
               )
        ),
        column(width = 6,
               box(width = NULL, status = "warning",
                   plotOutput("plot1")
               )
        ),
        column(width = 6,
               box(width = NULL,
                   DT::dataTableOutput("RMSEresults")
               ),
               box(width = NULL,
                   DT::dataTableOutput("MAPEresults")
               )
        )
    )
)


ui <- dashboardPage(
    header,
    dashboardSidebar(disable = TRUE),
    body
)


server <- function(input,output){
    
    
    output$RMSEresults <- renderDataTable({
        
        # Filter Date
        df1m <- raw_df %>% 
            filter(as.Date(time)==input$date) %>% 
            select(input$stock)
        
        df5m <- df1m[seq(1, nrow(df), 5), ]
        
        df15m <- df1m[seq(1, nrow(df), 15), ]
        
        df30m <- df1m[seq(1, nrow(df), 30), ]
        
        df1h <- df1m[seq(1, nrow(df), 60), ]
        
        #------------------------------------
        
        series1m <- ts(df1m)
        
        series5m <- ts(df5m)
        
        series15m <- ts(df15m)
        
        series30m <- ts(df30m)
        
        series1h <- ts(df1h)
        
        #------------------------------------
        
        fitseries <- function(series){
            trainsize <- round(length(series)*.75,0)
            train <- ts(series[1:trainsize-1], start=1, end=trainsize-1)
            test <- ts(series[trainsize:length(series)], start=trainsize, end=length(series))
            
            seriesfit1 <- meanf(train,h=length(test))
            seriesfit2 <- rwf(train,h=length(test))
            seriesfit3 <- rwf(train, drift=TRUE, h=length(test))
            seriesfit4 <- holt(train, h=length(test))
            
            RMSEresults <- rbind(accuracy(seriesfit1, test)[2,c(2)], 
                             accuracy(seriesfit2, test)[2,c(2)], 
                             accuracy(seriesfit3, test)[2,c(2)],
                             accuracy(seriesfit4, test)[2,c(2)])
            
            MAPEresults <- rbind(accuracy(seriesfit1, test)[2,c(5)], 
                                 accuracy(seriesfit2, test)[2,c(5)], 
                                 accuracy(seriesfit3, test)[2,c(5)],
                                 accuracy(seriesfit4, test)[2,c(5)])
            
            results <- cbind(round(RMSEresults, 3), round(MAPEresults, 3))
            
            results
        }
        
        results1m <- fitseries(series1m)
        results5m <- fitseries(series5m)
        results15m <- fitseries(series15m)
        results30m <- fitseries(series30m)
        results1h <- fitseries(series1h)
        
        #------------------------------------

        
        results <- cbind(c("Mean", "Naive", "Drift", "Holt"),results1m[,1], results5m[,1],results15m[,1],results30m[,1],results1h[,1],
                         c("Mean", "Naive", "Drift", "Holt"),results1m[,2], results5m[,2],results15m[,2],results30m[,1],results1h[,2]
        )
        
        
        datatable(
            results[,1:6],
            colnames = c("Model",
                         "RMSE1m",  "RMSE5m",  "RMSE15m",  "RMSE30m",  "RMSE1h")
        )
        
    })
    
    output$MAPEresults <- renderDataTable({
        
        # Filter Date
        df1m <- raw_df %>% 
            filter(as.Date(time)==input$date) %>% 
            select(input$stock)
        
        df5m <- df1m[seq(1, nrow(df), 5), ]
        
        df15m <- df1m[seq(1, nrow(df), 15), ]
        
        df30m <- df1m[seq(1, nrow(df), 30), ]
        
        df1h <- df1m[seq(1, nrow(df), 60), ]
        
        #------------------------------------
        
        series1m <- ts(df1m)
        
        series5m <- ts(df5m)
        
        series15m <- ts(df15m)
        
        series30m <- ts(df30m)
        
        series1h <- ts(df1h)
        
        #------------------------------------
        
        fitseries <- function(series){
            trainsize <- round(length(series)*.75,0)
            train <- ts(series[1:trainsize-1], start=1, end=trainsize-1)
            test <- ts(series[trainsize:length(series)], start=trainsize, end=length(series))
            seriesfit1 <- meanf(train,h=length(test))
            seriesfit2 <- rwf(train,h=length(test))
            seriesfit3 <- rwf(train, drift=TRUE, h=length(test))
            seriesfit4 <- holt(train, h=length(test))
            
            RMSEresults <- rbind(accuracy(seriesfit1, test)[2,c(2)], 
                                 accuracy(seriesfit2, test)[2,c(2)], 
                                 accuracy(seriesfit3, test)[2,c(2)], 
                                 accuracy(seriesfit4, test)[2,c(2)])
            
            MAPEresults <- rbind(accuracy(seriesfit1, test)[2,c(5)], 
                                 accuracy(seriesfit2, test)[2,c(5)], 
                                 accuracy(seriesfit3, test)[2,c(5)],
                                 accuracy(seriesfit4, test)[2,c(5)])
            
            results <- cbind(round(RMSEresults, 3), round(MAPEresults, 3))
            
            results
        }
        
        results1m <- fitseries(series1m)
        results5m <- fitseries(series5m)
        results15m <- fitseries(series15m)
        results30m <- fitseries(series30m)
        results1h <- fitseries(series1h)
        
        #------------------------------------
        
        
        results <- cbind(c("Mean", "Naive", "Drift", "Holt"),results1m[,1], results5m[,1],results15m[,1],results30m[,1],results1h[,1],
                         c("Mean", "Naive", "Drift", "Holt"),results1m[,2], results5m[,2],results15m[,2],results30m[,1],results1h[,2]
        )
        
        
        datatable(
            results[,7:ncol(results)],
            colnames = c("Model",
                         "MAPE1m", "MAPE5m", "MAPE15m", "MAPE30m", "MAPE1h")
        )
        
    })
    
    output$plot1<- renderPlot({
        
        df <- raw_df %>% 
            filter(as.Date(time)==input$date) %>% 
            select(input$stock)
        
        if (input$timestep == "1 min"){
            df <- df[seq(1, nrow(df), 1), ]
        }
        if (input$timestep == "5 min"){
            df <- df[seq(1, nrow(df), 5), ]
        }
        if (input$timestep == "15 min"){
            df <- df[seq(1, nrow(df), 15), ]
        }
        if (input$timestep == "30 min"){
            df <- df[seq(1, nrow(df), 30), ]
        }
        if (input$timestep == "1 hour"){
            df <- df[seq(1, nrow(df), 60), ]
        }
        
        series <- ts(df)
        
        trainsize <- round(length(series)*.75,0)
        train <- ts(series[1:trainsize-1], start=1, end=trainsize-1)
        test <- ts(series[trainsize:length(series)], start=trainsize, end=length(series))
        
        seriesfit1 <- meanf(train,h=length(test))
        seriesfit2 <- rwf(train,h=length(test))
        seriesfit3 <- rwf(train, drift=TRUE, h=length(test))
        seriesfit4 <- holt(train, h=length(test))
        
        autoplot(series) +
            autolayer(seriesfit1, series="Mean", PI=FALSE) +
            autolayer(seriesfit2, series="Naive", PI=FALSE) +
            autolayer(seriesfit3, series="Drift", PI=FALSE) +
            autolayer(seriesfit4, series="Holt", PI=FALSE) +
            xlab("Minutes") + ylab("Ticker Prise (US$)") +
            ggtitle(paste("Stock Data for ",input$stock," on ",input$date,sep="")) +
            guides(colour=guide_legend(title="Forecast"))
        
    })
    
    
}

shinyApp(ui = ui, server = server)
