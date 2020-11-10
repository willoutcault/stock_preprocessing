require(shinydashboard)
require(DT)
require(shiny)
require(tidyverse)
require(timeDate)
require(googlesheets4)
require(forecast)
require(xts)

sheet <- "Market Data Backup"
raw_df <- read_sheet("https://docs.google.com/spreadsheets/d/1ig3mbe43WIVZbHNuTI70AYLih4WryOqm6KujYc4bUeM/edit#gid=555070615", sheet=sheet)
raw_df[,2:length(raw_df)] <- sapply(raw_df[,2:length(raw_df)], str_remove_all, ",")
raw_df[,2:length(raw_df)] <- sapply(raw_df[,2:length(raw_df)], as.numeric)

dates <- unique(as.Date(raw_df$time))
stocks <- colnames(raw_df[,2:ncol(raw_df)])

header <- dashboardHeader(
    title = "NYS COVID-19 TRACKER"
)

body <- dashboardBody(
    includeCSS("https://raw.githubusercontent.com/willoutcault/608_final/master/styles.css"),
    fluidRow(
        column(width = 3,
               box(width = NULL, status = "warning",
                   selectInput("date", "Date: ", dates, selected = Sys.Date()),
                   selectInput("stock", "stock: ", stocks, selected = stocks[1])
               )
        ),
        column(width = 6,
               box(width = NULL,
                   DT::dataTableOutput("results")
               ),
               box(width = NULL, status = "warning",
                   plotOutput("plot1")
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
    
    
    output$results <- renderDataTable({
        
        
        
        # Filter Date
        df <- raw_df %>% 
            filter(as.Date(time)==input$date) %>% 
            select(input$stock)
        
        series <- ts(df)
        
        trainsize <- round(length(series)*.75,0)
        train <- ts(series[1:trainsize-1], start=1, end=trainsize-1)
        test <- ts(series[trainsize:length(series)], start=trainsize, end=length(series))
        
        seriesfit1 <- meanf(train,h=length(test))
        seriesfit2 <- rwf(train,h=length(test))
        seriesfit3 <- rwf(train, drift=TRUE, h=length(test))
        
        results <- rbind(accuracy(seriesfit1, test)[2,c(2,5)], 
                         accuracy(seriesfit2, test)[2,c(2,5)], 
                         accuracy(seriesfit3, test)[2,c(2,5)])
        
        data.table(
            Model = c("Mean", "Naive", "Drift"),
            results
        )
        
    })
    
    
    output$plot1<- renderPlot({
        
        df <- raw_df %>% 
            filter(as.Date(time)==input$date) %>% 
            select(input$stock)
        
        series <- ts(df)
        
        trainsize <- round(length(series)*.75,0)
        train <- ts(series[1:trainsize-1], start=1, end=trainsize-1)
        test <- ts(series[trainsize:length(series)], start=trainsize, end=length(series))
        
        seriesfit1 <- meanf(train,h=length(test))
        seriesfit2 <- rwf(train,h=length(test))
        seriesfit3 <- rwf(train, drift=TRUE, h=length(test))
        
        autoplot(series) +
            autolayer(seriesfit1, series="Mean", PI=FALSE) +
            autolayer(seriesfit2, series="Naive", PI=FALSE) +
            autolayer(seriesfit3, series="Drift", PI=FALSE) +
            xlab("Minutes") + ylab("Closing Price (US$") +
            ggtitle("Forecasts for quarterly series production") +
            guides(colour=guide_legend(title="Forecast"))
        
    })
    
    
}

shinyApp(ui = ui, server = server)
