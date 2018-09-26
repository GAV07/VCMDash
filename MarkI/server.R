#
# This is the server logic of a Shiny web application. You can run the 
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

require(shinydashboard)
require(ggplot2)
require(dplyr)
require(googlesheets)
require(tidyr)

#loading the dataset
gs_ls()
Metrics <- gs_title("Venture Café Thursday -Miami Metrics Tracking")
Metrics1 <- gs_read(Metrics, ws = "Formatted")

#adjusting for actual Time Values
Metrics1$Date <- as.POSIXct(strptime(Metrics1$Date, "%m/%d/%Y"))

#server function and logic
server <- function(input, output) { 
  output$TG <- renderPlot ({
    #write all R-code inside this
    ggplot(Metrics1, aes(Metrics1$Date, Metrics1$`Total Guest Visits`, fill = Metrics1$`Total Guest Visits`)) + geom_bar(stat="identity") + scale_fill_continuous(low="blue", high="red") 
  }) 
  
}
