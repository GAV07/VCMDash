#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shinydashboard)
require(shiny)

# Define UI for application that draws a histogram
dashboardPage(
  #defines header
  skin = "blue",
  
  #header of the dashboard
  dashboardHeader(
    title="Venture Cafe Miami" ,
    dropdownMenu()
  ),
  #defines sidebar of the dashboard
  dashboardSidebar(
    sidebarMenu(
      #the sidebar menu items
      menuItem("Dashboard", tabName = "dashboard", icon = icon("dashboard"))
    )),
  #defines the body of the dashboard
  dashboardBody(
    #to add external CSS
    tags$head(
      tags$link(rel = "stylesheet", type = "text/css", href = "custom.css")
    ),
    tabItems(
      #First TAB Menu-dashboard- first argument should be the 'tabName' value of the menuItem function  
      tabItem(tabName = "Thursday Gathering",
              fluidRow(
                
                       #box() is similar to a 'div' element in HTML
                       box(plotOutput("TG", height = 150)),# end box 
                #box for controlling barplot
                       box(
                         title = "Controls",
                         sliderInput("slider", "Number of observations: ", 5, 25, 50)
                ) 
      )
      )#end tabitems
)#end body
)
)#end dashboard
