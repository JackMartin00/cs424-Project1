#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinydashboard)
library(tidyverse)
library(lubridate)
# install.packages("DT")
library(DT)
# setwd("D:/R data 424/Project 1")
#------------------------------------
#reading in data for halsted
Halsted <- read.table(file = "./Halsted Stop.tsv", sep = "\t", header = TRUE)

#converting date type to workable column
newDate <- as.Date(Halsted$date, "%m/%d/%Y")
Halsted$newDate<-newDate
Halsted$date<-NULL
#------------------------------------
#menu items to select different years and different stations
years <- c(2001:2021)
#TODO:fill in station part
?dashboardSidebar
# Define UI for application that draws a histogram
ui <- dashboardPage(
    # Application title
    dashboardHeader(title = "Jack Martin Project 1"),
    dashboardSidebar(disable = FALSE, collapsed = FALSE,
      sidebarMenu(
        id = "tabs",
        menuItem("", tabName = "cheapBlankSpace", icon = NULL),
        menuItem("", tabName = "cheapBlankSpace", icon = NULL),
        menuItem("", tabName = "cheapBlankSpace", icon = NULL),
        menuItem("", tabName = "cheapBlankSpace", icon = NULL),
        menuItem("", tabName = "cheapBlankSpace", icon = NULL),
        menuItem("", tabName = "cheapBlankSpace", icon = NULL),
        menuItem("", tabName = "cheapBlankSpace", icon = NULL),
        selectInput("Year", "Select the year to visualize", years, selected = 2021),
        menuItem("Entries every Year", tabName = "allYears"),
        menuItem("Detailed View", tabName = "detailed" ),#empty for now
        menuItem("Data Charts", tabName = "charts"),
        menuItem("About", tabName = "about"))
      ),
        # Show a plot of the generated distribution
        dashboardBody(
          
          tabItems(
            tabItem(tabName = "allYears",
                box(title = "Entries over the Years", solidHeader = TRUE, status = "primary", width = 12,
                    plotOutput("entryYear")
                ) 
            ),
            
            tabItem(tabName = "detailed",
                  
                  
                    
                  box(title = "Plots ", solidHeader = TRUE, status = "primary", width = 12,
                        
                      splitLayout(cellWidths = c("33%", "33%", "33%"), plotOutput("eachDay"),plotOutput("eachMonth"), plotOutput("dayType"))
                  )
                  # box(title = "Charts", solidHeader = TRUE, status = "primary", width = 12,
                  # 
                  #     # gt(table1)
                  #     # dataTableOutput('table1')
                  #     DTOutput('yearTable')
                  # 
                  #     # splitLayout(cellWidths = c("33%"), tableOutput('table1'))
                  # )
                  
            ),
            tabItem(tabName = "charts",
                    box(title = "Charts", solidHeader = TRUE, status = "primary", width = 12,
                        
                        # gt(table1)
                        # dataTableOutput('table1')
                        # DTOutput('yearTable')
                        splitLayout(cellWidths = c("33%", "33%", "33%"), DTOutput('yearTable'),DTOutput('monthTable'), DTOutput('dayTable'))
                        # splitLayout(cellWidths = c("33%"), tableOutput('table1'))
                    )
            ),
            tabItem(tabName = "about",
                    box(title = "About", solidHeader = TRUE, status = "primary", width = 12,
                        
                        
                        h2("Jack Martin created this app for Project 1 of UIC's CS 424 - Visual Analytics."),
                        p("This data is from the Chicago Data Portal. More specifically, the \'CTA - Ridership - L Station Entries - Daily total\'.
                           The main components on why we are given this project is to teach us and give better familiarity with\n 
                          both the R language and Shiny and Shiny dashboard. We were tasked with analyzing and plotting Entries over\n
                          specific stations over 2001-2021 and over each Day of the Week and Month.")
                        )
                      
          )
        )
        )
)

# Define server logic
server <- function(input, output) {
  
  output$entryYear <- renderPlot({
    subset(Halsted,newDate > as.Date("2000-12-31")) %>%
    ggplot( aes(year(newDate),rides))+
      geom_bar(stat="identity",fill="#88CCEE")+
      labs(x = "Years", y = "Number of Entries", title = "Entries per Year")+
      theme_bw()+
      scale_y_continuous(expand = c(0,0), limits = c(0,max(sumOfRidesPerYear$sum) * 1.05))
  })
  
  output$eachDay <- renderPlot({
    subset(Halsted,newDate > as.Date("2020-12-31")) %>%
      ggplot()+
      geom_bar(aes(newDate,rides,fill= uniq.loc),stat = "identity",fill="#88CCEE")+
      labs(x = "Days of 2021", y = "Number of Entries", title = "Entries in 2021")+
      theme_bw()+
      scale_x_date(date_breaks = "1 month", date_labels = "%b", expand = c(0,0))+
      scale_y_continuous(expand = c(0,0))
  })
  
  output$eachMonth <- renderPlot({
    subset(Halsted,newDate > as.Date("2020-12-31")) %>%
      ggplot(aes(month(newDate,label = TRUE),rides))+
      geom_bar(stat = "identity",fill="#88CCEE")+
      labs(x = "Months", y = "Number of Entries", title = "Entries per Month")+
      theme_bw()+
      scale_y_continuous(expand = c(0,0))
    
  })
  
  output$dayType <- renderPlot({
    
    #groups sum of Rides per day together 
    temp1 = subset(Halsted,newDate > as.Date("2020-12-31")) 
    sumOfRidesPerDay = temp1 %>% group_by(wday(newDate)) %>% summarise(sum = sum(rides))
    
    subset(Halsted,newDate > as.Date("2020-12-31")) %>%
      ggplot(aes(wday(newDate,label = TRUE),rides))+
      geom_bar(stat = "identity",fill="#88CCEE")+
      theme_bw()+
      scale_y_continuous(expand = c(0,0), limits = c(0,max(sumOfRidesPerDay$sum) * 1.05), labels = scales::comma)+
      labs(x = "Day Type", y = "Number of Entries", title = "Entries per Day Type")
  })
  
  
  # -----------------Charts-------------------------
  
  #table stuff for each Year

  output$table1 <- renderTable({
    sumOfRidesPerYear = Halsted %>% group_by(year(newDate)) %>% summarise(sum = sum(rides))
    
    
    yearTable <- matrix(c(2001:2021,sumOfRidesPerYear$sum),ncol=2,byrow=FALSE)
    
    colnames(yearTable) <- c('Years','Entries')
    # yearTable
    yearTable

  })
  
  output$yearTable <- renderDT({
    
    sumOfRidesPerYear = Halsted %>% group_by(year(newDate)) %>% summarise(sum = sum(rides))
    
    
    yearTable <- matrix(c(2001:2021,sumOfRidesPerYear$sum),ncol=2,byrow=FALSE)
    
    colnames(yearTable) <- c('Years','Entries')
    # yearTable
    yearTable
  }, options = list(lengthChange = FALSE)
  )
  
  output$monthTable <- renderDT({
    sumOfRidesPerMonth = Halsted %>% group_by(month(newDate)) %>% summarise(sum = sum(rides))
    
    monthTable <- matrix(c(month.name,sumOfRidesPerMonth$sum),ncol=2,byrow=FALSE)
    monthTable
  }, options = list(lengthChange = FALSE))
  
  output$dayTable <- renderDT({
    sumOfRidesPerDay = temp1 %>% group_by(wday(newDate)) %>% summarise(sum = sum(rides))
    
    dayTable <- matrix(c(c('Sun','Mon','Tue','Wed','Thu','Fri','Sat'),sumOfRidesPerDay$sum),ncol=2,byrow=FALSE)
    dayTable
  }, options = list(lengthChange = FALSE))
  
}

# Run the application 

shinyApp(ui = ui, server = server)

