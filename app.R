library(shiny)
library(shinydashboard)
library(ggplot2)
library(lubridate)
library(DT)
library(jpeg)
library(grid)
library(leaflet)
library(scales)

#setwd("~/class/424/proj1")

# Load in our data
# make sure to disable quotes
ridership <- read.table(file = "ridership.tsv", sep = '\t', header = TRUE, quote="")

# fix the date

ridership$date_ymd <- ymd(paste(year(mdy(ridership$date)), month(mdy(ridership$date)), day(mdy(ridership$date)), sep="-"))
ridership$month <- month(ridership$date_ymd)
ridership$month_char <- month.abb[month(ridership$date_ymd)]
ridership$year <- year(ridership$date_ymd)
ridership$day <- day(ridership$date_ymd)
ridership$day_of_week <- weekdays(ridership$date_ymd)
#ridership$date_correct <- ymd(paste(ridership$Year, utility$Month, "01", sep="-"))

# TODO: Make these subsets into their own files

# first station is UIC-HALSTED
uic <- subset(ridership, stationname == "UIC-Halsted")

# second station is O'Hare
hare <- subset(ridership, stationname == "O'Hare Airport")

#third station is Jackson
jackson <- subset(ridership, stationname == "Jackson")

#UICperYear = ggplot(data=uic, aes(x=year, y=rides)) 
#UICperYear = UICperYear + geom_bar(stat = "identity", fill="#098CF9") 
#UICperYear = UICperYear + ggtitle("UIC Per Year Ridership") 
#UICperYear = UICperYear + scale_y_continuous(labels = scales::comma) 
#UICperYear = UICperYear + scale_x_continuous(breaks = seq(2001, 2021, 2))


#uic <- read.table("uicHalsted.csv", sep=",", header = TRUE, quote="")
#hare <- read.table("ohare.csv", sep=",", header = TRUE, quote="")

uic2021 <- subset(uic, year == 2021)

monthNums <- seq(1, 12)
months <- month.abb[monthNums]

weekdayNums <- seq(1, 7)
weekdayNums <- c("Sunday", "Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday")


# for(val in 2001:2021) {
# 
#   uic_this_year = subset(uic, year == val)
#   monthly_title <- paste("UIC", val,  "Monthly Ridership")
#   UICperMonth = ggplot(data=uic_this_year, aes(x=month_char, y=rides))
#   UICperMonth = UICperMonth + geom_bar(stat = "identity", fill="#098CF9")
#   UICperMonth = UICperMonth + ggtitle(monthly_title)
#   UICperMonth = UICperMonth + scale_y_continuous(labels = scales::comma)
#   UICperMonth = UICperMonth + scale_x_discrete("", limits = months)
#   print(UICperMonth)
# 
#   # make the chart
#   month_table_df <- data.frame(Month=character(), Rides=integer())
# 
#   for(mon in months) {
#     num <- with(uic_this_year, sum(rides[month_char == mon]))
#     rowdf <- data.frame(Month=mon, Rides=num)
#     month_table_df <- rbind(month_table_df, rowdf)
#   }
#   month_table = datatable(month_table_df, options = list(searching = FALSE, pagelength = 2))
#   print(month_table)
# 
#   # TODO: ADD TITLE TO TABLE
# 
#   UICperweekday = ggplot(data=uic_this_year, aes(x=day_of_week, y=rides))
#   UICperweekday = UICperweekday + geom_bar(stat = "identity", fill="#098CF9")
#   UICperweekday = UICperweekday + ggtitle("UIC 2021 Weekday Ridership")
#   UICperweekday = UICperweekday + scale_y_continuous(labels = scales::comma)
#   UICperweekday = UICperweekday + scale_x_discrete("", limits=weekdayNums, labels=c("Sunday" = "Sun","Monday" = "Mon", "Tuesday" = "Tues", "Wednesday" = "Wed", "Thursday" = "Thurs", "Friday" = "Fri", "Saturday" = "Sat"))
#   #UICperweekday
#   weekday_table_df <- data.frame(Weekday=character(), Rides=integer())
# 
#   for(wkday in weekdayNums) {
#     num <- with(uic_this_year, sum(rides[day_of_week == wkday]))
#     rowdf <- data.frame(Weekday=wkday, Rides=num)
#     weekday_table_df <- rbind(weekday_table_df, rowdf)
#   }
#   week_table = datatable(weekday_table_df, options = list(searching = FALSE, pagelength = 2))
#   week_table
# }
# 


# make plot based on year number 



# make the shiny page
ui <- dashboardPage(
  dashboardHeader(title = "CS424 Proj1"),
  dashboardSidebar(
    disable = TRUE, collapsed = FALSE),
  dashboardBody(
  
  # fluidRow(
  #   
  #   
  #   column(4, 
  #        conditionalPanel(
  #          condition = "input.stop1 == 'UIC-Halsted'", 
  #          plotOutput("uicPlotMonthly", height=200)
  #        ),
  #        conditionalPanel(
  #          condition = "input.stop1 == 'O-Hare'", 
  #          plotOutput("harePlotMonthly", height=200)
  #        )
  #        
  #        
  #        ),
  #   column(4, 
  #          conditionalPanel(
  #            condition = "input.stop1 == 'UIC-Halsted'", 
  #            plotOutput("uicPlotWeekly", height=200)
  #          ),
  #          conditionalPanel(
  #            condition = "input.stop1 == 'O-Hare'", 
  #            plotOutput("harePlotWeekly", height=200)
  #          )
  #   ),
  # ), 
    
  fluidRow(
    column(2, box(
      sliderInput(inputId = "Year",
                  label = "Year:",
                  min = 2001,
                  max = 2021,
                  value = 2001,
                  sep=""),
      selectInput(
        inputId = "stop1",
        label = "Stop",
        choices = c("UIC-Halsted", "O-Hare"),
        selected = NULL,
        multiple = FALSE,
        selectize = TRUE,
        width = NULL,
        size = NULL
      ),
      selectInput(
        inputId = "stop1Type",
        label = "Ridership by: ",
        choices = c("Year", "Month", "Day", "Weekday"),
        selected = "Year",
        multiple = FALSE,
        selectize = TRUE,
        width = NULL,
        size = NULL
      ),
      radioButtons("stop1isGraph", "Display as: ",
                   choices = list("Graph" = 1, "Table" = 0),selected = 1)
      , title = "Stop 1", background = "black", width = 12)
    ),
    column(8, dataTableOutput("uicTableWeekly", height=200)),
    column(8, plotOutput("plot1", height=200))
  )

  ))
  
server <- function(input, output, session) {


uicReactive <- reactive({subset(uic, uic$year == input$Year)})
hareReactive <- reactive({subset(hare, hare$year == input$Year)})

stops = c(uic, hare)
stopReactive <- reactive({subset(stops[input$stop1isGraph], stops[input$stop1isGraph] == input$Year)})

month_table_df <- data.frame(Month=character(), Rides=integer())
for(mon in months) {
  num <- with(uic2021, sum(rides[month_char == mon]))
  rowdf <- data.frame(Month=mon, Rides=num)
  month_table_df <- rbind(month_table_df, rowdf)
}

output$plot1 <- renderPlot({
  
  # get the dataset we're using
    stop1Data <- NULL
    if(input$stop1 == "UIC-Halsted") {
      monthly_title <- paste("UIC", input$Year,  "Monthly Ridership")
      stop1Data <- uicReactive()
    } else if(input$stop1 == "O-Hare") { 
      monthly_title <- paste("O-Hare", input$Year,  "Monthly Ridership")
      stop1Data <- hareReactive()
    }
    
    # get the metric for the graph
    
    # Month
    if(input$stop1Type == "Month") {
      
      if(input$stop1isGraph == 1) {
        ggplot(data=stop1Data, aes(x=month_char, y=rides)) + geom_bar(stat = "identity", fill="#098CF9") + ggtitle(monthly_title) + scale_y_continuous(labels = scales::comma) + scale_x_discrete("", limits = months)
      }
      else {
           month_table_df <- data.frame(Month=character(), Rides=integer())
        
           for(mon in months) {
             num <- with(stop1Data, sum(rides[month_char == mon]))
             rowdf <- data.frame(Month=mon, Rides=num)
             month_table_df <- rbind(month_table_df, rowdf)
           }
           renderDT(month_table_df, options = list(searching = FALSE, pagelength = 2))
      }
      
    }
    # Weekday
    else if(input$stop1Type == "Weekday") { 
      
      ggplot(data=stop1Data, aes(x=day_of_week, y=rides)) + geom_bar(stat = "identity", fill="#098CF9")  + ggtitle(monthly_title) + scale_y_continuous(labels = scales::comma)  + scale_x_discrete("", limits=weekdayNums, labels=c("Sunday" = "Sun","Monday" = "Mon", "Tuesday" = "Tues", "Wednesday" = "Wed", "Thursday" = "Thurs", "Friday" = "Fri", "Saturday" = "Sat"))
    }
    # Day 
    else if(input$stop1Type == "Day") { 
      ggplot(data=stop1Data, aes(x=date_ymd, y=rides)) + geom_bar(stat="identity") + ggtitle(monthly_title)
    }
    # Year
    else if(input$stop1Type == "Year") {
      
      # get our total dataset
      if(input$stop1 == "UIC-Halsted") {
        monthly_title <- paste("UIC", input$Year,  "Yearly Ridership")
        stop1Data <- uic
      } else if(input$stop1 == "O-Hare") { 
        monthly_title <- paste("O-Hare", input$Year,  "Yearly Ridership")
        stop1Data <- hare
      }
      ggplot(data=stop1Data, aes(x=year, y=rides)) + geom_bar(stat = "identity", fill="#098CF9") + ggtitle("UIC Per Year Ridership") + scale_y_continuous(labels = scales::comma) + scale_x_continuous(breaks = seq(2001, 2021, 2))
    }
    
  
  
})

  

output$uicPlotMonthly <- renderPlot({
  monthly_title <- paste("UIC", input$Year,  "Monthly Ridership")
  thisYearUIC <- uicReactive()
  ggplot(data=thisYearUIC, aes(x=month_char, y=rides)) + geom_bar(stat = "identity", fill="#098CF9") + ggtitle(monthly_title) + scale_y_continuous(labels = scales::comma) + scale_x_discrete("", limits = months)
  #ggplot(data=thisYearUIC, aes(x=day_of_week, y=rides)) + geom_bar(stat = "identity", fill="#098CF9")  + ggtitle("UIC 2021 Weekday Ridership") + scale_y_continuous(labels = scales::comma)  + scale_x_discrete("", limits=weekdayNums, labels=c("Sunday" = "Sun","Monday" = "Mon", "Tuesday" = "Tues", "Wednesday" = "Wed", "Thursday" = "Thurs", "Friday" = "Fri", "Saturday" = "Sat"))
  
  
})

output$uicPlotWeekly <- renderPlot({
    weekly_title <- paste("UIC", input$Year,  "Monthly Ridership")
  
    thisYearUIC <- uicReactive()
    
    ggplot(data=thisYearUIC, aes(x=day_of_week, y=rides)) + geom_bar(stat = "identity", fill="#098CF9")  + ggtitle(weekly_title) + scale_y_continuous(labels = scales::comma)  + scale_x_discrete("", limits=weekdayNums, labels=c("Sunday" = "Sun","Monday" = "Mon", "Tuesday" = "Tues", "Wednesday" = "Wed", "Thursday" = "Thurs", "Friday" = "Fri", "Saturday" = "Sat"))
  
})

output$uicTableWeekly <- DT::renderDataTable(DT::datatable({
  
  # TODO: Abstract this to days, years, months, weekdays
  # TODO: add conditionalPanel to the row which can decide to show table or graph
  
  month_table_df <- data.frame(Month=character(), Rides=integer())
  for(mon in months) {
    num <- with(uic2021, sum(rides[month_char == mon]))
    rowdf <- data.frame(Month=mon, Rides=num)
    month_table_df <- rbind(month_table_df, rowdf)
  }
  data <- month_table_df
  data
}))

output$uicPlotYearly <- renderPlot({
  yearly_title <- paste("UIC", input$Year,  "Yearly Ridership")
  
  thisYearUIC <- uicReactive()
  
  ggplot(thisYearUIC, aes(x=date_ymd, y=rides)) + geom_bar(stat="identity") + ggtitle(yearly_title)
  
})

output$harePlotMonthly <- renderPlot({
  monthly_title <- paste("O-Hare", input$Year,  "Monthly Ridership")
  thisYearHare <- hareReactive()
  ggplot(data=thisYearHare, aes(x=month_char, y=rides)) + geom_bar(stat = "identity", fill="#098CF9") + ggtitle(monthly_title) + scale_y_continuous(labels = scales::comma) + scale_x_discrete("", limits = months)
  #ggplot(data=thisYearUIC, aes(x=day_of_week, y=rides)) + geom_bar(stat = "identity", fill="#098CF9")  + ggtitle("UIC 2021 Weekday Ridership") + scale_y_continuous(labels = scales::comma)  + scale_x_discrete("", limits=weekdayNums, labels=c("Sunday" = "Sun","Monday" = "Mon", "Tuesday" = "Tues", "Wednesday" = "Wed", "Thursday" = "Thurs", "Friday" = "Fri", "Saturday" = "Sat"))
})

output$harePlotWeekly <- renderPlot({
  weekly_title <- paste("O-Hare", input$Year,  "weekly Ridership")
  
  thisYearHare <- hareReactive()
  
  ggplot(data=thisYearHare, aes(x=day_of_week, y=rides)) + geom_bar(stat = "identity", fill="#098CF9")  + ggtitle(weekly_title) + scale_y_continuous(labels = scales::comma)  + scale_x_discrete("", limits=weekdayNums, labels=c("Sunday" = "Sun","Monday" = "Mon", "Tuesday" = "Tues", "Wednesday" = "Wed", "Thursday" = "Thurs", "Friday" = "Fri", "Saturday" = "Sat"))
  
})
    
}
shinyApp(ui, server)



