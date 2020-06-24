dashboardPage(
  dashboardHeader(title = "COVID-19 Overview"),
  dashboardSidebar(
    sidebarMenu(menuItem("Dashboard",
               tabName = "dashboard",icon=icon("list-alt")),
               selectInput("inputcountry",label = "Select region",choices = country$Var1,
                           multiple = TRUE),
               selectInput("inputstatus",label = "Select Varaible",
                           choices = c('Confirmed','Recovered','Deaths')),
               sliderInput("inputdate","Select date",min = min(mydata$Date),
                           max = max(mydata$Date),value = max(mydata$Date)))
    ),
  dashboardBody(
    tabItems(
      tabItem("dashboard",
              fluidRow(
                valueBoxOutput("Total_Confirmed"),
                valueBoxOutput("Total_Recovered"),
                valueBoxOutput("Total_Deaths")
              ),
              fluidRow(
                valueBoxOutput("New_Confirmed"),
                valueBoxOutput("New_Recovered"),
                valueBoxOutput("New_Deaths")
              ),
              fluidRow(
                valueBoxOutput("Existing_Confirmed"),
                valueBoxOutput("Recovered_Rate"),
                valueBoxOutput("Deaths_Rate")
              ),
              fluidRow(
                box(
                  width = 8, status = "info", solidHeader = TRUE,
                  title = "The data Visualization",
                  tabsetPanel(
                    tabPanel("Line Chart", plotOutput("plot1")), 
                    tabPanel("Bar Chart", plotOutput("plot2")), 
                    tabPanel("Mapper", leafletOutput("plot3")),
                    tabPanel("Recovered Rate Ranking", plotOutput("plot4")),
                    tabPanel("Deaths Rate Ranking", plotOutput("plot5"))
                    )
                ),
                box(
                  width = 4, status = "info",
                  title = "The top 12 region",
                  tableOutput("packageTable")
                )
              ),
              HTML('<a href=https://raw.githubusercontent.com/datasets/covid-19/master/data/time-series-19-covid-combined.csv target="_blank" >
                 <font size="3">Note: The data of this shiny APP come from: https://raw.githubusercontent.com/datasets/covid-19/master/data/time-series-19-covid-combined.csv </font></a>')
      )
    )
  )
)
