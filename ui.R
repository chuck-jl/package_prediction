dashboardPage(
  dashboardHeader(title = "Popular R package download prediction"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Dashboard", tabName = "Dashboard"),
      menuItem("Raw Data", tabName = "Rawdata")
    )
  ),
  dashboardBody(
    tabItems(
      tabItem("Dashboard",
              fluidRow(
                valueBoxOutput("rcpp"),
                valueBoxOutput("ggplot"),
                valueBoxOutput("dplyr"),
                valueBoxOutput("stringr")
              ),   
              fluidRow(
                box(
                  width = 6, status = "info", solidHeader = TRUE,
                  title = "Prediction for Rcpp Package",
                  plotOutput("plot1")
                ),
                box(
                  width = 6, status = "info", solidHeader = TRUE,
                  title = "Prediction for GGplot Package",
                  plotOutput("plot2")
                )
              ),
              fluidRow(
                box(
                  width = 6, status = "info", solidHeader = TRUE,
                  title = "Prediction for Rcpp Package",
                  plotOutput("plot3")
                ),
                box(
                  width = 6, status = "info", solidHeader = TRUE,
                  title = "Prediction for GGplot Package",
                  plotOutput("plot4")
                )
              )
                
      ),
      tabItem("Rawdata",
              numericInput("maxrows", "Rows to show", 25, min=1, max=dim(data)[1]),
              verbatimTextOutput("rawtable"),
              downloadButton("downloadCsv", "Download All Data as CSV")
      )
    )
  )
)
