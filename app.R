#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

url <- "https://raw.githubusercontent.com/ERoske/CaseStudy2DDS/master/"
emp <- read.csv(paste0(url,"CaseStudy2-data-with_lm.csv"), header=TRUE)


ui <- fluidPage(

    # App title ----
    titlePanel("Frito-Lay Employee Analysis"),

    # Sidebar layout with input and output definitions ----
    sidebarLayout(

        # Sidebar panel for inputs ----
        sidebarPanel(

            #Input: Slider for the number of bins ----
            sliderInput(inputId = "bins",
                        label = "Number of bins:",
                        min = 1,
                        max = 50,
                        value = 10),

            #Copy the line below to make a select box
            selectInput("select", label = h3("Metric"),
                        choices = list(
                            "Hourly Rate"="HourlyRate",
                            "Monthly Rate"="MonthlyRate",
                            "Monthly Salary"="MonthlyIncome",
                            "Years at Company"="YearsAtCompany",
                            "Years in Current Role"="YearsInCurrentRole"
                                       ),
                        selected = 1),
            hr(),
            fluidRow(column(3, verbatimTextOutput("value")))
        ),

        # Main panel for displaying outputs ----
        mainPanel(

            # Output: Histogram ----
            plotOutput(outputId = "distPlot")

        )
    )
)


# Define server logic required to draw a histogram ----
server <- function(input, output) {

    # Histogram of Employee Data ----
    # with requested number of bins
    # This expression that generates a histogram is wrapped in a call
    # to renderPlot to indicate that:
    #
    # 1. It is "reactive" and therefore should be automatically
    #    re-executed when inputs (input$bins) change
    # 2. Its output type is a plot
    output$distPlot <- renderPlot({

        if(input$select == "HourlyRate")
        {
            x    <- emp$HourlyRate
            bins <- seq(min(x), max(x), length.out = input$bins + 1)
            hist(x, breaks = bins, col = "#75AADB", border = "white",
                 xlab = "Hourly Rate",
                 main = "Histogram of Employee Data")
        }
        if(input$select == "MonthlyRate")
        {
            x    <- emp$MonthlyRate
            bins <- seq(min(x), max(x), length.out = input$bins + 1)
            hist(x, breaks = bins, col = "#75AADB", border = "white",
                 xlab = "Monthly Rate",
                 main = "Histogram of Employee Data")
        }
        if(input$select == "MonthlyIncome")
        {
            x    <- emp$MonthlyIncome
            bins <- seq(min(x), max(x), length.out = input$bins + 1)
            hist(x, breaks = bins, col = "#75AADB", border = "white",
                 xlab = "Monthly Income",
                 main = "Histogram of Employee Data")
        }
        if(input$select == "YearsAtCompany")
        {
            x    <- emp$YearsAtCompany
            bins <- seq(min(x), max(x), length.out = input$bins + 1)
            hist(x, breaks = bins, col = "#75AADB", border = "white",
                 xlab = "Years at Company",
                 main = "Histogram of Employee Data")
        }
        if(input$select == "YearsInCurrentRole")
        {
            x    <- emp$YearsInCurrentRole
            bins <- seq(min(x), max(x), length.out = input$bins + 1)
            hist(x, breaks = bins, col = "#75AADB", border = "white",
                 xlab = "Years In Current Role",
                 main = "Histogram of Employee Data")
        }

            })

}

shinyApp(ui, server)
