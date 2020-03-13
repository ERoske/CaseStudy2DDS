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
            # Input: Select Box
            # Metric
            selectInput("metric", label = h3("Metric"),
                        choices = list(
                            "Hourly Rate"="HourlyRate",
                            "Monthly Rate"="MonthlyRate",
                            "Monthly Salary"="MonthlyIncome",
                            "Years at Company"="YearsAtCompany",
                            "Years in Current Role"="YearsInCurrentRole"
                                       ),
                        selected = 3),

            # Input: Select Box
            # Chart Type
            selectInput("chart", label=h3("Chart Type:"),
                        choices = list("Boxplot"="box", "Histogram"="hist"),
                        selected = 1),

            # Input: Select Box
            # Category
            selectInput("category", label=h3("Category for boxplot:"),
                        choices = list("Attrition", "Department", "Gender", "OverTime"),
                        selected = 3),

            #Input: Slider for the number of bins ----
            sliderInput(inputId = "bins",
                        label = "Number of bins for histograms:",
                        min = 1,
                        max = 50,
                        value = 10),


            hr(),
            fluidRow(column(3, verbatimTextOutput("value")))
        ),

        # Main panel for displaying outputs ----
        mainPanel(

            # Output: Chart ----
            plotOutput(outputId = "distPlot")

        )
    )
)


# Define server logic required to draw a histogram ----
server <- function(input, output) {
    # renderPlot indicates that:
    # 1. It is "reactive" and therefore should be automatically
    #    re-executed when inputs change
    # 2. Its output type is a plot
    output$distPlot <- renderPlot({

        # Metric
        # if(input$metric == "HourlyRate")
        #     { x    <- emp$HourlyRate
        #       y    <- "HourlyRate~Gender" }
        # if(input$metric == "MonthlyRate")
        #     { x    <- emp$MonthlyRate
        #       y    <- "MonthlyRate~Gender" }
        # if(input$metric == "MonthlyIncome")
        #     { x    <- emp$MonthlyIncome
        #       y    <- "MonthlyIncome~Gender" }
        # if(input$metric == "YearsAtCompany")
        #     { x    <- emp$YearsAtCompany
        #       y    <- "YearsAtCompany~Gender" }
        # if(input$metric == "YearsInCurrentRole")
        #     { x    <- emp$YearsInCurrentRole
        #       y    <- "YearsInCurrentRole~Gender" }

        if(input$metric == "HourlyRate")
        { x    <- emp$HourlyRate }
        if(input$metric == "MonthlyRate")
        { x    <- emp$MonthlyRate }
        if(input$metric == "MonthlyIncome")
        { x    <- emp$MonthlyIncome }
        if(input$metric == "YearsAtCompany")
        { x    <- emp$YearsAtCompany }
        if(input$metric == "YearsInCurrentRole")
        { x    <- emp$YearsInCurrentRole }


        # Chart Type
        if(input$chart == "hist")
        {
            bins <- seq(min(x), max(x), length.out = input$bins + 1)
            hist(x,
                 ylab="Count",
                 xlab=input$metric,
                 col="purple",
                 main=paste("Histogram of Employee Data:", input$metric)
            )
        }
        if(input$chart == "box")
        {
            boxplot(as.formula(paste(input$metric,input$category,sep="~")), data=emp,
                    xlab=input$category,
                    ylab=input$metric,
                    col=(c("red","blue")),
                    main=paste("Boxplot of Employee Data:", input$metric)
                                )
        }



            })

}

shinyApp(ui, server)
