
library(shiny)


shinyUI(fluidPage(
    
    # Application title
    titlePanel("Swiss Data Basic Exploration Plot"),
    h3("The plot tab contains a plot of Fertility vs chosen input 'x' variable"),
    selectInput("variable", "Variable:",
                c("Agriculture" = "Agriculture",
                  "Examination" = "Examination",
                  "Education" = "Education",
                  "Catholic" = "Catholic",
                  "Infant Mortality" = "Infant.Mortality")),
    checkboxInput("show_values", "Show/Hide Actual Slope & Intercept values", value = FALSE),
    
    sidebarLayout(
        sidebarPanel(
            h3("Slope"),
            textOutput("slopeOut"),
            h3("Intercept"),
            textOutput("intOut")
        ),
        
        # Show a the required plot
        mainPanel(
            tabsetPanel(type = "tabs", 
                        tabPanel("Documentation", br(), h3("Choose desired x variable from drop down 
                                                           and select data points using brush for linear model in the
                                                           plot tab.
                                                           Complete details of the dataset can be found from offical
                                                           documentation.
                                                           ")),
                        tabPanel("Plot", br(), plotOutput("plot1", brush = brushOpts(id = "brush1"))))
        )
    )
))
