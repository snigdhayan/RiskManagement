
library(shiny)
reactiveConsole(TRUE)


ui <- fluidPage(
    titlePanel("Input Features"),
    
    pageWithSidebar(
        sidebarPanel(
            numericInput('Duration', 'Duration in months of checking account', value = 24, min = 1, max = 100),
            numericInput('Age', 'Age in years', value = 40, min = 1, max = 100),
            actionButton("submit", label = "Calculate Credit Score")
        ),
        
        titlePanel('Credit Score'),
        mainPanel(
            tableOutput("table")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
 
    # Load saved model - can be updated anytime   
    model <- readRDS('./myCreditScoringModel.rds')
    
    # Set default input values server-side
    df <- data.frame("duration.in.month_woe" = 24, 
                     "age.in.years_woe" = 40)
    
    # Recompute prediction whenever user input changes
    recompute <- eventReactive(input$submit, {
        df <- data.frame("duration.in.month_woe" = as.numeric(input$Duration), 
                         "age.in.years_woe" = as.numeric(input$Age))
        prediction <- data.frame("Prediction" = predict(model, newdata = df, type = "response"), 
                                 "Regression_Value" = predict(model, newdata = df, type = "link"))
    })
    
    # Server response
    output$table <- renderTable(
        recompute()
    )
}

# Run the application 
shinyApp(ui = ui, server = server)
