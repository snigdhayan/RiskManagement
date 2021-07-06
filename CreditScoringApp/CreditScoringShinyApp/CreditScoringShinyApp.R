library(shiny)
# reactiveConsole(TRUE)



ui <- fluidPage(
    titlePanel("Credit Scoring App"),
    h4("Enter the values of the input parameters and click 'Calculate'."),
    fluidRow(
        column(4, wellPanel(
            h3("Input Parameters"),
            numericInput('Duration', 'Checking account (in months)', value = 24, min = 1, max = 100),
            
            numericInput('Age', 'Age (in years)', value = 40, min = 1, max = 100),
            
            br(),
            actionButton("submit", "Calculate")
        )),
        column(8, wellPanel(
            h3("Credit Score"),
            h4("The credit score is the regression value of a simple generalized linear model based on the two input parameters."),
            textOutput("score")
            )
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
        score <- predict(model, newdata = df, type = "link")
    })
    
    # Server response
    output$score <- renderText(
        recompute()
    )
}

# Run the application 
shinyApp(ui = ui, server = server)
