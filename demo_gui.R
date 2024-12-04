library(shiny)

# Source the MAGGIC and HAS-BLED score functions
source("maggic.R")
source("has_bled.R")

# Define the UI
ui <- fluidPage(
  titlePanel("Heart Failure Risk Score Calculator"),
  
  tabsetPanel(
    tabPanel("MAGGIC Score",
             sidebarLayout(
               sidebarPanel(
                 numericInput("ef", "Ejection fraction (%)", value = 35, min = 1, max = 95),
                 numericInput("age", "Age (years)", value = 65, min = 18, max = 110),
                 numericInput("sbp", "Systolic Blood Pressure (mmHg)", value = 120, min = 50, max = 250),
                 numericInput("bmi", "BMI (kg/m²)", value = 25, min = 10, max = 50),
                 numericInput("creatinine", "Creatinine (µmol/l)", value = 100, min = 20, max = 1400),
                 selectInput("nyha", "NYHA Class", choices = 1:4, selected = 2),
                 checkboxInput("male", "Male", value = TRUE),
                 checkboxInput("smoker", "Current Smoker", value = FALSE),
                 checkboxInput("diabetic", "Diabetic", value = TRUE),
                 checkboxInput("copd", "Diagnosis of COPD", value = FALSE),
                 checkboxInput("first_diagnosis", "First diagnosis of heart failure in the past 18 months", value = TRUE),
                 checkboxInput("no_beta_blocker", "Not on Beta Blocker", value = TRUE),
                 checkboxInput("no_acei_arb", "Not on ACEI/ARB", value = FALSE),
                 
                 actionButton("calculate_maggic", "Calculate MAGGIC Score")
               ),
               
               mainPanel(
                 h3("MAGGIC Score:"),
                 textOutput("score_output_maggic")
               )
             )
    ),
    
    tabPanel("HAS-BLED Score",
             sidebarLayout(
               sidebarPanel(
                 checkboxInput("uncontrolled_htn", "Uncontrolled hypertension", value = TRUE),
                 checkboxInput("renal_function", "Abnormal Renal Function", value = FALSE),
                 checkboxInput("liver_function", "Abnormal Liver Function", value = TRUE),
                 checkboxInput("stroke", "Stroke", value = FALSE),
                 checkboxInput("bleeding", "Bleeding history or predisposition", value = TRUE),
                 checkboxInput("labile_inr", "Labile international normalized ratio (INR)", value = FALSE),
                 checkboxInput("elderly", "Elderly", value = TRUE),
                 checkboxInput("drugs", "Drugs", value = TRUE),
                 checkboxInput("alcohol", "Alcohol", value = FALSE),
                 
                 actionButton("calculate_has_bled", "Calculate HAS-BLED Score")
               ),
               
               mainPanel(
                 h3("HAS-BLED Score:"),
                 textOutput("score_output_has_bled")
               )
             )
    )
  )
)

# Define the server logic
server <- function(input, output) {
  
  observeEvent(input$calculate_maggic, {
    # Parameters for calc_maggic_score function
    parameters <- list(
      `Ejection fraction (%)` = as.numeric(input$ef),
      `Age (years)` = as.numeric(input$age),
      `Systolic blood pressure (mmHg)` = as.numeric(input$sbp),
      `BMI (kg/m²)` = as.numeric(input$bmi),
      `Creatinine (µmol/l)` = as.numeric(input$creatinine),
      `NYHA Class` = as.numeric(input$nyha),
      Male = input$male,
      `Current smoker` = input$smoker,
      Diabetic = input$diabetic,
      `Diagnosis of COPD` = input$copd,
      `First diagnosis of heart failure in the past 18 months` = input$first_diagnosis,
      `Not on beta blocker` = input$no_beta_blocker,
      `Not on ACEI/ARB` = input$no_acei_arb
    )
    
    # Calculate score
    score <- calc_maggic_score(parameters)
    
    # Output score
    output$score_output_maggic <- renderText({
      paste("The calculated MAGGIC score is:", score)
    })
  })
  
  observeEvent(input$calculate_has_bled, {
    # Parameters for calc_has_bled_score function
    parameters <- list(
      `Uncontrolled hypertension` = input$uncontrolled_htn,
      `Abnormal Renal Function` = input$renal_function,
      `Abnormal Liver Function` = input$liver_function,
      `Stroke` = input$stroke,
      `Bleeding history or predisposition` = input$bleeding,
      `Labile international normalized ratio (INR)` = input$labile_inr,
      `Elderly` = input$elderly,
      `Drugs` = input$drugs,
      `Alcohol` = input$alcohol
    )
    
    # Calculate score
    score <- calc_has_bled_score(parameters)
    
    # Output score
    output$score_output_has_bled <- renderText({
      paste("The calculated HAS-BLED score is:", score)
    })
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
