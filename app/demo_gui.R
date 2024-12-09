library(shiny)

# Source requirements.R to install necessary packages
source("../requirements.R")

# Source the score functions
source("../scores/has_bled.R")
source("../scores/maggic.R")
source("../scores/barcelona_hf_v3.R")     
source("../scores/abc_af_bleeding.R")
source("../scores/abc_af_death.R")
source("../scores/abc_af_stroke.R")
source("../scores/charge_af.R")
source("../scores/chads_vasc.R")
source("../scores/smart.R")

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
    ),
    
    tabPanel("Barcelona HF Score",
             sidebarLayout(
               sidebarPanel(
                 numericInput("barcelona_age", "Age (years)", value = 40, min = 18, max = 110),
                 checkboxInput("female", "Female", value = TRUE),
                 selectInput("barcelona_nyha", "NYHA Class", choices = 1:4, selected = 2),
                 numericInput("barcelona_ef", "Ejection fraction (%)", value = 13, min = 1, max = 95),
                 numericInput("sodium", "Sodium (mmol/L)", value = 10, min = 120, max = 150),
                 numericInput("egfr", "eGFR in mL/min/1.73m²", value = 6, min = 6.4, max = 109.7),
                 numericInput("hemoglobin", "Hemoglobin (g/dL)", value = 12, min = 8.8, max = 17.1),
                 numericInput("loop_diuretic", "Loop Diuretic Furosemide Dose (mg)", value = 20, min = 0, max = 500),
                 checkboxInput("statin", "Statin", value = TRUE),
                 checkboxInput("acei_arb", "ACEi/ARB", value = FALSE),
                 checkboxInput("betablocker", "Betablocker", value = FALSE),
                 numericInput("hf_duration", "HF Duration in months", value = 1, min = 0.5, max = 246.948),
                 checkboxInput("diabetes", "Diabetis Mellitus", value = FALSE),
                 numericInput("hosp_prev_year", "Hospitalisation Prev. Year", value = 0, min = 0, max = 8),
                 checkboxInput("mra", "MRA", value = FALSE),
                 checkboxInput("icd", "ICD", value = FALSE),
                 checkboxInput("crt", "CRT", value = FALSE),
                 checkboxInput("arni", "ARNI", value = FALSE),
                 numericInput("nt_pro_bnp", "NT-proBNP in pg/mL", value = 100, min = 0, max = 35000),
                 numericInput("hs_ctnt", "hs-cTnT in ng/L", value = 1.112, min = 0, max = 265.45),
                 numericInput("st2", "ST2 (ng/mL)", value = 4, min = 0, max = 157.074),
                 checkboxInput("sglt2i", "SGLT2i", value = FALSE),
                 
                 actionButton("calculate_barcelona", "Calculate Barcelona HF Score")
               ),
               
               mainPanel(
                 h3("Barcelona HF Score:"),
                 textOutput("score_output_barcelona")
               )
             )
    ),
    tabPanel("ABC-AF Bleeding Score",
             sidebarLayout(
               sidebarPanel(
                 checkboxInput("prior_bleeding", "Prior Bleeding", value = TRUE),
                 numericInput("abc_age", "Age (years)", value = 65, min = 22, max = 95),
                 numericInput("troponin_t", "Troponin T in ng/L", value = 10.0, min = 3.0, max = 200.0),
                 numericInput("gdf_15", "GDF-15 in ng/L", value = 500.0, min = 400.0, max = 20000.0),
                 numericInput("abc_hemoglobin", "Hemoglobin in g/dL", value = 13.5, min = 9.0, max = 20.0),
                 checkboxInput("doac", "DOAC", value = TRUE),
                 checkboxInput("aspirin", "Aspirin", value = FALSE),
                 
                 actionButton("calculate_abc_af_bleeding", "Calculate ABC-AF Bleeding Score")
               ),
               
               mainPanel(
                 h3("ABC-AF Bleeding Score:"),
                 textOutput("score_output_abc_af_bleeding")
               )
             )
    ),
    tabPanel("ABC-AF Death Score",
             sidebarLayout(
               sidebarPanel(
                 checkboxInput("heart_failure", "Heart Failure", value = TRUE),
                 numericInput("abc_death_age", "Age (years)", value = 72, min = 65, max = 110),
                 numericInput("nt_pro_bnp_death", "NT-proBNP in ng/L", value = 600.0, min = 200, max = 35000),
                 numericInput("gdf_15_death", "GDF-15 in ng/L", value = 700.0, min = 400.0, max = 20000.0),
                 numericInput("troponin_t_death", "Troponin T in ng/L", value = 10.0, min = 3.0, max = 200.0),
                 
                 actionButton("calculate_abc_af_death", "Calculate ABC-AF Death Score")
               ),
               
               mainPanel(
                 h3("ABC-AF Death Score:"),
                 textOutput("score_output_abc_af_death_a"),
                 textOutput("score_output_abc_af_death_b")
               )
             )
    ),
    tabPanel("ABC-AF Stroke Score",
             sidebarLayout(
               sidebarPanel(
                 checkboxInput("prior_stroke_tia", "Prior Stroke/TIA", value = TRUE),
                 numericInput("abc_stroke_age", "Age (years)", value = 70, min = 22, max = 95),
                 numericInput("troponin_t_stroke", "Troponin T in ng/L", value = 15.0, min = 3.0, max = 200.0),
                 numericInput("nt_pro_bnp_stroke", "NT-proBNP in ng/L", value = 1000, min = 5, max = 21000),
                 checkboxInput("doac_stroke", "DOAC", value = TRUE),
                 checkboxInput("aspirin_stroke", "Aspirin", value = FALSE),
                 
                 actionButton("calculate_abc_af_stroke", "Calculate ABC-AF Stroke Score")
               ),
               
               mainPanel(
                 h3("ABC-AF Stroke Score:"),
                 textOutput("score_output_abc_af_stroke")
               )
             )
    ),
    tabPanel("CHARGE-AF Score",
             sidebarLayout(
               sidebarPanel(
                 numericInput("charge_af_age", "Age (years)", value = 60, min = 46, max = 94),
                 checkboxInput("race_white", "Race (White)", value = TRUE),
                 numericInput("height", "Height (cm)", value = 170, min = 100, max = 250),
                 numericInput("weight", "Weight (kg)", value = 70, min = 30, max = 200),
                 numericInput("systolic_bp", "Systolic Blood Pressure (mmHg)", value = 120, min = 80, max = 250),
                 numericInput("diastolic_bp", "Diastolic Blood Pressure (mmHg)", value = 80, min = 50, max = 150),
                 checkboxInput("smoking_current", "Smoking (Current)", value = FALSE),
                 checkboxInput("antihypertensive_use", "Antihypertensive Medication Use", value = TRUE),
                 checkboxInput("diabetes_yes", "Diabetes (Yes)", value = FALSE),
                 checkboxInput("heart_failure_yes", "Heart Failure (Yes)", value = FALSE),
                 checkboxInput("mi_yes", "Myocardial Infarction (Yes)", value = TRUE),
                 
                 actionButton("calculate_charge_af", "Calculate CHARGE-AF Score")
               ),
               
               mainPanel(
                 h3("CHARGE-AF Score:"),
                 textOutput("score_output_charge_af")
               )
             )
    ),
    tabPanel("CHADS-VASc Score",
             sidebarLayout(
               sidebarPanel(
                 checkboxInput("chf_lv_dysfunction", "Congestive heart failure/LV dysfunction", value = TRUE),
                 checkboxInput("hypertension", "Hypertension", value = TRUE),
                 checkboxInput("age_75_or_more", "Age ≥ 75 years", value = FALSE),
                 checkboxInput("diabetes_mellitus", "Diabetes Mellitus", value = TRUE),
                 checkboxInput("stroke_tia_te", "Stroke/TIA/TE", value = FALSE),
                 checkboxInput("vascular_diseases", "Vascular Diseases", value = TRUE),
                 checkboxInput("age_65_to_74", "Age 65-74 years", value = TRUE),
                 checkboxInput("sex_category", "Sex category (female)", value = TRUE),
                 
                 actionButton("calculate_chads_vasc", "Calculate CHADS-VASc Score")
               ),
               
               mainPanel(
                 h3("CHADS-VASc Score:"),
                 textOutput("score_output_chads_vasc")
               )
             )
    ),
    tabPanel("SMART Score",
             sidebarLayout(
               sidebarPanel(
                 numericInput("smart_age", "Age (years)", value = 40, min = 30, max = 90),
                 checkboxInput("smart_male", "Male", value = FALSE),
                 checkboxInput("smart_smoker", "Current smoker", value = FALSE),
                 numericInput("smart_systolic_bp", "Systolic blood pressure (mmHg)", value = 110, min = 70, max = 200),
                 checkboxInput("smart_diabetic", "Diabetic", value = FALSE),
                 checkboxInput("smart_cad", "History of coronary artery disease", value = FALSE),
                 checkboxInput("smart_cvd", "History of cerebrovascular disease", value = FALSE),
                 checkboxInput("smart_aaa", "Abdominal aortic aneurysm", value = FALSE),
                 checkboxInput("smart_pad", "Peripheral artery disease", value = FALSE),
                 numericInput("smart_years_vd", "Years since first diagnosis of vascular disease", value = 8, min = 0, max = 30),
                 numericInput("smart_hdl", "HDL-cholesterol in mmol/L", value = 1, min = 0.6, max = 2.5),
                 numericInput("smart_tc", "Total cholesterol in mmol/L", value = 3, min = 2.5, max = 8.0),
                 numericInput("smart_egfr", "eGFR in mL/min/1.73m²", value = 177, min = 21.60551, max = 178.39297),
                 numericInput("smart_hscrp", "hs-CRP in mg/dL", value = 6, min = 0.1, max = 15.0),
                 checkboxInput("smart_antithrombotic", "Antithrombotic treatment", value = FALSE),
                 
                 actionButton("calculate_smart", "Calculate SMART Score")
               ),
               
               mainPanel(
                 h3("SMART Score:"),
                 textOutput("score_output_smart")
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
  
  observeEvent(input$calculate_barcelona, {
    # Parameters for calc_barcelona_hf_score function
    parameters <- list(
      `Age (years)` = as.numeric(input$barcelona_age),
      Female = input$female,
      `NYHA Class` = as.numeric(input$barcelona_nyha),
      `Ejection fraction (%)` = as.numeric(input$barcelona_ef),
      `Sodium (mmol/L)` = as.numeric(input$sodium),
      `eGFR in mL/min/1.73m²` = as.numeric(input$egfr),
      `Hemoglobin (g/dL)` = as.numeric(input$hemoglobin),
      `Loop Diuretic Furosemide Dose` = as.numeric(input$loop_diuretic),
      Statin = input$statin,
      `ACEi/ARB` = input$acei_arb,
      Betablocker = input$betablocker,
      `HF Duration in months` = as.numeric(input$hf_duration),
      `Diabetis Mellitus` = input$diabetes,
      `Hospitalisation Prev. Year` = as.numeric(input$hosp_prev_year),
      MRA = input$mra,
      ICD = input$icd,
      CRT = input$crt,
      ARNI = input$arni,
      `NT-proBNP in pg/mL` = as.numeric(input$nt_pro_bnp),
      `hs-cTnT in ng/L` = as.numeric(input$hs_ctnt),
      `ST2 (ng/mL)` = as.numeric(input$st2),
      `SGLT2i` = input$sglt2i
    )
    
    # Calculate score
    score <- calc_barcelona_hf_score(parameters)
    
    # Output score
    output$score_output_barcelona <- renderText({
      paste("The calculated Barcelona HF scores are:", paste(unlist(score), collapse = ", "))
    })
  })
  # Add this to the server function for handling ABC-AF Bleeding Score calculations
  observeEvent(input$calculate_abc_af_bleeding, {
    # Parameters for calc_abc_af_bleeding_score function
    parameters <- list(
      `Prior Bleeding` = input$prior_bleeding,
      `Age` = as.numeric(input$abc_age),
      `Troponin T in ng/L` = as.numeric(input$troponin_t),
      `GDF-15 in ng/L` = as.numeric(input$gdf_15),
      `Hemoglobin in g/dL` = as.numeric(input$abc_hemoglobin),
      `DOAC` = input$doac,
      `Aspirin` = input$aspirin
    )
    
    # Calculate score
    score <- calc_abc_af_bleeding_score(parameters)
    
    # Output score
    output$score_output_abc_af_bleeding <- renderText({
      paste("The calculated ABC-AF Bleeding score is:", round(score, 2))
    })
  })
  observeEvent(input$calculate_abc_af_death, {
    # Parameters for calc_abc_af_death_score function
    parameters <- list(
      `Heart Failure` = input$heart_failure,
      `Age` = as.numeric(input$abc_death_age),
      `NT-proBNP in ng/L` = as.numeric(input$nt_pro_bnp_death),
      `GDF-15 in ng/L` = as.numeric(input$gdf_15_death),
      `Troponin T in ng/L` = as.numeric(input$troponin_t_death)
    )
    
    # Calculate score
    scores <- calc_abc_af_death_score(parameters)
    
    # Output scores for Model A and Model B
    output$score_output_abc_af_death_a <- renderText({
      paste("The calculated ABC-AF Death Score (Model A) is:", round(scores[[1]], 2))
    })
    
    output$score_output_abc_af_death_b <- renderText({
      paste("The calculated ABC-AF Death Score (Model B) is:", round(scores[[2]], 2))
    })
  })
  observeEvent(input$calculate_abc_af_stroke, {
    # Parameters for calc_abc_af_stroke_score function
    parameters <- list(
      `Prior Stroke/TIA` = input$prior_stroke_tia,
      `Age` = as.numeric(input$abc_stroke_age),
      `Troponin T in ng/L` = as.numeric(input$troponin_t_stroke),
      `NT-proBNP in ng/L` = as.numeric(input$nt_pro_bnp_stroke),
      `DOAC` = input$doac_stroke,
      `Aspirin` = input$aspirin_stroke
    )
    
    # Calculate score
    score <- tryCatch({
      calc_abc_af_stroke_score(parameters)
    }, error = function(e) {
      paste("Error:", e$message)
    })
    
    # Output score
    output$score_output_abc_af_stroke <- renderText({
      if (is.numeric(score)) {
        paste("The calculated ABC-AF Stroke score is:", round(score, 2))
      } else {
        score
      }
    })
  })
  observeEvent(input$calculate_charge_af, {
    # Parameters for calc_charge_af_score function
    parameters <- list(
      `Age` = as.numeric(input$charge_af_age),
      `Race (white)` = input$race_white,
      `Height` = as.numeric(input$height),
      `Weight` = as.numeric(input$weight),
      `Systolic Blood Pressure` = as.numeric(input$systolic_bp),
      `Diastolic Blood Pressure` = as.numeric(input$diastolic_bp),
      `Smoking (current)` = input$smoking_current,
      `Antihypertensive Medication Use (Yes)` = input$antihypertensive_use,
      `Diabetes (Yes)` = input$diabetes_yes,
      `Heart failure (Yes)` = input$heart_failure_yes,
      `Myocardial infarction (Yes)` = input$mi_yes
    )
    
    # Calculate score
    score <- tryCatch({
      calc_charge_af_score(parameters)
    }, error = function(e) {
      paste("Error:", e$message)
    })
    
    # Output score
    output$score_output_charge_af <- renderText({
      if (is.numeric(score)) {
        paste("The calculated CHARGE-AF score is:", round(score, 2))
      } else {
        score
      }
    })
  })
  observeEvent(input$calculate_chads_vasc, {
    # Parameters for calc_chads_vasc_score function
    parameters <- list(
      `Congestive heart failure/LV dysfunction` = input$chf_lv_dysfunction,
      `Hypertension` = input$hypertension,
      `Age ≥75y` = input$age_75_or_more,
      `Diabetes mellitus` = input$diabetes_mellitus,
      `Stroke/TIA/TE` = input$stroke_tia_te,
      `Vascular diseases` = input$vascular_diseases,
      `Age 65-74y` = input$age_65_to_74,
      `Sex category` = input$sex_category
    )
    
    # Calculate score
    score <- tryCatch({
      calc_chads_vasc_score(parameters)
    }, error = function(e) {
      paste("Error:", e$message)
    })
    
    # Output score
    output$score_output_chads_vasc <- renderText({
      if (is.numeric(score)) {
        paste("The calculated CHADS-VASc score is:", score)
      } else {
        score
      }
    })
  })
  observeEvent(input$calculate_smart, {
    # Parameters for calc_smart_score function
    parameters <- list(
      `Age in years` = as.numeric(input$smart_age),
      `Male` = input$smart_male,
      `Current smoker` = input$smart_smoker,
      `Systolic blood pressure in mmHg` = as.numeric(input$smart_systolic_bp),
      `Diabetic` = input$smart_diabetic,
      `History of coronary artery disease` = input$smart_cad,
      `History of cerebrovascular disease` = input$smart_cvd,
      `Abdominal aortic aneurysm` = input$smart_aaa,
      `Peripheral artery disease` = input$smart_pad,
      `Years since first diagnosis of vascular disease` = as.numeric(input$smart_years_vd),
      `HDL-cholesterol in mmol/L` = as.numeric(input$smart_hdl),
      `Total cholesterol in mmol/L` = as.numeric(input$smart_tc),
      `eGFR in mL/min/1.73m²` = as.numeric(input$smart_egfr),
      `hs-CRP in mg/dL` = as.numeric(input$smart_hscrp),
      `Antithrombotic treatment` = input$smart_antithrombotic
    )
    
    # Validate parameters
    validation_results <- validate_parameters(parameters)
    
    # Calculate score if validation passes
    score <- tryCatch({
      if (isTRUE(validation_results)) {
        calc_smart_score(parameters)
      } else {
        paste("Validation Errors:", paste(unlist(validation_results), collapse = "; "))
      }
    }, error = function(e) {
      paste("Error:", e$message)
    })
    
    # Output score
    output$score_output_smart <- renderText({
      if (is.numeric(score)) {
        paste("The calculated SMART score is:", round(score, 2))
      } else {
        score
      }
    })
  })
}

# Run the application 
shinyApp(ui = ui, server = server)