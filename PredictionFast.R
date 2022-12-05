#author: Ziqing Wang
#date: "12/04/2022"


# Import packages
library(shiny)
library(tidyverse)
library(srvyr)
library(survey)
library(shinycssloaders)
library(shiny)

# Setup: Import data sets
ind_depr = readRDS("./data/shiny_ind_data.rds") # outcome: depression
ind_mh_days = readRDS("./data/shiny_ind_data2.rds") # outcome: days of bad mental health
group_depr = readRDS("./data/shiny_group_data.rds") # outcome: depression
group_mh_days = readRDS("./data/shiny_group_data2.rds") # outcome: days of bad mental health

# Define UI for app 
ui <- fluidPage(
  
  # Dashboard title
  titlePanel(title = 'Individual-level and Group-level Predicted Probabilities'),
  
  # Set tabs
  mainPanel(
    tabsetPanel(
      tabPanel("Groupl-level predicted probabilities", 
               
               # User-specified factor levels for individual-level predicted probability calculation
               selectInput(inputId = "which_outcome_group", 
                           label = strong("Outcome to Predict"), 
                           choices = c("Self-reported depression diagnosis"  = "depression",
                                       "One or more days of bad mental health in the past 30 days" = "mh_bin"),
                           selected = "Self-reported depression diagnosis"),
               
               selectInput(inputId = "group_factor",
                           label = "The factor for which predictive means are wanted",
                           choices = c("Sex" = "sex",
                                       "Children" = "children",
                                       "Race/Ethnicity" = "race",
                                       "Age" = "age",
                                       "Education" = "education",
                                       "Annual income" = "income",
                                       "Employment status" = "employment",
                                       "General health" = "general_health",
                                       "Any Exercise in the past 30 days" = "exercise"),
                           selected = "Sex"),
               
               actionButton("submit_group_factor", "Go!"),
               textOutput("out_click_group"),
               textOutput("text1"),
               shinycssloaders::withSpinner(
                 tableOutput("group_pred")
               )),
      
      tabPanel("Individual-level predicted probabilities", 
               # User-specified factor levels for individual-level predicted probability calculation
               selectInput(inputId = "which_outcome_ind", 
                           label = strong("Outcome to Predict"), 
                           choices = c("Self-reported depression diagnosis"  = "depression",
                                       "One or more days of bad mental health in the past 30 days" = "mh_bin"),
                           selected = "Self-reported depression diagnosis"),
               selectInput(inputId = "gender",
                           label = "Gender",
                           choices = c("Female" = "female",
                                       "Male" = "male"),
                           selected = "Female"),
               
               selectInput(inputId = "children",
                           label = "Number of children in respondent's household",
                           choices = c("None" = "none",
                                       "One or two" = "one or two",
                                       "Three or more" = "three or more"),
                           selected = "None"),
               
               selectInput(inputId = "race",
                           label = "Race/Ethnicity",
                           choices = c("White" = "white",
                                       "Black" = "black",
                                       "Asian" = "asian",
                                       "Hispanic" = "hispanic",
                                       "Other" = "other"),
                           selected = "White"),
               
               selectInput(inputId = "age",
                           label = "Age group",
                           choices = c("18-29 years old" = "18-29",
                                       "30-49 years old" = "30-49",
                                       "50-64 years old" = "50-64",
                                       "65 years or older" = "65+"),
                           selected = "18-29 years old"),
               
               selectInput(inputId = "marital_status",
                           label = "Marital status",
                           choices = c("Married" = "married",
                                       "Not married" = "not married"),
                           selected = "Married"),
               
               selectInput(inputId = "education",
                           label = "Education attainment",
                           choices = c("Less than high school" = "less than high school",
                                       "High school or some college" = "high school or some college",
                                       "Bachelors or higher" = "bachelors or higher"),
                           selected = "Less than high school"),
               
               selectInput(inputId = "income",
                           label = "Annual income ($)",
                           choices = c("Less than 35,000" = "<=35000",
                                       "35,000-75,000" = "35000-75000",
                                       "75,000 or higher" = ">75000"),
                           selected = "Less than high school"),
               
               selectInput(inputId = "employment",
                           label = "Employment status",
                           choices = c("Employed for salary" = "employed",
                                       "Homemaker/Student/Retired" = "homemaker/student/retired",
                                       "Unemployed" = "unemployed"),
                           selected = "Employed for salary"),
               
               selectInput(inputId = "general_health",
                           label = "Self-reported general health status",
                           choices = c("Excellent" = "excellent",
                                       "Very good/good" = "very good/good",
                                       "Fair/poor" = "fair/poor"),
                           selected = "Excellent"),
               
               selectInput(inputId = "exercise",
                           label = "Any physical exercise in the past 30 days",
                           choices = c("Yes" = "yes",
                                       "No" = "no"),
                           selected = "Yes"),
               
               actionButton("submit_ind_chars", "Go!"),
               textOutput("out_click_ind"),
               textOutput("text2"),
               shinycssloaders::withSpinner(
                 tableOutput("ind_pred")
               ))
      
    )
  )
)



# Define server logic
server <- function(input, output) {
  
  # Individual-level panel outputs
  output$text2 <- renderText({
    "According to the model, the predicted probability of a participants 
    with the characteristics you specified is as follows."
  })
  
  output$out_click_ind <- renderText(paste("Times of recalculation: ", 
                                           input$submit_ind_chars, sep = ""))
  
  output$ind_pred <- renderTable({
    
    # run only if the Go! button was clicked
    input$submit_ind_chars
    
    isolate({
      if (input$which_outcome_ind == "depression"){
        out = ind_depr %>%
          filter(sex_l == input$gender, 
                 children_l == input$children,
                 race_l == input$race,
                 age_l == input$age,
                 marital_status_l == input$marital_status,
                 education_l == input$education,
                 income_l == input$income,
                 employment_l == input$employment,
                 general_health_l == input$general_health,
                 exercise_l == input$exercise) %>% 
          select(predict_output) %>% 
          unnest(predict_output)
      } else if (input$which_outcome_ind == "mh_bin"){
        out = ind_mh_days %>%
          filter(sex_l == input$gender, 
                 children_l == input$children,
                 race_l == input$race,
                 age_l == input$age,
                 marital_status_l == input$marital_status,
                 education_l == input$education,
                 income_l == input$income,
                 employment_l == input$employment,
                 general_health_l == input$general_health,
                 exercise_l == input$exercise) %>% 
          select(predict_output) %>% 
          unnest(predict_output)
      }
      
    })
    
  })
  
  # Group-level panel outputs
  output$text1 <- renderText({
    "The predictive marginal means for the selected group factor is as follows."
  })
  
  output$out_click_group <- renderText(paste("Times of recalculation: ", 
                                             input$submit_group_factor, sep = ""))
  
  output$group_pred <- renderTable({
    # run only if the Go! button was clicked
    input$submit_group_factor
    
    isolate({
      if (input$which_outcome_group == "depression"){
        out = group_depr[[input$group_factor]]
      } else if (input$which_outcome_group == "mh_bin") {
        out = group_mh_days[[input$group_factor]]
      }
    })
    
  }, rownames = TRUE)
  
  
}

# Create Shiny app ----
shinyApp(ui = ui, server = server)



