#author: Ziqing Wang
#date: "12/04/2022"

# Import packages
library(tidyverse)
library(shiny)
library(shinythemes)
library(rsconnect)

# Setup: Import data sets
ind_depr = readRDS("shiny_ind_data.rds") # outcome: depression
ind_mh_days = readRDS("shiny_ind_data2.rds") # outcome: days of bad mental health
group_depr = readRDS("shiny_group_data.rds") # outcome: depression
group_mh_days = readRDS("shiny_group_data2.rds") # outcome: days of bad mental health

# Define UI for app 
ui <- fluidPage(
  theme = shinytheme("cosmo"),
  
  # Dashboard title
  titlePanel(title = 'Alternative Ways to Demonstrate the Model Fitting Results'),
  
  # Set tabs
  mainPanel(
    tabsetPanel(
      tabPanel("Group-level: Predictive Marginal Means", 
               
               fluidRow(
                 column(5, 
                        # User-specified factor levels for individual-level predicted probability calculation
                        selectInput(inputId = "which_outcome_group", 
                                    label = strong("Outcome of interest"), 
                                    choices = c("Self-reported past depression"  = "depression",
                                                ">15 days of bad mental health in the past 30 days" = "mh_bin"),
                                    selected = "Self-reported depression diagnosis"),
                        
                        selectInput(inputId = "group_factor",
                                    label = "The factor for which predictive marginal means are wanted",
                                    choices = c("Gender" = "sex",
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
                        textOutput("out_click_group")
                      ),
                 
                 column(7, 
                        textOutput("pred_marg_intro"),
                        tags$style(type="text/css", "#pred_marg_intro {white-space: pre-wrap;}"),
                        tableOutput("group_pred")
                        )
                      )
               ),
               
               
      tabPanel("Individual-level: Predicted Probabilities", 
               # User-specified factor levels for individual-level predicted probability calculation
               fluidRow(
                 column(5, 
                        selectInput(inputId = "which_outcome_ind", 
                                    label = strong("Outcome of interest"), 
                                    choices = c("Self-reported past depression"  = "depression",
                                                ">15 days of bad mental health in the past 30 days" = "mh_bin"),
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
                        textOutput("out_click_ind")
                        ),
                 
                 fluidRow(column(6,
                                 textOutput("text2"),
                                 tags$style(type="text/css", "#text2 {white-space: pre-wrap;}"),
                                 tableOutput("ind_pred")             
                                 
                 ))
               )
    )
  )
))



# Define server logic
server <- function(input, output) {
  
  # Individual-level panel outputs
  output$text2 <- renderText({
    "According to the author of the survey package, the predict() function in R is compatible with the svyglm objects, i.e., the survey-weighted fitted logistic regression models we obtained from the statistical analysis.
    
Since we included 10 categorical variables in our model, there are hundreds of thousands of possible combinations of levels.
    
For these two reasons, we let you select the levels of categorical variables you are interested in. We then use the predict() function to calculate the predicted probability of the selected mental health outcome of a person who has the levels of categorical variables you specified, using our model. 

The predicted probability and its standard error are displayed below:
    
    "
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
    
  }, digits = 4)
  
  # Group-level panel outputs
  
  output$pred_marg_intro <- renderText({
    "According to the author of the survey package, the predictive marginal mean for one level of a factor is the probability-weighted average of the fitted values for the model on new data where all the observations are set to that level of the factor but have whatever values of adjustment variables they really have.
    
In our case where the outcome variable is binary and was fitted by a logistic regression model, this estimate shows the effect of a categorical variable on the predicted probability of the response variable, adjusting for all other covariates included in the model. This better demonstrates the magnitude of the variable's effect than the odds.
    
The predictive marginal means for levels of the selected group factor and the corresponding standard errors are as follows.
    
"
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
    
  }, rownames = TRUE, digits = 4)
  
  
}

# Create Shiny app ----
shinyApp(ui = ui, server = server)


