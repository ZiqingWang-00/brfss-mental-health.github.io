# title: "Preparation for a faster shiny app"
# author: "Ziqing Wang"
# date: "2022-12-04"

# set up
library(tidyverse)
library(survey)
library(srvyr)
library(stringr)
library(magrittr)

# Set options for allowing a single observation per stratum 
options(survey.lonely.psu = "adjust")

load(file = "./data/brfss_design.Rdata")

# For individual-level calculations
## get the regression models
model_used_depr = svyglm(depression ~ sex + children + # exposures
                           race + age + marital_status + # demographic covariates
                           education + income + employment + # socioeconomic covariates
                           general_health + exercise, # health-related covariates
                         brfss_design,
                         family = quasibinomial())  

model_used_mh_days = svyglm(mh_bin ~ sex + children + # exposures
                              race + age + marital_status + # demographic covariates
                              education + income + employment + # socioeconomic covariates
                              general_health + exercise, # health-related covariates
                            brfss_design,
                            family = quasibinomial())  

## Create an expand grid that contains all combinations of factor levels
factor_grid = expand_grid(
  sex_l = levels(brfss_design$variables$sex),
  children_l = levels(brfss_design$variables$children),
  race_l = levels(brfss_design$variables$race),
  age_l = levels(brfss_design$variables$age),
  marital_status_l = levels(brfss_design$variables$marital_status),
  education_l = levels(brfss_design$variables$education),
  income_l = levels(brfss_design$variables$income),
  employment_l = levels(brfss_design$variables$employment),
  general_health_l = levels(brfss_design$variables$general_health),
  exercise_l = levels(brfss_design$variables$exercise)) 
factor_grid


## create a list of the data frames that can be fed into predict()
df_list = list()
for (row in 1:nrow(factor_grid)){
  df_list[[row]] = data.frame(sex = factor_grid[row, "sex_l"]$sex_l, 
                              children = factor_grid[row, "children_l"]$children_l, 
                              race = factor_grid[row, "race_l"]$race_l, 
                              age = factor_grid[row, "age_l"]$age_l, 
                              marital_status = factor_grid[row, "marital_status_l"]$marital_status_l, 
                              education = factor_grid[row, "education_l"]$education_l, 
                              income = factor_grid[row, "income_l"]$income_l, 
                              employment = factor_grid[row, "employment_l"]$employment_l, 
                              general_health = factor_grid[row, "general_health_l"]$general_health_l, 
                              exercise = factor_grid[row, "exercise_l"]$exercise_l)
}

df_list[[3]] # sanity check

## For depression outcome
predicted_probs_ind = map(.x = df_list, 
~predict(model_used_depr, .x, type = "response"))

predicted_probs_ind[[2]] # sanity check

### convert svystat objects to tibble
cleaned_predicted_probs_ind = map(
  .x = predicted_probs_ind, 
  ~tibble(Predicted_Probability = coef(.x)[[1]],
          SE = diag(vcov(.x))))

names(cleaned_predicted_probs_ind) = sprintf("output_%s",
                                             seq(1:length(predicted_probs_ind)))

cleaned_predicted_probs_ind_tibble = as_tibble(cleaned_predicted_probs_ind)

cleaned_predicted_probs_ind_tibble = cleaned_predicted_probs_ind_tibble %>% 
  pivot_longer(cols = everything(),
               values_to = "predict_output") %>% 
  bind_cols(., factor_grid)

cleaned_predicted_probs_ind_tibble

### save as r object to be used in shiny app
saveRDS(cleaned_predicted_probs_ind_tibble, file = "./Shiny/shiny_ind_data.rds")


## Repeat the same code as above for the days of bad mental health outcome
predicted_probs_ind2 = map(.x = df_list, 
                           ~predict(model_used_mh_days, .x, type = "response"))

predicted_probs_ind2[[2]]

cleaned_predicted_probs_ind2 = map(
  .x = predicted_probs_ind2, 
  ~tibble(Predicted_Probability = coef(.x)[[1]],
          SE = diag(vcov(.x))))

names(cleaned_predicted_probs_ind2) = sprintf("output_%s",
                                              seq(1:length(predicted_probs_ind2)))

cleaned_predicted_probs_ind_tibble2 = as_tibble(cleaned_predicted_probs_ind2)

cleaned_predicted_probs_ind_tibble2 = cleaned_predicted_probs_ind_tibble2 %>% 
  pivot_longer(cols = everything(),
               values_to = "predict_output") %>% 
  bind_cols(., factor_grid)

cleaned_predicted_probs_ind_tibble2

### save as r object
saveRDS(cleaned_predicted_probs_ind_tibble2, file = "./Shiny/shiny_ind_data2.rds")

# For group-level calculations
## define function for future use
get_formula = function(which_outcome, group_factor){
  
  var_names = c("sex", "children", "race", "age", "marital_status",
                "education", "income", "employment", "general_health",
                "exercise")
  
  var_removed = var_names[!var_names == group_factor]
  
  if (which_outcome == "depression"){
    formula = as.formula(paste("depression ~ ", paste(var_removed, collapse ="+"), sep = " "))
    return(formula)
  } else if (which_outcome == "mh_bin"){
    formula = as.formula(paste("mh_bin ~ ", paste(var_removed, collapse ="+"), sep = " "))
    return(formula)
  }
}

## use the function above to create all formulas used in svypredmean()
all_predictors = c(sex = "sex", children = "children", race = "race", age = "age", marital_status = "marital_status", education = "education", income = "income", employment = "employment", general_health = "general_health", exercise = "exercise")

formulas = map(.x = all_predictors,
               ~get_formula("depression", .x))

formulas

## For depression
### obtain svystat objects 
pred_marg_means = map2(.x = formulas, .y = all_predictors,
                       ~svypredmeans(svyglm(.x,
                                            brfss_design,
                                            family = quasibinomial()), 
                                     as.formula(paste("~", .y)),
                       ))
pred_marg_means        

### make svystat objects to more useable form
clean_svypredmean_output = function(pred_marg_means){
  no_rownames = rbind(coef(pred_marg_means), sqrt(diag(vcov(pred_marg_means))))
  rownames(no_rownames) = c("Predicted Probability", "SE")
  return(no_rownames)
}

cleaned_pred_marg_means = map(
  .x = pred_marg_means, 
  ~clean_svypredmean_output(.x)) 

cleaned_pred_marg_means

### save for shiny app
saveRDS(cleaned_pred_marg_means, file = "./Shiny/shiny_group_data.rds")


## For days of bad mental health
### create the set of formulas
formulas2 = map(.x = all_predictors,
                ~get_formula("mh_bin", .x))

formulas2

### obtain the svystat objects
pred_marg_means2 = map2(.x = formulas2, .y = all_predictors,
                        ~svypredmeans(svyglm(.x,
                                             brfss_design,
                                             family = quasibinomial()), 
                                      as.formula(paste("~", .y)),
                        ))
pred_marg_means2      

### convert svystat objects to more useable forms
cleaned_pred_marg_means2 = map(
  .x = pred_marg_means2, 
  ~clean_svypredmean_output(.x)) 

cleaned_pred_marg_means2

### save as rds object to be loaded in shiny app
saveRDS(cleaned_pred_marg_means2, file = "./Shiny/shiny_group_data2.rds")
















