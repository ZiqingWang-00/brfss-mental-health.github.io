# title: "BRFSS Import"
# author: "Ziqing Wang"
# date: "2022-11-07"

library(tidyverse)
#library(rio)
library(haven)
library(readr)
library(forcats)
library(srvyr)

# Import the raw XPT data file into RStudio. 
BRFSS2021 = read_xpt(
  './data/LLCP2021.XPT',
  col_select = NULL,
  skip = 0,
  n_max = Inf,
  .name_repair = "unique"
) %>% janitor::clean_names()

# preview the data
head(BRFSS2021)

# Reduce the data set size by selecting only variables of interest 
# and variables necessary for data analysis (e.g., weight variables). 
tidy_brfss21 = BRFSS2021 %>%
  select(psu, llcpwt, ststr, # survey weight variables
         menthlth, addepev3, # mental health outcomes
         sex, chldcnt, # exposures
         state, race, educa, income3, employ1, marital, ageg5yr, renthom1, # demographic covariates
         exerany2, genhlth) %>% # health-related covariates
  filter(!state %in% c(66, 72, 78), 
         !menthlth %in% c(77, 99), 
         !renthom1 %in% c(7, 9),
         !addepev3 %in% c(7, 9), !is.na(addepev3)) %>% # remove observations with missing outcome variables and remove observations from US territories
  mutate(mh_cat3 = case_when(menthlth == 88 ~ "none",
                             menthlth %in% seq(1, 15) ~ "<=15 days",
                             menthlth %in% seq(16, 30) ~ ">15 days"),
         mh_bin = case_when(menthlth %in% c(seq(1,15), 88)  ~ "<=15 days",
                            menthlth %in% seq(16, 30) ~ ">15 days"),
         days_bad_mental_health = recode(menthlth, 
                                         `88` = 0),
         depression = case_when(addepev3 == 1 ~ "yes",
                                addepev3 == 2 ~ "no"),
         sex = case_when(sex == 1 ~ "male",
                         sex == 2 ~ "female"),
         children = case_when(chldcnt == 1 ~ "none",
                              chldcnt %in% c(2, 3) ~ "one or two",
                              chldcnt %in% c(4, 5, 6) ~ "three or more"),
         race = case_when(race == 1 ~ "white",
                          race == 2 ~ "black",
                          race == 4 ~ "asian",
                          race == 8 ~ "hispanic",
                          race %in% c(3,5,6,7) ~ "other"),
         education = case_when(educa %in% c(1,2,3) ~ "less than high school",
                               educa %in% c(4,5) ~ "high school or some college",
                               educa == 6 ~ "bachelors or higher"),
         income = case_when(income3 %in% seq(1,5) ~ "<=35000",
                            income3 %in% c(6,7) ~ "35000-75000",
                            income3 %in% c(8,9,10) ~ ">75000"),
         employment = case_when(employ1 %in% c(1,2) ~ "employed",
                                employ1 %in% c(5,6,7) ~ "homemaker/student/retired",
                                employ1 %in% c(3,4,8) ~ "unemployed"),
         marital_status = case_when(marital == 1 ~ "married",
                                    marital %in% seq(2,6) ~ "not married"),
         homeownership = case_when(renthom1 == 1 ~ "own",
                                   renthom1 == 2 ~ "rent"),
         age = case_when(ageg5yr %in% c(1,2) ~ "18-29",
                         ageg5yr %in% c(3,4,5,6) ~ "30-49",
                         ageg5yr %in% c(7,8,9) ~ "50-64",
                         ageg5yr %in% c(10,11,12,13) ~ "65+"),
         general_health = case_when(genhlth == 1 ~ "excellent",
                                    genhlth %in% c(2,3) ~ "very good/good",
                                    genhlth %in% c(4,5) ~ "fair/poor"),
         exercise = case_when(exerany2 == 1 ~ "yes",
                              exerany2 == 2 ~ "no"),
         state_code = case_when(state == 1 ~ "AL",
                                state == 2 ~ "AK",
                                state == 4 ~ "AZ",
                                state == 5 ~ "AR",
                                state == 6 ~ "CA",
                                state == 8 ~ "CO",
                                state == 9 ~ "CT",
                                state == 10 ~ "DE",
                                state == 11 ~ "DC",
                                state == 13 ~ "GA",
                                state == 15 ~ "HI",
                                state == 16 ~ "ID",
                                state == 17 ~ "IL",
                                state == 18 ~ "IN",
                                state == 19 ~ "IA",
                                state == 20 ~ "KS",
                                state == 21 ~ "KY",
                                state == 22 ~ "LA",
                                state == 23 ~ "ME",
                                state == 24 ~ "MD",
                                state == 25 ~ "MA",
                                state == 26 ~ "MI",
                                state == 27 ~ "MN",
                                state == 28 ~ "MS",
                                state == 29 ~ "MO",
                                state == 30 ~ "MT",
                                state == 31 ~ "NE",
                                state == 32 ~ "NV",
                                state == 33 ~ "NH",
                                state == 34 ~ "NJ",
                                state == 35 ~ "NM",
                                state == 36 ~ "NY",
                                state == 37 ~ "NC",
                                state == 38 ~ "ND",
                                state == 39 ~ "OH",
                                state == 40 ~ "OK",
                                state == 41 ~ "OR",
                                state == 42 ~ "PA",
                                state == 44 ~ "RI",
                                state == 45 ~ "SC",
                                state == 46 ~ "SD",
                                state == 47 ~ "TN",
                                state == 48 ~ "TX",
                                state == 49 ~ "UT",
                                state == 50 ~ "VT",
                                state == 51 ~ "VA",
                                state == 53 ~ "WA",
                                state == 54 ~ "WV",
                                state == 55 ~ "WI",
                                state == 56 ~ "WY")) 

# Export the csv with reduced number of variables and observations.
write_csv(tidy_brfss21, "./data/cleaned_brfss21.csv")


# Recode variables to factors with comprehensible labels 
# and save the cleaned dataframe as an R object for faster loading and future reference
brfss21_data = read_csv("./data/cleaned_brfss21.csv") %>%
  mutate(mh_cat3 = factor(mh_cat3, levels = c("none", "<=15 days", ">15 days")),
         mh_bin = factor(mh_bin, levels = c("<=15 days", ">15 days")),
         depression = factor(depression, levels = c("no", "yes")),
         sex = factor(sex, levels = c("male", "female")),
         children = factor(children, levels = c("none", "one or two", "three or more")),
         state = factor(state),
         homeownership = factor(homeownership, levels = c("own", "rent")), 
         race = factor(race, levels = c("white", "black", "asian", "hispanic", "other")),
         marital_status = factor(marital_status, levels = c("married", "not married")),
         education = factor(education, levels = c("less than high school", "high school or some college", "bachelors or higher")),
         income = factor(income, levels = c("<=35000", "35000-75000", ">75000")),
         employment = factor(employment, levels = c("employed", "homemaker/student/retired", "unemployed")),
         age = factor(age, levels = c("18-29", "30-49", "50-64", "65+")),
         general_health = factor(general_health, levels = c("excellent", "very good/good", "fair/poor")),
         exercise = factor(exercise, levels = c("yes", "no")))

# save to R object
save(brfss21_data, file = "./data/brfss21_data.Rdata")

# Next, create survey design for the cleaned and recoded data 
# and save it as an R object for fast loading and future reference.
brfss_design = as_survey_design(brfss21_data, id = 1, strata = ststr, weight = llcpwt)
save(brfss_design, file = "./data/brfss_design.Rdata")

# Import BRFSS 2012 data for post-hoc analysis on mental health stigma by gender
# This data set contains variables on attitudes on mental illness and mental health service utilization
# Impor XPT file 
BRFSS2012 = read_xpt(
  './data/LLCP2012.XPT',
  col_select = NULL,
  skip = 0,
  n_max = Inf,
  .name_repair = "unique"
) %>% janitor::clean_names()

# preview the data
head(BRFSS2012)

# Select variables of interest
tidy_brfss12 = BRFSS2012 %>%
  select(psu, llcpwt, ststr, # survey weight variables
         mistmnt, # mental health service utilization
         mistrhlp, # stigma on mental health treatment
         misphlpf, # perception on public opinion of mental illness
         sex, addepev2, menthlth) %>% 
  filter(!mistmnt %in% c(7, 9), 
        !mistrhlp %in% c(7, 9),
        !misphlpf %in% c(7, 9)) %>% # keep observations that responded to mental health questions
  mutate(mh_treatment = case_when(mistmnt == 1 ~ "yes",
                                  mistmnt == 2 ~ "no"),
         mh_help_opinion = case_when(mistrhlp == 1 ~ "agree strongly",
                                     mistrhlp == 2 ~ "agree slightly",
                                     mistrhlp == 3 ~ "neither agree nor disagree",
                                     mistrhlp == 4 ~ "disagree slightly",
                                     mistrhlp == 5 ~ "disagree strongly"),
         mh_pub_perc_opinion = case_when(misphlpf == 1 ~ "agree strongly",
                                         misphlpf == 2 ~ "agree slightly",
                                         misphlpf == 3 ~ "neither agree nor disagree",
                                         misphlpf == 4 ~ "disagree slightly",
                                         misphlpf == 5 ~ "disagree strongly"),
         depression = case_when(addepev2 == 1 ~ "yes",
                                addepev2 == 2 ~ "no"),
         days_badmh = replace(menthlth, menthlth == 88, 0),
         sex = case_when(sex == 1 ~ "male",
                         sex == 2 ~ "female"))

# Export the above data frame to a csv file
write_csv(tidy_brfss12, "./data/cleaned_brfss12.csv")

# code variables in csv as factor
# create survey design object and save as an R object
brfss12_data = read_csv("./data/cleaned_brfss12.csv") %>%
  mutate(mh_treatment = factor(mh_treatment, levels = c("no", "yes")),
         mh_help_opinion = factor(mh_help_opinion, levels = c("agree strongly", 
                                                              "agree slightly",
                                                              "neither agree nor disagree",
                                                              "disagree slightly",
                                                              "disagree strongly")),
         mh_pub_perc_opinion = factor(mh_pub_perc_opinion, levels = c("agree strongly", 
                                                              "agree slightly",
                                                              "neither agree nor disagree",
                                                              "disagree slightly",
                                                              "disagree strongly")),
         deepression = factor(depression, levels = c("no", "yes")),
         sex = factor(sex, levels = c("male", "female")))

brfss_design2012 = as_survey_design(brfss12_data, id = 1, strata = ststr, weight = llcpwt)
save(brfss_design2012, file = "./data/brfss_design2012.Rdata")