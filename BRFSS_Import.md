BRFSS Import
================
Ziqing Wang
2022-11-07

Import the raw XPT data file into RStudio.

``` r
BRFSS2021 = read_xpt(
  './data/LLCP2021.XPT',
  col_select = NULL,
  skip = 0,
  n_max = Inf,
  .name_repair = "unique"
) %>% janitor::clean_names()
```

Preview the data.

``` r
head(BRFSS2021)
```

    ## # A tibble: 6 × 303
    ##   state fmonth idate    imonth iday  iyear dispcode seqno    psu ctele…¹ pvtre…²
    ##   <dbl>  <dbl> <chr>    <chr>  <chr> <chr>    <dbl> <chr>  <dbl>   <dbl>   <dbl>
    ## 1     1      1 01192021 01     19    2021      1100 2021… 2.02e9       1       1
    ## 2     1      1 01212021 01     21    2021      1100 2021… 2.02e9       1       1
    ## 3     1      1 01212021 01     21    2021      1100 2021… 2.02e9       1       1
    ## 4     1      1 01172021 01     17    2021      1100 2021… 2.02e9       1       1
    ## 5     1      1 01152021 01     15    2021      1100 2021… 2.02e9       1       1
    ## 6     1      1 01142021 01     14    2021      1100 2021… 2.02e9       1       1
    ## # … with 292 more variables: colghous <dbl>, statere1 <dbl>, celphon1 <dbl>,
    ## #   ladult1 <dbl>, colgsex <dbl>, numadult <dbl>, landsex <dbl>, nummen <dbl>,
    ## #   numwomen <dbl>, respslct <dbl>, safetime <dbl>, ctelnum1 <dbl>,
    ## #   cellfon5 <dbl>, cadult1 <dbl>, cellsex <dbl>, pvtresd3 <dbl>,
    ## #   cclghous <dbl>, cstate1 <dbl>, landline <dbl>, hhadult <dbl>, sexvar <dbl>,
    ## #   genhlth <dbl>, physhlth <dbl>, menthlth <dbl>, poorhlth <dbl>,
    ## #   priminsr <dbl>, persdoc3 <dbl>, medcost1 <dbl>, checkup1 <dbl>, …

Reduce the data set size by selecting only variables of interest and
variables necessary for data analysis (e.g., weight variables).

``` r
tidy_brfss21 = BRFSS2021 %>%
  select(psu, llcpwt, ststr, # survey weight variables
         menthlth, addepev3, # mental health outcomes
         sex, chldcnt, # exposures
         state, race, educa, income3, employ1, marital, ageg5yr, # demographic covariates
         exerany2, genhlth) %>% # health-related covariates
  filter(!state %in% c(66, 72, 78), 
         !menthlth %in% c(77, 99), 
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
```

Export the csv with reduced number of variables and observations.

``` r
write_csv(tidy_brfss21, "./data/cleaned_brfss21.csv")
```