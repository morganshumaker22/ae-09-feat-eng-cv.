The Office
================
Mine Çetinkaya-Rundel

``` r
library(tidyverse)
library(tidymodels)
library(schrute)
library(lubridate)
```

Use `theoffice` data from the
[**schrute**](https://bradlindblad.github.io/schrute/) package to
predict IMDB scores for episodes of The Office.

``` r
glimpse(theoffice)
```

    ## Rows: 55,130
    ## Columns: 12
    ## $ index            <int> 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16…
    ## $ season           <int> 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,…
    ## $ episode          <int> 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,…
    ## $ episode_name     <chr> "Pilot", "Pilot", "Pilot", "Pilot", "Pilot", "Pilot",…
    ## $ director         <chr> "Ken Kwapis", "Ken Kwapis", "Ken Kwapis", "Ken Kwapis…
    ## $ writer           <chr> "Ricky Gervais;Stephen Merchant;Greg Daniels", "Ricky…
    ## $ character        <chr> "Michael", "Jim", "Michael", "Jim", "Michael", "Micha…
    ## $ text             <chr> "All right Jim. Your quarterlies look very good. How …
    ## $ text_w_direction <chr> "All right Jim. Your quarterlies look very good. How …
    ## $ imdb_rating      <dbl> 7.6, 7.6, 7.6, 7.6, 7.6, 7.6, 7.6, 7.6, 7.6, 7.6, 7.6…
    ## $ total_votes      <int> 3706, 3706, 3706, 3706, 3706, 3706, 3706, 3706, 3706,…
    ## $ air_date         <fct> 2005-03-24, 2005-03-24, 2005-03-24, 2005-03-24, 2005-…

Fix `air_date` for later use.

``` r
theoffice <- theoffice %>%
  mutate(air_date = ymd(as.character(air_date)))
```

We will

-   engineer features based on episode scripts
-   train a model
-   perform cross validation
-   make predictions

Note: The episodes listed in `theoffice` don’t match the ones listed in
the data we used in the [cross validation
lesson](https://ids-s1-20.github.io/slides/week-10/w10-d02-cross-validation/w10-d02-cross-validation.html).

``` r
theoffice %>%
  distinct(season, episode)
```

    ## # A tibble: 186 × 2
    ##    season episode
    ##     <int>   <int>
    ##  1      1       1
    ##  2      1       2
    ##  3      1       3
    ##  4      1       4
    ##  5      1       5
    ##  6      1       6
    ##  7      2       1
    ##  8      2       2
    ##  9      2       3
    ## 10      2       4
    ## # … with 176 more rows
    ## # ℹ Use `print(n = ...)` to see more rows

### Exercise 1 - Calculate the percentage of lines spoken by Jim, Pam, Michael, and Dwight for each episode of The Office.

``` r
officelines <- theoffice %>% 
  group_by(season, episode) %>% 
  mutate(n_lines = n(), 
         lines_jim = sum(character == "Jim")/n_lines,
         lines_pam = sum(character == "Pam")/n_lines,
         lines_michael = sum(character == "Michael")/n_lines,
         lines_dwight = sum(character == "Dwight")/n_lines) %>% 
  ungroup() %>% 
  select(season, episode, episode_name, contains("lines")) %>% 
  distinct(season, episode, episode_name, .keep_all = TRUE) %>% 
  print()
```

    ## # A tibble: 186 × 8
    ##    season episode episode_name      n_lines lines_jim lines_pam lines_…¹ lines…²
    ##     <int>   <int> <chr>               <int>     <dbl>     <dbl>    <dbl>   <dbl>
    ##  1      1       1 Pilot                 229    0.157     0.179     0.354  0.127 
    ##  2      1       2 Diversity Day         203    0.123     0.0591    0.369  0.0837
    ##  3      1       3 Health Care           244    0.172     0.131     0.230  0.254 
    ##  4      1       4 The Alliance          243    0.202     0.0905    0.280  0.193 
    ##  5      1       5 Basketball            230    0.0913    0.0609    0.452  0.109 
    ##  6      1       6 Hot Girl              346    0.159     0.130     0.306  0.0809
    ##  7      2       1 The Dundies           256    0.125     0.160     0.375  0.125 
    ##  8      2       2 Sexual Harassment     283    0.0565    0.0954    0.353  0.0389
    ##  9      2       3 Office Olympics       281    0.196     0.117     0.295  0.196 
    ## 10      2       4 The Fire              319    0.160     0.0690    0.216  0.204 
    ## # … with 176 more rows, and abbreviated variable names ¹​lines_michael,
    ## #   ²​lines_dwight
    ## # ℹ Use `print(n = ...)` to see more rows

### Exercise 2 - Identify episodes that touch on Halloween, Valentine’s Day, and Christmas.

``` r
theoffice <- theoffice %>% 
  mutate(text = tolower(text))

halloween_episodes <- theoffice %>% 
  filter(str_detect(text, "halloween")) %>% 
  count(episode_name) %>% 
  filter(n > 1) %>% 
  mutate(halloween = 1) %>% 
  select(-n)
```

``` r
valentine_episodes <- theoffice %>% 
  filter(str_detect(text, "valentine")) %>% 
  count(episode_name) %>% 
  filter(n > 1) %>% 
  mutate(valentine = 1) %>% 
  select(-n)
```

``` r
xmas_episodes <- theoffice %>% 
  filter(str_detect(text, "christmas")) %>% 
  count(episode_name) %>% 
  filter(n > 1) %>% 
  mutate(christmas = 1) %>% 
  select(-n)
```

### Exercise 3 - Put together a modeling dataset that includes features you’ve engineered. Also add an indicator variable called `michael` which takes the value `1` if Michael Scott (Steve Carrell) was there, and `0` if not. Note: Michael Scott (Steve Carrell) left the show at the end of Season 7.

``` r
office_df <- theoffice %>% 
  select(season, episode, episode_name, imdb_rating, total_votes, air_date) %>% 
  distinct(season, episode, .keep_all = TRUE) %>% 
  left_join(halloween_episodes, by = "episode_name") %>% 
  left_join(valentine_episodes, by = "episode_name") %>% 
  left_join(xmas_episodes, by = "episode_name") %>% 
  replace_na(list(halloween = 0, valentine = 0, christmas = 0)) %>% 
  mutate(michael = if_else(season > 7, 0, 1)) %>% 
  mutate(across(halloween:michael, as.factor)) %>% 
  left_join(officelines, by = c("season", "episode", "episode_name"))
```

### Exercise 4 - Split the data into training (75%) and testing (25%).

``` r
set.seed(1122)

office_split <- initial_split(office_df)
office_train <- training(office_split)
office_test <- testing(office_split)
```

### Exercise 5 - Specify a linear regression model.

``` r
office_mod <- linear_reg() %>% 
  set_engine("lm")
```

### Exercise 6 - Create a recipe that updates the role of `episode_name` to not be a predictor, removes `air_date` as a predictor, uses `season` as a factor, and removes all zero variance predictors.

``` r
office_rec <- recipe(imdb_rating ~ ., data = office_train) %>% 
  update_role(episode_name, new_role = "id") %>% 
  step_rm(air_date) %>% 
  step_dummy(all_nominal(), -episode_name) %>% 
  step_zv(all_predictors())
```

### Exercise 7 - Build a workflow for fitting the model specified earlier and using the recipe you developed to preprocess the data.

``` r
office_wflow <- workflow() %>% 
  add_model(office_mod) %>% 
  add_recipe(office_rec) 
```

### Exercise 8 - Fit the model to training data and interpret a couple of the slope coefficients.

``` r
office_fit <- office_wflow %>% 
  fit(data = office_train)

tidy(office_fit)
```

    ## # A tibble: 13 × 5
    ##    term           estimate std.error statistic  p.value
    ##    <chr>             <dbl>     <dbl>     <dbl>    <dbl>
    ##  1 (Intercept)    6.24     0.289       21.6    3.93e-44
    ##  2 season         0.0382   0.0221       1.73   8.67e- 2
    ##  3 episode        0.0122   0.00423      2.89   4.56e- 3
    ##  4 total_votes    0.000330 0.0000397    8.31   1.31e-13
    ##  5 n_lines        0.00127  0.000389     3.27   1.37e- 3
    ##  6 lines_jim      0.323    0.662        0.487  6.27e- 1
    ##  7 lines_pam     -0.227    0.675       -0.336  7.38e- 1
    ##  8 lines_michael  0.305    0.527        0.579  5.64e- 1
    ##  9 lines_dwight   0.846    0.503        1.68   9.51e- 2
    ## 10 halloween_X1   0.00586  0.174        0.0336 9.73e- 1
    ## 11 valentine_X1  -0.0499   0.173       -0.288  7.74e- 1
    ## 12 christmas_X1   0.243    0.125        1.95   5.37e- 2
    ## 13 michael_X1     0.479    0.139        3.44   8.00e- 4

### Exercise 9 - Perform 5-fold cross validation and view model performance metrics.

``` r
set.seed(345)
folds <- vfold_cv(office_train, v = 5)
folds
```

    ## #  5-fold cross-validation 
    ## # A tibble: 5 × 2
    ##   splits           id   
    ##   <list>           <chr>
    ## 1 <split [111/28]> Fold1
    ## 2 <split [111/28]> Fold2
    ## 3 <split [111/28]> Fold3
    ## 4 <split [111/28]> Fold4
    ## 5 <split [112/27]> Fold5

``` r
set.seed(456)
office_fit_rs <- office_wflow %>%
  fit_resamples(folds)

collect_metrics(office_fit_rs)
```

    ## # A tibble: 2 × 6
    ##   .metric .estimator  mean     n std_err .config             
    ##   <chr>   <chr>      <dbl> <int>   <dbl> <chr>               
    ## 1 rmse    standard   0.359     5  0.0455 Preprocessor1_Model1
    ## 2 rsq     standard   0.565     5  0.0399 Preprocessor1_Model1

### Exercise 10 - Use your model to make predictions for the testing data and calculate the RMSE. Also use the model developed in the [cross validation lesson](https://ids-s1-20.github.io/slides/week-10/w10-d02-cross-validation/w10-d02-cross-validation.html) to make predictions for the testing data and calculate the RMSE as well. Which model did a better job in predicting IMDB scores for the testing data?

#### New model

``` r
office_test_pred <- predict(office_fit, new_data = office_test) %>% 
  bind_cols(office_test %>% select(imdb_rating, episode_name))

rmse(office_test_pred, truth = imdb_rating, estimate = .pred)
```

    ## # A tibble: 1 × 3
    ##   .metric .estimator .estimate
    ##   <chr>   <chr>          <dbl>
    ## 1 rmse    standard       0.386

#### Old model

``` r
office_mod_old <- linear_reg() %>%
  set_engine("lm")

office_rec_old <- recipe(imdb_rating ~ season + episode + total_votes + air_date, data = office_train) %>%
  # extract month of air_date
  step_date(air_date, features = "month") %>%
  step_rm(air_date) %>%
  # make dummy variables of month 
  step_dummy(contains("month")) %>%
  # remove zero variance predictors
  step_zv(all_predictors())

office_wflow_old <- workflow() %>%
  add_model(office_mod_old) %>%
  add_recipe(office_rec_old)

office_fit_old <- office_wflow_old %>%
  fit(data = office_train)

tidy(office_fit_old)

___
```

    ## Error: <text>:22:1: unexpected input
    ## 21: 
    ## 22: _
    ##     ^
