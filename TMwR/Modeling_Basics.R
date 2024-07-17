# Tidy Modeling with R (Desktop Version) 
# Modeling Basics
library(tidymodels)
tidymodels_prefer()
data(ames)

# Data --------------------------------------------------------------------
ames
glimpse(ames)


# EDA 
# ggplot()
#

ames <- ames |> mutate(Sale_Price = log10(Sale_Price))


# Data splitting 
set.seed(502)
ames_split <- initial_split(ames, prop = 0.8, strata = Sale_Price)
ames_split 

ames_train <- training(ames_split)
ames_test <- testing(ames_split)


# Model Fitting -----------------------------------------------------------
# Create model: 
# Linear regression 
linear_reg() |> set_engine('lm') |> translate() # To see how parsnip maps code to package syntax 

lm_model <- linear_reg() |> set_engine('lm')
lm_form_fit <- lm_model |> fit(Sale_Price ~ Longitude + Latitude, data = ames_train)
lm_form_fit  

# Random forest 
rand_forest(trees = 1000, min_n = 5) |> # Main args set here 
  set_engine('ranger', verbose = T) |> # Engine args set here 
  set_mode('regression') |> 
  translate()

# Use results: 
lm_form_fit |> 
  extract_fit_engine() |> # Takes fitted model element from parsnip model object
  vcov() # Which can then be the target of other methods 

model_res <- lm_form_fit |> 
  extract_fit_engine() |> 
  summary() # Returns fit results 
class(model_res)
model_res

param_est <- coef(model_res) # Model coefficient table 
class(param_est)
param_est 

tidy(lm_form_fit) # Converts model object to a tibble 

# Make predictions:
ames_test_small <- ames_test |> slice(1:5)
predict(lm_form_fit, new_data = ames_test_small)

# Compare prediction to true values (w/ 95% confidence interval)
ames_test_small |>  select(Sale_Price) |> 
  bind_cols(predict(lm_form_fit, new_data = ames_test_small)) |> 
  bind_cols(predict(lm_form_fit, ames_test_small, type = 'pred_int'))

# Constant syntax across models - e.g., decision tree 
# tree_model <- decision_tree()
# 


# parsnip_addin() helps with model specs 


# Model Workflow ----------------------------------------------------------
lm_wflow <- workflow() |> 
  add_model(lm_model) |>  # Workflow requires parsnip object 
  add_formula(Sale_Price ~ Longitude + Latitude) # Set formula as pre-processor 
lm_wflow

lm_fit <- fit(lm_wflow, ames_train) # Fit model from workflow 
lm_fit

# Make predictions, change pre-processing, etc. 
predict(lm_fit, ames_test |> slice_head(n = 3)) 
lm_fit |> update_formula(Sale_Price ~ Longitude)

# Incorporating raw variables  
lm_wflow <- lm_wflow |> 
  remove_formula() |> 
  add_variables(outcomes = Sale_Price, predictors = c(Longitude, Latitude))
  # Could also use general helper functions since outcomes automatically dropped from predictors
lm_wflow 

fit(lm_wflow, ames_train) # So, add formula or variables as pre-processor depending on chosen model specs 

# Understanding workflow() formula processing -- considers model-specific formulas 
# Emulates how underlying model would treat formula whenever possible 
# Sometimes have special cases (e.g., lme4 and random effects)
library(nlme)
library(multilevelmod) # Parsnip extension 
tidymodels_prefer()

multilevel_spec <- linear_reg() %>% set_engine("lmer")

multilevel_wflow <- workflow() |> 
  add_variables(outcome = distance, predictors = c(Sex, age, Subject)) |> 
  #Specify model formula given column names from add_variables()
  add_model(multilevel_spec, formula = distance ~ Sex + (age | Subject)) 

multilevel_fit <- fit(multilevel_wflow, data = Orthodont)
multilevel_fit

# Would also use model-specific formula for survival analysis 
# parametric_spec <- survival reg() 
# parametric_wflow <- 
# 

# Creating multiple workflows via workflowsets (e.g., for easily testing multiple options)
# More thorough consideration in 15: Model Screening
location <- list(
  longitude = Sale_Price ~ Longitude,
  latitude = Sale_Price ~ Latitude,
  coords = Sale_Price ~ Longitude + Latitude,
  neighborhood = Sale_Price ~ Neighborhood
)

location_models <- workflow_set(preproc = location, models = list(lm = lm_model))
location_models # Can see workflow ID to pick out with extract()
extract_workflow(location_models, id = 'coords_lm')

location_models <- location_models |> mutate(fit = map(info, ~ fit(.x$workflow[[1]], ames_train)))
location_models
location_models$fit[[1]]

# Test set evaluation using last_fit() -- see 16: Dimensionality Reduction 
final_lm_res <- last_fit(lm_wflow, ames_split) # Convenience wrapper for entire dataset 
final_lm_res 

# Fitted workflow in .workflow, also have metrics and predictions -- use extract() or collect()
extract_workflow(final_lm_res)
collect_metrics(final_lm_res)
collect_predictions(final_lm_res)


# Feature Engineering -----------------------------------------------------


