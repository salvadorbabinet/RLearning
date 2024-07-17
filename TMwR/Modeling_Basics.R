# Tidy Modeling with R (Desktop Version) 
# Modeling Basics 
library(tidymodels)
tidymodels_prefer()
data(ames)

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
linear_reg() |> set_engine('lm') |> translate() # To see parsnip code mapping to package syntax 

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

