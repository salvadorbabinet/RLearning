# Tidy Modeling with R 
# Beyond the Basics 
library(tidymodels)
tidymodels_prefer()

# 16: Dimensionality Reduction 
library(beans)
library(corrplot)
library(bestNormalize) # To determine best way to normalize data 
library(patchwork) # For combining multiple ggplots 
library(ggforce) # Extends ggplot; useful feature extraction visualizations 

# Split data 
# Initial split 
set.seed(1601)
bean_split <- initial_validation_split(beans, strata = class, prop = c(0.75, 0.125))
bean_split

bean_train <- training(bean_split)
bean_test <- testing(bean_split)
bean_validation <- validation(bean_split)

# rset object for tune functions 
bean_val <- validation_set(bean_split)
bean_val$splits

# Data investigation 
# Return function for new color palette from given colors 
tmwr_cols <- colorRampPalette(c("#91CBD765", "#CA225E")) 

# Correlation structure 
bean_train |> 
  select(-class) |> 
  cor() |> 
  corrplot(col = tmwr_cols(200), tl.col = "black", method = "ellipse")

# Starting recipe 
# Normalize distributions 
beans_rec <- recipe(class ~ ., data = bean_train) |> 
  step_zv(all_numeric_predictors()) |> # Remove predictors with no variance (i.e., only one entry)
  step_orderNorm(all_numeric_predictors()) |> # bestNormalize ORQ function 
  step_normalize(all_numeric_predictors()) # Scales and centers data 
beans_rec

# Training: recipe prep() is analogous to modeling fit() 
# Calculate statistics from training set 
beans_rec_trained <- prep(beans_rec) # retain; verbose / log_changes args can be useful
beans_rec_trained  

# Processing: recipe bake() is analogous to modeling predict()
# Apply pre-processing (based on training set) to new data set 
beans_val_processed <- bake(beans_rec_trained, new_data = bean_validation)
beans_val_processed

# Compare processed to unprocessed 
p1 <- bean_validation |> 
  ggplot(aes(x = area)) + 
  geom_histogram(bins = 50, color = 'white') + 
  ggtitle('Original validation set') + 
  theme_bw()

p2 <- beans_val_processed |> 
  ggplot(aes(x = area)) +
  geom_histogram(bins = 50, color = 'white') +
  ggtitle('Processed validation set') +
  theme_bw()

p1 + p2 # Using patchwork  

# Feature extraction 
# Estimate transformation and visualize via scatter plot matrix 
plot_validation_results <- function(recipe, dat = bean_validation) {
  recipe |> 
    prep() |> # Estimate parameters 
    bake(new_data = dat) |> # Process data (with validation set unless specified)
    ggplot(aes(x = .panel_x, y = .panel_y, color = class, fill = class)) +
    geom_point(alpha = 0.5, size = 0.5) +
    geom_autodensity(alpha = 0.3) +
    facet_matrix(vars(-class), layer.diag = 2) +
    scale_color_brewer(palette = 'Dark2') + 
    scale_fill_brewer(palette = 'Dark2')
}
  
bean_rec_trained |> 
  step_pca(all_numeric_predictors(), num_comp = 4) |> 
  plot_validation_results() + 
  ggtitle("Principal Component Analysis")

# Investigating PCA further 
beans_pca_estimates <- beans_rec_trained |> 
  step_pca(all_numeric_predictors(), num_comp = 4) |> # Uses stats::prcomp() 
  prep() # Generates PCA info within pca step of recipe object 

pca_extract <- beans_pca_estimates$steps[[4]] |> # Subset PCA step 
  tidy() |> 
  mutate(component = parse_number(component))
  
pca_extract |> 
  group_by(component) |> 
  slice_max(abs(value), n = 3) # Print loadings -- equiv. to deprecated learntidymodels function 

