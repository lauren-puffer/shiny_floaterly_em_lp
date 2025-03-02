# Regression 
library(tidyverse)
library(tidymodels)
library(here)
library(janitor)

set.seed(123)  # You can use any integer (e.g., 123)

# Read and clean data
t_df <- read_csv(here("data", "Regression_data.csv")) %>%
  clean_names() %>%
  mutate(safety = tolower(safety)) %>%  # Convert to lowercase first
  mutate(safety = as.factor(safety))    # Then convert to a factor

# Split data
safety_split <- initial_split(t_df, prop = 0.80, strata = safety)
safety_train_df <- training(safety_split) %>%
  mutate(safety = as.factor(safety))  # Ensure factor in training set
safety_test_df <- testing(safety_split) %>%
  mutate(safety = as.factor(safety))  # Ensure factor in test set

# Build Model
log_md <- logistic_reg() %>%
  set_engine("glm")

# Make recipe
glm_rec <- recipe(safety ~ velocity_ft_s, data = safety_train_df)

# Train Logistic Regression
log_wf <- workflow() %>%
  add_recipe(glm_rec) %>%
  add_model(log_md)

log_fit <- log_wf %>%
  fit(safety_train_df)

# Make predictions
log_test <- safety_test_df %>%
  mutate(.pred_class = predict(log_fit, new_data = safety_test_df)$.pred_class)

# Ensure predictions are factors
log_test <- log_test %>%
  mutate(.pred_class = as.factor(.pred_class))

# Evaluate performance
log_test %>%
  accuracy(truth = safety, estimate = .pred_class)

#Doesnt work
#log_test |> 
  #yardstick::roc_auc(truth = safety, .pred_0)

folds<-vfold_cv(safety_train_df, v=10, strata = safety)

log_fit_folds<- log_fit |> 
  fit_resamples(folds)

collect_metrics(log_fit_folds)

log_fit |> 
  extract_fit_parsnip()|> 
  tidy()

final_log<-log_wf |>
  last_fit(safety_split)

final_log |>
  extract_fit_parsnip() |>
  tidy() |> 
  mutate(odds=exp(estimate),
         prob=odds/(1+estimate))


#0 is safe, 1 is unsafe

# Define a function to predict safety based on the model output
predict_safety <- function(velocity_ft_s) {
  # Model coefficients
  intercept <- -18.1
  velocity_coef <- 8.35
  
  # Calculate the log-odds (linear predictor)
  log_odds <- intercept + velocity_coef * velocity_ft_s
  
  # Convert log-odds to probability using the logistic function
  prob <- 1 / (1 + exp(-log_odds))
  
  # Predict safety: if prob > 0.5, predict unsafe (1), else safe (0)
  if (prob > 0.5) {
    prediction <- 1
    message <- "According to our model the water is not safe to swim in."
  } else {
    prediction <- 0
    message <- "According to our model the water is safe to swim in."
  }
  
  # Return the prediction and message
  return(list(prediction = prediction, message = message, probability = prob))
}

# Example: Predict safety for a given value of velocity_ft_s
velocity_input <- 2  # Replace with any value of velocity_ft_s
result <- predict_safety(velocity_input)

# Print the result
cat(result$message, "\n")

