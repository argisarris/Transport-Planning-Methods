# Task 2.3: Mode Choice Modeling with Apollo
# Author: Group A
# Date: Fall 2025
#
# This script estimates mode choice models using the apollo package
# Following requirements from Assignment TPM 25_PART1

# =============================================================================
# SETUP
# =============================================================================

# Install packages if needed (comment out after first run)
# install.packages("apollo")
# install.packages("tidyverse")

# Load libraries
library(apollo)
library(tidyverse)

# Define paths
path_data <- "C:/Users/ETH/Documents/GitHub/Transport-Planning-Methods/Task_2.3/Data/"
path_task22 <- "C:/Users/ETH/Documents/GitHub/Transport-Planning-Methods/Task_2.2/"
path_output <- "C:/Users/ETH/Documents/GitHub/Transport-Planning-Methods/Task_2.3/Output/"

# Create output directory if it doesn't exist
if (!dir.exists(path_output)) {
  dir.create(path_output, recursive = TRUE)
}

# =============================================================================
# 1. DATA PREPARATION
# =============================================================================

cat("\n=== 1. Loading and Preparing Data ===\n")

# Load the mode choice data
load(paste0(path_data, "ModeChoice_mzmv_tripsWAlternatives.Rda"))

# Convert CHOICE codes to verbal labels for validation
dat$chosen_mode <- factor(dat$CHOICE, levels = 1:4,
                          labels = c('car', 'pt', 'bike', 'walk'))

# Check data structure
cat("Total observations:", nrow(dat), "\n")
cat("Mode choice distribution:\n")
print(table(dat$chosen_mode))

# Create unique trip ID if not exists
if (!"id" %in% names(dat)) {
  dat$id <- 1:nrow(dat)
}

# Split data into 80% training and 20% test set
set.seed(123)  # For reproducibility
train_index <- sample(seq_len(nrow(dat)), size = 0.8 * nrow(dat))
estimate_set <- dat[train_index, ]
test_set <- dat[-train_index, ]

# Order by household and trip number if available
if (all(c("HHNR", "WEGNR") %in% names(estimate_set))) {
  estimate_set <- estimate_set %>% arrange(HHNR, WEGNR)
  test_set <- test_set %>% arrange(HHNR, WEGNR)
}

cat("Training set:", nrow(estimate_set), "observations\n")
cat("Test set:", nrow(test_set), "observations\n\n")

# =============================================================================
# 2. MODEL DEVELOPMENT - STEPWISE APPROACH
# =============================================================================

cat("\n=== 2. Model Development ===\n")

# We'll develop multiple models showing progression:
# Model 1: Basic model with generic time and cost parameters
# Model 2: Alternative-specific time parameters
# Model 3: Adding PT-specific attributes
# Model 4: Adding sociodemographic variables (if available)

# -----------------------------------------------------------------------------
# MODEL 1: BASIC MODEL (Generic time and cost)
# -----------------------------------------------------------------------------

cat("\n--- MODEL 1: Basic Model with Generic Parameters ---\n")

database <- estimate_set

apollo_initialise()

apollo_control <- list(
  modelName       = "MNL_Model1_Basic",
  modelDescr      = "Basic MNL with generic time and cost",
  indivID         = "id",
  mixing          = FALSE,
  nCores          = 1,
  panelData       = FALSE,
  outputDirectory = path_output,
  debug           = FALSE,
  workInLogs      = FALSE
)

apollo_control_1 <- apollo_control

# Starting values for parameters
apollo_beta <- c(
  asc_car   = 0,      # Fixed reference alternative
  asc_pt    = 0,
  asc_bike  = 0,
  asc_walk  = 0,
  b_time    = 0,      # Generic time parameter
  b_cost    = 0       # Generic cost parameter
)

# Fix car as reference alternative
apollo_fixed <- c("asc_car")

# Validate inputs
apollo_inputs <- apollo_validateInputs()

# Store for later use
apollo_beta_1 <- apollo_beta
apollo_fixed_1 <- apollo_fixed
apollo_inputs_1 <- apollo_inputs

# Define probability function
apollo_probabilities_1 <- function(apollo_beta, apollo_inputs, functionality = "estimate") {

  apollo_attach(apollo_beta, apollo_inputs)
  on.exit(apollo_detach(apollo_beta, apollo_inputs))

  # Define utilities
  V <- list()
  V[["car"]]  <- asc_car  + b_time * totalTravelTime_car + b_cost * cost_car
  V[["pt"]]   <- asc_pt   + b_time * totalTravelTime_pt  + b_cost * cost_pt
  V[["bike"]] <- asc_bike + b_time * totalTravelTime_bike
  V[["walk"]] <- asc_walk + b_time * totalTravelTime_walk

  # Define MNL settings
  mnl_settings <- list(
    alternatives = c(car = 1, pt = 2, bike = 3, walk = 4),
    avail        = list(car  = avail_car,
                       pt   = avail_pt,
                       bike = avail_bike,
                       walk = avail_walk),
    choiceVar    = CHOICE,
    V            = V
  )

  # Compute probabilities
  P <- list()
  P[["model"]] <- apollo_mnl(mnl_settings, functionality)
  P <- apollo_prepareProb(P, apollo_inputs, functionality)

  return(P)
}

# Estimate Model 1
model_1 <- apollo_estimate(apollo_beta_1, apollo_fixed_1,
                           apollo_probabilities_1, apollo_inputs_1)

# Save model output
apollo_modelOutput(model_1, modelOutput_settings = list(printToScreen = TRUE))
apollo_saveOutput(model_1, saveOutput_settings = list(printToScreen = FALSE))

cat("\nModel 1 Log-Likelihood:", model_1$LLout, "\n")

# -----------------------------------------------------------------------------
# MODEL 2: ALTERNATIVE-SPECIFIC TIME PARAMETERS
# -----------------------------------------------------------------------------

cat("\n--- MODEL 2: Alternative-Specific Time Parameters ---\n")

database <- estimate_set

apollo_control <- list(
  modelName       = "MNL_Model2_AltSpecTime",
  modelDescr      = "MNL with alternative-specific time parameters",
  indivID         = "id",
  mixing          = FALSE,
  nCores          = 1,
  panelData       = FALSE,
  outputDirectory = path_output,
  debug           = FALSE,
  workInLogs      = FALSE
)

apollo_control_2 <- apollo_control

apollo_beta <- c(
  asc_car     = 0,
  asc_pt      = 0,
  asc_bike    = 0,
  asc_walk    = 0,
  b_time_car  = 0,    # Alternative-specific time parameters
  b_time_pt   = 0,
  b_time_bike = 0,
  b_time_walk = 0,
  b_cost      = 0     # Generic cost parameter
)

apollo_fixed <- c("asc_car")

apollo_inputs <- apollo_validateInputs()

apollo_beta_2 <- apollo_beta
apollo_fixed_2 <- apollo_fixed
apollo_inputs_2 <- apollo_inputs

apollo_probabilities_2 <- function(apollo_beta, apollo_inputs, functionality = "estimate") {

  apollo_attach(apollo_beta, apollo_inputs)
  on.exit(apollo_detach(apollo_beta, apollo_inputs))

  V <- list()
  V[["car"]]  <- asc_car  + b_time_car  * totalTravelTime_car + b_cost * cost_car
  V[["pt"]]   <- asc_pt   + b_time_pt   * totalTravelTime_pt  + b_cost * cost_pt
  V[["bike"]] <- asc_bike + b_time_bike * totalTravelTime_bike
  V[["walk"]] <- asc_walk + b_time_walk * totalTravelTime_walk

  mnl_settings <- list(
    alternatives = c(car = 1, pt = 2, bike = 3, walk = 4),
    avail        = list(car = avail_car, pt = avail_pt,
                       bike = avail_bike, walk = avail_walk),
    choiceVar    = CHOICE,
    V            = V
  )

  P <- list()
  P[["model"]] <- apollo_mnl(mnl_settings, functionality)
  P <- apollo_prepareProb(P, apollo_inputs, functionality)

  return(P)
}

model_2 <- apollo_estimate(apollo_beta_2, apollo_fixed_2,
                           apollo_probabilities_2, apollo_inputs_2)

apollo_modelOutput(model_2, modelOutput_settings = list(printToScreen = TRUE))
apollo_saveOutput(model_2, saveOutput_settings = list(printToScreen = FALSE))

cat("\nModel 2 Log-Likelihood:", model_2$LLout, "\n")

# Compare models using likelihood ratio test
apollo_lrTest(model_1, model_2)

# -----------------------------------------------------------------------------
# MODEL 3: ADDING PT-SPECIFIC ATTRIBUTES
# -----------------------------------------------------------------------------

cat("\n--- MODEL 3: Adding PT-Specific Attributes ---\n")

database <- estimate_set

apollo_control <- list(
  modelName       = "MNL_Model3_PT_Attributes",
  modelDescr      = "MNL with PT-specific attributes",
  indivID         = "id",
  mixing          = FALSE,
  nCores          = 1,
  panelData       = FALSE,
  outputDirectory = path_output,
  debug           = FALSE,
  workInLogs      = FALSE
)

apollo_control_3 <- apollo_control

apollo_beta <- c(
  asc_car         = 0,
  asc_pt          = 0,
  asc_bike        = 0,
  asc_walk        = 0,
  b_time_car      = 0,
  b_time_pt       = 0,
  b_time_bike     = 0,
  b_time_walk     = 0,
  b_cost          = 0,
  b_access        = 0,    # PT access time
  b_wait          = 0,    # PT waiting time
  b_transfers     = 0,    # Number of transfers
  b_transfer_time = 0,    # Transfer waiting time
  b_frequency     = 0     # Headway/frequency
)

apollo_fixed <- c("asc_car")

apollo_inputs <- apollo_validateInputs()

apollo_beta_3 <- apollo_beta
apollo_fixed_3 <- apollo_fixed
apollo_inputs_3 <- apollo_inputs

apollo_probabilities_3 <- function(apollo_beta, apollo_inputs, functionality = "estimate") {

  apollo_attach(apollo_beta, apollo_inputs)
  on.exit(apollo_detach(apollo_beta, apollo_inputs))

  V <- list()
  V[["car"]]  <- asc_car  + b_time_car  * totalTravelTime_car + b_cost * cost_car
  V[["pt"]]   <- asc_pt   + b_time_pt   * totalTravelTime_pt  + b_cost * cost_pt +
                 b_access * access_time + b_wait * wait_time +
                 b_transfers * transfers_pt + b_transfer_time * transferWaitingTime_pt +
                 b_frequency * frequency_pt
  V[["bike"]] <- asc_bike + b_time_bike * totalTravelTime_bike
  V[["walk"]] <- asc_walk + b_time_walk * totalTravelTime_walk

  mnl_settings <- list(
    alternatives = c(car = 1, pt = 2, bike = 3, walk = 4),
    avail        = list(car = avail_car, pt = avail_pt,
                       bike = avail_bike, walk = avail_walk),
    choiceVar    = CHOICE,
    V            = V
  )

  P <- list()
  P[["model"]] <- apollo_mnl(mnl_settings, functionality)
  P <- apollo_prepareProb(P, apollo_inputs, functionality)

  return(P)
}

model_3 <- apollo_estimate(apollo_beta_3, apollo_fixed_3,
                           apollo_probabilities_3, apollo_inputs_3)

apollo_modelOutput(model_3, modelOutput_settings = list(printToScreen = TRUE))
apollo_saveOutput(model_3, saveOutput_settings = list(printToScreen = FALSE))

cat("\nModel 3 Log-Likelihood:", model_3$LLout, "\n")

# Compare with Model 2
apollo_lrTest(model_2, model_3)

# -----------------------------------------------------------------------------
# MODEL 4: ADDING SOCIODEMOGRAPHIC VARIABLES
# -----------------------------------------------------------------------------

cat("\n--- MODEL 4: Adding Sociodemographic Interactions ---\n")

# Based on data check, we have: income, pt_pass_ga, male, freq_cardriver
# We'll use these to improve model fit

database <- estimate_set

apollo_control <- list(
  modelName       = "MNL_Model4_Sociodem",
  modelDescr      = "MNL with sociodemographic interactions",
  indivID         = "id",
  mixing          = FALSE,
  nCores          = 1,
  panelData       = FALSE,
  outputDirectory = path_output,
  debug           = FALSE,
  workInLogs      = FALSE
)

apollo_control_4 <- apollo_control

apollo_beta <- c(
  asc_car         = 0,
  asc_pt          = 0,
  asc_bike        = 0,
  asc_walk        = 0,
  b_time_car      = 0,
  b_time_pt       = 0,
  b_time_bike     = 0,
  b_time_walk     = 0,
  b_cost          = 0,
  b_access        = 0,
  b_wait          = 0,
  b_transfers     = 0,
  b_transfer_time = 0,
  b_frequency     = 0,
  # Sociodemographic interactions
  b_income_cost   = 0,      # Income * cost interaction (higher income = less cost-sensitive)
  b_male_car      = 0,      # Gender effect on car choice
  b_male_bike     = 0,      # Gender effect on bike choice
  b_GA_pt         = 0,      # GA subscription effect on PT
  b_carfreq_car   = 0       # Car driving frequency effect on car choice
)

apollo_fixed <- c("asc_car")

apollo_inputs <- apollo_validateInputs()

apollo_beta_4 <- apollo_beta
apollo_fixed_4 <- apollo_fixed
apollo_inputs_4 <- apollo_inputs

apollo_probabilities_4 <- function(apollo_beta, apollo_inputs, functionality = "estimate") {

  apollo_attach(apollo_beta, apollo_inputs)
  on.exit(apollo_detach(apollo_beta, apollo_inputs))

  V <- list()
  V[["car"]]  <- asc_car  + b_time_car  * totalTravelTime_car +
                 (b_cost + b_income_cost * income) * cost_car +
                 b_male_car * male + b_carfreq_car * freq_cardriver
  V[["pt"]]   <- asc_pt   + b_time_pt   * totalTravelTime_pt  +
                 (b_cost + b_income_cost * income) * cost_pt +
                 b_access * access_time + b_wait * wait_time +
                 b_transfers * transfers_pt + b_transfer_time * transferWaitingTime_pt +
                 b_frequency * frequency_pt + b_GA_pt * pt_pass_ga
  V[["bike"]] <- asc_bike + b_time_bike * totalTravelTime_bike +
                 b_male_bike * male
  V[["walk"]] <- asc_walk + b_time_walk * totalTravelTime_walk

  mnl_settings <- list(
    alternatives = c(car = 1, pt = 2, bike = 3, walk = 4),
    avail        = list(car = avail_car, pt = avail_pt,
                       bike = avail_bike, walk = avail_walk),
    choiceVar    = CHOICE,
    V            = V
  )

  P <- list()
  P[["model"]] <- apollo_mnl(mnl_settings, functionality)
  P <- apollo_prepareProb(P, apollo_inputs, functionality)

  return(P)
}

model_4 <- apollo_estimate(apollo_beta_4, apollo_fixed_4,
                           apollo_probabilities_4, apollo_inputs_4)

apollo_modelOutput(model_4, modelOutput_settings = list(printToScreen = TRUE))
apollo_saveOutput(model_4, saveOutput_settings = list(printToScreen = FALSE))

cat("\nModel 4 Log-Likelihood:", model_4$LLout, "\n")

# Compare with Model 3
apollo_lrTest(model_3, model_4)

# =============================================================================
# SELECT FINAL MODEL FOR ANALYSIS
# =============================================================================

# Choose the best model based on fit statistics
# We'll use Model 4 as the final model (includes sociodemographics)
final_model <- model_4
final_apollo_probabilities <- apollo_probabilities_4
final_apollo_inputs <- apollo_inputs_4

cat("\n=== Final Model Selected: Model 4 (with Sociodemographics) ===\n")
cat("Log-Likelihood:", final_model$LLout, "\n")
cat("AIC:", final_model$AIC, "\n")
cat("BIC:", final_model$BIC, "\n\n")

# =============================================================================
# 3. MODEL PREDICTION PERFORMANCE
# =============================================================================

cat("\n=== 3. Testing Prediction Performance ===\n")

# Function to process predictions and create confusion matrix
process_predictions <- function(predictions, actual_data) {

  # Extract mode probabilities
  pred_df <- as.data.frame(predictions[c('car', 'pt', 'bike', 'walk')])

  # Determine predicted mode (highest probability)
  pred_df$predicted_mode <- apply(pred_df[, c('car', 'pt', 'bike', 'walk')],
                                  1, which.max)
  pred_df$predicted_mode <- factor(pred_df$predicted_mode,
                                   levels = 1:4,
                                   labels = c('car', 'pt', 'bike', 'walk'))

  # Create confusion matrix
  confusion <- table(Predicted = pred_df$predicted_mode,
                     Actual = actual_data$chosen_mode)

  # Calculate accuracy
  accuracy <- sum(pred_df$predicted_mode == actual_data$chosen_mode) /
              nrow(actual_data)

  return(list(
    predictions = pred_df,
    confusion_matrix = confusion,
    accuracy = accuracy
  ))
}

# --- Training Set Performance ---
cat("\n--- Training Set Performance ---\n")

database <- estimate_set
apollo_inputs <- apollo_validateInputs()

predictions_train <- apollo_prediction(final_model,
                                       final_apollo_probabilities,
                                       apollo_inputs)

results_train <- process_predictions(predictions_train, estimate_set)

cat("\nConfusion Matrix (Training):\n")
print(results_train$confusion_matrix)
cat("\nTraining Accuracy:", round(results_train$accuracy * 100, 2), "%\n")

# --- Test Set Performance ---
cat("\n--- Test Set Performance ---\n")

database <- test_set
apollo_inputs <- apollo_validateInputs()

predictions_test <- apollo_prediction(final_model,
                                      final_apollo_probabilities,
                                      apollo_inputs)

results_test <- process_predictions(predictions_test, test_set)

cat("\nConfusion Matrix (Test):\n")
print(results_test$confusion_matrix)
cat("\nTest Accuracy:", round(results_test$accuracy * 100, 2), "%\n")

# Calculate mode-specific accuracy
mode_accuracy <- function(confusion_matrix) {
  modes <- c('car', 'pt', 'bike', 'walk')
  accuracy_by_mode <- sapply(modes, function(mode) {
    correct <- confusion_matrix[mode, mode]
    total <- sum(confusion_matrix[, mode])
    if (total > 0) correct / total else NA
  })
  return(accuracy_by_mode)
}

cat("\nMode-specific accuracy (Test Set):\n")
print(round(mode_accuracy(results_test$confusion_matrix) * 100, 2))

# =============================================================================
# 4. VALUE OF TRAVEL TIME (VTT)
# =============================================================================

cat("\n=== 4. Value of Travel Time Calculation ===\n")

# VTT for car
vtt_car <- apollo_deltaMethod(
  final_model,
  deltaMethod_settings = list(
    operation = "ratio",
    parName1  = "b_time_car",
    parName2  = "b_cost"
  )
)

cat("\nValue of Travel Time for CAR:\n")
print(vtt_car)
# Extract the value (apollo_deltaMethod returns a matrix/vector)
vtt_car_value <- as.numeric(vtt_car["Value"])
cat("VTT (Car):", round(vtt_car_value, 2), "CHF per minute\n")
cat("VTT (Car):", round(vtt_car_value * 60, 2), "CHF per hour\n")

# VTT for PT
vtt_pt <- apollo_deltaMethod(
  final_model,
  deltaMethod_settings = list(
    operation = "ratio",
    parName1  = "b_time_pt",
    parName2  = "b_cost"
  )
)

cat("\nValue of Travel Time for PUBLIC TRANSPORT:\n")
print(vtt_pt)
vtt_pt_value <- as.numeric(vtt_pt["Value"])
cat("VTT (PT):", round(vtt_pt_value, 2), "CHF per minute\n")
cat("VTT (PT):", round(vtt_pt_value * 60, 2), "CHF per hour\n")

# Additional VTT calculations for other time components (if Model 3/4 is used)
if ("b_access" %in% names(final_model$estimate)) {

  vtt_access <- apollo_deltaMethod(
    final_model,
    deltaMethod_settings = list(
      operation = "ratio",
      parName1  = "b_access",
      parName2  = "b_cost"
    )
  )

  vtt_access_value <- as.numeric(vtt_access["Value"])
  cat("\nValue of Access Time:\n")
  cat("VTT (Access):", round(vtt_access_value * 60, 2), "CHF per hour\n")

  vtt_wait <- apollo_deltaMethod(
    final_model,
    deltaMethod_settings = list(
      operation = "ratio",
      parName1  = "b_wait",
      parName2  = "b_cost"
    )
  )

  vtt_wait_value <- as.numeric(vtt_wait["Value"])
  cat("\nValue of Waiting Time:\n")
  cat("VTT (Wait):", round(vtt_wait_value * 60, 2), "CHF per hour\n")
}

# =============================================================================
# 5. ELASTICITY CALCULATIONS
# =============================================================================

cat("\n=== 5. Elasticity Calculations ===\n")

# Reset database to training set
database <- estimate_set
apollo_inputs <- apollo_validateInputs()

# Get baseline probabilities
P_base <- apollo_prediction(final_model, final_apollo_probabilities, apollo_inputs)
probs_base <- as.data.frame(P_base[c("car", "pt", "bike", "walk")])

# --- Direct Elasticities ---

cat("\n--- Direct Elasticities ---\n")

# Variables to calculate elasticities for
elasticity_vars <- c(
  'cost_car', 'cost_pt',
  'totalTravelTime_car', 'totalTravelTime_pt',
  'totalTravelTime_bike', 'totalTravelTime_walk'
)

# Add PT-specific variables if in model
if ("b_access" %in% names(final_model$estimate)) {
  elasticity_vars <- c(elasticity_vars,
                       'access_time', 'wait_time',
                       'transfers_pt', 'frequency_pt')
}

direct_elasticities <- list()

for (var in elasticity_vars) {

  cat("\nCalculating elasticity for:", var, "\n")

  # Create modified dataset (+1%)
  database_modified <- database
  database_modified[[var]] <- database_modified[[var]] * 1.01

  # Validate modified inputs
  apollo_inputs_mod <- apollo_validateInputs()

  # Get modified probabilities
  P_mod <- apollo_prediction(final_model, final_apollo_probabilities, apollo_inputs_mod)
  probs_mod <- as.data.frame(P_mod[c("car", "pt", "bike", "walk")])

  # Calculate elasticity: (% change in P) / (% change in X)
  # Since X changes by 1%, elasticity = % change in P / 0.01
  elasticity <- ((probs_mod - probs_base) / probs_base) / 0.01

  # Store mean elasticities
  direct_elasticities[[var]] <- colMeans(elasticity, na.rm = TRUE)

  cat("Mean elasticities:\n")
  print(round(direct_elasticities[[var]], 4))
}

# Reset database
database <- estimate_set

# --- Cross Elasticities ---

cat("\n\n--- Cross Elasticities ---\n")

# Example 1: Cross-elasticity of car choice w.r.t. PT cost
cat("\nCross-elasticity: Effect of PT cost on CAR choice\n")
cat("(How does a 1% increase in PT cost affect car ridership?)\n")

# Baseline
database <- estimate_set
apollo_inputs <- apollo_validateInputs()
P_base <- apollo_prediction(final_model, final_apollo_probabilities, apollo_inputs)
prob_car_base <- mean(P_base$car, na.rm = TRUE)

# Modified PT cost (+1%)
database_modified <- database
database_modified$cost_pt <- database_modified$cost_pt * 1.01
apollo_inputs_mod <- apollo_validateInputs()
P_mod <- apollo_prediction(final_model, final_apollo_probabilities, apollo_inputs_mod)
prob_car_mod <- mean(P_mod$car, na.rm = TRUE)

cross_elast_car_ptcost <- ((prob_car_mod - prob_car_base) / prob_car_base) / 0.01
cat("Cross-elasticity (car w.r.t. PT cost):", round(cross_elast_car_ptcost, 4), "\n")

# Example 2: Cross-elasticity of PT choice w.r.t. car cost
cat("\nCross-elasticity: Effect of CAR cost on PT choice\n")
cat("(How does a 1% increase in car cost affect PT ridership?)\n")

# Baseline
database <- estimate_set
apollo_inputs <- apollo_validateInputs()
P_base <- apollo_prediction(final_model, final_apollo_probabilities, apollo_inputs)
prob_pt_base <- mean(P_base$pt, na.rm = TRUE)

# Modified car cost (+1%)
database_modified <- database
database_modified$cost_car <- database_modified$cost_car * 1.01
apollo_inputs_mod <- apollo_validateInputs()
P_mod <- apollo_prediction(final_model, final_apollo_probabilities, apollo_inputs_mod)
prob_pt_mod <- mean(P_mod$pt, na.rm = TRUE)

cross_elast_pt_carcost <- ((prob_pt_mod - prob_pt_base) / prob_pt_base) / 0.01
cat("Cross-elasticity (PT w.r.t. car cost):", round(cross_elast_pt_carcost, 4), "\n")

# Example 3: Cross-elasticity of bike choice w.r.t. car travel time
cat("\nCross-elasticity: Effect of CAR travel time on BIKE choice\n")

database <- estimate_set
apollo_inputs <- apollo_validateInputs()
P_base <- apollo_prediction(final_model, final_apollo_probabilities, apollo_inputs)
prob_bike_base <- mean(P_base$bike, na.rm = TRUE)

database_modified <- database
database_modified$totalTravelTime_car <- database_modified$totalTravelTime_car * 1.01
apollo_inputs_mod <- apollo_validateInputs()
P_mod <- apollo_prediction(final_model, final_apollo_probabilities, apollo_inputs_mod)
prob_bike_mod <- mean(P_mod$bike, na.rm = TRUE)

cross_elast_bike_cartime <- ((prob_bike_mod - prob_bike_base) / prob_bike_base) / 0.01
cat("Cross-elasticity (bike w.r.t. car time):", round(cross_elast_bike_cartime, 4), "\n")

# Example 4: Cross-elasticity of walk choice w.r.t. PT travel time
cat("\nCross-elasticity: Effect of PT travel time on WALK choice\n")

database <- estimate_set
apollo_inputs <- apollo_validateInputs()
P_base <- apollo_prediction(final_model, final_apollo_probabilities, apollo_inputs)
prob_walk_base <- mean(P_base$walk, na.rm = TRUE)

database_modified <- database
database_modified$totalTravelTime_pt <- database_modified$totalTravelTime_pt * 1.01
apollo_inputs_mod <- apollo_validateInputs()
P_mod <- apollo_prediction(final_model, final_apollo_probabilities, apollo_inputs_mod)
prob_walk_mod <- mean(P_mod$walk, na.rm = TRUE)

cross_elast_walk_pttime <- ((prob_walk_mod - prob_walk_base) / prob_walk_base) / 0.01
cat("Cross-elasticity (walk w.r.t. PT time):", round(cross_elast_walk_pttime, 4), "\n")

# =============================================================================
# 6. APPLICATION TO TRIP DISTRIBUTION (TASK 2.2)
# =============================================================================

cat("\n\n=== 6. Applying Model to OD Matrix from Task 2.2 ===\n")

# Load OD matrix from Task 2.2
od_matrix <- read_csv(
  paste0(path_task22, "OD_Matrix.csv"),
  col_types = cols(
    start_row.id = col_double(),
    ziel_row.id = col_double(),
    n_Work = col_double(),
    geometry = col_character()
  )
)

cat("OD Matrix dimensions:", nrow(od_matrix), "rows\n")
cat("Total work trips:", sum(od_matrix$n_Work, na.rm = TRUE), "\n")

# We need to match OD pairs with travel data from our mode choice dataset
# Join OD matrix with actual trip data that has the same origin-destination

# First, check if we have zone information in our dataset
if (all(c("start_row.id", "ziel_row.id") %in% names(estimate_set))) {

  cat("\nJoining OD matrix with trip-level mode choice data...\n")

  # For each OD pair, we'll use the average predicted probabilities
  # from all trips with that OD combination

  # Get predictions for all trips in the training set
  database <- estimate_set
  apollo_inputs <- apollo_validateInputs()

  P_all <- apollo_prediction(final_model, final_apollo_probabilities, apollo_inputs)

  # Add predictions to dataset
  trips_with_probs <- estimate_set %>%
    mutate(
      prob_car  = P_all$car,
      prob_pt   = P_all$pt,
      prob_bike = P_all$bike,
      prob_walk = P_all$walk
    )

  # Calculate average probabilities per OD pair
  od_probs <- trips_with_probs %>%
    group_by(start_row.id, ziel_row.id) %>%
    summarise(
      n_trips_in_data = n(),
      prob_car  = mean(prob_car, na.rm = TRUE),
      prob_pt   = mean(prob_pt, na.rm = TRUE),
      prob_bike = mean(prob_bike, na.rm = TRUE),
      prob_walk = mean(prob_walk, na.rm = TRUE),
      .groups = "drop"
    )

  # Join with OD matrix
  od_with_modes <- od_matrix %>%
    left_join(od_probs, by = c("start_row.id", "ziel_row.id"))

  # Calculate trips by mode
  od_with_modes <- od_with_modes %>%
    mutate(
      trips_car  = n_Work * prob_car,
      trips_pt   = n_Work * prob_pt,
      trips_bike = n_Work * prob_bike,
      trips_walk = n_Work * prob_walk
    )

  # Calculate overall mode shares
  mode_shares <- od_with_modes %>%
    summarise(
      total_work_trips = sum(n_Work, na.rm = TRUE),
      total_car_trips  = sum(trips_car, na.rm = TRUE),
      total_pt_trips   = sum(trips_pt, na.rm = TRUE),
      total_bike_trips = sum(trips_bike, na.rm = TRUE),
      total_walk_trips = sum(trips_walk, na.rm = TRUE)
    ) %>%
    mutate(
      share_car  = total_car_trips / total_work_trips,
      share_pt   = total_pt_trips / total_work_trips,
      share_bike = total_bike_trips / total_work_trips,
      share_walk = total_walk_trips / total_work_trips
    )

  cat("\n=== Modal Split for Commuting Trips ===\n")
  cat("Total work trips:", round(mode_shares$total_work_trips, 0), "\n\n")
  cat("Mode Shares:\n")
  cat("  Car:  ", round(mode_shares$share_car * 100, 2), "%\n")
  cat("  PT:   ", round(mode_shares$share_pt * 100, 2), "%\n")
  cat("  Bike: ", round(mode_shares$share_bike * 100, 2), "%\n")
  cat("  Walk: ", round(mode_shares$share_walk * 100, 2), "%\n")

  # Save results
  write_csv(od_with_modes, paste0(path_output, "OD_with_mode_probabilities.csv"))
  write_csv(mode_shares, paste0(path_output, "modal_split_summary.csv"))

  cat("\nResults saved to:", path_output, "\n")

} else {
  cat("\nWarning: Zone IDs not found in mode choice dataset.\n")
  cat("Cannot directly link to OD matrix from Task 2.2.\n")
  cat("You may need to use average probabilities across all trips.\n")

  # Alternative approach: Use overall average probabilities
  database <- estimate_set
  apollo_inputs <- apollo_validateInputs()
  P_avg <- apollo_prediction(final_model, final_apollo_probabilities, apollo_inputs)

  avg_probs <- data.frame(
    mode = c("car", "pt", "bike", "walk"),
    probability = c(
      mean(P_avg$car, na.rm = TRUE),
      mean(P_avg$pt, na.rm = TRUE),
      mean(P_avg$bike, na.rm = TRUE),
      mean(P_avg$walk, na.rm = TRUE)
    )
  )

  cat("\nAverage mode choice probabilities:\n")
  print(avg_probs)

  # Apply to OD matrix
  od_with_modes <- od_matrix %>%
    mutate(
      prob_car  = avg_probs$probability[avg_probs$mode == "car"],
      prob_pt   = avg_probs$probability[avg_probs$mode == "pt"],
      prob_bike = avg_probs$probability[avg_probs$mode == "bike"],
      prob_walk = avg_probs$probability[avg_probs$mode == "walk"],
      trips_car  = n_Work * prob_car,
      trips_pt   = n_Work * prob_pt,
      trips_bike = n_Work * prob_bike,
      trips_walk = n_Work * prob_walk
    )

  mode_shares <- data.frame(
    total_work_trips = sum(od_matrix$n_Work, na.rm = TRUE),
    share_car  = avg_probs$probability[avg_probs$mode == "car"],
    share_pt   = avg_probs$probability[avg_probs$mode == "pt"],
    share_bike = avg_probs$probability[avg_probs$mode == "bike"],
    share_walk = avg_probs$probability[avg_probs$mode == "walk"]
  )

  cat("\n=== Modal Split for Commuting Trips (using average probabilities) ===\n")
  cat("Total work trips:", round(mode_shares$total_work_trips, 0), "\n\n")
  cat("Mode Shares:\n")
  cat("  Car:  ", round(mode_shares$share_car * 100, 2), "%\n")
  cat("  PT:   ", round(mode_shares$share_pt * 100, 2), "%\n")
  cat("  Bike: ", round(mode_shares$share_bike * 100, 2), "%\n")
  cat("  Walk: ", round(mode_shares$share_walk * 100, 2), "%\n")

  write_csv(od_with_modes, paste0(path_output, "OD_with_mode_probabilities.csv"))
  write_csv(mode_shares, paste0(path_output, "modal_split_summary.csv"))
}

# =============================================================================
# 7. SUMMARY AND MODEL COMPARISON
# =============================================================================

cat("\n\n=== MODEL COMPARISON SUMMARY ===\n")

model_comparison <- data.frame(
  Model = c("Model 1", "Model 2", "Model 3", "Model 4"),
  Description = c(
    "Generic time & cost",
    "Alt-specific time",
    "PT-specific attributes",
    "With sociodemographics"
  ),
  LogLikelihood = c(model_1$LLout, model_2$LLout, model_3$LLout, model_4$LLout),
  AIC = c(model_1$AIC, model_2$AIC, model_3$AIC, model_4$AIC),
  BIC = c(model_1$BIC, model_2$BIC, model_3$BIC, model_4$BIC),
  Parameters = c(
    length(model_1$estimate),
    length(model_2$estimate),
    length(model_3$estimate),
    length(model_4$estimate)
  )
)

print(model_comparison)

cat("\n=== ANALYSIS COMPLETE ===\n")
cat("All results have been saved to:", path_output, "\n")
cat("\nKey outputs:\n")
cat("  - Model estimation results (multiple models)\n")
cat("  - Prediction accuracy (train and test)\n")
cat("  - Value of Travel Time (VTT)\n")
cat("  - Direct and cross elasticities\n")
cat("  - Modal split for commuting trips\n")
cat("  - OD matrix with mode probabilities\n")

# Clean up
database <- estimate_set
