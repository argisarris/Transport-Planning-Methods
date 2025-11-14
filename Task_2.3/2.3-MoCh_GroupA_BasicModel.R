# #Author: Xuan He, IVT-ETH

# Group A

install.packages("apollo")

# load libraries
library(apollo) 
library(tidyverse)
library(readr)


##### ----0) Define paths  ##### 
  path_data = "/Users/julianbirkel/Documents/Master/Sem 3/Transport Planning Methods/Exercise 2.3/"

##### ----1) read data and prepare  ##### 
  load(paste0(path_data, "ModeChoice_mzmv_tripsWAlternatives.Rda"))

#convert CHOSEN codes to verbal
  dat$chosen_mode <- factor(dat$CHOICE, levels=1:4, labels=c('car', 'pt', 'bike', 'walk'))

# later we want to use the household income information in MZMV_HH, so we convert to numbers: 
  

# divide data in 80% training and 20% test set
  set.seed(123)
  index = sample(seq_len(nrow(dat)), size = 0.8 * nrow(dat))
  estimate_set = dat[index, ]
  test_set = dat[-index, ] 
  
  estimate_set = estimate_set %>% # order again
    arrange(HHNR, WEGNR) 


##### ----2) Defining and estimating the model   ##### 
### a. Set core controls
#please look at the manual: https://www.apollochoicemodelling.com/manual.html
#code example here: https://cran.r-project.org/web/packages/apollo/vignettes/apollofirstexample.R
  
  database = estimate_set #Mandatory to name the data "database"
  
  apollo_initialise()
  apollo_control <- list(
    modelName  = "ModeChoice_MNL",        # Model name (used in output files)
    modelDescr = "Multinomial Logit model for mode choice",  
    indivID    = "id",                    # Unique identifier per individual - here you could use "trip_id" or just "id"
    mixing     = FALSE,                   # No random parameters for MNL
    nCores     = 1,                       # Single-core is fine for MNL
    panelData  = FALSE,                   # Set TRUE only for panel data
    outputDirectory = "/Users/julianbirkel/Documents/Master/Sem 3/Transport Planning Methods/Exercise 2.3/",           # Folder for model output
    debug      = FALSE,                   # Can be TRUE for troubleshooting
    workInLogs = FALSE                    # Keep FALSE unless log-transforming data
  )
  
  
### b. utility function specification   ##### this was run multiple times and calibrated
  apollo_beta <- c(
    asc_car   = 0,       # <-- fixed reference alternative
    asc_pt    = -0.703,
    asc_bike  = -1.702,
    asc_walk  = 0.072,
    b_time    = -0.05,
    b_cost    = -0.03
  )
  
  
# Vector with names (in quotes) of parameters to be kept fixed at their starting value in apollo_beta, use apollo_beta_fixed = c() if none
  apollo_fixed <- c("asc_car")  # <-- car is reference; adjust if different
  
### c. validate inputs
  apollo_inputs <- apollo_validateInputs()
  
### d. define model
  apollo_probabilities <- function(apollo_beta, apollo_inputs, functionality = "estimate"){
    
  # Make parameters visible
    for (param in names(apollo_beta)) assign(param, apollo_beta[[param]])
    
  # Attach database manually (inside apollo_inputs)
    attach(apollo_inputs$database, warn.conflicts = FALSE)
    on.exit(detach(apollo_inputs$database), add = TRUE)
    
    # Define utilities
    V <- list()
    V[["car"]]  <- asc_car  + b_time * totalTravelTime_car  - b_cost * cost_car
    V[["pt"]]   <- asc_pt   + b_time * totalTravelTime_pt   - b_cost * cost_pt
    V[["bike"]] <- asc_bike + b_time * totalTravelTime_bike - b_cost * 0   #since there is no cost_bike we assume its zero
    V[["walk"]] <- asc_walk + b_time * totalTravelTime_walk - b_cost * 0   #since there is no cost_walk we assume its zero
    
    # Define MNL settings
    MNL_settings <- list(
      alternatives = c(car = 1, pt = 2, bike = 3, walk = 4),
      avail        = list(
        car  = avail_car,
        pt   = avail_pt,
        bike = avail_bike,
        walk = avail_walk
      ),
      choiceVar = CHOICE,
      V = V
    )
    
    # Compute probabilities
    P <- list()
    P[["model"]] <- apollo_mnl(MNL_settings, functionality)
    
    # Prepare probabilities for Apollo
    P <- apollo_prepareProb(P, apollo_inputs, functionality)
    
    return(P)
  }
  
  
### e. estimate model
  model <- apollo_estimate(apollo_beta, apollo_fixed, apollo_probabilities, apollo_inputs)
  
### f. Analyze model
  apollo_modelOutput(model)
  
  # will print this:
  #   
  #   Estimates:
  #   Estimate        s.e.   t.rat.(0)    Rob.s.e. Rob.t.rat.(0)
  # asc_car      0.00000          NA          NA          NA            NA
  # asc_pt      -0.70349    0.024576     -28.625     0.03333       -21.108
  # asc_bike    -1.70217    0.014015    -121.450     0.01562      -108.946
  # asc_walk     0.07249    0.015369       4.717     0.02012         3.604
  # b_time      -0.05861  5.9707e-04     -98.170  9.6834e-04       -60.530
  # b_cost      -0.01715    0.006525      -2.628     0.01043        -1.645



##### ----3) Estimate mode choice
  # postprocess predictons
  proc_preds=function(df,base_df){
    df$estimated_mode <- apply(df[, c('car', 'pt', 'bike', 'walk')], 1, which.max) #column 1,2,3,4 has the maximum
    df$estimated_mode <- factor(df$estimated_mode, levels=1:4, labels=c('car', 'pt', 'bike', 'walk'))
    
    ### generate confusion matrix
    print(table(df$estimated_mode, base_df$chosen_mode))
    
    return(df)
  }

# a) training dataset - the 80%
  database=estimate_set
  predictions_base <- apollo_prediction(model, apollo_probabilities, apollo_inputs)
  predictions_base=proc_preds(predictions_base,estimate_set)
  sum(predictions_base$estimated_mode==estimate_set$chosen_mode)/nrow(estimate_set)
  
  
  # this will be printed:
  # >   predictions_base=proc_preds(predictions_base,estimate_set)
  # 
  #       car    pt  bike  walk
  # car  52182  3954  5350  9840
  # pt       0  2541   303   259
  # bike     0   588   194   105
  # walk    95  1722  2752  9836
  # this table means that off all car drivers 52182 were predicted correctly, 3954 were falsely classified to pt, 5350 to bike and 9840 to walk...
  # >   sum(predictions_base$estimated_mode==estimate_set$chosen_mode)/nrow(estimate_set)
  # [1] 0.7217151
  # this value means, that 72.17% of the predictions were correct, which is alright for a first model -> but the model should be improved and tested again here
  # even though the model was tested on the training data, it does not predict the mode choice 100% correctly, this is because the model works with 
  #  probabilities for each mode and chooses the mode with the hightes probability - but its not actually chosen in real life always like this


# b) test data - the 20%
# Prepare new inputs for the new dataset
  database=test_set
  apollo_inputs=apollo_validateInputs() 

# Apply the prediction function
  predictions_test <- apollo_prediction(model, apollo_probabilities, apollo_inputs)
  predictions_test = proc_preds(predictions_test, test_set)
  sum(predictions_test$estimated_mode == test_set$chosen_mode) / nrow(test_set)
  
  
  # this will be printed:
  # >   predictions_test = proc_preds(predictions_test, test_set)
  # 
  # car    pt  bike  walk
  # car  12961  1001  1395  2370
  # pt       0   611    72    73
  # bike     0   142    70    25
  # walk    23   404   736  2548
  # this table means the same as the one above but is only about 20% of the data -since we divided them like that for testing
  # >   sum(predictions_test$estimated_mode == test_set$chosen_mode) / nrow(test_set)
  # [1] 0.721769
  # this value is, same as above, the share of correctly predicted modes. It is a very good sign, that the values are very similar, which means the model is consistent
  # consistency means the model works and can predict, but a low share means, the model needs better fine tuning in what data it uses for prediction.
      
      
##### ---- 4)	Calculate the value of travel time (VTT)    ##### 
  valueTravelTime <- apollo_deltaMethod(
    model,
    deltaMethod_settings = list(
      name       = "VTT",
      expression = "b_time / b_cost"
    )
  )
  valueTravelTime
  
  # what will be printed:
  # Value	3.4176 monetary units per time unit
  # Significance	t ≈ -1.65 → borderline significant
  # Interpretation	People are willing to pay about 3.4 currency units to save one unit of travel time



    
    
    
##### ---- 5) Calculate elasticities ---- #####
  
  elasticities <- list()
  
  elasticity_vars <- c(
    'cost_car','cost_pt',
    'totalTravelTime_car','totalTravelTime_pt',
    'totalTravelTime_walk','totalTravelTime_bike'
  )
  
# Baseline individual probabilities
  P_base <- apollo_probabilities(apollo_beta = model$estimate, apollo_inputs, functionality = "prediction")
  predictions_base <- as.data.frame(P_base$model[c("car","pt","bike","walk")])
  
  for (v in elasticity_vars) {
    
    # Copy and modify variable by +1%
      database_1p <- database
      database_1p[[v]] <- database_1p[[v]] * 1.01
    
    # Validate inputs for modified dataset
      database <- database_1p
      apollo_inputs <- apollo_validateInputs()
    
    # Predict with modified variable
      P_1p <- apollo_probabilities(apollo_beta = model$estimate, apollo_inputs, functionality = "prediction")
      predictions_1p <- as.data.frame(P_1p$model[c("car","pt","bike","walk")])
    
    # Compute elasticity: %ΔP / %ΔX
      elasticity_df <- (predictions_1p - predictions_base) / predictions_base / 0.01
    
    # Store mean elasticities across individuals
      elasticities[[v]] <- colMeans(elasticity_df, na.rm = TRUE)
  }
  
  elasticities
  
  # what will be printed:
  # >   elasticities
  # $cost_car
  # car           pt         bike         walk 
  # 0.001863357 -0.013866584 -0.009442046 -0.004593987 
  # 
  # $cost_pt
  # car           pt         bike         walk 
  # -0.001851709  0.046718011 -0.015110013 -0.009742333 
  # 
  # $totalTravelTime_car
  # car          pt        bike        walk 
  # -0.09788741  0.51582991  0.33704785  0.20883044 
  # 
  # $totalTravelTime_pt
  # car           pt         bike         walk 
  # 0.008465219 -1.568262573  0.502773804  0.353196745 
  # 
  # $totalTravelTime_walk
  # car         pt       bike       walk 
  # 0.1579224 -1.3606909  0.7282839 -1.4468025 
  # 
  # $totalTravelTime_bike
  # car         pt       bike       walk 
  # 0.2068704 -1.2768211 -0.4756441 -1.3769388
  
  # these are the elasticities -> one percent higher car cost (cost is negative so it will have a positive impact) will increase 0.1863...% the car ridership (and decrease pt ridership by 1.386%)
      

  
    
    
    
    
##### ---- 6)	Apply to trip distribution from 2.2   ##### - I am here now and I should only run this when the model has been improved enough
  
  # the OD.csv only has start -> end + n_work data, so I don't know how to match the model - just say that

  od <- read_csv(
    "/Users/julianbirkel/Documents/Master/Sem 3/Transport Planning Methods/Exercise 2.2/OD.csv",
    col_names = c("start_row.id", "ziel_row.id", "n_Work", "geometry"),
    show_col_types = FALSE
  ) %>%
    select(-geometry)  # drop geometry column
  
  od <- od[-1, ]  # remove first row that repeats the column names
  
  od <- od %>%
    mutate(
      start_row.id = as.numeric(start_row.id),
      ziel_row.id  = as.numeric(ziel_row.id),
      n_Work       = as.numeric(n_Work)
    )
  
# Compute predicted probabilities from your estimated MNL model - hier kommt Fehler auf
  P_base <- apollo_probabilities(
    apollo_beta = model$estimate,
    apollo_inputs,
    functionality = "prediction"
  )
  
  
  # 
  # 
  # #########
  # database <- database %>%
  #   mutate(
  #     access_time = ifelse(is.na(access_time), 0, access_time),
  #     wait_time   = ifelse(is.na(wait_time), 0, wait_time),
  #     egress_time = ifelse(is.na(egress_time), 0, egress_time)
  #   )
  # ##########
  # 
  # 
  # 
  # # a) — Add dummy variables for travel times and costs
  # od <- od %>%
  #   mutate(
  #     # Temporary travel times in minutes (I’ll replace these in the following code with real values)
  #     totalTravelTime_car  = 30,
  #     totalTravelTime_pt   = 40,
  #     totalTravelTime_bike = 20,
  #     totalTravelTime_walk = 15,
  #     
  #     # Temporary costs in CHF
  #     cost_car  = 5,
  #     cost_pt   = 3,
  #     cost_bike = 0,
  #     cost_walk = 0,
  #     
  #     # All modes available
  #     avail_car  = 1,
  #     avail_pt   = 1,
  #     avail_bike = 1,
  #     avail_walk = 1
  #   )
  # 
  # 
  # # b) — Prepare OD dataset for Apollo
  # database <- od  # Apollo expects the data to be called 'database'
  # 
  #   # Apollo needs an ID for each "individual" (here: each OD pair)
  #   database$id <- 1:nrow(database)
  #   
  #   # Create dummy choice variable (required only for structure)
  #   database$CHOICE <- 1  # arbitrary, Apollo ignores it during prediction
  #   
  #   # Validate the new data structure
  #   apollo_inputs <- apollo_validateInputs()
  #   
  #   # Now use your previously estimated coefficients to predict probabilities
  #   P_od <- apollo_probabilities(
  #     apollo_beta = model$estimate,
  #     apollo_inputs,
  #     functionality = "prediction"
  # )
  # 
  # # c) — Attach predicted mode probabilities to OD data
  # od_with_probs <- od %>%
  #   mutate(
  #     car  = P_od$model$car,
  #     pt   = P_od$model$pt,
  #     bike = P_od$model$bike,
  #     walk = P_od$model$walk
  #   ) %>%
  #   mutate(
  #     trips_car  = n_Work * car,
  #     trips_pt   = n_Work * pt,
  #     trips_bike = n_Work * bike,
  #     trips_walk = n_Work * walk
  #   )
  # 
  # # d) — Summarise mode shares across the entire OD matrix
  # mode_shares <- od_with_probs %>%
  #   summarise(
  #     total_trips = sum(n_Work, na.rm = TRUE),
  #     car_trips   = sum(trips_car, na.rm = TRUE),
  #     pt_trips    = sum(trips_pt, na.rm = TRUE),
  #     bike_trips  = sum(trips_bike, na.rm = TRUE),
  #     walk_trips  = sum(trips_walk, na.rm = TRUE)
  #   ) %>%
  #   mutate(
  #     share_car  = car_trips  / total_trips,
  #     share_pt   = pt_trips   / total_trips,
  #     share_bike = bike_trips / total_trips,
  #     share_walk = walk_trips / total_trips
  #   )
  # 
  # mode_shares
  # 
  # # c) — save as csv
  # write_csv(
  #   od_with_probs,
  #   "/Users/julianbirkel/Documents/Master/Sem 3/Transport Planning Methods/Exercise 3/od_with_mode_shares.csv"
  # )
  # 
  # 
  # 
  # 
  # 
  # 
  # 
  # # optional: plot
  # 
  # library(ggplot2)
  # library(tidyr)
  # 
  # # Convert mode shares into long format for plotting
  # mode_shares_long <- mode_shares %>%
  #   pivot_longer(cols = starts_with("share_"),
  #                names_to = "mode",
  #                values_to = "share") %>%
  #   mutate(mode = gsub("share_", "", mode))
  # 
  # # Plot mode shares
  # ggplot(mode_shares_long, aes(x = mode, y = share)) +
  #   geom_bar(stat = "identity") +
  #   scale_y_continuous(labels = scales::percent) +
  #   labs(
  #     title = "Predicted Mode Shares (Work Trips)",
  #     x = "Mode",
  #     y = "Share of Trips"
  #   ) +
  #   theme_minimal()
  # 
  # 
  # 
  # 
  # 
  # 
  # 
  #   