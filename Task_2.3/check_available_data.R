# Quick Data Check Script for Task 2.3
# Run this to see what variables are available in your dataset

library(tidyverse)

# Load the mode choice data
load("C:/Users/ETH/Documents/GitHub/Transport-Planning-Methods/Task_2.3/Data/ModeChoice_mzmv_tripsWAlternatives.Rda")

cat("====================================\n")
cat("TASK 2.3 DATA CHECK\n")
cat("====================================\n\n")

# Basic info
cat("Dataset name:", ls()[ls() != "ls"], "\n")
cat("Number of observations:", nrow(dat), "\n")
cat("Number of variables:", ncol(dat), "\n\n")

# Show all variable names
cat("====================================\n")
cat("ALL VARIABLES IN DATASET\n")
cat("====================================\n")
print(names(dat))

cat("\n\n====================================\n")
cat("CHECKING REQUIRED VARIABLES\n")
cat("====================================\n\n")

# Check required mode choice variables
required_vars <- list(
  "Core Variables" = c("CHOICE", "HHNR", "WEGNR", "id"),
  "Car Variables" = c("totalTravelTime_car", "cost_car", "avail_car"),
  "PT Variables" = c("totalTravelTime_pt", "cost_pt", "avail_pt",
                     "access_time", "wait_time", "transfers_pt",
                     "transferWaitingTime_pt", "frequency_pt"),
  "Bike Variables" = c("totalTravelTime_bike", "avail_bike"),
  "Walk Variables" = c("totalTravelTime_walk", "avail_walk"),
  "Zone IDs" = c("start_row.id", "ziel_row.id")
)

for (category in names(required_vars)) {
  cat("\n", category, ":\n", sep = "")
  for (var in required_vars[[category]]) {
    exists <- var %in% names(dat)
    status <- ifelse(exists, "✓ FOUND", "✗ MISSING")
    cat(sprintf("  %-30s %s\n", var, status))
  }
}

cat("\n\n====================================\n")
cat("SEARCHING FOR SOCIODEMOGRAPHIC VARIABLES\n")
cat("====================================\n\n")

# Search for sociodemographic variables
socio_searches <- list(
  "Age" = c("alter", "age"),
  "Gender" = c("gesl", "gender", "sex"),
  "Income" = c("income", "f20601", "einkommen"),
  "Employment" = c("employment", "f40800", "erwerbstaetig", "job"),
  "Education" = c("education", "f40120", "bildung"),
  "Car Ownership" = c("car_own", "f42100", "auto_besitz", "car_avail"),
  "PT Subscription - GA" = c("GA", "f41600_01a", "generalabo"),
  "PT Subscription - HalbTax" = c("halbtax", "f41600_01b", "half_fare", "halfare"),
  "Household Size" = c("HH_size", "hhsize", "household_size"),
  "Other PT Subscriptions" = c("f41600", "subscription", "abo")
)

found_sociodem <- FALSE

for (category in names(socio_searches)) {
  patterns <- socio_searches[[category]]
  matches <- c()

  for (pattern in patterns) {
    found <- names(dat)[grepl(pattern, names(dat), ignore.case = TRUE)]
    matches <- unique(c(matches, found))
  }

  if (length(matches) > 0) {
    found_sociodem <- TRUE
    cat(category, ":\n")
    cat("  Found variables: ", paste(matches, collapse = ", "), "\n\n", sep = "")
  }
}

if (!found_sociodem) {
  cat("No sociodemographic variables found with common naming patterns.\n")
  cat("This doesn't mean they don't exist - they might have different names.\n\n")
}

cat("\n====================================\n")
cat("SUMMARY STATISTICS\n")
cat("====================================\n\n")

# CHOICE variable
if ("CHOICE" %in% names(dat)) {
  cat("Mode Choice Distribution:\n")
  choice_table <- table(dat$CHOICE)
  choice_df <- data.frame(
    Mode = c("Car", "PT", "Bike", "Walk")[1:length(choice_table)],
    Count = as.numeric(choice_table),
    Percentage = round(as.numeric(choice_table) / sum(choice_table) * 100, 2)
  )
  print(choice_df)
  cat("\n")
}

# Check availability variables
if (all(c("avail_car", "avail_pt", "avail_bike", "avail_walk") %in% names(dat))) {
  cat("Mode Availability (% of trips where mode is available):\n")
  cat("  Car: ", round(mean(dat$avail_car, na.rm = TRUE) * 100, 1), "%\n", sep = "")
  cat("  PT:  ", round(mean(dat$avail_pt, na.rm = TRUE) * 100, 1), "%\n", sep = "")
  cat("  Bike:", round(mean(dat$avail_bike, na.rm = TRUE) * 100, 1), "%\n", sep = "")
  cat("  Walk:", round(mean(dat$avail_walk, na.rm = TRUE) * 100, 1), "%\n\n", sep = "")
}

# Summary of key numeric variables
cat("Summary of Key Numeric Variables:\n")
key_numeric <- c("totalTravelTime_car", "totalTravelTime_pt",
                 "totalTravelTime_bike", "totalTravelTime_walk",
                 "cost_car", "cost_pt")
available_numeric <- intersect(key_numeric, names(dat))

if (length(available_numeric) > 0) {
  summary_stats <- dat %>%
    select(any_of(available_numeric)) %>%
    summary()
  print(summary_stats)
}

cat("\n\n====================================\n")
cat("RECOMMENDATIONS\n")
cat("====================================\n\n")

# Check completeness
all_required <- unlist(required_vars)
missing_required <- all_required[!all_required %in% names(dat)]

if (length(missing_required) == 0) {
  cat("✓ All core required variables are present!\n")
} else {
  cat("⚠ Missing some required variables:\n")
  cat("  ", paste(missing_required, collapse = ", "), "\n", sep = "")
}

if (found_sociodem) {
  cat("✓ Sociodemographic variables found - you can use Model 4!\n")
  cat("  → Update Model 4 in the complete script with the exact variable names above\n\n")
} else {
  cat("✗ No sociodemographic variables found with standard names\n")
  cat("  → Option 1: Check for different variable names manually\n")
  cat("  → Option 2: Skip Model 4 (Models 1-3 already meet requirements)\n")
  cat("  → Option 3: Merge with additional MZMV files if available\n\n")
}

if (!all(c("start_row.id", "ziel_row.id") %in% names(dat))) {
  cat("⚠ Zone IDs not found\n")
  cat("  → Task 6 will use average probability approach (automatic fallback)\n\n")
} else {
  cat("✓ Zone IDs present - can link directly to Task 2.2 OD matrix\n\n")
}

cat("====================================\n")
cat("NEXT STEPS\n")
cat("====================================\n\n")
cat("1. Review the variable list above\n")
cat("2. If sociodemographic variables exist:\n")
cat("   - Uncomment Model 4 in 2.3_Mode-Choice_GroupA_Complete.R\n")
cat("   - Update variable names to match what's in your data\n")
cat("3. Run the complete script!\n\n")

cat("Data check complete!\n")
