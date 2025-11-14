# Quick script to explore the .Rda file structure
load("C:/Users/ETH/Documents/GitHub/Transport-Planning-Methods/Task_2.3/Data/ModeChoice_mzmv_tripsWAlternatives.Rda")

cat("=== Dataset Information ===\n")
cat("Dataset name:", ls(), "\n\n")

cat("Dimensions:", dim(dat), "\n")
cat("Rows:", nrow(dat), "\n")
cat("Columns:", ncol(dat), "\n\n")

cat("=== Column Names ===\n")
print(names(dat))

cat("\n=== First 3 rows (selected columns) ===\n")
print(head(dat[, 1:min(15, ncol(dat))], 3))

cat("\n=== Structure (first 20 columns) ===\n")
str(dat[, 1:min(20, ncol(dat))])

cat("\n=== Summary Statistics (key variables) ===\n")
key_vars <- c("CHOICE", "totalTravelTime_car", "cost_car", "totalTravelTime_pt",
              "cost_pt", "totalTravelTime_bike", "totalTravelTime_walk",
              "avail_car", "avail_pt", "avail_bike", "avail_walk")
available_vars <- intersect(key_vars, names(dat))
if(length(available_vars) > 0) {
  print(summary(dat[, available_vars]))
}

cat("\n=== Unique values in CHOICE ===\n")
if("CHOICE" %in% names(dat)) {
  print(table(dat$CHOICE))
}

cat("\n=== Check for sociodemographic variables ===\n")
sociodem_patterns <- c("age", "gender", "income", "HH", "person", "employment", "car_own")
sociodem_cols <- names(dat)[grepl(paste(sociodem_patterns, collapse="|"), names(dat), ignore.case=TRUE)]
cat("Potential sociodemographic columns:\n")
print(sociodem_cols)
