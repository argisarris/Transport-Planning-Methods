# ------------------------------------------------------------------------------------------------------------------
# title: "2.1 Trip Generation Exercise"
#initial author: "Xuan He, IVT, ETHZ"
#secondary author: "Anargyros Sarris, IVT, ETHZ"

### Step 0: Load packages and define base paths

# libraries
library(tidyverse)
library(data.table)
library(arrow)
library(ggplot2)
library(sf)
library(MASS)
library(dplyr)
library(tidyr)
library(broom)
library(readxl)

# ------------------------------------------------------------------------------------------------------------------
# Step 0: Define base paths

base_dir <- "C:/Users/ETH/ETH Zurich/Transport Planning Methods - Documents/03 Data/Materials for Trip Generation-20251008"

# Subfolders
dir_mzmv   <- file.path(base_dir, "01_MZMV")
dir_struct <- file.path(base_dir, "03_Structural")
dir_gis    <- file.path(base_dir, "04_GIS")
dir_out    <- file.path(base_dir, "09_Output")

dir_figures <- file.path(dir_out, "figures")
dir_maps    <- file.path(dir_out, "maps")
dir_models <- file.path(dir_out, "models")
dir_tables <- file.path(dir_out, "tables")

# File paths
f_wege   <- file.path(dir_mzmv, "wegeinland.csv")
f_hh     <- file.path(dir_mzmv, "haushalte.csv")
f_pers   <- file.path(dir_mzmv, "zielpersonen.csv")
f_strukt <- file.path(dir_struct, "Strukturgrössen.xlsx")
f_npvm   <- file.path(dir_gis, "NPVM2010MetroZH.shp")

# ------------------------------------------------------------------------------------------------------------------
### Step 1: Load Data

df_tripsCH <- fread(f_wege)
df_hh      <- fread(f_hh)
df_persons <- fread(f_pers)

# Using parquet version instead of Excel
Strukturdaten <- read_parquet(file.path(dir_struct, "Strukturgroessen.parquet"))
npvm_zones_ZH <- read_sf(f_npvm)

# --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------- 
### Step 2: Keep relevant columns and build a compact join
# Trip records (keep household id + main purpose so we can still filter on it later).
trip_cols_relevant <- c("HHNR", "wzweck1")

df_tripsCH_reduced <- df_tripsCH %>%
  dplyr::select(dplyr::any_of(trip_cols_relevant)) %>%
  standardise_hh_id() %>%
  dplyr::relocate(hh_id)

# Person-level descriptors requested by the user.
person_cols_relevant <- c(
  "HHNR",                   # alternative household identifier name
  "WP",                  # person weight
  "alter",                  # age
  "gesl",               # gender
  "f40800_01",           # employment status
  "f40120",            # highest education
  "f42100e", # car availability
  "f41600_01a",    # GA subscription
  "f41600_01b",  # half-fare subscription
  "f41600_01c",  # zone subscription
  "f41600_01d", # route subscription
  "f41600_01e", # seven25/track7 subscription
  "f41600_01f",         # junior/child subscription
  "f41600_01g"          # other subscription types
)

df_persons_reduced <- df_persons %>%
  dplyr::select(dplyr::any_of(person_cols_relevant)) %>%
  standardise_hh_id() %>%
  dplyr::relocate(hh_id)

# Household-level information limited to the requested income attribute.
household_cols_relevant <- c("HHNR", "f20601")

df_hh_reduced <- df_hh %>%
  dplyr::select(dplyr::any_of(household_cols_relevant)) %>%
  standardise_hh_id() %>%
  dplyr::relocate(hh_id)

# Join the trimmed tables on the harmonised household identifier to produce a
# compact analysis-ready file and export it for reuse.
df_trips_persons_hh <- df_tripsCH_reduced %>%
  left_join(df_persons_reduced, by = "hh_id") %>%
  left_join(df_hh_reduced, by = "hh_id")

write_csv(
  df_trips_persons_hh,
  file.path(dir_tables, "df_combined_reduced.csv")
)

# ------------------------------------------------------------------------------------------------------------------
# Step 3: Data cleaning
# Merge all person and trip-level data first

# Clean and transform variables
df_cleaned <- df_trips_persons_hh %>%
  mutate(
    # Age groups (binary flags)
    n_age_0_17  = as.integer(alter <= 17),
    n_age_18_24 = as.integer(alter >= 18 & alter <= 24),
    n_age_25_44 = as.integer(alter >= 25 & alter <= 44),
    n_age_45_64 = as.integer(alter >= 45 & alter <= 64),
    n_age_65_74 = as.integer(alter >= 65 & alter <= 74),
    n_age_75p   = as.integer(alter >= 75),
    
    # Gender recode
    gesl = case_when(
      gesl == 1 ~ "male",
      gesl == 2 ~ "female",
      TRUE ~ NA_character_
    ),
    
    # Employment recode
    employment = case_when(
      f40800_01 %in% c(1, 2, 3, 9) ~ "employed",
      f40800_01 == 4 ~ "unemployed",
      TRUE ~ NA_character_
    ),
    
    # Trip purpose recode
    purpose = case_when(
      wzweck1 %in% c(2, 6) ~ "Work",
      wzweck1 == 3 ~ "Education",
      wzweck1 %in% c(8, 11, 12, 13) ~ "Leisure",
      wzweck1 == 4 ~ "Shop",
      wzweck1 %in% c(5, 7) ~ "Errands",
      TRUE ~ NA_character_
    ),
    
    # Car availability recode
    car_avail = case_when(
      f42100e %in% c(1, 2) ~ "Available",
      f42100e == 3 ~ "Unavailable",
      TRUE ~ NA_character_
    ),
    
    # PT availability from subscriptions
    pt_available = if_else(
      rowSums(across(dplyr::starts_with("f41600_01"), ~ .x == 1), na.rm = TRUE) > 0,
      "Available", "Unavailable"
    ),
    
    # Household income group
    income_group = case_when(
      f20601 == 1 ~ "Under CHF 2000",
      f20601 == 2 ~ "CHF 2000 to 4000",
      f20601 == 3 ~ "CHF 4001 to 6000",
      f20601 == 4 ~ "CHF 6001 to 8000",
      f20601 == 5 ~ "CHF 8001 to 10000",
      f20601 == 6 ~ "CHF 10001 to 12000",
      f20601 == 7 ~ "CHF 12001 to 14000",
      f20601 == 8 ~ "CHF 14001 to 16000",
      f20601 == 9 ~ "More than CHF 16000",
      TRUE ~ NA_character_
    )
  )

# Drop PT subscription detail columns
df_cleaned <- df_cleaned %>%
  dplyr::select(-dplyr::starts_with("f41600_01"))

# Drop raw columns already recoded
df_cleaned <- df_cleaned %>%
  dplyr::select(-dplyr::any_of(c("f40800_01", "f40120", "f42100e", "f20601")))

# ---- Drop rows with missing values
n_before_na <- nrow(df_cleaned)
df_cleaned <- df_cleaned %>% drop_na()
n_after_na <- nrow(df_cleaned)
cat("🚫 Dropped due to NA values: ", n_before_na - n_after_na, "\n")

# ---- Drop rows with invalid or unmapped trip purposes
n_before_purpose <- nrow(df_cleaned)
df_cleaned <- df_cleaned %>% filter(!is.na(purpose))
n_after_purpose <- nrow(df_cleaned)
cat("🚫 Dropped due to invalid/missing trip purpose: ", 
    n_before_purpose - n_after_purpose, "\n")

# ---- Plot: Weighted total trips by purpose
trip_summary <- df_cleaned %>%
  group_by(purpose) %>%
  summarise(total_trips = sum(WP, na.rm = TRUE)) %>%
  arrange(desc(total_trips))

# Create plot
trip_plot <- ggplot(trip_summary, aes(x = reorder(purpose, -total_trips), y = total_trips)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  labs(
    title = "Total Trips by Purpose (Weighted)",
    x = "Trip Purpose",
    y = "Weighted Trip Count"
  ) +
  theme_minimal()

# Print and save plot
print(trip_plot)
ggsave(
  filename = file.path(dir_figures, "trips_by_purpose_weighted.png"),
  plot = trip_plot,
  width = 8,
  height = 6,
  dpi = 300
)

# ---- Final row count
cat("✅ Final number of rows in cleaned dataset: ", nrow(df_cleaned), "\n")

write_csv(df_cleaned, file.path(dir_tables, "df_cleaned_combined.csv"))

# ---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
### Step 4: Prepare Work Trip Model Input ----

# Filter to keep only work trips for subsequent analysis
df_work_trips <- df_cleaned %>%
  filter(purpose == "Work")

# Count number of work trips per household (both raw and weighted)
df_work_counts <- df_work_trips %>%
  group_by(hh_id) %>%
  summarise(
    n_trips_work = n(),
    weighted_trips = sum(WP, na.rm = TRUE),
    .groups = "drop"
  )

# Attach the household-level counts back to the work-trip table for convenience
df_work_trips <- df_work_trips %>%
  left_join(df_work_counts, by = "hh_id")

# Save full table with all attributes but only work trips
write_csv(df_work_trips, file.path(dir_tables, "df_cleaned_worktrips.csv"))

# --- PLOTS ---

# Helper to compute weighted totals safely
sum_work_trips <- function(data, grouping_var) {
  data %>%
    group_by({{ grouping_var }}) %>%
    summarise(total_work_trips = sum(WP, na.rm = TRUE), .groups = "drop")
}

# 1. Work trips by car availability
p_car <- sum_work_trips(df_work_trips, car_avail) %>%
  ggplot(aes(x = car_avail, y = total_work_trips, fill = car_avail)) +
  geom_col() +
  labs(title = "Total Work Trips by Car Availability", x = "Car Availability", y = "Weighted Work Trips") +
  theme_minimal() +
  theme(legend.position = "none")

# 2. Work trips by PT availability
p_pt <- sum_work_trips(df_work_trips, pt_available) %>%
  ggplot(aes(x = pt_available, y = total_work_trips, fill = pt_available)) +
  geom_col() +
  labs(title = "Total Work Trips by PT Availability", x = "PT Availability", y = "Weighted Work Trips") +
  theme_minimal() +
  theme(legend.position = "none")

# 3. Work trips by employment status
p_emp <- sum_work_trips(df_work_trips, employment) %>%
  ggplot(aes(x = employment, y = total_work_trips, fill = employment)) +
  geom_col() +
  labs(title = "Total Work Trips by Employment Status", x = "Employment", y = "Weighted Work Trips") +
  theme_minimal() +
  theme(legend.position = "none")

# 4. Work trips by income group
p_income <- sum_work_trips(df_work_trips, income_group) %>%
  ggplot(aes(x = reorder(income_group, total_work_trips), y = total_work_trips, fill = income_group)) +
  geom_col() +
  labs(title = "Total Work Trips by Income Group", x = "Income Group", y = "Weighted Work Trips") +
  coord_flip() +
  theme_minimal() +
  theme(legend.position = "none")

# 5. Work trips by gender
p_gender <- sum_work_trips(df_work_trips, gesl) %>%
  ggplot(aes(x = gesl, y = total_work_trips, fill = gesl)) +
  geom_col() +
  labs(title = "Total Work Trips by Gender", x = "Gender", y = "Weighted Work Trips") +
  theme_minimal() +
  theme(legend.position = "none")

# 6. Work trips by age group
df_age_long <- df_work_trips %>%
  pivot_longer(
    cols = starts_with("n_age_"),
    names_to = "age_group",
    values_to = "is_in_group"
  ) %>%
  filter(is_in_group == 1)

# Optional: Rename age group labels for readability
df_age_long <- df_age_long %>%
  mutate(age_group = recode(age_group,
                            "n_age_0_17" = "0–17",
                            "n_age_18_24" = "18–24",
                            "n_age_25_44" = "25–44",
                            "n_age_45_64" = "45–64",
                            "n_age_65_74" = "65–74",
                            "n_age_75p"   = "75+"
  ))

p_age <- df_age_long %>%
  group_by(age_group) %>%
  summarise(total_work_trips = sum(WP, na.rm = TRUE), .groups = "drop") %>%
  ggplot(aes(x = age_group, y = total_work_trips, fill = age_group)) +
  geom_col() +
  labs(title = "Total Work Trips by Age Group", x = "Age Group", y = "Weighted Work Trips") +
  theme_minimal() +
  theme(legend.position = "none")

# ---- Display the plots
print(p_car)
print(p_pt)
print(p_emp)
print(p_income)
print(p_gender)
print(p_age)

# ---- Save all plots to disk
ggsave(file.path(dir_figures, "work_trips_by_car.png"), plot = p_car, width = 6, height = 4, dpi = 300)
ggsave(file.path(dir_figures, "work_trips_by_pt.png"), plot = p_pt, width = 6, height = 4, dpi = 300)
ggsave(file.path(dir_figures, "work_trips_by_employment.png"), plot = p_emp, width = 6, height = 4, dpi = 300)
ggsave(file.path(dir_figures, "work_trips_by_income.png"), plot = p_income, width = 7, height = 5, dpi = 300)
ggsave(file.path(dir_figures, "work_trips_by_gender.png"), plot = p_gender, width = 6, height = 4, dpi = 300)
ggsave(file.path(dir_figures, "work_trips_by_age.png"), plot = p_age, width = 6, height = 4, dpi = 300)

# ---- Print total weighted work trips

total_weighted_work_trips <- df_work_trips %>%
  summarise(total = sum(WP, na.rm = TRUE)) %>%
  pull(total)

cat("✅ Total weighted work trips:", round(total_weighted_work_trips), "\n")

# ---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
### Step 5: Estimate Work Trip Models ----

# ---- Assemble household-level predictors prepared earlier in the workflow

if ("person_id" %in% names(df_cleaned)) {
  df_people_clean <- df_cleaned %>%
    arrange(hh_id, person_id) %>%
    distinct(hh_id, person_id, .keep_all = TRUE)
} else {
  df_people_clean <- df_cleaned %>%
    distinct(
      hh_id,
      alter,
      gesl,
      employment,
      car_avail,
      pt_available,
      .keep_all = TRUE
    )
}

df_household_summary <- df_people_clean %>%
  group_by(hh_id) %>%
  summarise(
    n_age_0_17 = sum(n_age_0_17, na.rm = TRUE),
    n_age_18_24 = sum(n_age_18_24, na.rm = TRUE),
    n_age_25_44 = sum(n_age_25_44, na.rm = TRUE),
    n_age_45_64 = sum(n_age_45_64, na.rm = TRUE),
    n_age_65_74 = sum(n_age_65_74, na.rm = TRUE),
    n_age_75p = sum(n_age_75p, na.rm = TRUE),
    female_share = {
      if (all(is.na(gesl))) NA_real_ else mean(gesl == "female", na.rm = TRUE)
    },
    employed_share = {
      if (all(is.na(employment))) NA_real_ else mean(employment == "employed", na.rm = TRUE)
    },
    car_any = any(car_avail == "Available", na.rm = TRUE),
    car_all_na = all(is.na(car_avail)),
    pt_any = any(pt_available == "Available", na.rm = TRUE),
    pt_all_na = all(is.na(pt_available)),
    .groups = "drop"
  ) %>%
  mutate(
    female_share = if_else(is.nan(female_share), NA_real_, female_share),
    employed_share = if_else(is.nan(employed_share), NA_real_, employed_share),
    car_group = case_when(
      car_all_na ~ NA_character_,
      car_any ~ "Available",
      TRUE ~ "Unavailable"
    ),
    pt_group = case_when(
      pt_all_na ~ NA_character_,
      pt_any ~ "Available",
      TRUE ~ "Unavailable"
    )
  ) %>%
  dplyr::select(-car_any, -car_all_na, -pt_any, -pt_all_na)

df_household_predictors <- df_hh_reduced %>%
  distinct(hh_id) %>%
  left_join(df_household_summary, by = "hh_id")

income_levels <- c(
  "Under CHF 2000",
  "CHF 2000 to 4000",
  "CHF 4001 to 6000",
  "CHF 6001 to 8000",
  "CHF 8001 to 10000",
  "CHF 10001 to 12000",
  "CHF 12001 to 14000",
  "CHF 14001 to 16000",
  "More than CHF 16000"
)

df_income_lookup <- df_hh_reduced %>%
  mutate(
    income_group = case_when(
      f20601 == 1 ~ income_levels[1],
      f20601 == 2 ~ income_levels[2],
      f20601 == 3 ~ income_levels[3],
      f20601 == 4 ~ income_levels[4],
      f20601 == 5 ~ income_levels[5],
      f20601 == 6 ~ income_levels[6],
      f20601 == 7 ~ income_levels[7],
      f20601 == 8 ~ income_levels[8],
      f20601 == 9 ~ income_levels[9],
      TRUE ~ NA_character_
    )
  ) %>%
  dplyr::select(hh_id, income_group)

df_work_model_input <- df_household_predictors %>%
  left_join(df_income_lookup, by = "hh_id") %>%
  left_join(df_work_counts %>% dplyr::select(hh_id, n_trips_work), by = "hh_id") %>%
  mutate(
    n_work = coalesce(n_trips_work, 0L),
    car_group = factor(car_group, levels = c("Unavailable", "Available")),
    pt_group = factor(pt_group, levels = c("Unavailable", "Available")),
    income_group = factor(income_group, levels = income_levels, ordered = TRUE)
  ) %>%
  dplyr::select(
    hh_id,
    n_work,
    n_age_0_17,
    n_age_18_24,
    n_age_25_44,
    n_age_45_64,
    n_age_65_74,
    n_age_75p,
    car_group,
    pt_group,
    female_share,
    employed_share,
    income_group
  ) %>%
  drop_na()

cat("✅ Households available for modelling:", nrow(df_work_model_input), "\n")

write_csv(df_work_model_input, file.path(dir_tables, "df_work_model_input.csv"))

# ---- Define model specifications

model_specs <- list(
  basic = list(
    description = "Age + car/PT availability",
    formula = n_work ~ n_age_0_17 + n_age_18_24 + n_age_25_44 + n_age_45_64 +
      n_age_65_74 + n_age_75p + car_group + pt_group
  ),
  full = list(
    description = "Age + car/PT availability + gender, employment, income",
    formula = n_work ~ n_age_0_17 + n_age_18_24 + n_age_25_44 + n_age_45_64 +
      n_age_65_74 + n_age_75p + car_group + pt_group + female_share +
      employed_share + income_group
  )
)

fit_count_models <- function(model_name, model_formula, data) {
  message("\n--- Fitting ", model_name, " model: ", deparse(model_formula), " ---")
  
  mean_trips <- mean(data$n_work)
  var_trips <- var(data$n_work)
  
  model_poisson <- glm(model_formula, family = poisson(), data = data)
  pearson_overdispersion <- sum(residuals(model_poisson, type = "pearson")^2) /
    df.residual(model_poisson)
  
  model_negbin <- glm.nb(model_formula, data = data)
  
  comparison <- tibble(
    model = c("Poisson", "NegBin"),
    logLik = c(logLik(model_poisson), logLik(model_negbin)),
    AIC = c(AIC(model_poisson), AIC(model_negbin))
  )
  
  irr <- broom::tidy(model_negbin, exponentiate = TRUE, conf.int = TRUE)
  anova_nb <- broom::tidy(anova(model_negbin, test = "Chisq"))
  overdispersion_stats <- tibble(
    model = model_name,
    mean_trips = mean_trips,
    var_trips = var_trips,
    pearson_overdispersion = pearson_overdispersion
  )
  
  cat("Mean =", round(mean_trips, 3), " Var =", round(var_trips, 3), "\n")
  cat("Poisson Pearson overdispersion =", round(pearson_overdispersion, 2), "\n")
  cat(
    "NB theta =",
    round(model_negbin$theta, 3),
    "(higher ~ closer to Poisson; lower ~ more overdispersion)\n"
  )
  print(as.data.frame(dplyr::arrange(comparison, AIC)), row.names = FALSE)
  print(anova_nb)
  print(dplyr::select(irr, term, estimate, conf.low, conf.high, p.value))
  
  saveRDS(model_poisson, file = file.path(dir_models, paste0(model_name, "_poisson.rds")))
  saveRDS(model_negbin, file = file.path(dir_models, paste0(model_name, "_negbin.rds")))
  
  write_csv(irr, file.path(dir_models, paste0(model_name, "_nb_model_IRRs.csv")))
  write_csv(comparison, file.path(dir_models, paste0(model_name, "_model_comparison.csv")))
  write_csv(overdispersion_stats, file.path(dir_models, paste0(model_name, "_poisson_overdispersion.csv")))
  write_csv(anova_nb, file.path(dir_models, paste0(model_name, "_nb_model_anova.csv")))
  
  list(
    poisson = model_poisson,
    negbin = model_negbin,
    comparison = comparison,
    irr = irr,
    overdispersion = overdispersion_stats,
    anova = anova_nb
  )
}

model_results <- purrr::imap(model_specs, ~ fit_count_models(.y, .x$formula, df_work_model_input))

# ---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
### Step 6: Apply your model to the Region

basic_work_model <- model_results$basic$negbin

# Helper lookups for the age brackets and access categories available in the
# structural data. Each "ANZPERSONEN(R_<age code>_<access code>)" column holds the
# population count for an age group combined with a car/PT availability
# configuration.
age_brackets <- c(
  n_age_0_17 = "R_0017",
  n_age_18_24 = "R_1824",
  n_age_25_44 = "R_2544",
  n_age_45_64 = "R_4564",
  n_age_65_74 = "R_6574",
  n_age_75p  = "R_75XX"
)

access_configurations <- tibble::tribble(
  ~car_group,     ~pt_group,     ~suffix,
  "Available",    "Available",   "CARTC",
  "Available",    "Unavailable", "CARNOTC",
  "Unavailable",  "Available",   "NOCTC",
  "Unavailable",  "Unavailable", "NOCNOTC"
)

# Build one prediction record per zone × access configuration. Each record keeps
# the car/PT factors along with the proportional age composition derived from
# the structural counts. The raw counts are retained as `weight` so that the
# modelled rate can be scaled back up to the zone population afterwards.
predict_components <- purrr::map_dfr(
  seq_len(nrow(Strukturdaten)),
  function(row_id) {
    zone_row <- Strukturdaten[row_id, ]
    
    access_configurations %>%
      purrr::pmap_dfr(function(car_group, pt_group, suffix) {
        age_cols <- sprintf("ANZPERSONEN(%s_%s)", age_brackets, suffix)
        missing_cols <- setdiff(age_cols, names(zone_row))
        
        if (length(missing_cols) > 0) {
          stop("Missing expected structural columns: ", paste(missing_cols, collapse = ", "))
        }
        
        age_counts <- purrr::map_dbl(
          age_cols,
          ~ {
            value <- zone_row[[.x]]
            if (is.null(value) || is.na(value)) 0 else as.numeric(value)
          }
        )
        total_people <- sum(age_counts, na.rm = TRUE)
        
        if (is.na(total_people) || total_people <= 0) {
          return(NULL)
        }
        
        tibble::tibble(
          zone_index = row_id,
          car_group = factor(car_group, levels = c("Unavailable", "Available")),
          pt_group = factor(pt_group, levels = c("Unavailable", "Available")),
          weight = total_people
        ) %>%
          dplyr::bind_cols(stats::setNames(as.list(age_counts), names(age_brackets)))
      })
  }
)

if (nrow(predict_components) == 0) {
  stop("Structural data did not yield any records for prediction.")
}

# Convert age counts to per-person proportions so that the basic household model
# receives inputs on the same scale as it was trained. Predictions are then
# multiplied by the population weight per record.
predict_data <- predict_components %>%
  dplyr::mutate(
    dplyr::across(dplyr::all_of(names(age_brackets)), ~ .x / weight)
  )

# Optional diagnostics: aggregate the implied population totals per age group
# prior to prediction.
age_group_totals <- predict_components %>%
  dplyr::summarise(
    dplyr::across(dplyr::all_of(names(age_brackets)), ~ sum(.x, na.rm = TRUE))
  ) %>%
  as.list()

cat("✅ Total people assigned to model by age group:\n")
print(age_group_totals)

prediction_matrix <- predict_data %>%
  dplyr::select(dplyr::all_of(names(age_brackets)), car_group, pt_group)

predicted_rates <- predict(basic_work_model, newdata = prediction_matrix, type = "response")

zone_predictions <- predict_data %>%
  dplyr::mutate(predicted_trips = predicted_rates * weight) %>%
  dplyr::group_by(zone_index) %>%
  dplyr::summarise(trips_work = sum(predicted_trips, na.rm = TRUE), .groups = "drop")

Strukturdaten$trips_work <- zone_predictions$trips_work[match(seq_len(nrow(Strukturdaten)), zone_predictions$zone_index)]

# ---- Merge into spatial data
npvm_zones_ZH$trips_work <- Strukturdaten$trips_work
npvm_zones_ZH$attractor_work <- Strukturdaten$FTE

# ------------------------------------------------------------------------------------------------------------------
### Step 6a: Visualize and Save Plots

# Plot 1: Work trip production
p1 <- ggplot() +
  geom_sf(data = npvm_zones_ZH, aes(fill = trips_work)) +
  scale_fill_gradient(low = "lightyellow", high = "red") +
  labs(title = "Predicted Work Trips per Zone", fill = "Trips")

ggsave(file.path(dir_figures, "predicted_trips_work.png"), plot = p1, width = 8, height = 6)

# Plot 2: Employment attractor
p2 <- ggplot() +
  geom_sf(data = npvm_zones_ZH, aes(fill = attractor_work)) +
  scale_fill_gradient(low = "lightblue", high = "darkblue") +
  labs(title = "Employment Attractor (FTE)", fill = "FTE")

ggsave(file.path(dir_figures, "employment_attractor_FTE.png"), plot = p2, width = 8, height = 6)

# ------------------------------------------------------------------------------------------------------------------
### Step 6b: Save GeoPackage

output_gpkg <- file.path(dir_maps, "npvm_zones_with_trips.gpkg")
write_sf(npvm_zones_ZH, output_gpkg)
