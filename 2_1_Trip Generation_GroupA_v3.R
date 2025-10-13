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

'
na_summary <- sort(colSums(is.na(df_to_TEST)), decreasing = TRUE)
na_summary[na_summary > 0]   # show only columns that have at least one NA

# ------------------------------------------------------------------------------------------------------------------
### Export attribute tables

# Create attribute data frames
df_tripsCH_attributes      <- data.frame(names(df_tripsCH))
df_hh_attributes           <- data.frame(names(df_hh))
df_persons_attributes      <- data.frame(names(df_persons))
Strukturdaten_attributes   <- data.frame(names(Strukturdaten))
npvm_zones_ZH_attributes   <- data.frame(names(npvm_zones_ZH))

dir_export <- file.path(dir_out, "attributes")
dir.create(dir_export, showWarnings = FALSE)

# Write to CSV
write.csv(df_tripsCH_attributes,    file.path(dir_export, "df_tripsCH_attributes.csv"),    row.names = FALSE)
write.csv(df_hh_attributes,         file.path(dir_export, "df_hh_attributes.csv"),         row.names = FALSE)
write.csv(df_persons_attributes,    file.path(dir_export, "df_persons_attributes.csv"),    row.names = FALSE)
write.csv(Strukturdaten_attributes,file.path(dir_export, "Strukturdaten_attributes.csv"), row.names = FALSE)
write.csv(npvm_zones_ZH_attributes,file.path(dir_export, "npvm_zones_ZH_attributes.csv"), row.names = FALSE)
'
# --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------- 
### Step 2: Combine Data

## Combine
#df_zp_hh <- full_join(df_hh, df_persons)
#df_zp_hh_attributes <- data.frame(names(df_zp_hh))

## trips by household & purpose
df_n_trips_hh_all <- df_tripsCH %>%
  filter(wzweck1 > 0) %>%
  group_by(HHNR, wzweck1) %>%
  summarise(n_trips = n(), .groups = "drop")

## house hold attributes
hh_attributes <- df_persons %>%
  group_by(HHNR) %>%
  summarise(
    # age composition (adjust age column name if different)
    n_age_0_17  = sum(alter <= 17, na.rm = TRUE),
    n_age_18_24 = sum(alter >= 18 & alter <= 24, na.rm = TRUE),
    n_age_25_44 = sum(alter >= 25 & alter <= 44, na.rm = TRUE),
    n_age_45_64 = sum(alter >= 45 & alter <= 64, na.rm = TRUE),
    n_age_65_74 = sum(alter >= 65 & alter <= 74, na.rm = TRUE),
    n_age_75p   = sum(alter >= 75, na.rm = TRUE),
    
    
    hh_car_avail  = as.integer(any(f42100e %in% c(1,2), na.rm = TRUE)), # household car availability: any person indicates car availability
    hh_pt_avail   = as.integer(any(dplyr::if_any(f41600_01a:f41600_01g, ~ .x == 1), na.rm = TRUE)), # household PT subscription: any person has an abo in any of these cols
    hh_employed = as.integer(any(f40800_01 %in% c(1,2,3,4,5), na.rm = TRUE)) # employment
  )

hh_size_from_hh <- df_hh %>%
  transmute(HHNR, hh_size = hhgr)

## Base HH table (one row per household) + predictors
df_hh_base <- df_hh %>%
  left_join(hh_attributes, by = "HHNR") %>%
  left_join(hh_size_from_hh, by = "HHNR") %>%
  mutate(
    car_group = factor(if_else(hh_car_avail == 1, "CAR", "NO_CAR"), levels = c("NO_CAR","CAR")),
    pt_group  = factor(if_else(hh_pt_avail  == 1, "PT",  "NO_PT"), levels = c("NO_PT","PT"))
  )

## checks & cleanup
stopifnot(all(c("HHNR","car_group","pt_group","hh_size") %in% names(df_hh_base)))
table(df_hh_base$car_group, useNA="ifany")
table(df_hh_base$pt_group,  useNA="ifany")

rm(hh_size_from_hh)
rm(hh_attributes)

# ---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
### Step 3: Clean up & remove NAs

## clean up attributes
predictor_cols <- c("n_age_0_17","n_age_18_24","n_age_25_44","n_age_45_64","n_age_65_74","n_age_75p",
                    "hh_car_avail","hh_pt_avail","hh_employed","hh_size","car_group","pt_group")

df_hh_clean <- df_hh_base %>% drop_na(all_of(predictor_cols))

# Households that appear in trips at all (any record)
hh_any_trip <- df_tripsCH %>%
  distinct(HHNR)

# Households with at least one valid purpose (>0)
hh_with_valid_purpose <- df_tripsCH %>%
  filter(!is.na(wzweck1) & wzweck1 > 0) %>%
  distinct(HHNR)

# Households whose trips exist but have NO valid purpose (only NA/invalid)
hh_only_na <- hh_any_trip %>% 
  anti_join(hh_with_valid_purpose, by = "HHNR")

# Drop ONLY those
n_before <- nrow(df_hh_clean)
df_hh_clean <- df_hh_clean %>%
  anti_join(hh_only_na, by = "HHNR")
n_after <- nrow(df_hh_clean)

message("Households kept after NA drop: ", n_after," (dropped: ", n_before - n_after, ")")
rm(n_before,n_after,hh_only_na,hh_with_valid_purpose,hh_any_trip)


na_counts <- df_hh_base %>%
  summarise(across(all_of(predictor_cols), ~sum(is.na(.))))
print(na_counts)
rm(na_counts)


# ---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
### Step 4: filter for work

work_codes <- c(2L,6L)
table(df_n_trips_hh_all$wzweck1)

df_work_counts <- df_n_trips_hh_all %>%
  filter(wzweck1 %in% work_codes) %>%
  group_by(HHNR) %>%
  summarise(n_trips_work = sum(n_trips), .groups = "drop")

df_work <- df_hh_clean %>%
  left_join(df_work_counts, by = "HHNR") %>%
  mutate(n_work = coalesce(n_trips_work, 0L)) ##

# ---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
### Step 5: Model Choice

summary(df_work$n_work)
mc_mean_n <- mean(df_work$n_work); mc_var_n <- var(df_work$n_work) #mc = model check
message("mean=", round(mc_mean_n,3), " var=", round(mc_var_n,3), 
        " (", ifelse(mc_var_n > 1.5*mc_mean_n, "use NegBin", "Poisson ok"), ")")


## more complicated

mc_form <- n_work ~ n_age_0_17 + n_age_18_24 + n_age_25_44 +
  n_age_45_64 + n_age_65_74 + n_age_75p +
  car_group + pt_group

# Fit Poisson and check overdispersion
mc_m_pois <- glm(mc_form, family = poisson(), data = df_work)

mc_pearson_od <- sum(residuals(mc_m_pois, type = "pearson")^2) / df.residual(mc_m_pois)

cat("Mean =", round(mc_mean_n,3), " Var =", round(mc_var_n,3), "\n")
cat("Poisson Pearson overdispersion =", round(mc_pearson_od, 2), "\n")

# Fit Negative Binomial
mc_m_nb <- glm.nb(mc_form, data = df_work)
mc_theta <- mc_m_nb$theta
cat("NB theta =", round(mc_theta,3), "(higher ~ closer to Poisson; lower ~ more overdispersion)\n")

# Compare fit (LogLik & AIC)
mc_comp <- data.frame(
  model = c("Poisson","NegBin"),
  logLik = c(logLik(mc_m_pois), logLik(mc_m_nb)),
  AIC    = c(AIC(mc_m_pois),    AIC(mc_m_nb))
)
print(mc_comp[order(mc_comp$AIC), ], row.names = FALSE)

# Likelihood-ratio comparison
anova(mc_m_pois, mc_m_nb, test = "Chisq")

# Parsimony checks (ANOVA)
anova(mc_m_nb, test = "Chisq")

# Coefficients as IRRs (incident rate ratios)
irr <- broom::tidy(mc_m_nb, exponentiate = TRUE, conf.int = TRUE)
print(irr[, c("term","estimate","conf.low","conf.high","p.value")])

### Save model objects
saveRDS(mc_m_pois, file = file.path(dir_models, "model_poisson.rds"))
saveRDS(mc_m_nb,   file = file.path(dir_models, "model_negbin.rds"))

### Save IRRs as CSV
write.csv(irr[, c("term","estimate","conf.low","conf.high","p.value")],
          file = file.path(dir_models, "nb_model_IRRs.csv"), row.names = FALSE)

### Save model comparison table
write.csv(mc_comp, file = file.path(dir_models, "model_comparison.csv"), row.names = FALSE)

### Save work trip data used for model estimation
write.csv(df_work, file = file.path(dir_tables, "df_work_model_input.csv"), row.names = FALSE)

### Save cleaned household data for future use or validation
write.csv(df_hh_clean, file = file.path(dir_models, "df_hh_clean.csv"), row.names = FALSE)

### Save Poisson overdispersion test
overdisp_stats <- data.frame(
  model = "Poisson",
  mean_trips = mc_mean_n,
  var_trips = mc_var_n,
  pearson_overdispersion = mc_pearson_od
)
write.csv(overdisp_stats, file = file.path(dir_models, "poisson_overdispersion.csv"), row.names = FALSE)

### Save model ANOVA (NegBin)
anova_nb <- broom::tidy(anova(mc_m_nb, test = "Chisq"))
write.csv(anova_nb, file = file.path(dir_models, "nb_model_anova.csv"), row.names = FALSE)

# ---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
### Step 6: Apply your model to the Region

# Build predictor data from detailed age × mode access categories
predict_data <- Strukturdaten %>%
  transmute(
    # Age groups - totals per category
    n_age_0_17 = `ANZPERSONEN(R_0017_CARTC)` + `ANZPERSONEN(R_0017_CARNOTC)` +
      `ANZPERSONEN(R_0017_NOCTC)` + `ANZPERSONEN(R_0017_NOCNOTC)`,
    
    n_age_18_24 = `ANZPERSONEN(R_1824_CARTC)` + `ANZPERSONEN(R_1824_CARNOTC)` +
      `ANZPERSONEN(R_1824_NOCTC)` + `ANZPERSONEN(R_1824_NOCNOTC)`,
    
    n_age_25_44 = `ANZPERSONEN(R_2544_CARTC)` + `ANZPERSONEN(R_2544_CARNOTC)` +
      `ANZPERSONEN(R_2544_NOCTC)` + `ANZPERSONEN(R_2544_NOCNOTC)`,
    
    n_age_45_64 = `ANZPERSONEN(R_4564_CARTC)` + `ANZPERSONEN(R_4564_CARNOTC)` +
      `ANZPERSONEN(R_4564_NOCTC)` + `ANZPERSONEN(R_4564_NOCNOTC)`,
    
    n_age_65_74 = `ANZPERSONEN(R_6574_CARTC)` + `ANZPERSONEN(R_6574_CARNOTC)` +
      `ANZPERSONEN(R_6574_NOCTC)` + `ANZPERSONEN(R_6574_NOCNOTC)`,
    
    n_age_75p   = `ANZPERSONEN(R_75XX_CARTC)` + `ANZPERSONEN(R_75XX_CARNOTC)` +
      `ANZPERSONEN(R_75XX_NOCTC)` + `ANZPERSONEN(R_75XX_NOCNOTC)`,
    
    # Intermediate counts to determine mode access groupings
    car_available = `ANZPERSONEN(R_0017_CARTC)` + `ANZPERSONEN(R_0017_CARNOTC)` +
      `ANZPERSONEN(R_1824_CARTC)` + `ANZPERSONEN(R_1824_CARNOTC)` +
      `ANZPERSONEN(R_2544_CARTC)` + `ANZPERSONEN(R_2544_CARNOTC)` +
      `ANZPERSONEN(R_4564_CARTC)` + `ANZPERSONEN(R_4564_CARNOTC)` +
      `ANZPERSONEN(R_6574_CARTC)` + `ANZPERSONEN(R_6574_CARNOTC)` +
      `ANZPERSONEN(R_75XX_CARTC)` + `ANZPERSONEN(R_75XX_CARNOTC)`,
    
    pt_available  = `ANZPERSONEN(R_0017_CARTC)` + `ANZPERSONEN(R_0017_NOCTC)` +
      `ANZPERSONEN(R_1824_CARTC)` + `ANZPERSONEN(R_1824_NOCTC)` +
      `ANZPERSONEN(R_2544_CARTC)` + `ANZPERSONEN(R_2544_NOCTC)` +
      `ANZPERSONEN(R_4564_CARTC)` + `ANZPERSONEN(R_4564_NOCTC)` +
      `ANZPERSONEN(R_6574_CARTC)` + `ANZPERSONEN(R_6574_NOCTC)` +
      `ANZPERSONEN(R_75XX_CARTC)` + `ANZPERSONEN(R_75XX_NOCTC)`,
    
    # Model grouping factors
    car_group = factor(if_else(car_available > 0, "CAR", "NO_CAR"), levels = c("NO_CAR", "CAR")),
    pt_group  = factor(if_else(pt_available  > 0, "PT",  "NO_PT"),  levels = c("NO_PT",  "PT"))
  )

# ---- Optional: Print total population per age group to validate
age_group_totals <- colSums(predict_data[, c("n_age_0_17", "n_age_18_24", "n_age_25_44",
                                             "n_age_45_64", "n_age_65_74", "n_age_75p")],
                            na.rm = TRUE)

cat("✅ Total people assigned to model by age group:\n")
print(age_group_totals)

# ---- Important: Keep only predictor variables (exclude response var)
predict_vars <- labels(terms(mc_m_nb))  # exclude "n_work"
predict_data_model <- dplyr::select(predict_data, all_of(predict_vars))

# ---- Predict work trips
Strukturdaten$trips_work <- predict(mc_m_nb, newdata = predict_data_model, type = "response")

# ---- Merge into spatial data
npvm_zones_ZH$trips_work     <- Strukturdaten$trips_work
npvm_zones_ZH$attractor_work <- Strukturdaten$FTE

# ---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
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

# ---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
### Step 6b: Save GeoPackage

output_gpkg <- file.path(dir_maps, "npvm_zones_with_trips.gpkg")
write_sf(npvm_zones_ZH, output_gpkg)
