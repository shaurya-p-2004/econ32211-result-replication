# ============================================================
# REPLICATION SCRIPT
# Does Public EV Charging Infrastructure Drive BEV Adoption?
# Evidence from English Local Authorities
#
# ECON32211 Applied Economics Dissertation
# Student ID: 11020506
#
# R version: 4.5.3
# Key packages: tidyverse, fixest, janitor, ggplot2, scales
#
# HOW TO REPLICATE:
#   1. Place panel_clean.csv in a folder called data_out/
#      in the same directory as this script
#   2. Open this script in RStudio
#   3. Set working directory to the folder containing
#      this script: Session -> Set Working Directory ->
#      To Source File Location
#   4. Run the script in full (Ctrl+Alt+R / Cmd+Alt+R)
#   All tables are printed to the console and all figures
#   are saved to data_out/
# ============================================================


# ---- 0) LIBRARIES ----

library(tidyverse)
library(fixest)
library(janitor)
library(ggplot2)
library(scales)

options(scipen = 999)

# ---- 1) LOAD DATA AND CONSTRUCT BARTIK INSTRUMENT ----

panel <- readRDS("data_out/panel_clean.rds") %>%
  select(-any_of(c("base_density", "loo_growth", "bartik_iv",
                   "bartik_iv_lag1", "devices_lag1", "year_num",
                   "mean_gdhi", "deprivation_tercile", "income_tercile",
                   "gdhi_pc_1000", "most_deprived_dummy", "least_deprived_dummy",
                   "low_income_dummy", "high_income_dummy", "urban_dummy",
                   "chargers_x_most_deprived", "chargers_x_least_deprived",
                   "chargers_x_low_income", "chargers_x_high_income",
                   "chargers_x_urban", "bartik_x_most_deprived",
                   "bartik_x_least_deprived", "bartik_x_low_income",
                   "bartik_x_high_income", "bartik_x_urban")))

# Baseline 2019 charger density (the "share" component)
baseline_2019 <- panel %>%
  filter(year == 2019) %>%
  select(lad_code, base_density = devices_per_1000_pop)

# National totals by year
nat_by_year <- panel %>%
  group_by(year) %>%
  summarise(nat_total = sum(devices_eoy, na.rm = TRUE),
            .groups = "drop")

nat_2019 <- nat_by_year %>%
  filter(year == 2019) %>%
  pull(nat_total)

# 2019 device counts per LA (needed for leave-one-out)
devices_2019 <- panel %>%
  filter(year == 2019) %>%
  select(lad_code, devices_2019 = devices_eoy)

# Leave-one-out national growth rates
# Each LA's instrument is constructed from all other LAs'
# charger trajectories, preventing mechanical endogeneity
loo_data <- panel %>%
  select(lad_code, year, devices_eoy) %>%
  left_join(nat_by_year, by = "year") %>%
  left_join(devices_2019, by = "lad_code") %>%
  mutate(
    nat_excl_t    = nat_total - replace_na(devices_eoy, 0),
    nat_excl_2019 = nat_2019  - replace_na(devices_2019, 0),
    loo_growth    = nat_excl_t / nat_excl_2019
  ) %>%
  select(lad_code, year, loo_growth)

# Merge Bartik instrument and derived variables into panel
panel <- panel %>%
  left_join(baseline_2019, by = "lad_code") %>%
  left_join(loo_data, by = c("lad_code", "year")) %>%
  mutate(
    # Bartik shift-share instrument: Z_it = base_density_i * loo_growth_t
    bartik_iv     = base_density * loo_growth,
    year_num      = as.numeric(year),
    # Lagged variables for robustness checks
    devices_lag1   = lag(devices_per_1000_pop, 1),
    bartik_iv_lag1 = lag(bartik_iv, 1),
    # Income scaled to £1,000s for coefficient readability
    gdhi_pc_1000 = gdhi_pc / 1000,
    # Deprivation and income terciles
    # ntile on negative IMD so tercile 1 = most deprived
    deprivation_tercile = ntile(-imd25_decile_mean, 3),
    income_tercile      = ntile(gdhi_pc, 3),
    # Group dummies for interaction 2SLS specifications
    most_deprived_dummy  = as.integer(deprivation_tercile == 1),
    least_deprived_dummy = as.integer(deprivation_tercile == 3),
    low_income_dummy     = as.integer(income_tercile == 1),
    high_income_dummy    = as.integer(income_tercile == 3),
    urban_dummy          = as.integer(is_rural == 0),
    # Interaction terms: endogenous variable × group dummy
    chargers_x_most_deprived  = devices_per_1000_pop * most_deprived_dummy,
    chargers_x_least_deprived = devices_per_1000_pop * least_deprived_dummy,
    chargers_x_low_income     = devices_per_1000_pop * low_income_dummy,
    chargers_x_high_income    = devices_per_1000_pop * high_income_dummy,
    chargers_x_urban          = devices_per_1000_pop * urban_dummy,
    # Instrument interaction terms
    bartik_x_most_deprived  = bartik_iv * most_deprived_dummy,
    bartik_x_least_deprived = bartik_iv * least_deprived_dummy,
    bartik_x_low_income     = bartik_iv * low_income_dummy,
    bartik_x_high_income    = bartik_iv * high_income_dummy,
    bartik_x_urban          = bartik_iv * urban_dummy
  )

cat("Panel loaded:", nrow(panel), "rows,",
    n_distinct(panel$lad_code), "LAs,",
    n_distinct(panel$year), "years\n")
cat("Bartik IV missing:", sum(is.na(panel$bartik_iv)), "\n")


# ---- 2) DESCRIPTIVE STATISTICS ----

cat("\n=== DESCRIPTIVE STATISTICS ===\n")

key_vars <- c("bev_share_cars", "bev_per_1000_pop",
              "devices_per_1000_pop", "gdhi_pc",
              "car_fuel_per_1000_pop", "imd25_decile_mean",
              "imd25_share_1_2", "no_offstreet_share",
              "is_rural", "orcs_cumulative",
              "evhs_per_1000_pop", "pop", "total_cars")

desc_stats <- panel %>%
  summarise(across(
    all_of(intersect(key_vars, names(panel))),
    list(
      n      = ~ sum(!is.na(.)),
      mean   = ~ round(mean(., na.rm = TRUE), 4),
      sd     = ~ round(sd(., na.rm = TRUE), 4),
      min    = ~ round(min(., na.rm = TRUE), 4),
      median = ~ round(median(., na.rm = TRUE), 4),
      max    = ~ round(max(., na.rm = TRUE), 4)
    ),
    .names = "{.col}__{.fn}"
  )) %>%
  pivot_longer(everything(),
               names_to  = c("variable", "stat"),
               names_sep = "__") %>%
  pivot_wider(names_from = stat) %>%
  arrange(variable)

print(desc_stats, n = Inf, width = Inf)

cat("\n--- Key Variables Over Time ---\n")
panel %>%
  group_by(year) %>%
  summarise(
    mean_bev_share    = round(mean(bev_share_cars, na.rm = TRUE), 4),
    mean_devices_1000 = round(mean(devices_per_1000_pop, na.rm = TRUE), 4),
    mean_gdhi         = round(mean(gdhi_pc, na.rm = TRUE), 0),
    mean_orcs         = round(mean(orcs_cumulative, na.rm = TRUE), 2),
    .groups = "drop"
  ) %>%
  print()


# ---- 3) OLS BASELINE ----

cat("\n=== OLS BASELINE ===\n")

ols_baseline <- feols(
  bev_share_cars ~ devices_per_1000_pop +
    gdhi_pc_1000 + car_fuel_per_1000_pop |
    lad_code + year,
  data    = panel,
  cluster = ~lad_code
)
summary(ols_baseline)


# ---- 4) BARTIK INSTRUMENT DIAGNOSTICS ----

cat("\n=== BARTIK INSTRUMENT DIAGNOSTICS ===\n")

# Leave-one-out growth rates should be near-identical across LAs
# in any given year — confirms no single LA dominates the national total
cat("--- LOO Growth Rates by Year ---\n")
panel %>%
  group_by(year) %>%
  summarise(
    mean_growth = round(mean(loo_growth, na.rm = TRUE), 3),
    min_growth  = round(min(loo_growth, na.rm = TRUE), 3),
    max_growth  = round(max(loo_growth, na.rm = TRUE), 3),
    .groups = "drop"
  ) %>%
  print()

cat("\nCorrelation Bartik vs actual charger density:",
    round(cor(panel$bartik_iv, panel$devices_per_1000_pop,
              use = "complete.obs"), 3), "\n")

# First stage regression
fs_bartik <- feols(
  devices_per_1000_pop ~ bartik_iv +
    gdhi_pc + car_fuel_per_1000_pop |
    lad_code + year,
  data    = panel,
  cluster = ~lad_code
)

cat("\n--- First Stage ---\n")
summary(fs_bartik)

cat("\nWald F on Bartik:\n")
print(wald(fs_bartik, "bartik_iv"))

# Falsification test: instrument should not predict pre-period
# adoption levels in cross-section once controls are included
cat("\n--- Falsification Test (2019 cross-section) ---\n")
falsification <- feols(
  bev_share_cars ~ bartik_iv +
    gdhi_pc + car_fuel_per_1000_pop +
    imd25_decile_mean + no_offstreet_share,
  data    = panel %>% filter(year == 2019),
  cluster = ~lad_code
)
summary(falsification)

# Pre-trends check: 2019 baseline density vs 2019-2020 BEV growth
pre_trends <- panel %>%
  filter(year %in% c(2019, 2020)) %>%
  select(lad_code, year, bev_share_cars, base_density) %>%
  pivot_wider(names_from  = year,
              values_from = bev_share_cars,
              names_prefix = "bev_") %>%
  mutate(bev_growth_2019_2020 = bev_2020 - bev_2019)

cat("\nPre-trends correlation (2019 charger density vs 2019-2020 BEV growth):\n")
cat(round(cor(pre_trends$base_density,
              pre_trends$bev_growth_2019_2020,
              use = "complete.obs"), 3), "\n")

# LAs with zero chargers in 2019 — these receive a Bartik instrument
# of zero throughout the panel and cannot contribute to identification
zero_2019 <- panel %>%
  filter(year == 2019, base_density == 0) %>%
  select(lad_code, year, base_density, is_rural)

cat("\nLAs with zero chargers in 2019:", nrow(zero_2019), "\n")
cat("Of which rural:", sum(zero_2019$is_rural), "\n")


# ---- 5) BASELINE 2SLS ----

cat("\n=== BASELINE 2SLS ===\n")

iv_baseline <- feols(
  bev_share_cars ~ gdhi_pc_1000 + car_fuel_per_1000_pop |
    lad_code + year |
    devices_per_1000_pop ~ bartik_iv,
  data    = panel,
  cluster = ~lad_code
)

cat("--- First Stage ---\n")
summary(iv_baseline, stage = 1)
cat("\n--- Second Stage ---\n")
summary(iv_baseline, stage = 2)

cat("\n--- OLS vs 2SLS Comparison (Table 2) ---\n")
etable(
  ols_baseline, iv_baseline,
  headers = c("OLS", "2SLS"),
  cluster = ~lad_code
)


# ---- 6) ROBUSTNESS CHECKS ----

cat("\n=== ROBUSTNESS CHECKS ===\n")

# Lagged charger density: t-1 instrument addresses remaining
# concerns about within-year simultaneity
iv_lagged <- feols(
  bev_share_cars ~ gdhi_pc_1000 + car_fuel_per_1000_pop |
    lad_code + year |
    devices_lag1 ~ bartik_iv_lag1,
  data    = panel %>% filter(year > 2019),
  cluster = ~lad_code
)
cat("--- Lagged Chargers (t-1) ---\n")
summary(iv_lagged, stage = 2)

# Medium-run window 2020-2022 excludes the pandemic year (2019)
# and the post-mandate acceleration year (2023)
iv_medium <- feols(
  bev_share_cars ~ gdhi_pc_1000 + car_fuel_per_1000_pop |
    lad_code + year |
    devices_per_1000_pop ~ bartik_iv,
  data    = panel %>% filter(year %in% 2020:2022),
  cluster = ~lad_code
)
cat("\n--- Medium Run 2020-2022 ---\n")
summary(iv_medium, stage = 2)

# Alternative outcome: BEV stock per 1,000 population
iv_alt_outcome <- feols(
  bev_per_1000_pop ~ gdhi_pc_1000 + car_fuel_per_1000_pop |
    lad_code + year |
    devices_per_1000_pop ~ bartik_iv,
  data    = panel,
  cluster = ~lad_code
)
cat("\n--- Alternative Outcome (BEV per 1,000 population) ---\n")
summary(iv_alt_outcome, stage = 2)

# ORCS control: adds cumulative on-street residential chargepoints
# to check whether results are driven by ORCS co-deployment
iv_orcs <- feols(
  bev_share_cars ~ gdhi_pc_1000 + car_fuel_per_1000_pop +
    orcs_cumulative |
    lad_code + year |
    devices_per_1000_pop ~ bartik_iv,
  data    = panel,
  cluster = ~lad_code
)
cat("\n--- With ORCS Control ---\n")
summary(iv_orcs, stage = 2)

# LA-specific linear time trends: a demanding specification
# that absorbs each LA's own charger growth trajectory.
# Expected to weaken the Bartik instrument by design.
iv_trends <- feols(
  bev_share_cars ~ gdhi_pc_1000 + car_fuel_per_1000_pop |
    lad_code + year + lad_code[year_num] |
    devices_per_1000_pop ~ bartik_iv,
  data    = panel,
  cluster = ~lad_code
)
cat("\n--- LA Linear Trends ---\n")
summary(iv_trends, stage = 2)
cat("First-stage Wald F with LA trends:\n")
print(wald(iv_trends, "bartik_iv", stage = 1))

cat("\n--- Robustness Comparison Table (Table 6) ---\n")
etable(
  iv_baseline, iv_lagged, iv_medium,
  iv_alt_outcome, iv_orcs,
  headers = c("Baseline", "Lagged", "Med Run",
              "Alt Outcome", "ORCS"),
  cluster = ~lad_code
)


# ---- 7) HETEROGENEITY ANALYSIS ----

cat("\n=== HETEROGENEITY ANALYSIS ===\n")

# --- 7a) Subsample 2SLS ---

# Urban LAs: instrument has strong first-stage power
iv_urban <- feols(
  bev_share_cars ~ gdhi_pc_1000 + car_fuel_per_1000_pop |
    lad_code + year |
    devices_per_1000_pop ~ bartik_iv,
  data    = panel %>% filter(is_rural == 0),
  cluster = ~lad_code
)
cat("--- Urban Subsample ---\n")
summary(iv_urban, stage = 2)

# Rural LAs: reported as descriptive only due to weak instrument
# (LAs with zero 2019 chargers receive Bartik = 0 throughout)
iv_rural_only <- feols(
  bev_share_cars ~ gdhi_pc_1000 + car_fuel_per_1000_pop |
    lad_code + year |
    devices_per_1000_pop ~ bartik_iv,
  data    = panel %>% filter(is_rural == 1),
  cluster = ~lad_code
)
cat("\n--- Rural Subsample (descriptive only — weak instrument) ---\n")
summary(iv_rural_only, stage = 2)

# Rural threshold sample: excludes the five rural LAs with
# zero 2019 chargers to test whether weak IV drives rural result
panel_rural_threshold <- panel %>%
  filter(is_rural == 1, base_density > 0)

cat("\nRural threshold sample — LAs:",
    n_distinct(panel_rural_threshold$lad_code), "\n")

iv_rural_threshold <- feols(
  bev_share_cars ~ gdhi_pc_1000 + car_fuel_per_1000_pop |
    lad_code + year |
    devices_per_1000_pop ~ bartik_iv,
  data    = panel_rural_threshold,
  cluster = ~lad_code
)
cat("\n--- Rural Subsample (threshold: base_density > 0) ---\n")
summary(iv_rural_threshold, stage = 2)
cat("Wald F on rural threshold sample:\n")
print(wald(iv_rural_threshold, "bartik_iv", stage = 1))

# Deprivation terciles
iv_most_deprived <- feols(
  bev_share_cars ~ gdhi_pc_1000 + car_fuel_per_1000_pop |
    lad_code + year |
    devices_per_1000_pop ~ bartik_iv,
  data    = panel %>% filter(deprivation_tercile == 1),
  cluster = ~lad_code
)
cat("\n--- Most Deprived Tercile ---\n")
summary(iv_most_deprived, stage = 2)

iv_least_deprived <- feols(
  bev_share_cars ~ gdhi_pc_1000 + car_fuel_per_1000_pop |
    lad_code + year |
    devices_per_1000_pop ~ bartik_iv,
  data    = panel %>% filter(deprivation_tercile == 3),
  cluster = ~lad_code
)
cat("\n--- Least Deprived Tercile ---\n")
summary(iv_least_deprived, stage = 2)

# Income terciles
iv_low_income <- feols(
  bev_share_cars ~ gdhi_pc_1000 + car_fuel_per_1000_pop |
    lad_code + year |
    devices_per_1000_pop ~ bartik_iv,
  data    = panel %>% filter(income_tercile == 1),
  cluster = ~lad_code
)
cat("\n--- Low Income Tercile ---\n")
summary(iv_low_income, stage = 2)

iv_high_income <- feols(
  bev_share_cars ~ gdhi_pc_1000 + car_fuel_per_1000_pop |
    lad_code + year |
    devices_per_1000_pop ~ bartik_iv,
  data    = panel %>% filter(income_tercile == 3),
  cluster = ~lad_code
)
cat("\n--- High Income Tercile ---\n")
summary(iv_high_income, stage = 2)

cat("\n--- Subsample Heterogeneity Table (Table 4) ---\n")
etable(
  iv_urban, iv_most_deprived, iv_least_deprived,
  iv_low_income, iv_high_income,
  headers = c("Urban", "Most Deprived", "Least Deprived",
              "Low Income", "High Income"),
  cluster = ~lad_code
)

# --- 7b) Interaction 2SLS ---
# Instruments devices x group and chargers x group jointly with
# bartik_iv and bartik_iv x group, allowing a heterogeneous
# treatment effect within a single pooled specification

# Deprivation interactions
iv_interact_deprivation <- feols(
  bev_share_cars ~ gdhi_pc_1000 + car_fuel_per_1000_pop +
    most_deprived_dummy + least_deprived_dummy |
    lad_code + year |
    devices_per_1000_pop + chargers_x_most_deprived +
    chargers_x_least_deprived ~
    bartik_iv + bartik_x_most_deprived + bartik_x_least_deprived,
  data    = panel,
  cluster = ~lad_code
)
cat("\n--- Interaction 2SLS: Deprivation ---\n")
summary(iv_interact_deprivation, stage = 2)

# Income interactions
iv_interact_income <- feols(
  bev_share_cars ~ gdhi_pc_1000 + car_fuel_per_1000_pop +
    low_income_dummy + high_income_dummy |
    lad_code + year |
    devices_per_1000_pop + chargers_x_low_income +
    chargers_x_high_income ~
    bartik_iv + bartik_x_low_income + bartik_x_high_income,
  data    = panel,
  cluster = ~lad_code
)
cat("\n--- Interaction 2SLS: Income ---\n")
summary(iv_interact_income, stage = 2)

# Urban/rural interaction
iv_interact_urban <- feols(
  bev_share_cars ~ gdhi_pc_1000 + car_fuel_per_1000_pop +
    urban_dummy |
    lad_code + year |
    devices_per_1000_pop + chargers_x_urban ~
    bartik_iv + bartik_x_urban,
  data    = panel,
  cluster = ~lad_code
)
cat("\n--- Interaction 2SLS: Urban/Rural ---\n")
summary(iv_interact_urban, stage = 2)

cat("\n--- Interaction 2SLS Heterogeneity Table (Table 9) ---\n")
etable(
  iv_interact_deprivation, iv_interact_income, iv_interact_urban,
  iv_rural_threshold,
  headers = c("Deprivation", "Income", "Urban/Rural",
              "Rural Threshold"),
  cluster = ~lad_code
)


# ---- 8) INEQUALITY ANALYSIS ----

cat("\n=== INEQUALITY ANALYSIS ===\n")

# Gini coefficient function
gini <- function(x) {
  x <- x[!is.na(x)]
  x <- sort(x)
  n <- length(x)
  if (n == 0 || sum(x) == 0) return(NA_real_)
  2 * sum(x * seq_len(n)) / (n * sum(x)) - (n + 1) / n
}

# Gini by year for key variables
gini_by_year <- panel %>%
  group_by(year) %>%
  summarise(
    gini_bev_share        = gini(bev_share_cars),
    gini_bev_per_1000     = gini(bev_per_1000_pop),
    gini_devices_per_1000 = gini(devices_per_1000_pop),
    .groups = "drop"
  )

cat("--- Gini Coefficients by Year ---\n")
print(gini_by_year)

# Lorenz curve helper function
lorenz_data <- function(df, var, year_val) {
  x <- df %>% filter(year == year_val) %>% pull({{ var }})
  x <- sort(x[!is.na(x)])
  n <- length(x)
  tibble(
    cum_pop   = c(0, seq_len(n) / n),
    cum_share = c(0, cumsum(x) / sum(x)),
    year      = year_val
  )
}

lorenz_bev      <- map_dfr(2019:2023,
                           ~ lorenz_data(panel, bev_share_cars, .x))
lorenz_chargers <- map_dfr(2019:2023,
                           ~ lorenz_data(panel, devices_per_1000_pop, .x))

# Figure: Lorenz curves — BEV adoption
p_lorenz_bev <- ggplot(
  lorenz_bev %>% mutate(year = factor(year)),
  aes(x = cum_pop, y = cum_share, colour = year)
) +
  geom_line(linewidth = 0.8) +
  geom_abline(slope = 1, intercept = 0,
              linetype = "dashed", colour = "black") +
  scale_colour_viridis_d(option = "plasma", end = 0.85) +
  labs(
    title    = "Lorenz Curves — BEV Adoption",
    subtitle = "Cumulative BEV share vs cumulative LA share",
    x        = "Cumulative share of LAs (ranked by BEV share)",
    y        = "Cumulative share of total BEV adoption",
    colour   = "Year",
    caption  = "Dashed line = perfect equality"
  ) +
  theme_minimal(base_size = 12)

ggsave("data_out/lorenz_bev.png", p_lorenz_bev,
       width = 8, height = 5, dpi = 300)

# Figure: Lorenz curves — charging infrastructure
p_lorenz_chargers <- ggplot(
  lorenz_chargers %>% mutate(year = factor(year)),
  aes(x = cum_pop, y = cum_share, colour = year)
) +
  geom_line(linewidth = 0.8) +
  geom_abline(slope = 1, intercept = 0,
              linetype = "dashed", colour = "black") +
  scale_colour_viridis_d(option = "plasma", end = 0.85) +
  labs(
    title    = "Lorenz Curves — Public Charging Infrastructure",
    subtitle = "Cumulative charger share vs cumulative LA share",
    x        = "Cumulative share of LAs (ranked by charger density)",
    y        = "Cumulative share of total charging devices",
    colour   = "Year",
    caption  = "Dashed line = perfect equality"
  ) +
  theme_minimal(base_size = 12)

ggsave("data_out/lorenz_chargers.png", p_lorenz_chargers,
       width = 8, height = 5, dpi = 300)

# PCS-LIN: share of BEVs and chargers held by disadvantaged groups
# Bottom income tercile
pcs_income <- panel %>%
  group_by(year) %>%
  mutate(bottom_income = as.integer(ntile(gdhi_pc, 3) == 1)) %>%
  summarise(
    share_bev_bottom_income      =
      sum(bev_stock[bottom_income == 1], na.rm = TRUE) /
      sum(bev_stock, na.rm = TRUE),
    share_chargers_bottom_income =
      sum(devices_eoy[bottom_income == 1], na.rm = TRUE) /
      sum(devices_eoy, na.rm = TRUE),
    .groups = "drop"
  )

# Most deprived tercile
pcs_deprivation <- panel %>%
  group_by(year) %>%
  mutate(most_deprived = as.integer(ntile(imd25_decile_mean, 3) == 1)) %>%
  summarise(
    share_bev_most_deprived      =
      sum(bev_stock[most_deprived == 1], na.rm = TRUE) /
      sum(bev_stock, na.rm = TRUE),
    share_chargers_most_deprived =
      sum(devices_eoy[most_deprived == 1], na.rm = TRUE) /
      sum(devices_eoy, na.rm = TRUE),
    .groups = "drop"
  )

cat("\n--- PCS-LIN by Income ---\n")
print(pcs_income)
cat("\n--- PCS-LIN by Deprivation ---\n")
print(pcs_deprivation)

# Figure: PCS-LIN combined plot
pcs_combined <- bind_rows(
  pcs_income %>%
    select(year,
           bev      = share_bev_bottom_income,
           chargers = share_chargers_bottom_income) %>%
    mutate(group = "Bottom income tercile"),
  pcs_deprivation %>%
    select(year,
           bev      = share_bev_most_deprived,
           chargers = share_chargers_most_deprived) %>%
    mutate(group = "Most deprived tercile")
) %>%
  pivot_longer(cols      = c(bev, chargers),
               names_to  = "variable",
               values_to = "share") %>%
  mutate(variable = if_else(variable == "bev",
                            "BEV stock", "Public chargers"))

p_pcs <- ggplot(
  pcs_combined,
  aes(x        = year,
      y        = share,
      colour   = variable,
      linetype = group,
      shape    = group)
) +
  geom_line(linewidth = 0.9) +
  geom_point(size = 2.5) +
  geom_hline(yintercept = 1/3,
             linetype   = "dashed",
             colour     = "grey50",
             linewidth  = 0.6) +
  scale_y_continuous(
    labels = percent_format(accuracy = 1),
    limits = c(0, 0.70)
  ) +
  scale_x_continuous(breaks = 2019:2023) +
  scale_colour_manual(
    values = c("BEV stock"       = "#E31A1C",
               "Public chargers" = "#1F78B4")
  ) +
  scale_linetype_manual(
    values = c("Bottom income tercile" = "solid",
               "Most deprived tercile" = "dashed")
  ) +
  scale_shape_manual(
    values = c("Bottom income tercile" = 16,
               "Most deprived tercile" = 17)
  ) +
  labs(
    title    = "Share of BEVs and Chargers in Disadvantaged Areas",
    subtitle = "Dashed line = equal distribution (33%)",
    x        = "Year",
    y        = "Share of national total",
    colour   = "Variable",
    linetype = "Group",
    shape    = "Group"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    legend.position  = "bottom",
    legend.box       = "horizontal",
    panel.grid.minor = element_blank()
  )

ggsave("data_out/pcs_lin.png", p_pcs,
       width = 8, height = 5, dpi = 300)

# Inequality summary table
cat("\n--- Inequality Summary Table ---\n")
gini_by_year %>%
  left_join(pcs_income, by = "year") %>%
  left_join(pcs_deprivation, by = "year") %>%
  mutate(across(where(is.numeric), ~ round(., 3))) %>%
  print()


# ---- 9) SAVE FINAL PANEL ----

write_csv(panel, "data_out/panel_clean.csv")
saveRDS(panel, "data_out/panel_clean.rds")

cat("\nAnalysis complete. Panel saved to data_out/\n")
cat("Figures saved: lorenz_bev.png, lorenz_chargers.png, pcs_lin.png\n")

