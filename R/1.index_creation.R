rm(list = ls())

packages = c("haven", "dplyr", "tidyr", "stringr", "ggplot2", "psych", "lme4","readxl", "writexl", "gridExtra", "data.table", "patchwork"
             , "gt", "tibble", "flextable", "officer", "ggforce", "grid")

package.check <- lapply(
  packages,
  FUN = function(x) {
    if (!require(x, character.only = TRUE)) {
      install.packages(x, dependencies = TRUE)
      library(x, character.only = TRUE)
    }
  }
)

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

df_raw <- read.csv("data/ESS6e02_7.csv")

# Select relevant variables
vars <- read.csv("data/data_selection.csv")
head(vars)

var_list <- vars$variable

df <- df_raw[, var_list]
names(df)

# Data cleaning

df_raw$uid <- paste(df_raw$cntry, df_raw$idno, sep = "_")

config <- read_excel("data/data_config.xlsx")
head(config)

mhi_vars <- config$variable[config$include == 1]
mhi_vars


df_clean <- df
for(i in 1:nrow(config)){
  
  var <- config$variable[i]
  
  refusal <- config$refusal[i]
  dk <- config$`dont know`[i]
  noans <- config$`no answer`[i]
  
  df_clean[[var]][df_clean[[var]] %in% c(refusal, dk, noans)] <- NA
}
df_clean <- df_clean[complete.cases(df_clean), ]

# Check if question is missing for all respondents in a country (if there is then delete the variable)

all_na_by_country <- lapply(mhi_vars, function(v) {
  df_clean %>%
    group_by(cntry) %>%
    summarise(all_na = all(is.na(.data[[v]])), .groups = "drop") %>%
    mutate(variable = v)
})

all_na_by_country <- bind_rows(all_na_by_country)

vars_to_drop <- bind_rows(
  lapply(mhi_vars, function(v) {
    df_clean %>%
      group_by(cntry) %>%
      summarise(all_na = all(is.na(.data[[v]])), .groups = "drop") %>%
      filter(all_na == TRUE) %>%
      mutate(variable = v)
  })
) %>%
  pull(variable) %>%
  unique()
vars_to_drop

df_clean <- df_clean %>%
  select(-all_of(vars_to_drop))

mhi_vars <- setdiff(mhi_vars, vars_to_drop)

# Delete rows with missing values in the MHI variables 
df_clean <- df_clean[complete.cases(df_clean[, mhi_vars]), ]

#check each cntry number of observations
table(df_clean$cntry)

# Scaling all variables
df_scaled <- df_clean

for(i in 1:nrow(config)){
  
  var <- config$variable[i]
  
  if(var %in% mhi_vars && var %in% names(df_scaled)){
    
    worst <- config$worst[i]
    best  <- config$best[i]
    
    df_scaled[[var]] <- ((df_scaled[[var]] - worst) / (best - worst)) * 10
  }
}
sapply(df_scaled[, mhi_vars], range, na.rm = TRUE)

cor_results <- corr.test(df_scaled[, mhi_vars], use = "pairwise")
cor_matrix <- cor_results$r
round(cor_matrix, 2)

# Test if PCA will be applyable to this dataset

##Cronbach's Alpha (internal consistency)
psych::alpha(df_scaled[, mhi_vars])

##KMO test (sampling adequacy)
KMO(df_scaled[, mhi_vars])

##Bartlett’s Test of Sphericity
cortest.bartlett(cor(df_scaled[, mhi_vars]), n = nrow(df_scaled))

##Parallel analysis
fa.parallel(df_scaled[, mhi_vars], fa = "pc")
pca <- principal(df_scaled[, mhi_vars], nfactors = 10, rotate = "varimax")
print(pca$loadings, cutoff = 0.3)

###based on paralell analysis keep variables that explain the same component
mhi <- c(
  "happy",
  "stflife",
  "wrhpp",
  "enjlf",
  "enrglot",
  "fltpcfl",
  "health",
  "fltdpr",
  "fltsd",
  "fltanx",
  "fltlnl",
  "flteeff",
  "slprl"
)

#re-run tests
psych::alpha(df_scaled[, mhi])
KMO(df_scaled[, mhi])
cortest.bartlett(cor(df_scaled[, mhi]), n = nrow(df_scaled))

fa.parallel(df_scaled[, mhi], fa = "pc")
fa(df_scaled[, mhi], nfactors = 3, rotate = "oblimin")

mhi2 <- c(
  "happy",
  "stflife",
  "wrhpp",
  "enjlf",
  "enrglot",
  "fltpcfl",
  "fltdpr",
  "fltsd",
  "fltanx",
  "fltlnl",
  "flteeff",
  "slprl"
)

psych::alpha(df_scaled[, mhi2])
KMO(df_scaled[, mhi2])
fa(df_scaled[, mhi2], nfactors = 3, rotate = "oblimin")

#based on more reliable statistics mhi2 is chosen
fa_mhi <- fa(df_scaled[, mhi2], nfactors = 3, rotate = "oblimin", scores = "regression")
fa_mhi$scores

df_scaled$F_distress <- -fa_mhi$scores[, "MR1"]
df_scaled$F_positive <-  fa_mhi$scores[, "MR3"]
df_scaled$F_lifeeval <-  fa_mhi$scores[, "MR2"]

df_scaled$F_distress_z <- scale(df_scaled$F_distress)
df_scaled$F_positive_z <- scale(df_scaled$F_positive)
df_scaled$F_lifeeval_z <- scale(df_scaled$F_lifeeval)

df_scaled$MHI_latent <- rowMeans(
  cbind(
    df_scaled$F_distress_z,
    df_scaled$F_positive_z,
    df_scaled$F_lifeeval_z
  ),
  na.rm = TRUE
)

min_val <- min(df_scaled$MHI_latent, na.rm = TRUE)
max_val <- max(df_scaled$MHI_latent, na.rm = TRUE)

df_scaled$MHI <- 10 * (df_scaled$MHI_latent - min_val) / (max_val - min_val)

summary(df_scaled$MHI)
range(df_scaled$MHI)
summary(df_scaled$MHI_latent)
hist(df_scaled$MHI)
#Insert weights for population representativeness

df_scaled$uid <- paste(df_scaled$cntry, df_scaled$idno, sep = "_")
weights_df <- df_raw[, c("uid", "anweight")]

df_scaled <- df_scaled %>%
  left_join(weights_df, by = "uid")

sum(duplicated(df_raw$uid))

final_df <- df_scaled %>%
  dplyr::select(
    uid,
    cntry,
    region,
    MHI,
    agea,
    gndr,
    domicil,
    eduyrs,
    anweight
  )

dim(final_df)
summary(final_df$MHI)

#Grouping by country
country_mhi <- final_df %>%
  group_by(cntry) %>%
  summarise(
    MHI_country = weighted.mean(MHI, anweight, na.rm = TRUE),
    N = n(),
    weight_sum = sum(anweight, na.rm = TRUE)
  ) %>%
  arrange(desc(MHI_country))

#Visualizing
overall_mean <- weighted.mean(final_df$MHI, final_df$anweight, na.rm = TRUE)
ggplot(country_mhi, aes(y = reorder(cntry, MHI_country), x = MHI_country)) +
  geom_col(fill = "steelblue", color = "white", width = 1) +
  geom_vline(xintercept = overall_mean, linetype = 2, color = "red", linewidth = 1) +
  coord_cartesian(
    xlim = c(
      min(country_mhi$MHI_country) - 0.15,
      max(country_mhi$MHI_country) + 0.15
    )
  ) +
  labs(
    title = "Average Mental Health Index by Country (ESS Round 6)",
    subtitle = "Red dashed line = European average",
    x = "Mental Health Index",
    y = "Country"
  ) +
  theme_minimal() +
  theme(panel.grid.major.y = element_blank())

#Grouping

# 1) Hungary only
hu_df <- final_df %>%
  filter(cntry == "HU")

# 2) Create grouped variables
hu_df <- hu_df %>%
  mutate(
    age_group = case_when(
      agea < 18 ~ "0-18",
      agea >= 18 & agea < 30 ~ "18-30",
      agea >= 30 & agea < 40 ~ "30-40",
      agea >= 40 & agea < 50 ~ "40-50",
      agea >= 50 & agea < 65 ~ "50-65",
      agea >= 65 ~ "65+",
      TRUE ~ NA_character_
    ),
    edu_group = case_when(
      eduyrs >= 0  & eduyrs <= 8  ~ "0-8",
      eduyrs >= 9  & eduyrs <= 12 ~ "9-12",
      eduyrs >= 13 & eduyrs <= 15 ~ "13-15",
      eduyrs >= 16 & eduyrs <= 18 ~ "16-18",
      eduyrs >= 19                ~ "19+",
      TRUE ~ NA_character_
    ),
    gender_group = as.character(gndr),
    domicile_group = as.character(domicil),
    region = as.character(region),
    region_nuts2 = case_when(
      region == "HU101" ~ "HU11",
      region == "HU102" ~ "HU12",
      TRUE ~ substr(region, 1, 4)
    )
  )

# 4) Weighted mean MHI by NUTS 2 region
hu_region_mhi <- hu_df %>%
  group_by(region_nuts2) %>%
  summarise(
    MHI = weighted.mean(MHI, anweight, na.rm = TRUE),
    n = n(),
    .groups = "drop"
  ) %>%
  arrange(desc(MHI))

regions_map <- read_excel("data/huregions.xlsx") %>%
  rename(region_nuts2 = code)

hu_region_mhi <- hu_region_mhi %>%
  left_join(regions_map, by = "region_nuts2")


# 5) Weighted mean MHI by education group
hu_edu_mhi <- hu_df %>%
  group_by(edu_group) %>%
  summarise(
    MHI = weighted.mean(MHI, anweight, na.rm = TRUE),
    n = n()
  )

hu_edu_mhi$edu_group <- factor(
  hu_edu_mhi$edu_group,
  levels = c("0-8", "9-12", "13-15", "16-18", "19+")
)

# 6) Weighted mean MHI by gender
hu_gender_mhi <- hu_df %>%
  group_by(gender_group) %>%
  summarise(
    MHI = weighted.mean(MHI, anweight, na.rm = TRUE),
    n = n()
  )
hu_gender_mhi <- hu_gender_mhi %>%
  mutate(
    gender_group = case_when(
      gender_group == 1 ~ "Male",
      gender_group == 2 ~ "Female"
    )
  )

# 7) Weighted mean MHI by domicile
hu_domicile_mhi <- hu_df %>%
  group_by(domicile_group) %>%
  summarise(
    MHI = weighted.mean(MHI, anweight, na.rm = TRUE),
    n = n()
  ) %>%
  arrange(desc(MHI))

hu_domicile_mhi <- hu_domicile_mhi %>%
  mutate(
    domicile_group = case_when(
      domicile_group == 1 ~ "Big city",
      domicile_group == 2 ~ "Suburbs/outskirts",
      domicile_group == 3 ~ "Town/small city",
      domicile_group == 4 ~ "Country village",
      domicile_group == 5 ~ "Farm/home in countryside",
      TRUE ~ as.character(domicile_group)
    )
  )


hu_mean <- weighted.mean(hu_df$MHI, hu_df$anweight, na.rm = TRUE)

table(df_clean$region[df_clean$cntry == "HU"])

unique(final_df$cntry)

# Save data for further analysis
saveRDS(country_mhi, "data/country_mhi.rds")
saveRDS(hu_region_mhi, "data/hu_region_mhi.rds")
saveRDS(final_df, "data/final_dataset.rds")

#############FIGURE1###################
vars <- c(
  "happy",    # happiness
  "stflife",  # life satisfaction
  "wrhpp",    # work-life happiness
  "enjlf",    # enjoyment of life
  "enrglot",  # energy
  "fltpcfl",  # calmness
  "fltdpr",   # depression
  "fltsd",    # sadness
  "fltanx",   # anxiety
  "fltlnl",   # loneliness
  "flteeff",  # self-efficacy
  "slprl"     # sleep/restlessness
)

plot_raw <- copy(df_clean)

rescale_0_10 <- function(x) {
  ((x - min(x, na.rm = TRUE)) /
     (max(x, na.rm = TRUE) - min(x, na.rm = TRUE))) * 10
}
plot_vars <- c("health", "fltdpr", "happy", "slprl")
plot_raw <- as.data.table(plot_raw)
raw_long <- data.table::melt(
  plot_raw,
  measure.vars = plot_vars,
  variable.name = "Variable",
  value.name = "Value"
)

raw_long[, Scale := "Raw"]

plot_scaled <- copy(plot_raw)
plot_scaled[, (plot_vars) := lapply(.SD, rescale_0_10), .SDcols = plot_vars]

scaled_long <- melt(
  plot_scaled,
  measure.vars = plot_vars,
  variable.name = "Variable",
  value.name = "Value"
)
scaled_long[, Scale := "Rescaled (0–10)"]

label_map <- c(
  health = "Self-rated health",
  fltdpr = "Depression",
  happy = "Happiness",
  slprl = "Sleep quality"
)

combined <- rbind(raw_long, scaled_long)
combined[, Variable := factor(Variable,
                              levels = names(label_map),
                              labels = label_map)]

p <- ggplot(combined[!is.na(Value)], aes(x = Value)) +
  geom_density(fill = "steelblue", alpha = 0.45, color = "black") +
  facet_grid(Scale ~ Variable, scales = "free_x") +
  labs(
    title = "Raw vs Rescaled Indicator Distributions",
    x = "Value",
    y = "Density"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    strip.text = element_text(face = "bold"),
    plot.title = element_text(face = "bold", hjust = 0.5),
    panel.grid.minor = element_blank()
  )

ggsave(
  "plots/raw_vs_rescaled_distributions.png",
  p,
  width = 15,
  height = 8,
  dpi = 900
)
##########################################
#GRAPHS FOR APPENDIX (not rellevant code)#
##########################################

# Final factor analysis: 3 factors, oblimin rotation
fa_final <- fa(
  df_scaled[, mhi2],
  nfactors = 3,
  rotate = "oblimin",
  fm = "ml"   # maximum likelihood
)

print(fa_final$loadings, cutoff = 0.3)

# Variance explained
fa_final$Vaccounted

# Final oblimin factor loading table
fa_loading_table <- tribble(
  ~Indicator, ~`Factor 1 (Emotional distress)`, ~`Factor 2 (Positive functioning)`, ~`Factor 3 (Life evaluation)`,
  "Happiness",            NA,    NA,    0.929,
  "Life satisfaction",    NA,    NA,    0.718,
  "Work-life happiness",  NA,    0.741, NA,
  "Enjoyment of life",    NA,    0.779, NA,
  "Energy",               NA,    0.597, NA,
  "Calmness",             NA,    0.505, NA,
  "Depression",           0.704, NA,    NA,
  "Sadness",              0.742, NA,    NA,
  "Anxiety",              0.655, NA,    NA,
  "Loneliness",           0.551, NA,    NA,
  "Self-efficacy",        0.570, NA,    NA,
  "Sleep/restlessness",   0.558, NA,    NA
)

fa_gt <- fa_loading_table %>%
  gt() %>%
  tab_header(
    title = "Factor Loadings for Final Three-Factor Model (Oblimin Rotation)"
  ) %>%
  fmt_number(
    columns = c(
      `Factor 1 (Emotional distress)`,
      `Factor 2 (Positive functioning)`,
      `Factor 3 (Life evaluation)`
    ),
    decimals = 3
  ) %>%
  sub_missing(
    columns = everything(),
    missing_text = "—"
  ) %>%
  tab_source_note(
    source_note = "Note: Loadings below 0.30 are suppressed. Source: Author’s own calculations based on ESS Round 6 (2012/2013) data."
  ) %>%
  tab_options(
    table.font.names = "Times New Roman",
    table.font.size = 12,
    heading.title.font.size = 14,
    heading.title.font.weight = "bold",
    column_labels.font.weight = "bold",
    table.border.top.width = px(1),
    table.border.bottom.width = px(1),
    column_labels.border.top.width = px(1),
    column_labels.border.bottom.width = px(1),
    data_row.padding = px(4)
  )

fa_gt

# Save as PNG
gtsave(
  fa_gt,
  filename = "plots/appendix/final_factor_loadings_oblimin.png",
  vwidth = 1600,
  vheight = 1000,
  zoom = 2
)

fa_final$Phi

# Factor scores
factor_scores <- as.data.frame(fa_final$scores)

# Rename
colnames(factor_scores) <- c(
  "Factor 1 (Emotional distress)",
  "Factor 2 (Positive functioning)",
  "Factor 3 (Life evaluation)"
)

# Summary stats for factors
factor_summary <- data.frame(
  Factor = colnames(factor_scores),
  Mean = sapply(factor_scores, mean, na.rm = TRUE),
  SD = sapply(factor_scores, sd, na.rm = TRUE),
  Min = sapply(factor_scores, min, na.rm = TRUE),
  Max = sapply(factor_scores, max, na.rm = TRUE)
)

factor_summary

# Latent index before rescaling
latent_index <- rowMeans(scale(factor_scores), na.rm = TRUE)

latent_summary <- data.frame(
  Statistic = c("Mean", "SD", "Min", "Max"),
  Value = c(
    mean(latent_index, na.rm = TRUE),
    sd(latent_index, na.rm = TRUE),
    min(latent_index, na.rm = TRUE),
    max(latent_index, na.rm = TRUE)
  )
)

latent_summary

# Clean formatting
latent_summary_clean <- latent_summary %>%
  mutate(
    Value = round(Value, 3)
  )

# Create polished table
latent_gt <- latent_summary_clean %>%
  gt() %>%
  tab_header(
    title = "Summary Statistics of Latent Index Before Rescaling"
  ) %>%
  cols_label(
    Statistic = "Statistic",
    Value = "Value"
  ) %>%
  fmt_number(
    columns = Value,
    decimals = 3
  ) %>%
  tab_source_note(
    source_note = "Source: Author’s own calculations based on ESS Round 6 (2012/2013) data."
  ) %>%
  tab_options(
    table.font.names = "Times New Roman",
    table.font.size = 12,
    heading.title.font.size = 14,
    heading.title.font.weight = "bold",
    column_labels.font.weight = "bold",
    table.border.top.width = px(1),
    table.border.bottom.width = px(1),
    column_labels.border.top.width = px(1),
    column_labels.border.bottom.width = px(1),
    data_row.padding = px(5)
  )

# Preview
latent_gt

# Save to appendix folder
gtsave(
  latent_gt,
  filename = "plots/appendix/latent_index_summary_table.png",
  vwidth = 1200,
  vheight = 700,
  zoom = 2
)

summary(final_df$MHI)

mean(final_df$MHI, na.rm = TRUE)
median(final_df$MHI, na.rm = TRUE)

quantile(final_df$MHI, probs = c(0.25, 0.75), na.rm = TRUE)
sd(final_df$MHI, na.rm = TRUE)

######################################################################
