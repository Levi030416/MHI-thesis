# ============================================================
# Descriptive analysis
# ============================================================


# 1. Packages -------------------------------------------------

library(dplyr)
library(ggplot2)


# 2. Load cleaned dataset ------------------------------------

final_df <- readRDS("data/final_dataset.rds")

unique(final_df$cntry)

summary(final_df$MHI)
sd(final_df$MHI)
boxplot.stats(final_df$MHI)$out

#Histogram

mean_mhi <- mean(final_df$MHI, na.rm = TRUE)
median_mhi <- median(final_df$MHI, na.rm = TRUE)



# ============================================================
# Subgroup analysis 1: Average MHI by age (binned histogram)
# ============================================================

# 3. Check required variables --------------------------------

stopifnot("agea" %in% names(final_df))
stopifnot("MHI" %in% names(final_df))


# 4. Create custom age bins ----------------------------------

final_df <- final_df %>%
  mutate(age_bin = case_when(
    agea >= 0  & agea < 18 ~ "0-17",
    agea >= 18 & agea < 30 ~ "18-29",
    agea >= 30 & agea < 40 ~ "30-39",
    agea >= 40 & agea < 50 ~ "40-49",
    agea >= 50 & agea < 65 ~ "50-64",
    agea >= 65             ~ "65+",
    TRUE ~ NA_character_
  ))

summary(final_df$agea)
# 5. Compute average MHI by bin -------------------------------

age_bin_avg <- final_df %>%
  filter(!is.na(age_bin), !is.na(MHI)) %>%
  group_by(age_bin) %>%
  summarise(
    mean_MHI = weighted.mean(MHI, w = anweight, na.rm = TRUE),
    n = n(),
    .groups = "drop"
  )


# 6. Ensure correct bin order --------------------------------

age_bin_avg$age_bin <- factor(
  age_bin_avg$age_bin,
  levels = c("0-17", "18-29", "30-39", "40-49", "50-64", "65+")
)


# 7. Overall average MHI -------------------------------------

overall_mean <- weighted.mean(final_df$MHI, w = final_df$anweight, na.rm = TRUE)


# 8. Plot -----------------------------------------------------

ggplot(age_bin_avg, aes(y = age_bin, x = mean_MHI)) +
  geom_col(fill = "lightblue", color = "black", width = 1) +
  geom_vline(xintercept = overall_mean, linetype = "dashed", color = "red", linewidth = 1) +
  scale_x_continuous(limits = c(0, 10)) +
  labs(
    title = "Average Mental Health Index by Age Group",
    subtitle = "Red dashed line = overall average",
    x = "Average weighted MHI",
    y = "Age Group"
  ) +
  theme_minimal()
  ggsave("plots/age_histogram.png", width = 9, height = 6, dpi = 300)

# ============================================================
# Subgroup analysis 2: Average MHI by education (years)
# ============================================================

# 2.1. Create education bins -----------------------------------

final_df <- final_df %>%
  mutate(edu_bin = case_when(
    eduyrs < 10 ~ "Low (0-9)",
    eduyrs < 13 ~ "Lower-mid (10-12)",
    eduyrs < 16 ~ "Upper-mid (13-15)",
    eduyrs < 20 ~ "High (16-19)",
    eduyrs >= 20 ~ "Very high (20+)",
    TRUE ~ NA_character_
  ))


# 2.2. Compute average MHI by education ------------------------

edu_avg_mhi <- final_df %>%
  filter(!is.na(edu_bin), !is.na(MHI)) %>%
  group_by(edu_bin) %>%
  summarise(
    mean_MHI = weighted.mean(MHI, w = anweight, na.rm = TRUE),
    n = n(),
    .groups = "drop"
  )


# 2.3. Order bins ----------------------------------------------

edu_avg_mhi$edu_bin <- factor(
  edu_avg_mhi$edu_bin,
  levels = c(
    "Low (0-9)",
    "Lower-mid (10-12)",
    "Upper-mid (13-15)",
    "High (16-19)",
    "Very high (20+)"
  )
)


# 2.4. Overall mean --------------------------------------------

overall_mean <- weighted.mean(final_df$MHI, w = final_df$anweight, na.rm = TRUE)


# 2.5. Plot -----------------------------------------------------

ggplot(edu_avg_mhi, aes(y = edu_bin, x = mean_MHI)) +
  geom_col(fill = "steelblue", color = "black", width = 1) +
  geom_vline(xintercept = overall_mean, linetype = "dashed", color = "red", linewidth = 1) +
  scale_x_continuous(limits = c(0, 10)) +
  labs(
    title = "Average Mental Health Index by Education (Years)",
    subtitle = "Red dashed line = overall average",
    x = "Average weighted MHI",
    y = "Education Level"
  ) +
  theme_minimal()
  ggsave("plots/edu_histogram.png", width = 9, height = 6, dpi = 300)


# 2.6. Inspect --------------------------------------------------

print(edu_avg_mhi)

# 2.7. Distribution --------------------------------------------------

ggplot(final_df, aes(x = eduyrs)) +
  geom_histogram(binwidth = 1, color = "black", fill = "gray70") +
  labs(
    title = "Distribution of Years of Education",
    x = "Years of Education",
    y = "Frequency"
  ) +
  theme_minimal()
  ggsave("plots/edu_distribution.png", width = 9, height = 6, dpi = 300)
  
##
  
  summary(final_df$eduyrs)
  
  sum(final_df$eduyrs == 0, na.rm = TRUE)
  
  sum(final_df$eduyrs > 30, na.rm = TRUE)
  
  final_df %>%
    filter(eduyrs > 30 | eduyrs == 0) %>%
    count(eduyrs) %>%
    arrange(eduyrs)

# ============================================================
# Subgroup analysis 3: Average MHI by domicile
# ============================================================

# 3.1 Recode domicile ----------------------------------------

final_df <- final_df %>%
  mutate(domicile = case_when(
    domicil == 1 ~ "Big city",
    domicil == 2 ~ "Suburbs",
    domicil == 3 ~ "Town / small city",
    domicil == 4 ~ "Village",
    domicil == 5 ~ "Countryside",
    TRUE ~ NA_character_
  ))


# 3.2 Compute average MHI by domicile ------------------------

domicile_avg_mhi <- final_df %>%
  filter(!is.na(domicile), !is.na(MHI)) %>%
  group_by(domicile) %>%
  summarise(
    mean_MHI = weighted.mean(MHI, w = anweight, na.rm = TRUE),
    n = n(),
    .groups = "drop"
  )


# 3.3 Order domicile categories ------------------------------

domicile_avg_mhi$domicile <- factor(
  domicile_avg_mhi$domicile,
  levels = c(
    "Big city",
    "Suburbs",
    "Town / small city",
    "Village",
    "Countryside"
  )
)


# 3.4 Overall average MHI ------------------------------------

overall_mean <- weighted.mean(final_df$MHI, w = final_df$anweight, na.rm = TRUE)


# 3.5 Plot average MHI by domicile ---------------------------

ggplot(domicile_avg_mhi, aes(y = domicile, x = mean_MHI)) +
  geom_col(fill = "steelblue", color = "black", width = 1) +
  geom_vline(xintercept = overall_mean, linetype = "longdash", color = "red", linewidth = 1) +
  scale_x_continuous(limits = c(0, 10)) +
  labs(
    title = "Average Mental Health Index by Domicile",
    subtitle = "Red dashed line = overall average",
    x = "Average weighted MHI",
    y = "Domicile"
  ) +
  theme_minimal()
  ggsave("plots/domicile_histogram.png", width = 9, height = 6, dpi = 300)


# 3.6 Inspect table ------------------------------------------

print(domicile_avg_mhi)

domicile_avg <- final_df %>%
  filter(!is.na(domicile), !is.na(MHI), !is.na(anweight)) %>%
  group_by(domicile) %>%
  summarise(
    mean_MHI = weighted.mean(MHI, anweight, na.rm = TRUE),
    n = n(),
    weighted_n = sum(anweight, na.rm = TRUE),
    .groups = "drop"
    ) %>%
  arrange(mean_MHI)
print(domicile_avg)
  

# ============================================================
# Subgroup analysis 4: Interaction (Age × Education)
# ============================================================

# 4.1 Compute weighted average MHI by age and education ------

age_edu_avg <- final_df %>%
  filter(!is.na(age_bin), !is.na(edu_bin), !is.na(MHI)) %>%
  group_by(age_bin, edu_bin) %>%
  summarise(
    mean_MHI = weighted.mean(MHI, w = anweight, na.rm = TRUE),
    n = n(),
    .groups = "drop"
  )


# 4.2 Ensure ordering ----------------------------------------

age_edu_avg$age_bin <- factor(
  age_edu_avg$age_bin,
  levels = c("0-17", "18-29", "30-39", "40-49", "50-64", "65+")
)

age_edu_avg$edu_bin <- factor(
  age_edu_avg$edu_bin,
  levels = c(
    "Low (0-9)",
    "Lower-mid (10-12)",
    "Upper-mid (13-15)",
    "High (16-19)",
    "Very high (20+)"
  )
)


# 4.3 Plot: Age patterns by education ------------------------

ggplot(age_edu_avg, aes(x = age_bin, y = edu_bin, fill = mean_MHI)) +
  geom_tile(color = "white") +
  scale_fill_gradient2(
    low = "white",
    mid = "lightblue",
    high = "darkblue",
    midpoint = overall_mean
  ) +
  labs(
    title = "Interaction of Age and Education on MHI",
    x = "Age Group",
    y = "Education Level",
    fill = "Weighted MHI"
  ) +
  theme_minimal()
  ggsave("plots/age_education_interaction_heatmap.png", width = 9, height = 6, dpi = 300)


# 4.4 Inspect ------------------------------------------------

print(age_edu_avg)
  
  names(age_edu_avg)
  
  interaction_table <- age_edu_avg %>%
    group_by(age_bin, edu_bin) %>%
    summarise(
      mean_MHI = mean(mean_MHI, na.rm = TRUE),
      n = sum(n, na.rm = TRUE),
      .groups = "drop"
    )
  
# descstat
country_counts <- final_df %>%
  group_by(cntry) %>%
  summarise(
    n = n(),
    weighted_n = sum(anweight, na.rm = TRUE)
    ) %>%
  arrange(desc(n))



interaction_table <- final_df %>%
  filter(
    !is.na(age_bin),
    !is.na(edu_bin),
    !is.na(MHI),
    !is.na(anweight)
  ) %>%
  group_by(age_bin, edu_bin) %>%
  summarise(
    mean_MHI = weighted.mean(MHI, w = anweight, na.rm = TRUE),
    n = n(),
    weighted_n = sum(anweight, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    age_bin = factor(
      age_bin,
      levels = c("0-17", "18-29", "30-39", "40-49", "50-64", "65+")
    ),
    edu_bin = factor(
      edu_bin,
      levels = c(
        "Low (0-9)",
        "Lower-mid (10-12)",
        "Upper-mid (13-15)",
        "High (16-19)",
        "Very high (20+)"
      )
    )
  ) %>%
  arrange(edu_bin, age_bin)

print(interaction_table)

weighted.mean(final_df$MHI, final_df$anweight, na.rm = TRUE)


interaction_gap <- age_edu_avg %>%
  filter(edu_bin %in% c("Low (0-9)", "Very high (20+)")) %>%
  select(age_bin, edu_bin, mean_MHI) %>%
  tidyr::pivot_wider(
    names_from = edu_bin,
    values_from = mean_MHI
  ) %>%
  mutate(
    edu_gap = `Very high (20+)` - `Low (0-9)`
  )

print(interaction_gap)



####Other descriptive stat
final_df %>%
  filter(agea >= 90) %>%
  count(agea) %>%
  arrange(desc(agea))

zero_edu_profile <- final_df %>%
  filter(eduyrs == 0) %>%
  group_by(cntry, age_bin, domicile) %>%
  summarise(
    n = n(),
    mean_age = mean(agea, na.rm = TRUE),
    mean_MHI = weighted.mean(MHI, anweight, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(desc(n))

print(zero_edu_profile)

##############################

# Overall weighted mean
overall_mean <- weighted.mean(final_df$MHI, final_df$anweight, na.rm = TRUE)

# ----------------------------
# AGE
# ----------------------------

# Age MHI
age_plot <- ggplot(age_bin_avg, aes(y = age_bin, x = mean_MHI)) +
  geom_col(fill = "lightblue", color = "black") +
  geom_vline(xintercept = overall_mean, linetype = "dashed", color = "red") +
  scale_x_continuous(limits = c(0, 10)) +
  labs(title = "Age and MHI", x = "Weighted average MHI", y = NULL) +
  theme_minimal(base_size = 11)

gender_freq <- final_df %>%
  filter(gndr %in% c(1, 2)) %>%
  mutate(
    gender = case_when(
      gndr == 1 ~ "Male",
      gndr == 2 ~ "Female"
    )
  ) %>%
  count(gender)

gender_dist_plot <- ggplot(gender_freq, aes(x = reorder(gender, n), y = n)) +
  geom_col(fill = "lightblue", color = "black") +
  coord_flip() +
  labs(
    title = "Gender Distribution",
    x = NULL,
    y = "Count"
  ) +
  theme_minimal(base_size = 11)

# ----------------------------
# EDUCATION
# ----------------------------

# Education MHI
edu_plot <- ggplot(edu_avg_mhi, aes(y = edu_bin, x = mean_MHI)) +
  geom_col(fill = "lightblue", color = "black") +
  geom_vline(xintercept = overall_mean, linetype = "dashed", color = "red") +
  scale_x_continuous(limits = c(0, 10)) +
  labs(title = "Education and MHI", x = "Weighted average MHI", y = NULL) +
  theme_minimal(base_size = 11)

# Education frequency
edu_dist_plot <- ggplot(final_df, aes(x = eduyrs)) +
  geom_histogram(fill = "lightblue", color = "black", bins = 25) +
  labs(title = "Education Distribution", x = "Years of education", y = "Count") +
  theme_minimal(base_size = 11)

# ----------------------------
# DOMICILE
# ----------------------------

# Domicile MHI
domicile_plot <- ggplot(domicile_avg_mhi, aes(y = domicile, x = mean_MHI)) +
  geom_col(fill = "lightblue", color = "black") +
  geom_vline(xintercept = overall_mean, linetype = "dashed", color = "red") +
  scale_x_continuous(limits = c(0, 10)) +
  labs(title = "Domicile and MHI", x = "Weighted average MHI", y = NULL) +
  theme_minimal(base_size = 11)

# Domicile frequency
domicile_freq <- final_df %>%
  filter(!is.na(domicile)) %>%
  count(domicile)

domicile_dist_plot <- ggplot(domicile_freq, aes(x = reorder(domicile, n), y = n)) +
  geom_col(fill = "lightblue", color = "black") +
  coord_flip() +
  labs(title = "Domicile Distribution", x = NULL, y = "Count") +
  theme_minimal(base_size = 11)

# ----------------------------
# 3 Columns × 2 Rows
# ----------------------------

combined_grid <- (age_plot | edu_plot | domicile_plot) /
  (gender_dist_plot | edu_dist_plot | domicile_dist_plot) +
  plot_annotation(
    title = "Descriptive Overview of Mental Health Index and Demographic Distributions",
    subtitle = "Top row: weighted average MHI by subgroup | Bottom row: subgroup frequency distributions"
  )

combined_grid

###########
readable_theme <- theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(
      face = "bold",
      size = 14,
      hjust = 0.5
    ),
    axis.title = element_text(
      size = 12,
      face = "bold"
    ),
    axis.text = element_text(
      size = 11
    ),
    axis.text.y = element_text(
      size = 11,
      face = "bold"
    ),
    panel.grid.minor = element_blank(),
    panel.grid.major.y = element_blank(),
    legend.position = "none"
  )

# Then add + readable_theme to each plot:
# Example:
# age_plot <- age_plot + readable_theme

age_plot <- age_plot + readable_theme
edu_plot <- edu_plot + readable_theme
domicile_plot <- domicile_plot + readable_theme
gender_dist_plot <- gender_dist_plot + readable_theme
edu_dist_plot <- edu_dist_plot + readable_theme
domicile_dist_plot <- domicile_dist_plot + readable_theme

# Also improve final combined layout:
combined_grid <- (age_plot | edu_plot | domicile_plot) /
  (gender_dist_plot | edu_dist_plot | domicile_dist_plot) +
  plot_annotation(
    theme = theme(
      plot.title = element_text(size = 18, face = "bold", hjust = 0.5),
      plot.subtitle = element_text(size = 13, hjust = 0.5)
    )
  )

ggsave(
  "plots/descriptive_grid.png",
  combined_grid,
  width = 20,
  height = 12,
  dpi = 900
)