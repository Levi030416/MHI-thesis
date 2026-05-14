setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

library(haven)
library(dplyr)
library(readr)
library(lmerTest)

# Load main dataset
final_df <- readRDS("data/final_dataset.rds")

# Load corruption data and check correctness
corr_df <- readRDS("data/corr_df.rds")

df_corr <- final_df %>%
  left_join(corr_df, by = "uid")

summary(df_corr$corruption)

df_corr %>%
  group_by(cntry) %>%
  summarise(sd_corr = sd(corruption)) %>%
  arrange(desc(sd_corr))

df_corr %>%
  summarise(n = n(), missing = sum(is.na(corruption)))

# Regression

model_corr <- lmer(
  MHI ~ corruption + agea + gndr + eduyrs + domicil + (1 | cntry),
  data = df_corr
)

summary(model_corr)

coef_corr <- summary(model_corr)$coefficients
coef_corr["corruption", ]

ICC_corruption = 0.04165 / (0.04165 + 1.22040)

df_corr %>%
  group_by(cntry) %>%
  summarise(
    mean_corruption = mean(corruption, na.rm = TRUE),
    mean_MHI = weighted.mean(MHI, anweight, na.rm = TRUE)
  ) %>%
  arrange(desc(mean_corruption))

standardize_parameters(model_corr)
r2(model_corr)
null_corr <- lmer(
  MHI ~ agea + gndr + eduyrs + domicil + (1 | cntry),
  data = df_corr,
  REML = FALSE
)

model_corr_ml <- lmer(
  MHI ~ corruption + agea + gndr + eduyrs + domicil + (1 | cntry),
  data = df_corr,
  REML = FALSE
)

anova(null_corr, model_corr_ml)

# ============================================================
# Corruption scatterplot + top/bottom countries
# ============================================================


country_corr_mhi <- df_corr %>%
  group_by(cntry) %>%
  summarise(
    mean_corruption = mean(corruption, na.rm = TRUE),
    mean_MHI = weighted.mean(MHI, anweight, na.rm = TRUE),
    n = n(),
    .groups = "drop"
  )

# Top / Bottom
top_5_corr <- country_corr_mhi %>%
  arrange(desc(mean_corruption)) %>%
  slice(1:5)

bottom_5_corr <- country_corr_mhi %>%
  arrange(mean_corruption) %>%
  slice(1:5)

print(top_5_corr)
print(bottom_5_corr)

# Correlation
cor_corr_mhi <- cor(
  country_corr_mhi$mean_corruption,
  country_corr_mhi$mean_MHI,
  use = "complete.obs"
)

print(cor_corr_mhi)

# Plot
corr_scatter <- ggplot(country_corr_mhi, aes(x = mean_corruption, y = mean_MHI)) +
  geom_point(size = 3, color = "darkblue") +
  geom_smooth(
    method = "lm",
    se = TRUE,
    color = "red",
    linetype = "dashed"
  ) +
  geom_text_repel(
    aes(label = cntry),
    size = 4
  ) +
  labs(
    title = "Country-Level Relationship Between Corruption Perceptions and Mental Health",
    subtitle = "Higher CPI = lower corruption",
    x = "Corruption Perceptions Index (higher = cleaner)",
    y = "Weighted Mean MHI"
  ) +
  theme_minimal(base_size = 14)

corr_scatter