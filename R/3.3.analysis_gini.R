setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

packages_gini = c("haven", "dplyr", "readr", "lmerTest", "ggplot2", "ggrepel")

package.check <- lapply(
  packages_gini,
  FUN = function(x) {
    if (!require(x, character.only = TRUE)) {
      install.packages(x, dependencies = TRUE)
      library(x, character.only = TRUE)
    }
  }
)

# Load main dataset
final_df <- readRDS("data/final_dataset.rds")

# Load GINI and check correctness
gini_df <- read_csv("data/worldbank_gini.csv") %>%
  rename(
    cntry = country,
    gini = gini
  )

gini_df <- final_df %>%
  left_join(gini_df, by = "cntry")

summary(gini_df$gini)

gini_df %>%
  summarise(missing = sum(is.na(gini)))

# Regression

model_gini <- lmer(
  MHI ~ gini + agea + gndr + eduyrs + domicil + (1 | cntry),
  data = gini_df
)

summary(model_gini)
standardize_parameters(model_gini)
r2(model_gini)


null_gini <- lmer(
  MHI ~ agea + gndr + eduyrs + domicil + (1 | cntry),
  data = gini_df,
  REML = FALSE
)

model_gini_ml <- lmer(
  MHI ~ gini + agea + gndr + eduyrs + domicil + (1 | cntry),
  data = gini_df,
  REML = FALSE
)

# Likelihood ratio test
anova(null_gini, model_gini_ml)

# ------------------------------------------------------------
# Optional: ICC
# ------------------------------------------------------------

ICC_gini <- 0.09774 / (0.09774 + 1.22040)
print(ICC_gini)

#Plots

country_gini_mhi <- gini_df %>%
  group_by(cntry) %>%
  summarise(
    mean_gini = mean(gini, na.rm = TRUE),
    mean_MHI = weighted.mean(MHI, anweight, na.rm = TRUE),
    n = n(),
    .groups = "drop"
  )

# Lowest and highest inequality countries
lowest_5_gini <- country_gini_mhi %>%
  arrange(mean_gini) %>%
  slice(1:5)

highest_5_gini <- country_gini_mhi %>%
  arrange(desc(mean_gini)) %>%
  slice(1:5)

print(lowest_5_gini)
print(highest_5_gini)

# Correlation
cor_gini_mhi <- cor(
  country_gini_mhi$mean_gini,
  country_gini_mhi$mean_MHI,
  use = "complete.obs"
)

print(cor_gini_mhi)

# Scatterplot
gini_scatter <- ggplot(country_gini_mhi, aes(x = mean_gini, y = mean_MHI)) +
  geom_point(size = 3, color = "darkblue") +
  geom_smooth(
    method = "lm",
    se = TRUE,
    color = "red",
    linetype = "dashed"
  ) +
  geom_text_repel(
    aes(label = cntry),
    size = 4,
    max.overlaps = 30
  ) +
  labs(
    subtitle = "Higher Gini = higher income inequality",
    x = "Gini coefficient",
    y = "Weighted Mean MHI"
  ) +
  theme_minimal(base_size = 14)

gini_scatter

ggsave(
  "plots/gini_mhi_country_scatter.png",
  gini_scatter,
  width = 10,
  height = 6,
  dpi = 900
)