setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

packages_gdp = c("haven", "dplyr", "readr", "lmerTest", "performance", 
                 "parameters", "dplyr", "ggplot2", "ggrepel",
                 "grid", "gridExtra", "broom.mixed", "scales")

package.check <- lapply(
  packages_gdp,
  FUN = function(x) {
    if (!require(x, character.only = TRUE)) {
      install.packages(x, dependencies = TRUE)
      library(x, character.only = TRUE)
    }
  }
)



# 1. Import CSV
gdp_ext <- read_csv("data/worldbank_gdp.csv")
final_df <- readRDS("data/final_dataset.rds")

# 2. Rename columns
gdp_ext <- gdp_ext %>%
  rename(
    cntry = code,
    gdp_ppp = gdp_per_capita
  )

# 3. Merge with your existing dataset and check
df_gdp <- final_df %>%
  left_join(gdp_ext, by = "cntry")

df_gdp %>%
  group_by(cntry) %>%
  summarise(gdp = mean(gdp_ppp, na.rm = TRUE))

df_gdp %>%
  summarise(missing_gdp = sum(is.na(gdp_ppp)))

length(unique(df_gdp$cntry))

# 4. Prepare data
df_gdp <- df_gdp %>%
  mutate(log_gdp = log(gdp_ppp))
summary(df_gdp$log_gdp)

df_gdp %>%
  group_by(cntry) %>%
  summarise(sd_gdp = sd(log_gdp)) %>%
  arrange(desc(sd_gdp))

df_gdp %>%
  summarise(n = n(), missing = sum(is.na(log_gdp)))

# 5. Regression

model_gdp <- lmer(
  MHI ~ log_gdp + agea + gndr + eduyrs + domicil + (1 | cntry),
  data = df_gdp
)
summary(model_gdp)

ICC_gdp = 0.053 / (0.053 + 1.220)

cor(df_gdp$MHI, df_gdp$log_gdp, use = "complete.obs")

ICC_gdp
r2(model_gdp)

null_gdp <- lmer(
  MHI ~ agea + gndr + eduyrs + domicil + (1 | cntry),
  data = df_gdp,
  REML = FALSE
)

model_gdp_ml <- lmer(
  MHI ~ log_gdp + agea + gndr + eduyrs + domicil + (1 | cntry),
  data = df_gdp,
  REML = FALSE
)

anova(null_gdp, model_gdp_ml)
standardize_parameters(model_gdp)

# ------------------------------------------------------------
# 1. Country-level averages
# ------------------------------------------------------------

country_gdp_mhi <- df_gdp %>%
  group_by(cntry) %>%
  summarise(
    mean_gdp = mean(gdp_ppp, na.rm = TRUE),
    log_gdp = mean(log_gdp, na.rm = TRUE),
    mean_MHI = weighted.mean(MHI, anweight, na.rm = TRUE),
    n = n(),
    .groups = "drop"
  ) %>%
  arrange(desc(mean_gdp))

print(country_gdp_mhi)

# ------------------------------------------------------------
# 2. Top 5 and Bottom 5 GDP countries
# ------------------------------------------------------------

top_5_gdp <- country_gdp_mhi %>%
  arrange(desc(mean_gdp)) %>%
  slice(1:5)

bottom_5_gdp <- country_gdp_mhi %>%
  arrange(mean_gdp) %>%
  slice(1:5)

print(top_5_gdp)
print(bottom_5_gdp)

# ------------------------------------------------------------
# 3. Scatterplot: GDP vs MHI
# ------------------------------------------------------------

gdp_scatter <- ggplot(country_gdp_mhi, aes(x = mean_gdp, y = mean_MHI)) +
  
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
    title = "Country-Level Relationship Between GDP and Mental Health",
    subtitle = "GDP PPP per capita vs weighted average Mental Health Index",
    x = "GDP PPP per capita (current international $)",
    y = "Weighted Mean MHI"
  ) +
  
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(face = "bold", size = 18),
    plot.subtitle = element_text(size = 12),
    axis.title = element_text(face = "bold")
  )

gdp_scatter

cor_gdp_mhi <- cor(
  country_gdp_mhi$mean_gdp,
  country_gdp_mhi$mean_MHI,
  use = "complete.obs"
)

print(cor_gdp_mhi)

#ResultsTable
gdp_mhi_plot <- ggplot(country_gdp_mhi, aes(x = mean_gdp, y = mean_MHI)) +
  geom_point(
    size = 3.2,
    alpha = 0.85
  ) +
  geom_smooth(
    method = "lm",
    se = TRUE,
    linetype = "dashed",
    linewidth = 0.9
  ) +
  geom_text_repel(
    aes(label = cntry),
    size = 3.6,
    max.overlaps = 40
  ) +
  scale_x_continuous(
    labels = comma
  ) +
  labs(
    subtitle = "GDP PPP per capita and weighted mean MHI across ESS Round 6 countries",
    x = "GDP PPP per capita",
    y = "Weighted mean Mental Health Index"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5, size = 16),
    plot.subtitle = element_text(hjust = 0.5, size = 11),
    axis.title = element_text(face = "bold"),
    panel.grid.minor = element_blank()
  )

gdp_mhi_plot

ggsave(
  "plots/gdp_mhi_country_scatter.png",
  gdp_mhi_plot,
  width = 10,
  height = 6,
  dpi = 900
)
