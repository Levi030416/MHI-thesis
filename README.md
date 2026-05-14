# Mental Health Index Thesis Repository

This repository contains the R code, data inputs, generated outputs, and supporting literature used for a thesis project on constructing and analysing a Mental Health Index (MHI) from European Social Survey data.

The project builds an individual-level composite Mental Health Index, creates descriptive subgroup analyses, links the index to country-level macro indicators, estimates multilevel regression models, and produces static and interactive visual outputs.

## Repository structure

```text
MHI-thesis/
├── R/
│   ├── 1.index_creation.R
│   ├── 2.analysis_descriptive.R
│   ├── 3.1.analysis_corruption.R
│   ├── 3.2.analysis_gdp.R
│   ├── 3.3.analysis_gini.R
│   ├── map.R
│   ├── data/
│   │   ├── ESS6e02_7.csv
│   │   ├── data_selection.csv
│   │   ├── data_config.xlsx
│   │   ├── corr_df.rds
│   │   ├── worldbank_gdp.csv
│   │   ├── worldbank_gini.csv
│   │   ├── huregions.xlsx
│   │   ├── final_dataset.rds
│   │   ├── country_mhi.rds
│   │   └── hu_region_mhi.rds
│   ├── plots/
│   │   └── appendix/
│   └── html/
└── related_literature/
    ├── OECD_index_construction.pdf
    ├── ESS6e02_7 codebook.html
    ├── ESS6_data_documentation_report_e02_4.pdf
    ├── ESS_weighting_data_1_2.pdf
    └── additional supporting papers
```

## Main workflow

Run the scripts from the `R/` folder in the following order.

### 1. Index construction

```r
source("1.index_creation.R")
```

This script:

- loads the ESS Round 6 dataset,
- selects the relevant mental health and demographic variables,
- applies the coding rules from `data_config.xlsx`,
- handles missing and invalid response categories,
- rescales the selected indicators,
- checks internal consistency and factorability using Cronbach's alpha, KMO, Bartlett's test, PCA, and factor analysis,
- constructs the final Mental Health Index,
- creates country-level and Hungarian regional summaries,
- saves cleaned datasets for later analysis.

Important outputs saved to `R/data/`:

```text
final_dataset.rds
country_mhi.rds
hu_region_mhi.rds
```

### 2. Descriptive analysis

```r
source("2.analysis_descriptive.R")
```

This script uses `final_dataset.rds` to produce weighted descriptive results and subgroup analyses by:

- age,
- education,
- domicile,
- age and education interaction.

Main plot outputs are saved to `R/plots/`, including:

```text
age_histogram.png
edu_histogram.png
edu_distribution.png
domicile_histogram.png
age_education_interaction_heatmap.png
```

### 3. Corruption model

```r
source("3.1.analysis_corruption.R")
```

This script links the individual-level MHI data to a country-level corruption indicator and estimates a multilevel model with individuals nested within countries.

The model includes:

- age,
- gender,
- education,
- domicile,
- country-level corruption.

### 4. GDP model

```r
source("3.2.analysis_gdp.R")
```

This script imports GDP per capita PPP data from `worldbank_gdp.csv`, merges it with the cleaned ESS dataset, log-transforms GDP, and estimates a multilevel model.

It also creates a country-level scatterplot of GDP and average MHI.

### 5. Gini model

```r
source("3.3.analysis_gini.R")
```

This script imports Gini coefficient data from `worldbank_gini.csv`, merges it with the cleaned ESS dataset, and estimates a multilevel model linking inequality to MHI.

It also creates a country-level scatterplot of inequality and average MHI.

### 6. Interactive and static maps

```r
source("map.R")
```

This script creates:

- an interactive European MHI map,
- a Hungary regional MHI map,
- exported HTML versions of the maps,
- static map outputs.

HTML outputs are saved to the `R/html/` folder.

## Required R packages

The scripts install missing packages automatically where this is included in the code. The main required packages are:

```r
haven
dplyr
tidyr
stringr
ggplot2
psych
lme4
lmerTest
readxl
readr
writexl
data.table
patchwork
gridExtra
gt
tibble
flextable
officer
ggforce
grid
performance
parameters
ggrepel
shiny
leaflet
sf
rnaturalearth
rnaturalearthdata
giscoR
htmlwidgets
```

If package installation does not run automatically, install them manually with:

```r
install.packages(c(
  "haven", "dplyr", "tidyr", "stringr", "ggplot2", "psych", "lme4",
  "lmerTest", "readxl", "readr", "writexl", "data.table", "patchwork",
  "gridExtra", "gt", "tibble", "flextable", "officer", "ggforce", "grid",
  "performance", "parameters", "ggrepel", "shiny", "leaflet", "sf",
  "rnaturalearth", "rnaturalearthdata", "giscoR", "htmlwidgets"
))
```

## Data sources

The repository uses:

- European Social Survey Round 6 data,
- ESS documentation and codebook files,
- country-level corruption data,
- World Bank GDP per capita PPP data,
- World Bank Gini coefficient data,
- Hungarian regional crosswalk information.

The main raw ESS file used by the scripts is:

```text
R/data/ESS6e02_7.csv
```

## How to reproduce the analysis

1. Open the repository in RStudio.
2. Set the working directory to the `R/` folder.
3. Run the scripts in numerical order:

```r
source("1.index_creation.R")
source("2.analysis_descriptive.R")
source("3.1.analysis_corruption.R")
source("3.2.analysis_gdp.R")
source("3.3.analysis_gini.R")
source("map.R")
```

The first script creates the cleaned datasets required by the later scripts. If `final_dataset.rds`, `country_mhi.rds`, and `hu_region_mhi.rds` already exist in `R/data/`, the later scripts can be run independently.

## Notes on reproducibility

The code assumes that file paths are relative to the `R/` folder. If the project is opened from the repository root, either set the working directory to `R/` or adjust the file paths accordingly.

Some scripts use local output folders such as:

```text
plots/
plots/appendix/
html/
```

These folders should exist before running the scripts. Empty folders are not tracked by GitHub unless they contain a placeholder file such as `.gitkeep`.

## Project purpose

The purpose of the project is to examine how mental health varies across European countries and social groups, and how these differences relate to broader country-level economic and institutional conditions. The analysis combines composite index construction, weighted descriptive statistics, multilevel modelling, and geographic visualisation.

## Author

Levente Szabó (QAXTCV)  
Repository prepared for MSc thesis work.
