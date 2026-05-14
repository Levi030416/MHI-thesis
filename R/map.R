setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

packages = c("shiny", "leaflet", "sf", "dplyr", "readr", "rnaturalearth", "rnaturalearthdata","giscoR", "ggplot2", "htmlwidgets")

package.check <- lapply(
  packages,
  FUN = function(x) {
    if (!require(x, character.only = TRUE)) {
      install.packages(x, dependencies = TRUE)
      library(x, character.only = TRUE)
    }
  }
)

# Load data
country_mhi <- readRDS("data/country_mhi.rds")
hu_region_mhi <- readRDS("data/hu_region_mhi.rds")
world <- ne_countries(scale = "medium", returnclass = "sf")

world <- world %>%
  mutate(join_code = case_when(
    iso_a2 != "-99" ~ iso_a2,
    iso_a2_eh != "-99" ~ iso_a2_eh,
    TRUE ~ NA_character_
  ))

map_data <- world %>%
  left_join(country_mhi, by = c("join_code" = "cntry"))

# Check
map_data %>%
  st_drop_geometry() %>%
  filter(join_code %in% country_mhi$cntry) %>%
  select(name, join_code, MHI_country, N, weight_sum) %>%
  arrange(name)

sum(!is.na(map_data$MHI_country))

# Color palette: low = light blue, high = dark blue
blue_palette <- colorRampPalette(c(
  "#d8efff",
  "#9ccff5",
  "#5fa8dd",
  "#2369a6",
  "#08306b"
))

# Country tooltip
map_data <- map_data %>%
  mutate(
    flag_url = paste0("https://flagcdn.com/w40/", tolower(join_code), ".png"),
    mhi_rank = rank(-MHI_country, ties.method = "min", na.last = "keep"),
    tooltip = ifelse(
      is.na(MHI_country),
      NA,
      paste0(
        "<div class='map-tooltip wow-tooltip'>",
        "<div class='tooltip-top'>",
        "<div class='tooltip-left'>",
        "<div class='tooltip-header'>",
        "<img class='tooltip-flag' src='", flag_url, "'>",
        "<div>",
        "<div class='tooltip-title'>", name, " (", join_code, ")</div>",
        "<div class='tooltip-subtitle'>Rank #", mhi_rank, " by MHI</div>",
        "</div>",
        "</div>",
        "</div>",
        "<div class='tooltip-scorebox'>",
        "<div class='tooltip-score'>", round(MHI_country, 2), "</div>",
        "<div class='tooltip-score-label'>MHI</div>",
        "</div>",
        "</div>",
        "<div class='tooltip-divider'></div>",
        "<div class='tooltip-bottom'>",
        "<span>Observations</span>",
        "<strong>", format(N, big.mark = ","), "</strong>",
        "</div>",
        "</div>"
      )
    )
  )

# Building Hungarian map
hu_regions <- gisco_get_nuts(
  country = "HU",
  nuts_level = 2,
  resolution = "20",
  year = "2021"
)

hu_name_translation <- c(
  "Budapest" = "Budapest",
  "Pest" = "Pest",
  "Közép-Dunántúl" = "Central Transdanubia",
  "Nyugat-Dunántúl" = "Western Transdanubia",
  "Dél-Dunántúl" = "Southern Transdanubia",
  "Észak-Magyarország" = "Northern Hungary",
  "Észak-Alföld" = "Northern Great Plain",
  "Dél-Alföld" = "Southern Great Plain"
)

hu_map_data <- hu_regions %>%
  left_join(hu_region_mhi, by = c("NUTS_ID" = "region_nuts2")) %>%
  mutate(
    region_name_en = dplyr::recode(regionname, !!!hu_name_translation),
    region_rank = rank(-MHI, ties.method = "min", na.last = "keep"),
    tooltip = paste0(
      "<div class='map-tooltip wow-tooltip'>",
      "<div class='tooltip-top'>",
      "<div class='tooltip-left'>",
      "<div class='tooltip-title'>", region_name_en, " (", NUTS_ID, ")</div>",
      "<div class='tooltip-subtitle'>Rank #", region_rank, " among HU regions</div>",
      "</div>",
      "<div class='tooltip-scorebox'>",
      "<div class='tooltip-score'>", round(MHI, 2), "</div>",
      "<div class='tooltip-score-label'>MHI</div>",
      "</div>",
      "</div>",
      "<div class='tooltip-divider'></div>",
      "<div class='tooltip-bottom'>",
      "<span>Observations</span>",
      "<strong>", format(n, big.mark = ","), "</strong>",
      "</div>",
      "</div>"
    )
  )

# Dynamic color domains so differences remain visible
pal <- colorNumeric(
  palette = blue_palette(100),
  domain = map_data$MHI_country,
  na.color = "#eeeeee"
)

pal_hu <- colorNumeric(
  palette = blue_palette(100),
  domain = hu_map_data$MHI,
  na.color = "#eeeeee"
)

# Legend labels show theoretical 0–10 MHI scale
eu_min <- 0
eu_max <- 10

hu_min <- 0
hu_max <- 10

eu_legend <- htmltools::HTML(paste0(
  "<div class='custom-mhi-legend eu-legend'>",
  "<div class='legend-title'>Mental Health Index</div>",
  "<div class='legend-body'>",
  "<div class='legend-bar'></div>",
  "<div class='legend-labels'>",
  "<div>", eu_max, "</div>",
  "<div>5</div>",
  "<div>", eu_min, "</div>",
  "</div>",
  "</div>",
  "</div>"
))

hu_legend <- htmltools::HTML(paste0(
  "<div class='custom-mhi-legend hu-legend'>",
  "<div class='legend-title'>Mental Health Index</div>",
  "<div class='legend-body'>",
  "<div class='legend-bar'></div>",
  "<div class='legend-labels'>",
  "<div>", hu_max, "</div>",
  "<div>5</div>",
  "<div>", hu_min, "</div>",
  "</div>",
  "</div>",
  "</div>"
))

# Hungary standalone map
hungary_map <- leaflet() %>%
  addProviderTiles("CartoDB.PositronNoLabels") %>%
  setView(lng = 19.2, lat = 47.2, zoom = 7) %>%
  addPolygons(
    data = hu_map_data,
    layerId = ~NUTS_ID,
    fillColor = ~pal_hu(MHI),
    weight = 1,
    color = "white",
    fillOpacity = 0.85,
    label = ~lapply(tooltip, htmltools::HTML),
    labelOptions = labelOptions(
      direction = "auto",
      opacity = 1,
      textsize = "12px"
    ),
    highlightOptions = highlightOptions(
      weight = 2,
      color = "#2f2f2f",
      bringToFront = TRUE
    )
  )

hungary_map

# Europe interactive map
europe_map <- leaflet() %>%
  addProviderTiles("CartoDB.PositronNoLabels") %>%
  setView(lng = 15, lat = 54, zoom = 4) %>%
  
  addPolygons(
    data = map_data %>% filter(is.na(MHI_country)),
    group = "europe_countries",
    fillColor = "#e6e6e6",
    weight = 1,
    color = "white",
    fillOpacity = 0.8,
    options = pathOptions(clickable = FALSE)
  ) %>%
  
  addPolygons(
    data = map_data %>% filter(!is.na(MHI_country)),
    group = "europe_countries",
    layerId = ~join_code,
    options = pathOptions(
      className = ~ifelse(join_code == "HU", "hungary-country", "other-country")
    ),
    fillColor = ~pal(MHI_country),
    weight = 1,
    color = "white",
    fillOpacity = 0.82,
    label = ~lapply(tooltip, htmltools::HTML),
    labelOptions = labelOptions(
      direction = "auto",
      opacity = 1,
      textsize = "12px"
    ),
    highlightOptions = highlightOptions(
      weight = 2,
      color = "#2f2f2f",
      bringToFront = TRUE
    )
  ) %>%
  
  addPolygons(
    data = hu_map_data,
    group = "hungary_regions",
    fillColor = ~pal_hu(MHI),
    weight = 1,
    color = "white",
    opacity = 0,
    fillOpacity = 0,
    options = pathOptions(className = "hu-region"),
    label = ~lapply(tooltip, htmltools::HTML),
    labelOptions = labelOptions(
      direction = "auto",
      opacity = 1,
      textsize = "12px"
    ),
    highlightOptions = highlightOptions(
      weight = 2,
      color = "#2f2f2f",
      bringToFront = TRUE
    )
  ) %>%
  
  addControl(eu_legend, position = "bottomleft", className = "euLegendControl") %>%
  addControl(hu_legend, position = "bottomleft", className = "huLegendControl") %>%
  
  htmlwidgets::onRender("
  function(el, x) {

    var map = this;

    var style = document.createElement('style');
    style.innerHTML = `
      .custom-mhi-legend {
        font-family: Arial, sans-serif;
        background: rgba(255,255,255,0.92);
        border: 1px solid rgba(0,0,0,0.14);
        border-radius: 10px;
        box-shadow: 0 4px 14px rgba(0,0,0,0.18);
        padding: 10px 11px;
      }

      .custom-mhi-legend .legend-title {
        font-size: 12px;
        font-weight: 800;
        color: #08306b;
        margin-bottom: 7px;
      }

      .legend-body {
        display: flex;
        align-items: stretch;
        gap: 7px;
      }

      .legend-bar {
        width: 14px;
        height: 78px;
        border-radius: 4px;
        background: linear-gradient(to bottom, #08306b, #2369a6, #5fa8dd, #9ccff5, #d8efff);
        border: 1px solid rgba(0,0,0,0.10);
      }

      .legend-labels {
        height: 78px;
        display: flex;
        flex-direction: column;
        justify-content: space-between;
        font-size: 11px;
        color: #444;
      }

      .leaflet-tooltip {
        background: transparent !important;
        border: none !important;
        box-shadow: none !important;
        padding: 0 !important;
      }

      .wow-tooltip {
        min-width: 265px;
        padding: 13px 14px;
        font-family: Arial, sans-serif;
        background: linear-gradient(145deg, rgba(255,255,255,0.99), rgba(232,244,255,0.97));
        border: 1px solid rgba(8,48,107,0.14);
        border-radius: 16px;
        box-shadow: 0 8px 24px rgba(8,48,107,0.22);
      }

      .tooltip-top {
        display: flex;
        justify-content: space-between;
        align-items: flex-start;
        gap: 14px;
      }

      .tooltip-left {
        min-width: 0;
      }

      .tooltip-header {
        display: flex;
        align-items: center;
        gap: 10px;
      }

      .tooltip-flag {
        width: 34px;
        height: 23px;
        object-fit: cover;
        border-radius: 4px;
        box-shadow: 0 1px 4px rgba(0,0,0,0.25);
      }

      .tooltip-title {
        font-weight: 800;
        font-size: 14px;
        color: #08306b;
        line-height: 16px;
      }

      .tooltip-subtitle {
        font-size: 11px;
        color: #5f7285;
        margin-top: 2px;
      }

      .tooltip-scorebox {
        text-align: right;
        min-width: 58px;
      }

      .tooltip-score {
        font-size: 26px;
        font-weight: 900;
        color: #08306b;
        line-height: 26px;
      }

      .tooltip-score-label {
        font-size: 10px;
        font-weight: 700;
        color: #6b7f91;
        letter-spacing: 0.5px;
        margin-top: 2px;
      }

      .tooltip-divider {
        height: 1px;
        background: linear-gradient(to right, rgba(8,48,107,0.28), rgba(8,48,107,0));
        margin: 10px 0 9px 0;
      }

      .tooltip-bottom {
        display: flex;
        justify-content: space-between;
        font-size: 12px;
      }

      .tooltip-bottom span {
        color: #667;
      }

      .tooltip-bottom strong {
        color: #111;
        font-weight: 800;
      }

      #backToEurope {
        background: rgba(255,255,255,0.94);
        border: 1px solid rgba(0,0,0,0.16);
        border-radius: 8px;
        padding: 7px 11px;
        font-size: 12px;
        font-weight: 700;
        cursor: pointer;
        box-shadow: 0 3px 10px rgba(0,0,0,0.14);
      }

      #backToEurope:hover {
        background: white;
      }
    `;
    document.head.appendChild(style);

    document.querySelectorAll('.huLegendControl').forEach(function(el){
      el.style.display = 'none';
    });

    var backButton = L.control({position: 'topright'});

    backButton.onAdd = function(map) {
      var div = L.DomUtil.create('div', 'info legend');
      div.id = 'backButtonContainer';
      div.style.display = 'none';
      div.innerHTML = '<button id=\"backToEurope\">Back to Europe</button>';
      return div;
    };

    backButton.addTo(map);

    document.getElementById('backToEurope').onclick = function() {
      location.reload();
    };

    setTimeout(function() {

      map.eachLayer(function(layer) {

        if (layer._path && layer.options && layer.options.className === 'hu-region') {
          layer._path.style.pointerEvents = 'none';
        }

        if (layer._path && layer.options && layer.options.className === 'hungary-country') {

          layer.on('click', function() {

            map.fitBounds([
              [45.7, 16.0],
              [48.7, 22.9]
            ]);

            map.eachLayer(function(l) {
              if (l.options && l.options.group === 'europe_countries') {
                map.removeLayer(l);
              }
            });

            map.eachLayer(function(l) {
              if (l._path && l.options && l.options.className === 'hu-region') {
                l.setStyle({
                  opacity: 1,
                  fillOpacity: 0.85
                });
                l._path.style.pointerEvents = 'auto';
              }
            });

            document.querySelectorAll('.euLegendControl').forEach(function(el){
              el.style.display = 'none';
            });

            document.querySelectorAll('.huLegendControl').forEach(function(el){
              el.style.display = 'block';
            });

            document.getElementById('backButtonContainer').style.display = 'block';

          });

        }

      });

    }, 500);

  }
  ")

europe_map

# Creating html
htmlwidgets::saveWidget(
  europe_map,
  "html/index.html",
  selfcontained = TRUE
)

htmlwidgets::saveWidget(
  hungary_map,
  "html/hungary_map.html",
  selfcontained = TRUE
)
#-------------------------------------------
label_data <- map_data %>%
  filter(!is.na(MHI_country)) %>%
  st_point_on_surface() %>%
  mutate(
    label = paste0(join_code, "\n", sprintf("%.2f", MHI_country))
  )

p_static <- ggplot() +
  geom_sf(
    data = map_data,
    aes(fill = MHI_country),
    color = "white",
    linewidth = 0.25
  ) +
  geom_sf_label(
    data = label_data,
    aes(label = label, geometry = geometry),
    stat = "sf_coordinates",
    size = 2.5,
    fontface = "bold",
    label.size = 0.12,
    label.padding = unit(0.10, "lines"),
    fill = "white",
    alpha = 0.80,
    color = "black"
  ) +
  coord_sf(
    xlim = c(-25, 45),
    ylim = c(29, 72),
    expand = FALSE
  ) +
  scale_fill_gradient(
    low = "#deebf7",
    high = "#08519c",
    na.value = "grey88",
    name = "MHI"
  ) +
  labs(title = "Mental Health Index across European countries") +
  theme_void() +
  theme(
    plot.title = element_text(face = "bold", size = 16, hjust = 0.5),
    legend.position = "right",
    legend.title = element_text(face = "bold"),
    legend.key.height = unit(1.4, "cm"),
    plot.background = element_rect(fill = "white", color = NA),
    panel.background = element_rect(fill = "white", color = NA)
  )

p_static

ggsave(
  "plots/static_europe_mhi.png",
  plot = p_static,
  width = 15,
  height = 8,
  dpi = 900
)
