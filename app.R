library(shiny)
library(leaflet)
library(readxl)
library(dplyr)
library(htmltools)
library(bslib)
library(scales)
library(sf)

data_path <- file.path("data", "sample_001-080_metadata.xlsx")
boundary_path <- file.path("data", "boundaries", "sample_site_boundaries.geojson")
targeted_may_path <- file.path("data", "targeted-may2026.csv")
targeted_june_path <- file.path("data", "targeted-june2026.csv")

sample_group <- "Existing samples"

null_coalesce <- function(x, y) {
  if (is.null(x)) y else x
}

desert_labels <- c(
  great_basin = "Great Basin",
  `GB-M_transition` = "Great Basin–Mojave Transition",
  mojave = "Mojave",
  sonoran = "Anza-Borrego"
)

desert_order <- names(desert_labels)

color_families <- list(
  great_basin = c("#acd3fa", "#66b0fa", "#003f7d"),
  mojave = c("#a6e695", "#51a83b", "#115400"),
  sonoran = c("#db7967", "#c41d02"),
  `GB-M_transition` = c("#4bb4cc")
)

pretty_label <- function(x) {
  x <- gsub("[-_]+", " ", as.character(x))
  tools::toTitleCase(x)
}

family_colors <- function(desert, n) {
  family <- color_families[[desert]]
  if (is.null(family)) {
    family <- c("#eeeeee", "#999999", "#444444")
  }

  ramp <- grDevices::colorRampPalette(family)(max(n, 3))
  if (n == 1) {
    ramp[2]
  } else {
    ramp[seq_len(n)]
  }
}

blank_to_missing <- function(x) {
  x <- as.character(x)
  ifelse(is.na(x) | trimws(x) == "", "Not recorded", x)
}

coordinate_number <- function(x) {
  as.numeric(gsub("\u2212", "-", as.character(x), fixed = TRUE))
}

read_targeted_csv <- function(path) {
  bytes <- readBin(path, "raw", n = file.info(path)$size)
  bom <- as.raw(c(0xef, 0xbb, 0xbf))
  utf8_minus <- as.raw(c(0xe2, 0x88, 0x92))

  if (length(bytes) >= 3 && identical(bytes[seq_len(3)], bom)) {
    bytes <- bytes[-seq_len(3)]
  }

  normalized <- raw()
  i <- 1
  while (i <= length(bytes)) {
    if (
      i <= length(bytes) - 2 &&
        identical(bytes[i:(i + 2)], utf8_minus)
    ) {
      normalized <- c(normalized, charToRaw("-"))
      i <- i + 3
    } else {
      normalized <- c(normalized, bytes[[i]])
      i <- i + 1
    }
  }

  text <- rawToChar(normalized)

  read.csv(
    text = text,
    stringsAsFactors = FALSE,
    check.names = FALSE
  )
}

make_targeted_star_svg <- function(fill_color, stroke_color) {
  paste0(
    "<svg xmlns='http://www.w3.org/2000/svg' width='44' height='44' viewBox='0 0 44 44'>",
    "<polygon points='22,2 27.9,15.1 42,16.6 31.5,26.2 34.4,40.2 22,33.1 9.6,40.2 12.5,26.2 2,16.6 16.1,15.1' fill='",
    fill_color,
    "' stroke='",
    stroke_color,
    "' stroke-width='2.4' stroke-linejoin='round'/>",
    "</svg>"
  )
}

make_targeted_star_icon <- function(fill_color, stroke_color, icon_size = 28) {
  makeIcon(
    iconUrl = paste0(
      "data:image/svg+xml;charset=UTF-8,",
      URLencode(make_targeted_star_svg(fill_color, stroke_color), reserved = TRUE)
    ),
    iconWidth = icon_size,
    iconHeight = icon_size,
    iconAnchorX = icon_size / 2,
    iconAnchorY = icon_size / 2,
    popupAnchorX = 0,
    popupAnchorY = -(icon_size * 0.45)
  )
}

build_targeted_layer <- function(path, group_label, fill_color, stroke_color) {
  empty <- data.frame(
    lat = numeric(),
    lng = numeric(),
    site_label = character(),
    site_names_html = character(),
    targeted_count = integer(),
    marker_label = character(),
    popup_html = character(),
    stringsAsFactors = FALSE
  )

  if (!file.exists(path)) {
    return(list(
      group = group_label,
      fill_color = fill_color,
      stroke_color = stroke_color,
      icon = make_targeted_star_icon(fill_color, stroke_color),
      points = empty
    ))
  }

  points <- read_targeted_csv(path) |>
    mutate(
      sample_ID = as.character(sample_ID),
      lat = ifelse(
        toupper(latitude) == "S",
        -abs(coordinate_number(lat_coord)),
        abs(coordinate_number(lat_coord))
      ),
      lng = ifelse(
        toupper(longitude) == "W",
        -abs(coordinate_number(long_coord)),
        abs(coordinate_number(long_coord))
      ),
      site_label = pretty_label(sample_ID),
      site_label_html = htmlEscape(site_label)
    ) |>
    filter(!is.na(lat), !is.na(lng)) |>
    group_by(lat, lng) |>
    summarise(
      site_label = paste(site_label, collapse = ", "),
      site_names_html = paste(site_label_html, collapse = "<br>"),
      targeted_count = n(),
      .groups = "drop"
    ) |>
    mutate(
      marker_label = ifelse(
        targeted_count == 1,
        paste(group_label, site_label, sep = ": "),
        paste(group_label, paste(targeted_count, "sites"), sep = ": ")
      ),
      popup_html = paste0(
        "<div class='targeted-popup'>",
        "<strong>", htmlEscape(group_label), "</strong>",
        "<span class='popup-site'>", site_names_html, "</span>",
        "<dl>",
        "<dt>Coordinates</dt><dd>", sprintf("%.6f, %.6f", lat, lng), "</dd>",
        "</dl>",
        "</div>"
      )
    )

  list(
    group = group_label,
    fill_color = fill_color,
    stroke_color = stroke_color,
    icon = make_targeted_star_icon(fill_color, stroke_color),
    points = points
  )
}

sample_data <- readxl::read_excel(data_path, sheet = "metadata") |>
  mutate(
    desert = as.character(desert),
    site_ID = as.character(site_ID),
    desert_label = ifelse(
      desert %in% names(desert_labels),
      unname(desert_labels[desert]),
      pretty_label(desert)
    ),
    site_label = pretty_label(site_ID),
    lat = ifelse(toupper(latitude) == "S", -abs(lat_coord), abs(lat_coord)),
    lng = ifelse(toupper(longitude) == "W", -abs(long_coord), abs(long_coord)),
    collect_date = as.Date(collect_date),
    collect_date_label = ifelse(
      is.na(collect_date),
      "Not recorded",
      format(collect_date, "%b %d, %Y")
    ),
    gps_flag_label = blank_to_missing(GPS_flag),
    elevation_label = ifelse(
      is.na(`elevation (ft)`),
      "Not recorded",
      paste0(scales::comma(`elevation (ft)`), " ft")
    ),
    county_state = paste(pretty_label(county), state, sep = ", ")
  ) |>
  arrange(factor(desert, levels = desert_order), site_label, sample_ID)

site_palette <- sample_data |>
  distinct(desert, desert_label, site_ID, site_label) |>
  arrange(factor(desert, levels = desert_order), site_label) |>
  group_by(desert) |>
  mutate(site_color = family_colors(first(desert), n())) |>
  ungroup()

sample_data <- sample_data |>
  left_join(site_palette |> select(desert, site_ID, site_color), by = c("desert", "site_ID")) |>
  mutate(
    marker_label = paste0("Sample ", sample_ID, " | ", site_label),
    popup_html = paste0(
      "<div class='sample-popup'>",
      "<strong>Sample ", htmlEscape(sample_ID), "</strong>",
      "<span class='popup-site'>", htmlEscape(desert_label), " / ", htmlEscape(site_label), "</span>",
      "<dl>",
      "<dt>Date</dt><dd>", htmlEscape(collect_date_label), "</dd>",
      if_else(
        is.na(GPS_flag) | trimws(as.character(GPS_flag)) == "",
        "",
        paste0("<dt>GPS_flag</dt><dd>", htmlEscape(gps_flag_label), "</dd>")
      ),
      "<dt>County</dt><dd>", htmlEscape(county_state), "</dd>",
      "<dt>Coordinates</dt><dd>", sprintf("%.6f, %.6f", lat, lng), "</dd>",
      "<dt>Elevation</dt><dd>", htmlEscape(elevation_label), "</dd>",
      "</dl>",
      "</div>"
    )
  )

targeted_layers <- list(
  build_targeted_layer(
    targeted_may_path,
    "Proposed sampling: May 2026",
    "#f7c948",
    "#7a4a00"
  ),
  build_targeted_layer(
    targeted_june_path,
    "Proposed sampling: June 2026",
    "#DC0073",
    "#8a004f"
  )
)

empty_boundaries <- function() {
  sf::st_sf(
    boundary_id = character(),
    boundary_name = character(),
    boundary_group = character(),
    boundary_type = character(),
    source_name = character(),
    stroke_color = character(),
    fill_color = character(),
    weight = numeric(),
    dash_array = character(),
    fill_opacity = numeric(),
    boundary_popup = character(),
    geometry = sf::st_sfc(crs = 4326)
  )
}

boundary_layer_specs <- data.frame(
  boundary_name = c(
    "Burns Pinon Ridge Reserve",
    "Steele/Burnand Anza-Borrego Desert Research Center",
    "Inyo National Forest",
    "Inyo Mountains Wilderness",
    "Piper Mountain Wilderness",
    "Pipes Canyon Preserve",
    "Death Valley National Park",
    "Anza-Borrego Desert State Park"
  ),
  boundary_group = c(
    "UC Nature Reserves",
    "UC Nature Reserves",
    "National Forest Service",
    "Bureau of Land Management",
    "Bureau of Land Management",
    "The Wildlands Conservancy",
    "National Park Service",
    "California State Parks"
  ),
  boundary_type = c(
    "UC reserve",
    "UC reserve",
    "National forest",
    "Wilderness",
    "Wilderness",
    "Conservation land",
    "National park",
    "State park"
  ),
  stringsAsFactors = FALSE
)

boundary_group_styles <- data.frame(
  boundary_group = c(
    "UC Nature Reserves",
    "National Forest Service",
    "Bureau of Land Management",
    "The Wildlands Conservancy",
    "National Park Service",
    "California State Parks"
  ),
  boundary_stroke_color = c(
    "#FBB13C",
    "#854D27",
    "#0B6E4F",
    "#2B4162",
    "#700353",
    "#320D6D"
  ),
  boundary_fill_color = c(
    "#FBB13C",
    "#854D27",
    "#0B6E4F",
    "#2B4162",
    "#700353",
    "#320D6D"
  ),
  stringsAsFactors = FALSE
)

boundary_data <- if (file.exists(boundary_path)) {
  sf::st_read(boundary_path, quiet = TRUE) |>
    sf::st_transform(4326) |>
    filter(boundary_name %in% boundary_layer_specs$boundary_name) |>
    mutate(
      spec_idx = match(boundary_name, boundary_layer_specs$boundary_name),
      boundary_group = boundary_layer_specs$boundary_group[spec_idx],
      boundary_type = boundary_layer_specs$boundary_type[spec_idx],
      style_idx = match(boundary_group, boundary_group_styles$boundary_group),
      stroke_color = boundary_group_styles$boundary_stroke_color[style_idx],
      fill_color = boundary_group_styles$boundary_fill_color[style_idx],
      dash_array = ifelse(is.na(dash_array), "", dash_array),
      boundary_popup = paste0(
        "<div class='boundary-popup'>",
        "<strong>", htmlEscape(boundary_name), "</strong>",
        "<dl>",
        "<dt>Type</dt><dd>", htmlEscape(boundary_type), "</dd>",
        "<dt>Layer</dt><dd>", htmlEscape(boundary_group), "</dd>",
        "</dl>",
        "</div>"
      )
    ) |>
    group_by(
      boundary_name,
      boundary_group,
      boundary_type,
      source_name,
      stroke_color,
      fill_color,
      weight,
      dash_array,
      fill_opacity
    ) |>
    summarise(
      boundary_id = first(boundary_id),
      boundary_popup = first(boundary_popup),
      do_union = TRUE,
      .groups = "drop"
    )
} else {
  empty_boundaries()
}

boundary_group_order <- c(
  "UC Nature Reserves",
  "National Forest Service",
  "Bureau of Land Management",
  "The Wildlands Conservancy",
  "National Park Service",
  "California State Parks"
)

boundary_draw_order <- c(
  "National Forest Service",
  "Bureau of Land Management",
  "The Wildlands Conservancy",
  "National Park Service",
  "California State Parks",
  "UC Nature Reserves"
)

boundary_group_choices <- intersect(boundary_group_order, unique(boundary_data$boundary_group))
boundary_group_choices <- setNames(boundary_group_choices, boundary_group_choices)

desert_choices <- c(
  "All deserts" = "all",
  setNames(desert_order, desert_labels[desert_order])
)

legend_control <- function(points, targeted_layers = targeted_layers) {
  key <- points |>
    distinct(desert, desert_label, site_ID, site_label, site_color) |>
    arrange(factor(desert, levels = desert_order), site_label)

  active_deserts <- intersect(desert_order, unique(key$desert))
  other_deserts <- setdiff(unique(key$desert), active_deserts)
  active_deserts <- c(active_deserts, other_deserts)
  tags$div(
    class = "map-legend",
    tags$div(class = "legend-title", "Site"),
    if (length(targeted_layers) > 0) {
      tags$div(
        class = "legend-targets",
        lapply(targeted_layers, function(layer) {
          tags$div(
            class = "legend-target",
            tags$span(
              class = "legend-target-star",
              style = paste0(
                "background:", layer$fill_color, ";",
                "box-shadow: inset 0 0 0 1px ", layer$stroke_color, ";"
              )
            ),
            tags$span(layer$group)
          )
        })
      )
    },
    lapply(active_deserts, function(desert_name) {
      rows <- key[key$desert == desert_name, , drop = FALSE]

      tags$div(
        class = "legend-group",
        tags$div(class = "legend-desert", rows$desert_label[[1]]),
        lapply(seq_len(nrow(rows)), function(i) {
          tags$div(
            class = "legend-row",
            tags$span(
              class = "legend-swatch",
              style = paste0("background:", rows$site_color[[i]], ";")
            ),
            tags$span(rows$site_label[[i]])
          )
        })
      )
    })
  )
}

add_sample_markers <- function(map, points, show_labels) {
  labels <- if (isTRUE(show_labels)) points$marker_label else NULL

  map |>
    addCircleMarkers(
      data = points,
      group = sample_group,
      lng = ~lng,
      lat = ~lat,
      radius = 8,
      stroke = TRUE,
      color = "#1f2933",
      weight = 1.1,
      fillColor = ~site_color,
      fillOpacity = 0.9,
      popup = ~popup_html,
      label = labels,
      options = pathOptions(pane = "samplePane"),
      labelOptions = labelOptions(
        direction = "auto",
        textsize = "12px",
        style = list(
          "font-weight" = "600",
          "color" = "#17202a",
          "box-shadow" = "none"
        )
      )
    )
}

add_targeted_markers <- function(map, layer) {
  points <- layer$points
  if (nrow(points) == 0) {
    return(map)
  }

  map |>
    addMarkers(
      data = points,
      group = layer$group,
      lng = ~lng,
      lat = ~lat,
      icon = layer$icon,
      popup = ~popup_html,
      label = ~marker_label,
      options = markerOptions(
        pane = "targetedPane",
        zIndexOffset = 1000,
        riseOnHover = TRUE
      ),
      labelOptions = labelOptions(
        direction = "auto",
        textsize = "12px",
        style = list(
          "font-weight" = "700",
          "color" = "#17202a",
          "box-shadow" = "none"
        )
      )
    )
}

add_boundary_polygons <- function(map, boundaries) {
  if (nrow(boundaries) == 0) {
    return(map)
  }

  map |>
    addPolygons(
      data = boundaries,
      group = "Boundaries",
      color = ~stroke_color,
      opacity = 0.92,
      weight = ~weight,
      dashArray = ~dash_array,
      fillColor = ~fill_color,
      fillOpacity = ~fill_opacity,
      smoothFactor = 0.7,
      popup = ~boundary_popup,
      label = ~boundary_name,
      options = pathOptions(pane = "boundaryPane"),
      labelOptions = labelOptions(
        direction = "auto",
        textsize = "12px",
        style = list(
          "font-weight" = "700",
          "color" = "#17202a",
          "box-shadow" = "none"
        )
      ),
      highlightOptions = highlightOptions(
        weight = 3,
        opacity = 1,
        bringToFront = TRUE
      )
    )
}

fit_map_to_points <- function(proxy, points, single_zoom = 13) {
  if (nrow(points) == 0) {
    return(proxy)
  }

  if (nrow(points) == 1) {
    return(proxy |> setView(lng = points$lng[[1]], lat = points$lat[[1]], zoom = single_zoom))
  }

  lng_pad <- max((max(points$lng) - min(points$lng)) * 0.12, 0.01)
  lat_pad <- max((max(points$lat) - min(points$lat)) * 0.12, 0.01)

  proxy |>
    fitBounds(
      lng1 = min(points$lng) - lng_pad,
      lat1 = min(points$lat) - lat_pad,
      lng2 = max(points$lng) + lng_pad,
      lat2 = max(points$lat) + lat_pad
    )
}

ui <- fluidPage(
  theme = bs_theme(version = 5, primary = "#2f6878"),
  tags$head(
    tags$title("Sample Locations"),
    tags$style(HTML("
      html, body, .container-fluid {
        height: 100%;
        margin: 0;
        padding: 0;
        color: #17202a;
        background: #f4f6f2;
        font-family: -apple-system, BlinkMacSystemFont, 'Segoe UI', sans-serif;
      }

      .app-shell {
        display: grid;
        grid-template-columns: minmax(290px, 350px) minmax(0, 1fr);
        min-height: 100vh;
      }

      .sidebar {
        background: #fbfcf8;
        border-right: 1px solid #d9dfd2;
        padding: 22px 20px;
        overflow-y: auto;
        z-index: 500;
      }

      .sidebar h1 {
        font-size: 1.55rem;
        line-height: 1.2;
        margin: 0 0 18px;
        letter-spacing: 0;
      }

      .sidebar h2 {
        font-size: 0.82rem;
        font-weight: 800;
        letter-spacing: 0.08em;
        margin: 22px 0 10px;
        text-transform: uppercase;
        color: #52605a;
      }

      .stat-grid {
        display: grid;
        grid-template-columns: 1fr 1fr;
        gap: 10px;
        margin-bottom: 18px;
      }

      .stat {
        border: 1px solid #dfe5d8;
        border-radius: 8px;
        padding: 10px 12px;
        background: #ffffff;
      }

      .stat-value {
        display: block;
        font-size: 1.35rem;
        font-weight: 800;
        line-height: 1.1;
      }

      .stat-label {
        display: block;
        color: #657267;
        font-size: 0.78rem;
        margin-top: 4px;
      }

      .form-label, .control-label {
        font-size: 0.78rem;
        font-weight: 800;
        color: #52605a;
        letter-spacing: 0.06em;
        text-transform: uppercase;
      }

      .form-select, .form-control {
        border-radius: 7px;
        border-color: #cbd5c3;
      }

      .form-check {
        margin-top: 4px;
      }

      .site-list {
        display: grid;
        gap: 8px;
      }

      .boundary-list {
        display: grid;
        gap: 8px;
        margin-top: 10px;
      }

      .boundary-row {
        align-items: center;
        display: grid;
        grid-template-columns: 22px minmax(0, 1fr) auto;
        gap: 8px;
      }

      .boundary-line {
        border-top: 3px solid #52605a;
        width: 22px;
      }

      .boundary-name {
        min-width: 0;
        overflow-wrap: anywhere;
        font-weight: 650;
      }

      .boundary-count {
        color: #657267;
        font-size: 0.82rem;
      }

      .site-row {
        display: grid;
        grid-template-columns: 20px minmax(0, 1fr) auto;
        align-items: center;
        gap: 8px;
        padding: 8px 0;
        border-bottom: 1px solid #e3e8df;
      }

      .site-swatch, .legend-swatch {
        box-sizing: border-box;
        display: block;
        width: 16px;
        height: 16px;
        border-radius: 50%;
        border: 1px solid rgba(23, 32, 42, 0.35);
      }

      .legend-swatch {
        flex: 0 0 18px;
        width: 18px;
        height: 18px;
      }

      .site-name {
        min-width: 0;
        overflow-wrap: anywhere;
        font-weight: 650;
      }

      .site-count {
        color: #657267;
        font-size: 0.82rem;
      }

      .map-shell {
        min-height: 100vh;
        position: relative;
      }

      #map {
        height: 100vh !important;
        width: 100%;
      }

      .map-legend {
        background: rgba(255, 255, 255, 0.95);
        border: 1px solid rgba(23, 32, 42, 0.16);
        border-radius: 8px;
        box-shadow: 0 10px 30px rgba(23, 32, 42, 0.16);
        color: #17202a;
        max-height: min(520px, calc(100vh - 150px));
        max-width: min(340px, calc(100vw - 36px));
        overflow-y: auto;
        padding: 14px 16px;
        min-width: 270px;
      }

      .legend-title {
        font-size: 0.95rem;
        font-weight: 800;
        margin-bottom: 8px;
      }

      .legend-target {
        align-items: center;
        display: flex;
        gap: 10px;
        font-weight: 750;
        line-height: 1.3;
        min-height: 24px;
      }

      .legend-targets {
        border-bottom: 1px solid #dde4d7;
        display: grid;
        gap: 7px;
        margin-bottom: 9px;
        padding-bottom: 9px;
      }

      .legend-target-star {
        background: #f7c948;
        clip-path: polygon(50% 0%, 63% 34%, 99% 36%, 71% 58%, 81% 93%, 50% 74%, 19% 93%, 29% 58%, 1% 36%, 37% 34%);
        display: block;
        flex: 0 0 20px;
        height: 20px;
        width: 20px;
      }

      .legend-group + .legend-group {
        border-top: 1px solid #dde4d7;
        margin-top: 9px;
        padding-top: 8px;
      }

      .legend-desert {
        font-size: 0.82rem;
        font-weight: 800;
        margin-bottom: 6px;
        color: #4f5b55;
      }

      .legend-row {
        align-items: center;
        display: flex;
        gap: 10px;
        line-height: 1.3;
        margin-top: 7px;
        min-height: 20px;
      }

      .leaflet-popup-content {
        margin: 12px 14px;
      }

      .sample-popup, .targeted-popup {
        min-width: 210px;
      }

      .sample-popup strong, .targeted-popup strong {
        display: block;
        font-size: 1rem;
        margin-bottom: 2px;
      }

      .popup-site {
        display: block;
        font-weight: 700;
        margin-bottom: 8px;
      }

      .sample-popup dl, .targeted-popup dl {
        display: grid;
        grid-template-columns: 82px minmax(0, 1fr);
        gap: 4px 10px;
        margin: 0;
      }

      .sample-popup dt, .targeted-popup dt {
        color: #657267;
        font-weight: 700;
      }

      .sample-popup dd, .targeted-popup dd {
        margin: 0;
      }

      .boundary-popup {
        min-width: 220px;
      }

      .boundary-popup strong {
        display: block;
        font-size: 1rem;
        margin-bottom: 8px;
      }

      .boundary-popup dl {
        display: grid;
        grid-template-columns: 66px minmax(0, 1fr);
        gap: 4px 10px;
        margin: 0;
      }

      .boundary-popup dt {
        color: #657267;
        font-weight: 700;
      }

      .boundary-popup dd {
        margin: 0;
      }

      .sample-table-wrap {
        margin-top: 12px;
        max-height: 230px;
        overflow: auto;
        border: 1px solid #dfe5d8;
        border-radius: 8px;
        background: #ffffff;
      }

      .sample-table-wrap table {
        margin-bottom: 0;
        font-size: 0.78rem;
      }

      @media (max-width: 820px) {
        .app-shell {
          grid-template-columns: 1fr;
          grid-template-rows: auto minmax(520px, 1fr);
        }

        .sidebar {
          border-right: 0;
          border-bottom: 1px solid #d9dfd2;
          max-height: 48vh;
        }

        .map-shell, #map {
          min-height: 520px;
          height: 52vh !important;
        }
      }
    "))
  ),
  div(
    class = "app-shell",
    tags$aside(
      class = "sidebar",
      h1("Sample Locations"),
      div(
        class = "stat-grid",
        div(
          class = "stat",
          span(class = "stat-value", textOutput("sample_count", inline = TRUE)),
          span(class = "stat-label", "Samples")
        ),
        div(
          class = "stat",
          span(class = "stat-value", textOutput("site_count", inline = TRUE)),
          span(class = "stat-label", "Sites")
        )
      ),
      selectInput("desert", "Desert", choices = desert_choices, selected = "all"),
      uiOutput("site_filter"),
      checkboxGroupInput(
        "sample_layers",
        "Sample Layers",
        choices = c(
          "Existing samples" = "existing",
          "Proposed sampling" = "proposed"
        ),
        selected = c("existing", "proposed")
      ),
      # checkboxInput("show_labels", "Show sample labels", value = TRUE),
      if (length(boundary_group_choices) > 0) {
        tagList(
          h2("Boundaries"),
          checkboxGroupInput(
            "boundary_groups",
            "Boundary Layers",
            choices = boundary_group_choices,
            selected = boundary_group_choices
          ),
          uiOutput("boundary_summary")
        )
      },
      h2("Samples"),
      div(class = "sample-table-wrap", tableOutput("sample_table"))
    ),
    tags$main(
      class = "map-shell",
      leafletOutput("map")
    )
  )
)

server <- function(input, output, session) {
  filtered_by_desert <- reactive({
    if (identical(input$desert, "all")) {
      sample_data
    } else {
      sample_data |> filter(desert == input$desert)
    }
  })

  output$site_filter <- renderUI({
    sites <- filtered_by_desert() |>
      distinct(site_ID, site_label) |>
      arrange(site_label)

    selectInput(
      "site",
      "Site",
      choices = c("All sites" = "all", setNames(sites$site_ID, sites$site_label)),
      selected = "all"
    )
  })

  filtered_samples <- reactive({
    points <- filtered_by_desert()
    selected_site <- input$site

    if (!is.null(selected_site) && !identical(selected_site, "all")) {
      points <- points |> filter(site_ID == selected_site)
    }

    points
  })

  visible_boundaries <- reactive({
    if (nrow(boundary_data) == 0) {
      return(boundary_data)
    }

    selected_groups <- null_coalesce(input$boundary_groups, character())
    if (length(selected_groups) == 0) {
      return(boundary_data[0, ])
    }

    boundary_data |>
      filter(boundary_group %in% selected_groups) |>
      arrange(factor(boundary_group, levels = boundary_draw_order), boundary_name)
  })

  output$sample_count <- renderText({
    scales::comma(nrow(filtered_samples()))
  })

  output$site_count <- renderText({
    scales::comma(n_distinct(filtered_samples()$site_ID))
  })

  output$boundary_summary <- renderUI({
    rows <- visible_boundaries() |>
      sf::st_drop_geometry() |>
      count(boundary_type, stroke_color, dash_array, name = "features") |>
      arrange(boundary_type)

    if (nrow(rows) == 0) {
      return(tags$div(class = "boundary-count", "No boundary layers selected"))
    }

    tags$div(
      class = "boundary-list",
      lapply(seq_len(nrow(rows)), function(i) {
        tags$div(
          class = "boundary-row",
          tags$span(
            class = "boundary-line",
            style = paste0(
              "border-color:",
              rows$stroke_color[[i]],
              ";",
              if (!identical(rows$dash_array[[i]], "")) {
                "border-style:dashed;"
              } else {
                ""
              }
            )
          ),
          tags$span(class = "boundary-name", rows$boundary_type[[i]]),
          tags$span(class = "boundary-count", paste(scales::comma(rows$features[[i]]), "features"))
        )
      })
    )
  })

  output$sample_table <- renderTable(
    {
      filtered_samples() |>
        transmute(
          Sample = sample_ID,
          Desert = desert_label,
          Site = site_label,
          Date = collect_date_label,
          Latitude = round(lat, 6),
          Longitude = round(lng, 6)
        )
    },
    striped = TRUE,
    hover = TRUE,
    width = "100%"
  )

  output$map <- renderLeaflet({
    leaflet(options = leafletOptions(preferCanvas = TRUE)) |>
      addMapPane("boundaryPane", zIndex = 410) |>
      addMapPane("targetedPane", zIndex = 600) |>
      addMapPane("samplePane", zIndex = 700) |>
      addProviderTiles(providers$Esri.WorldImagery, group = "Satellite") |>
      addProviderTiles(providers$CartoDB.Positron, group = "Light") |>
      addProviderTiles(providers$Esri.WorldTopoMap, group = "Topographic") |>
      addLayersControl(
        baseGroups = c("Topographic", "Satellite", "Light"),
        options = layersControlOptions(collapsed = TRUE)
      ) |>
      addScaleBar(position = "bottomleft")
  })

  observe({
    points <- filtered_samples()
    boundaries <- visible_boundaries()
    req(nrow(points) > 0)
    visible_layers <- null_coalesce(input$sample_layers, c("existing", "proposed"))
    show_existing_samples <- "existing" %in% visible_layers
    show_proposed_sampling <- "proposed" %in% visible_layers

    proxy <- leafletProxy("map") |>
      clearGroup(sample_group) |>
      clearGroup("Boundaries") |>
      removeControl("siteLegend")

    for (layer in targeted_layers) {
      proxy <- proxy |> clearGroup(layer$group)
    }

    proxy <- add_boundary_polygons(proxy, boundaries)
    if (show_existing_samples) {
      proxy <- add_sample_markers(proxy, points, input$show_labels)
    }
    if (show_proposed_sampling) {
      for (layer in targeted_layers) {
        proxy <- add_targeted_markers(proxy, layer)
      }
    }

    proxy <- proxy |>
      addControl(
        html = legend_control(points, targeted_layers),
        position = "bottomright",
        layerId = "siteLegend"
      )
  })

  observeEvent(input$desert, {
    points <- filtered_by_desert()
    req(nrow(points) > 0)
    fit_map_to_points(leafletProxy("map"), points)
  }, ignoreInit = FALSE)

  observeEvent(input$site, {
    points <- filtered_samples()
    req(nrow(points) > 0)
    fit_map_to_points(leafletProxy("map"), points)
  })
}

shinyApp(ui, server)
