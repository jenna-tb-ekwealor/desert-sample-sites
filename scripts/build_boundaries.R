library(sf)
library(dplyr)
library(readxl)

sf_use_s2(FALSE)

html_unescape <- function(x) {
  x <- gsub("&amp;", "&", x, fixed = TRUE)
  x <- gsub("&#38;", "&", x, fixed = TRUE)
  x <- gsub("&lt;", "<", x, fixed = TRUE)
  x <- gsub("&gt;", ">", x, fixed = TRUE)
  x <- gsub("&quot;", "\"", x, fixed = TRUE)
  x[x == "<Null>"] <- NA_character_
  x
}

extract_cell <- function(desc, key) {
  pattern <- paste0("<td>", key, "</td>[[:space:]]*<td>([^<]*)</td>")
  matches <- regmatches(desc, regexec(pattern, desc))

  out <- vapply(
    matches,
    function(match) {
      if (length(match) >= 2) {
        match[[2]]
      } else {
        NA_character_
      }
    },
    character(1)
  )

  html_unescape(out)
}

read_kmz_layer <- function(path, layer = NULL) {
  tmp <- tempfile()
  dir.create(tmp)
  unzip(path, exdir = tmp)
  kml <- file.path(tmp, "doc.kml")

  if (is.null(layer)) {
    st_read(kml, quiet = TRUE)
  } else {
    st_read(kml, layer = layer, quiet = TRUE)
  }
}

clean_geom <- function(x) {
  x |>
    st_zm(drop = TRUE, what = "ZM") |>
    st_transform(4326) |>
    st_make_valid() |>
    st_collection_extract("POLYGON", warn = FALSE)
}

make_boundary <- function(
    x,
    boundary_id,
    boundary_name,
    boundary_group,
    boundary_type,
    source_name,
    stroke_color,
    fill_color,
    weight = 2.2,
    dash_array = NA_character_,
    fill_opacity = 0.08,
    tolerance_m = 35) {
  x <- clean_geom(x)
  geom <- st_union(st_geometry(x))

  out <- st_sf(
    boundary_id = boundary_id,
    boundary_name = boundary_name,
    boundary_group = boundary_group,
    boundary_type = boundary_type,
    source_name = source_name,
    stroke_color = stroke_color,
    fill_color = fill_color,
    weight = weight,
    dash_array = dash_array,
    fill_opacity = fill_opacity,
    geometry = geom,
    crs = 4326
  )

  projected <- st_transform(out, 3310)
  projected$geometry <- st_simplify(
    st_geometry(projected),
    dTolerance = tolerance_m,
    preserveTopology = TRUE
  )

  st_transform(st_make_valid(projected), 4326)
}

required_files <- c(
  steele_zip = "/Users/jennaekwealor/Downloads/Steele-Burnan Anza Borrego Reserve/Steele_Burnand_A-B_DRC.zip",
  burns_zip = "/Users/jennaekwealor/Downloads/Burns Pinon Ridge Reserve (14)/Burns_Pinion.zip",
  irma_kmz = "/Users/jennaekwealor/Downloads/-Other Maps of Interest/IRMA NPS and ca.gov sites.kmz",
  california_kmz = "/Users/jennaekwealor/Downloads/California.kmz"
)

missing_files <- required_files[!file.exists(required_files)]
if (length(missing_files) > 0) {
  stop(
    "Missing boundary source files:\n",
    paste(names(missing_files), missing_files, sep = ": ", collapse = "\n"),
    call. = FALSE
  )
}

california_kml <- file.path(tempdir(), "california_boundaries_doc.kml")
unzip(
  required_files[["california_kmz"]],
  files = "doc.kml",
  exdir = dirname(california_kml),
  overwrite = TRUE
)
invisible(file.rename(file.path(dirname(california_kml), "doc.kml"), california_kml))

boundaries <- list(
  make_boundary(
    st_read(
      paste0(
        "/vsizip/",
        required_files[["steele_zip"]],
        "/Steele_Burnand_A-B_DRC_Boundary.shp"
      ),
      quiet = TRUE
    ),
    "steele_burnand_anza",
    "Steele/Burnand Anza-Borrego Desert Research Center",
    "UC reserve boundaries",
    "UC reserve",
    "Steele-Burnand Anza Borrego Reserve",
    "#8a3f5f",
    "#d98aae",
    2.8,
    NA_character_,
    0.12,
    10
  ),
  make_boundary(
    st_read(
      paste0(
        "/vsizip/",
        required_files[["burns_zip"]],
        "/Burns_Pinion_Ridge_BoundaryLW.shp"
      ),
      quiet = TRUE
    ),
    "burns_pinon_ridge",
    "Burns Pinon Ridge Reserve",
    "UC reserve boundaries",
    "UC reserve",
    "Burns Pinon Ridge Reserve",
    "#8a3f5f",
    "#d98aae",
    2.8,
    NA_character_,
    0.12,
    10
  ),
  make_boundary(
    st_read(
      paste0(
        "/vsizip/",
        required_files[["steele_zip"]],
        "/Steele_Burnand_A-B_DSP_Boundary.shp"
      ),
      quiet = TRUE
    ),
    "anza_borrego_dsp",
    "Anza-Borrego Desert State Park",
    "Park and public-land boundaries",
    "State park",
    "Steele-Burnand Anza Borrego Reserve",
    "#2f6b4f",
    "#8ec7a6",
    2.2,
    "6,4",
    0.06,
    80
  )
)

irma_other <- read_kmz_layer(
  required_files[["irma_kmz"]],
  "Other Boundary Files"
)
death_valley <- irma_other |> filter(Name == "Death Valley National Park")
if (nrow(death_valley) > 0) {
  boundaries <- c(
    boundaries,
    list(
      make_boundary(
        death_valley,
        "death_valley_np",
        "Death Valley National Park",
        "Park and public-land boundaries",
        "National park",
        "IRMA NPS and ca.gov sites",
        "#6f5a24",
        "#d8c16f",
        2.1,
        "8,5",
        0.05,
        90
      )
    )
  )
}

samples <- read_excel("data/sample_001-080_metadata.xlsx", sheet = "metadata") |>
  mutate(
    lat = ifelse(toupper(latitude) == "S", -abs(lat_coord), abs(lat_coord)),
    lng = ifelse(toupper(longitude) == "W", -abs(long_coord), abs(long_coord))
  )

sample_bbox <- c(
  xmin = min(samples$lng) - 0.3,
  ymin = min(samples$lat) - 0.3,
  xmax = max(samples$lng) + 0.3,
  ymax = max(samples$lat) + 0.3
)

wkt_filter <- sprintf(
  "POLYGON((%f %f,%f %f,%f %f,%f %f,%f %f))",
  sample_bbox[["xmin"]],
  sample_bbox[["ymin"]],
  sample_bbox[["xmax"]],
  sample_bbox[["ymin"]],
  sample_bbox[["xmax"]],
  sample_bbox[["ymax"]],
  sample_bbox[["xmin"]],
  sample_bbox[["ymax"]],
  sample_bbox[["xmin"]],
  sample_bbox[["ymin"]]
)

california <- st_read(
  california_kml,
  layer = "California",
  wkt_filter = wkt_filter,
  quiet = TRUE
) |>
  st_zm(drop = TRUE, what = "ZM")

california$local_name <- extract_cell(california$Description, "Primary Local Name")
california$owner_name <- extract_cell(california$Description, "Owner Name")
california$designation <- extract_cell(
  california$Description,
  "Primary Designation Type"
)

selected_local_names <- c(
  "Death Valley Wilderness",
  "Inyo National Forest",
  "Piper Mountain Wilderness",
  "White Mountains Wilderness",
  "San Gorgonio Wilderness",
  "Big Morongo Canyon Preserve",
  "San Felipe Valley Wildlife Area",
  "Black Mesa Significant Ecological Area",
  "Pipes Canyon Preserve",
  "Tubb Cyn Vicinity",
  "Tubb Cyn vicinity",
  "Anza-Borrego Foundation",
  "Las Arenas Ranch"
)

ca_selected <- california |>
  filter(local_name %in% selected_local_names) |>
  mutate(
    boundary_name = ifelse(
      local_name == "Tubb Cyn vicinity",
      "Tubb Cyn Vicinity",
      local_name
    ),
    boundary_type = case_when(
      grepl("Wilderness", local_name) |
        grepl("Wilderness", designation) ~ "Wilderness",
      grepl("National Forest", local_name) ~ "National forest",
      grepl("Wildlife Area", local_name) ~ "Wildlife area",
      grepl("Preserve|Foundation|Ranch|Ecological", local_name) ~
        "Conservation land",
      TRUE ~ "Public land"
    )
  ) |>
  group_by(boundary_name, boundary_type, owner_name, designation) |>
  summarise(do_union = TRUE, .groups = "drop")

if (nrow(ca_selected) > 0) {
  ca_selected <- clean_geom(ca_selected) |>
    mutate(
      boundary_id = paste0("ca_", seq_len(n())),
      boundary_group = "Nearby conservation/public lands",
      source_name = "California.kmz",
      stroke_color = case_when(
        boundary_type == "National forest" ~ "#2b6770",
        boundary_type == "Wilderness" ~ "#8a6a2a",
        boundary_type == "Wildlife area" ~ "#4d7c3f",
        TRUE ~ "#7a5443"
      ),
      fill_color = case_when(
        boundary_type == "National forest" ~ "#8bbcc2",
        boundary_type == "Wilderness" ~ "#d6bd76",
        boundary_type == "Wildlife area" ~ "#a8cf8d",
        TRUE ~ "#c99d86"
      ),
      weight = 1.6,
      dash_array = "3,5",
      fill_opacity = 0.035
    ) |>
    select(
      boundary_id,
      boundary_name,
      boundary_group,
      boundary_type,
      source_name,
      stroke_color,
      fill_color,
      weight,
      dash_array,
      fill_opacity,
      geometry
    )

  projected <- st_transform(ca_selected, 3310)
  projected$geometry <- st_simplify(
    st_geometry(projected),
    dTolerance = 75,
    preserveTopology = TRUE
  )
  ca_selected <- st_transform(st_make_valid(projected), 4326)
  boundaries <- c(boundaries, list(ca_selected))
}

out <- do.call(rbind, boundaries) |>
  st_make_valid() |>
  arrange(boundary_group, boundary_name)

dir.create("data/boundaries", recursive = TRUE, showWarnings = FALSE)
out_path <- "data/boundaries/sample_site_boundaries.geojson"
if (file.exists(out_path)) {
  unlink(out_path)
}

st_write(out, out_path, driver = "GeoJSON", quiet = TRUE)

cat("Wrote", nrow(out), "boundary features to", out_path, "\n")
print(
  as_tibble(
    st_drop_geometry(out) |>
      select(boundary_group, boundary_name, boundary_type, source_name)
  ),
  n = Inf
)
