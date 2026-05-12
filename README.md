# Desert Sample Sites

Interactive Shiny/Leaflet map for the Desert Moss Project.

Open the hosted app here: <https://ekwealorjtb.shinyapps.io/desert-sample-sites/>

## About

This map contains samples from the Desert Moss Project, proposed sample locations, and relevant and nearby land ownership/permitter layers.

The app includes:

- Existing sample locations from `data/sample_001-080_metadata.xlsx`
- Proposed May 2026 sample locations from `data/targeted-may2026.csv`
- Proposed June 2026 sample locations from `data/targeted-june2026.csv`
- Boundary layers from `data/boundaries/sample_site_boundaries.geojson`
- Desert and layer filters
- Leaflet basemaps, popups, labels, and a map legend

## Run locally

Clone or download this repository, then open the project folder in R or RStudio.

Install the required packages:

```r
install.packages(c(
  "shiny",
  "leaflet",
  "readxl",
  "dplyr",
  "htmltools",
  "bslib",
  "scales",
  "sf"
))
```

Run the app from the repository root:

```r
shiny::runApp()
```

The repository root should contain `app.R` and the `data/` folder.

## Data files used by the app

```text
app.R
data/sample_001-080_metadata.xlsx
data/targeted-may2026.csv
data/targeted-june2026.csv
data/boundaries/sample_site_boundaries.geojson
```

## Notes

The app-ready boundary file is stored at `data/boundaries/sample_site_boundaries.geojson`. The app does not need the original boundary source files to run locally or on shinyapps.io.

The `scripts/build_boundaries.R` script is a local helper for rebuilding the boundary GeoJSON file when source boundary files are available.
