# Desert Sample Sites

Interactive Shiny/Leaflet map for the Desert Moss Project.

Open the hosted app here: <https://ekwealorjtb.shinyapps.io/desert-sample-sites/>

## About

This map contains samples from the Desert Moss Project, proposed sample locations, and relevant and nearby land ownership/permitter layers.

The app includes:

- Existing sample locations from the wrangled Garmin/metadata file `data/garmin_abbey/aug_10_sample_metadata.csv`
- Species IDs joined from the January, February, May, and June ID spreadsheets in `data/`
- Proposed August 2026 sample locations from `data/targeted-august2026.csv`
- Boundary layers from `data/boundaries/sample_site_boundaries.geojson`
- Desert and layer filters
- Species filters for `S. ruralis`, `S. caninervis`, and `Other`
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
data/garmin_abbey/aug_10_sample_metadata.csv
data/garmin_abbey/garmin_export.csv
data/garmin_abbey/sample 001-210 metadata.xlsx
data/January deep spring id 001-036.xlsx
data/February socal id 037-080 .xlsx
data/May socal 2026 ID.xlsx
data/June 2026 deep spring ID 181-210.xlsx
data/targeted-august2026.csv
data/boundaries/sample_site_boundaries.geojson
```

## Notes

The app-ready boundary file is stored at `data/boundaries/sample_site_boundaries.geojson`. The app does not need the original boundary source files to run locally or on shinyapps.io.

The `scripts/build_boundaries.R` script is a local helper for rebuilding the boundary GeoJSON file when source boundary files are available.

The wrangled sample file combines the metadata workbook with Garmin waypoint coordinates. It supplies dates and sample descriptions from metadata, and coordinates from Garmin; Garmin dates and times are not used. June ends at sample 210; when August field samples are added, the final June sample should be labeled `210a` and the first August sample `210b` to keep the two trips distinct. The local `scripts/01wrangle.R` script contains the same warning before its numeric sample-ID cleaning step.

The optional CARTO Light basemap reads `CARTO_API_KEY` from the environment. For local use, put `CARTO_API_KEY=your_key_here` in a project-level `.Renviron` file; `.Renviron` is ignored by Git. Do not commit the key. The Topographic and Satellite basemaps do not require this setting.
