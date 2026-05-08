# Sample Locations

Shiny Leaflet app for the 001-080 sample metadata workbook.

## Run Locally

Open R in this repository and run:

```r
shiny::runApp()
```

The app reads `data/sample_001-080_metadata.xlsx`, maps the sample coordinates, and colors sites within each desert color family:

- Great Basin: blues
- Mojave: greens
- Anza-Borrego / Sonoran: reds
- Great Basin-Mojave Transition: teals

The map legend groups sites under their desert so the color structure stays visible as filters change.

The map opens with a topographic basemap and also includes satellite imagery and light basemap options.

Targeted May 2026 sampling locations from `data/targeted-may2026.csv` are shown as larger golden yellow stars.

Boundary layers from the local GIS files are prebuilt into `data/boundaries/sample_site_boundaries.geojson`. To rebuild that file from the source KMZ/shapefile downloads, run:

```r
source("scripts/build_boundaries.R")
```
