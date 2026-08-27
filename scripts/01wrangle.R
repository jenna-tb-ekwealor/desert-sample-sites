#DESERTS PROJECT SAMPLE METADATA ------
# use this script to clean exported data from the garmin device. must use an exported csv from the basecamp software, which is how garmin data is accessed. see links below for guidance.

##https://www.garmin.com/en-US/software/basecamp/ ####
##https://support.garmin.com/en-US/?faq=QGKVinNujX4ijcVYhzqyI8###
# PACKAGES------
library(readxl)
library(readr)
library(stringr)
library(forcats)
library(rstudioapi)
library(janitor)
library(dplyr)

# WORKING DIR -------------------------------------------------------------
if (rstudioapi::isAvailable()) {
  wd_script_location <- dirname(rstudioapi::getSourceEditorContext()$path)
  setwd(wd_script_location)
}

# FILE PATHS, everything is in the same folder, use this for most recent CSVs --------------------------------------------------------------

#there is no way to export a cleanish CSV from basecamp in the fashion we save our data in on the garmin device. It comes with alot of messy metadata since the device has so many gps features. this script cleans that messy CSV by eliminating alot of useless rows that are at the top of an export, so check to see what your export looks like before trying it with this script.
garmin_raw_path          <- "garmin_export.csv"

#   LOAD TABLE------
garmin_lines <- readr::read_lines(garmin_raw_path)
wpt_start <- which(garmin_lines == "wpt") + 1
wpt_end <- wpt_start + which(garmin_lines[(wpt_start + 1):length(garmin_lines)] == "")[1] - 1

garmin_raw <- readr::read_csv(
  paste(garmin_lines[wpt_start:wpt_end], collapse = "\n"),
  name_repair = "minimal",
  show_col_types = FALSE
) %>%
  select(1:4, 8) %>%
  setNames(c("ID", "lat_coord", "long_coord", "elevation(ft)", "sample_ID"))

garmin_clean <- garmin_raw %>%
  filter(str_detect(sample_ID, "^\\s*\\d")) %>%
  mutate(
    sample_ID = str_extract(sample_ID, "^\\s*\\d+"),
    sample_ID = str_sub(sample_ID, -3),
    sample_ID = as.numeric(sample_ID)
  ) %>%
  filter(sample_ID <= 500) %>%
  arrange(sample_ID) %>%
  mutate(sample_ID = str_pad(sample_ID, width = 3, pad = "0"))

# IMPORTANT SAMPLE-NUMBER NOTE:
# June ends at sample 210. When August field samples are added, rename June's
# final sample to 210a and August's first sample to 210b before joining. Keep
# the suffixes in the metadata so the two trips remain distinct; do not let
# numeric coercion collapse both records to sample 210.

#missing metadata------
metadata <- read_excel(
  "sample 001-210 metadata.xlsx",
  sheet = 1
) %>%
  clean_names() %>%
  rename(
    sample_ID = sample_id) %>%
  mutate(
    sample_ID = str_pad(as.numeric(sample_ID), width = 3, pad = "0")
  ) %>%
  select(sample_ID, lat_coord, long_coord, everything())


# In Garmin but not metadata
garmin_missing_from_metadata <- garmin_clean %>%
  anti_join(metadata, by = "sample_ID")

# In metadata but not Garmin, should just be 081 and 180
metadata_missing_from_garmin <- metadata %>%
  anti_join(garmin_clean, by = "sample_ID")

#join metadata------
metadata_keep <- metadata %>%
  mutate(
    sample_ID = str_pad(as.numeric(sample_ID), width = 3, pad = "0")
  ) %>%
  select(
    sample_ID,
    collect_date,
    desert,
    site_id,
    full_coord,
    latitude,
    longitude,
    county,
    state,
    permit,
    gps_flag
  )

garmin_with_metadata <- garmin_clean %>%
  left_join(metadata_keep, by = "sample_ID")

#save most recent version, each time you add more coords for samples, save the most recent version and avoid over writing old ones so we have a backup log------

dir.create("output", showWarnings = FALSE)
today_label <- format(Sys.Date(), "%b_%d") %>%
  str_to_lower()
base_path <- file.path(
  "output",
  paste0(today_label, "_sample_metadata.csv")
)
output_path <- base_path
version <- 2

while (file.exists(output_path)) {
  output_path <- file.path(
    "output",
    paste0(today_label, "_sample_metadata_v", version, ".csv")
  )
  version <- version + 1
}

readr::write_csv(garmin_with_metadata, output_path)
