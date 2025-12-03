library(tidyverse)

gbc <- read_csv("thesis_data/ARCGIS_background_conditions/arcgispro_exportedcsvs/PWCLIP_GBC15a_bathySTX2025_POINTS.csv")

usc_zip <- "thesis_data/ARCGIS_background_conditions/arcgispro_exportedcsvs/USC10m_bathySTX2025_POINTS.zip"
usc_extractcsv_path <- "thesis_data/arcgispro_exportedcsvs"
unzip(usc_zip, exdir = usc_extractcsv_path)


# CALCULATE MEAN, SEM, SD, MIN, MAX FOR GBC AND USC RASTERDEPTH DFs

gbc_summary <- gbc %>%
  select("grid_code") %>%
  rename(GBC = grid_code) %>%
  pivot_longer(
    cols = everything(),
    names_to  = "Resolution",
    values_to = "Value"
  ) %>%
  group_by(Resolution) %>%
  summarise(
    n     = sum(!is.na(Value)),
    Mean  = mean(Value, na.rm = TRUE),
    SEM   = sd(Value, na.rm = TRUE) / sqrt(n),
    SD    = sd(Value, na.rm = TRUE),
    Min   = min(Value, na.rm = TRUE),
    Max   = max(Value, na.rm = TRUE),
    .groups = "drop"
  )

usc_summary <- usc %>%
  select("grid_code") %>%
  rename(USC = grid_code) %>%
  pivot_longer(
    cols = everything(),
    names_to  = "Resolution",
    values_to = "Value"
  ) %>%
  group_by(Resolution) %>%
  summarise(
    n     = sum(!is.na(Value)),
    Mean  = mean(Value, na.rm = TRUE),
    SEM   = sd(Value, na.rm = TRUE) / sqrt(n),
    SD    = sd(Value, na.rm = TRUE),
    Min   = min(Value, na.rm = TRUE),
    Max   = max(Value, na.rm = TRUE),
    .groups = "drop"
  )
  

# COMBINE USC AND GBC ROWS FROM EACH SUMMARY DF

# Create new combined df
combined_summary <- bind_rows(usc_summary, gbc_summary)

# Calculate difference row (USC - GBC)
difference_row <- combined_summary %>%
  summarise(
    Resolution = "Difference (USC-GBC)",
    n    = n[1] - n[2],
    Mean = Mean[1] - Mean[2],
    SEM  = SEM[1] - SEM[2],
    SD   = SD[1] - SD[2],
    Min  = Min[1] - Min[2],
    Max  = Max[1] - Max[2]
  )

# Bind difference row to summary table
combined_summary <- bind_rows(combined_summary, difference_row)

# MEAN RASTERDEPTH: USC = 15.1976 m deeper than GBC
# MAX RASTERDEPTH: GBC deepest point = 758.1826 m deeper than USC
# Overall (mean) bathymetric depth values MINIMALLY changed (+/-15.1976 m) and GBC has greater depth range (758.1826 m deeper) than USC 

# *** = USE GBC15a RASTERS FOR DEPTH AND SLOPE ***
