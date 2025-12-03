# NEED TO FIND BACKGROUND MISSION CONDITIONS FOR THE FOLLOWING VARIABLES:
    # "RASTERDEPTH_m" ~ "Bathymetric Depth (m)",
    # "RASTERSLOPE_deg" ~ "Bathymetric Slope (°)",
    # "NEAR_DIST" ~ "Distance to Nearest Shoreline (m)"

# IN BACKGROUND DISTANCE TO NEAREST SHORELINE DATAFRAME:
# *** NEAR_DIST (unique values n=3844): The distance (m) from each point (evenly spaced every 100 m along the KMZ mission path) to the nearest shoreline; USE TO CALCULATE BACKGROUND CONDITIONS FOR DISTANCE TO NEAREST SHORELINE ***
  # ORIG_FID (all rows = 1): OBJECTID of the single line object (aka single row, hence why all rows = 1) from KMLSTX2025_PATHLINE layer
  # ORIG_LEN (unique values n=3844): Cumulative distance from first point (aka row 1); last row = 3.625973 = length of entire line in KMLSTX2025_PATHLINE layer
  # ORIG_SEQ (unique values n=3844): OBJECTID of points in KMLSTX2025_POINTS
  # SUM_Shape_Length, Shape_Length (all rows = 3.625973): Distance of entire single line in KMLSTX2025_PATHLINE layer
  # NEAR_FID (unique values n=4): OBJECTID of the nearest shoreline from shorelines_AroundSTX2025 layer
  # NEAR_X, NEAR_Y: The x- and y-coordinates of the nearest shoreline from shorelines_AroundSTX2025 layer
  # Nearest_Shoreline: Full name of nearest shoreline indicated by identifier code in NEAR_FID col



################################################################################
# IMPORT & REFINE CSVs CREATED IN ARCGIS PRO
################################################################################

library(tidyverse)

# Distance to Nearest Shoreline (m) --------------------------------------------
Distance_to_Nearest_Shoreline <- read_csv("THESIS_DATA/arcgispro_exportedcsvs/KMLSTX2025_POINTS.csv")  # import CSV with 1 row for each point evenly spaced 100-m apart along entire KMZ mission path line AND col with calculated distance (m) from each point in the KMZ mission path to the nearest shoreline

NEARDIST <- Distance_to_Nearest_Shoreline %>%
  select(
    ORIG_SEQ, 
    NEAR_DIST, 
    Nearest_Shoreline
    ) %>%
  rename(
    ptID_NEARDIST = ORIG_SEQ,
    NEAR_DIST_m = NEAR_DIST
    )

# Bathymetric Depth (m) --------------------------------------------------------
Bathymetric_Depth <- read_csv("THESIS_DATA/arcgispro_exportedcsvs/GBC15a_bathySTX2025_POINTS_DD.csv")

RASTERDEPTH <- Bathymetric_Depth %>%
  select(
    pointid, 
    grid_code
    ) %>%
  rename(
    ptID_RASTERDEPTH = pointid,
    RASTERDEPTH_m = grid_code
    )

# Bathymetric Slope (°) --------------------------------------------------------
Bathymetric_Slope <- read_csv("THESIS_DATA/arcgispro_exportedcsvs/GBC15a_slopeSTX2025_POINTS_DD.csv")

RASTERSLOPE <- Bathymetric_Slope %>%
  select(
    pointid, 
    grid_code
    ) %>%
  rename(
    ptID_RASTERSLOPE = pointid,
    RASTERSLOPE_deg = grid_code
    )


################################################################################
# COMBINE DATAFRAMES AND PIVOT LONGER
################################################################################
# start with three completely separate data frames -
# each contain a different number of rows (NEARDIST: 3,844 rows, RASTERDEPTH: 173,735 rows, RASTERSLOPE: 172,829 rows) and a different identifier column (ptID_NEARDIST, ptID_RASTERDEPTH, ptID_RASTERSLOPE) 
# ***BUT each one has only one numeric variable you care about***
# (1) NEAR_DIST_m     (2) RASTERDEPTH_m     (3) RASTERSLOPE_deg

# Create unified long-format data frame with two columns (Variable, Value); rewrites each data frame into the two-column structure and stack them on top of each other
arc_long <- bind_rows(
  NEARDIST %>%
    transmute(
      Variable = "NEAR_DIST_m",
      Value    = NEAR_DIST_m
    ),
  RASTERDEPTH %>%
    transmute(
      Variable = "RASTERDEPTH_m",
      Value    = RASTERDEPTH_m
    ),
  RASTERSLOPE %>%
    transmute(
      Variable = "RASTERSLOPE_deg",
      Value    = RASTERSLOPE_deg
    )
)

arc_long <- arc_long %>%
  rename(Oceanographic_Variable = Variable)

# Create **ARC** BACKGROUND summary table dataframe
arc_summary <- arc_long %>%
  group_by(Oceanographic_Variable) %>%
  summarise(
    n    = sum(!is.na(Value)),
    Mean = mean(Value, na.rm = TRUE),
    SEM  = sd(Value, na.rm = TRUE) / sqrt(n),
    SD   = sd(Value, na.rm = TRUE),
    Min  = min(Value, na.rm = TRUE),
    Max  = max(Value, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    Oceanographic_Variable = case_when(
      Oceanographic_Variable == "RASTERDEPTH_m" ~ "Bathymetric Depth (m)",
      Oceanographic_Variable == "RASTERSLOPE_deg" ~ "Bathymetric Slope (°)",
      Oceanographic_Variable == "NEAR_DIST_m" ~ "Distance to Nearest Shoreline (m)",
      TRUE ~ Oceanographic_Variable
    )
  )




# Reorder rows for table
hbk_summary <- hbk_summary %>%
  mutate(
    Oceanographic_Variable = factor(Oceanographic_Variable,
                                    levels = c(
                                      "Glider Depth (m)",
                                      "Water Temperature (°C)",
                                      "Salinity (PSU)",
                                      "Dissolved Oxygen Concentration (µmol/L)",
                                      "Bathymetric Depth (m)",
                                      "Bathymetric Slope (°)",
                                      "Distance to Nearest Shoreline (m)"
                                    )
    )
  ) %>%
  arrange(Oceanographic_Variable)

