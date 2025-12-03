#######################################################################################
# SALINITY OUTLIER DISCUSSION!!!!!!
#######################################################################################

# There is 1 row in BKG where salinity = 26.23719 (datetime = 2025-03-17 09:13:22; glider_depth = 299.9690). 

# 2025-03-17 09:13:20 salinity = 36.35493 PSU
# *OUTLIER* 2025-03-17 09:13:22 salinity = 26.23719 PSU (>10.1 PSU less than measurements 2 sec before/1 sec after)
# 2025-03-17 09:13:23 salinity = 36.36598 PSU
    # (also considering that all other salinity measurements in BKG were between 35.47295 to 37.77246 PSU (range = ~2.3), a measurement that is ~9.3 PSU lower than the next smallest measurement is weird)

# No detections (HBK) occurred between 3/14/2025 19:41:55 (~3 days before outlier) and 3/17/2025 14:42:02 (~5 hours after outlier). In this window, there were 159,002 salinity measurements recorded - therefore, it is pretty safe to say no interpolated salinity values in HBK were affected by the single outlier in BGK.

#######################################################################################
# OXYGEN CONCENTRATION OUTLIER DISCUSSION!!!!!!
#######################################################################################

# There are 4 rows in BKG where oxygen_concentration = 0.000 (obviously incorrect measurements) that were caused by the sensor being out of the water (glider_depth < 0.08 m for all 4 rows). 

# *One of these BKG oxy=0.000 measurements occurred on 2025-03-17 at 19:45:38 UTC.
# 2025-03-17 16:12:12 UTC: oxy = 192.151 µmol/L (glider_depth = 2.550596 m)
# (3 hours 33 minutes 26 seconds)
# *OUTLIER* 2025-03-17 19:45:38 UTC: oxy = 0.000 µmol/L (glider_depth = 0.00437378 m)
# (51 seconds)
# 2025-03-17 19:46:29 UTC: oxy = 192.496 µmol/L (glider_depth = NA)

# During the window BEFORE the BKG outlier (~3.5 hrs), **19 detections occurred in HBK**. All 19 rows in HBK inside this interval are mathematically forced to be interpolated toward 0 (obviously the closer the timestamp gets to 19:45:38 UTC, the more the interpolated oxy value is pulled towards 0).

# | #  | Timestamp (UTC)     | Latitude  | Longitude  | Glider_depth_m | Oxygen (µmol/L) |
# | -- | ------------------- | --------- | ---------- | -------------- | --------------- |
# | 1  | 2025-03-17 17:00:25 | 17.336611 | −64.250970 | 231.809340     | **148.747372**  |
# | 2  | 2025-03-17 17:00:44 | 17.336649 | −64.250993 | 228.683266     | **148.462284**  |
# | 3  | 2025-03-17 17:02:14 | 17.336825 | −64.251102 | 213.195241     | **147.111867**  |
# | 4  | 2025-03-17 17:39:41 | 17.341240 | −64.253821 | 160.557368     | **113.396448**  |
# | 5  | 2025-03-17 17:40:38 | 17.341352 | −64.253890 | 172.417537     | **112.541183**  |
# | 6  | 2025-03-17 17:41:00 | 17.341395 | −64.253917 | 176.805963     | **112.211081**  |
# | 7  | 2025-03-17 17:44:08 | 17.341764 | −64.254144 | 214.602798     | **109.390210**  |
# | 8  | 2025-03-17 17:51:43 | 17.342658 | −64.254695 | 297.296232     | **102.563100**  |
# | 9  | 2025-03-17 17:55:09 | 17.343063 | −64.254944 | 283.503885     | **99.472145**   |
# | 10 | 2025-03-17 17:56:21 | 17.343204 | −64.255031 | 271.623114     | **98.391811**   |
# | 11 | 2025-03-17 17:57:06 | 17.343293 | −64.255085 | 264.261333     | **97.716602**   |
# | 12 | 2025-03-17 17:57:27 | 17.343334 | −64.255111 | 260.739672     | **97.401505**   |
# | 13 | 2025-03-17 18:09:28 | 17.344750 | −64.255983 | 144.249995     | **86.583162**   |
# | 14 | 2025-03-17 18:54:21 | 17.350041 | −64.259242 | 301.189954     | **46.175675**   |
# | 15 | 2025-03-17 18:54:27 | 17.350053 | −64.259249 | 301.662274     | **46.085647**   |
# | 16 | 2025-03-17 18:54:36 | 17.350070 | −64.259260 | 302.377612     | **45.950605**   |
# | 17 | 2025-03-17 18:59:56 | 17.350699 | −64.259647 | 257.681961     | **41.149121**   |
# | 18 | 2025-03-17 19:00:26 | 17.350758 | −64.259684 | 252.619896     | **40.698982**   |
# | 19 | 2025-03-17 19:04:14 | 17.351206 | −64.259959 | 213.163658     | **37.277925**   |
# ----------------------------------------------------------------------------------------


#######################################################################################
#######################################################################################
#######################################################################################


#######################################################################################
# RECALCULATE INERPOLATED SALINITY AND D.O. VALUES REMOVING OUTLIERS IN ERDDAP (BKG) DATA
#######################################################################################
library(tidyverse)
library(lubridate)

# Import background mission data
bkg <- read_csv("thesis_data/OUTLIERS_fix_interpolation_calculations/original_data_OUTLIERS/outliers_erddap_UVI2_Around_STX_20250306.csv")

# Import humpback detection data with GIS calculations
hbk <- read_csv("thesis_data/OUTLIERS_fix_interpolation_calculations/original_data_OUTLIERS/outliers_GISCALC_HUWH_AroundSTX2025.csv")

# -----------------------------------------------------------------------------------------
# PREPARE HBK DETECTION DATA
# -----------------------------------------------------------------------------------------

# REMOVE SALINITY & OXYGEN CONCENTRATION COLUMNS
FIX_hbk <- hbk %>%
  select(-"Salinity_PSU", -"Oxygen_concentration_µmol_L")

# DATETIME FORMAT (convert time format to as.POSIXct in new 'datetime_UTC' column and sort chronologically (mdy_hms() automatically handles: 1- or 2-digit months, 1- or 2-digit days, and 12-hour clock with no AM/PM))
FIX_hbk <- FIX_hbk %>%
  mutate(datetime_UTC = mdy_hms(DateTime_UTC, tz = "UTC")) %>%   # new datetime_UTC col with POSIXct format
  select(-"DateTime_UTC") %>%   # remove old DateTime_UTC col that has chr format (not POSIXct)
  rename(datetime_AST = DateTime_Local) %>%   # rename local datetime column to match new UTC col naming format
  mutate(datetime_AST = mdy_hms(datetime_AST, tz = "America/St_Thomas"), datetime_AST = format(datetime_AST, "%Y-%m-%d %H:%M:%S")) %>%   # convert datetime_AST col to POSIXct using mdy_hms(); ensure datetime_AST col is printing in yyyy-mm-dd HH:MM:SS format
  arrange(datetime_UTC) %>%   # sort rows chronologically
  relocate(datetime_UTC, .before = 1) %>% relocate(Trajectory, .after = last_col())  # reorder cols (for some reason last_col() is a tidyverse function but first_col() is not)

# -----------------------------------------------------------------------------------------
# PREPARE BKG ERDDAP MISSION DATA
# -----------------------------------------------------------------------------------------
# datetime column in BKG dataframe is already in POSIXct format (dttm (1): datetime)

# REMOVE OUTLIERS (change all cells with outlier values to NA *WITHOUT AFFECTING ANY OTHER COLUMNS IN THOSE ROWS!!!)
bkg_CLEAN <- bkg %>%
  mutate(
    salinity = if_else(
      salinity < 35,  # removes value in 1 cell (= 26.23719 PSU) with implausibly low reading considering all other measurements between 35-38 PSU
      NA_real_,       # set outlier cell to NA
      salinity        # keep original value in row for all other columns!
    ),
    oxygen_concentration = if_else(
      oxygen_concentration < 100,  # removes values in 4 cells (= 0.0000 µmol/L) that have DO measurements recorded when sensor was out of water at surface (glider depth < 0.08 m)
      NA_real_,
      oxygen_concentration
    )
  )

# RENAME COLS TO MATCH HBK COL NAMES (for simplicity writing/running interpolation functions)
bkg_CLEAN <- bkg_CLEAN %>%
  rename(
    datetime_UTC             = datetime,
    Latitude                 = latitude,
    Longitude                = longitude,
    Glider_depth_m           = glider_depth,
    Water_temperature_C      = water_temperature,
    Salinity_PSU             = salinity,
    Oxygen_concentration_µmol_L = oxygen_concentration,
    Source_file              = source_file,
    Trajectory               = trajectory
  ) %>%
  select(-date) %>%  # remove unnecessary date col (already have datetime_UTC)
  relocate(Trajectory, Source_file, .after = last_col())  # reorder cols (move trajectory/source_file to end)

# -----------------------------------------------------------------------------------------
# INTERPOLATION FUNCTIONS -----------------------------------------------------------------
# -----------------------------------------------------------------------------------------

# Create 'helper' interpolation function ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# 'interp_one_var' function estimates the value of one variable (e.g., latitude) at a specific timestamp (target_time) by interpolating from 'bkg_CLEAN' dataframe (df)
interp_one_var <- function(target_time, df, varname) {
  
  # remove NA rows for specified variable or time column (so interpolation only uses clean data)
  df <- df %>% filter(!is.na(.data[[varname]]), !is.na(datetime_UTC))
  
  # if the data is empty or the requested time is outside the available bkg_CLEAN time range, return NA
  if (nrow(df) == 0 || target_time < min(df$datetime_UTC) || target_time > max(df$datetime_UTC)) return(NA)
  
# find row in 'bkg_CLEAN' df immediately before and just after target time:
  # 'slice_tail(n = 1)' gets the most recent row before/equal to target_time
  before <- df %>% filter(datetime_UTC <= target_time) %>% slice_tail(n = 1)
  # 'slice_head(n = 1)' gets the earliest row after or equal to target_time
  after  <- df %>% filter(datetime_UTC >= target_time) %>% slice_head(n = 1)
  
  # if either before or after row is missing, return NA
  if (nrow(before) == 0 || nrow(after) == 0) return(NA)
  # if both rows are at the same time (exact match), return the known value (no need to interpolate)
  if (before$datetime_UTC == after$datetime_UTC) return(before[[varname]])
  
  # convert times to numbers (sec since epoch) to calculate a weight (w), which is the fractional position of the target time between the two known times
  t0 <- as.numeric(before$datetime_UTC)
  t1 <- as.numeric(after$datetime_UTC)
  w  <- (as.numeric(target_time) - t0) / (t1 - t0)
  
  # linearly interpolate between values 'b' and 'a' using weight 'w'
  b <- before[[varname]]
  a <- after[[varname]]
  return(b + w * (a - b))
}

# Create environmental variables interpolation function ~~~~~~~~~~~~~~~~~~~~~~~~~

# 'interp_enviro' function returns a tibble for a given time (t) with interpolated environmental variable values by calling 'interp_one_var()' function for each
# SINCE GLIDER DEPTH AND WATER TEMP ALREADY HAVE CORRECT VALUES (NO OUTLIER ISSUES), NOT INCLUDING THESE IN LIST OF ENVIRO VARIABLES TO BE INTERPOLATED; **ONLY INCLUDING VARIABLES WITH OUTLIER ISSUES** !!
interp_enviro <- function(t) {
  tibble(
    Salinity_PSU = interp_one_var(t, bkg_CLEAN, "Salinity_PSU"),
    Oxygen_concentration_µmol_L = interp_one_var(t, bkg_CLEAN, "Oxygen_concentration_µmol_L")
  )
}

# -----------------------------------------------------------------------------------------
# RUN INTERPOLATION FUNCTIONS
# -----------------------------------------------------------------------------------------

# Run 'interp_enviro' function for each datetime_UTC (aka row) in HBK dataframe
# 'map_dfr()' applies 'interp_enviro()' to each timestamp in 'FIX_hbk$datetime_UTC'; results are combined row-by-row into a single dataframe ('enviro')
NEW_interpolated_envirovars <- map_dfr(FIX_hbk$datetime_UTC, interp_enviro)

# Create new dataframe ('INTERP_hbk') that combines detections data in 'FIX_hbk' with new interpolated data for salinity & oxygen in 'NEW_interpolated_envirovars'
INTERP_hbk <- bind_cols(FIX_hbk, NEW_interpolated_envirovars)

# -----------------------------------------------------------------------------------------
# ORGANIZE AND EXPORT FIXED ERDDAP (BKG) AND DETECTION (HBK) DATAFRAMES!
# -----------------------------------------------------------------------------------------

# Move fixed salinity and DO cols back to original location by the rest of glider sensor enviro variable cols
INTERP_hbk <- INTERP_hbk %>%
  relocate(
    Salinity_PSU, Oxygen_concentration_µmol_L,
    .after = Water_temperature_C
  )

# Export HBK detection data with recalculated salinity and D.O. interpolation values as CSV
write_csv(INTERP_hbk, "docs/data/GISCALC_HUWH_AroundSTX2025.csv")

# Export BKG mission data with salinity and D.O. outliers removed
write_csv(bkg_CLEAN, "docs/data/erddap_UVI2_Around_STX_20250306.csv")
