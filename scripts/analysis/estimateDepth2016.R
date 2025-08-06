# site depth wasn't measured in first few lakes in 2016 (oops) but we have bathymetry data
# for those lakes. need to add depth estimates for 2016 lakes missing this measurement 
# (1001, 1002, 1012, 1013, 1016, 1017). See estimateDepth2016.R


# Create list of bathymetry files. Reading rasters created from TIN in Pro.
bathy_2016 <- list(
  bathy_act = raster(paste0(userPath, "lakeDsn/2016_survey/acton/acton_tin_ras")),
  bathy_alum = raster(paste0(userPath, "lakeDsn/2016_survey/alumCreek/Bathymetry/alum_tin_ras")),
  bathy_cowan = raster(paste0(userPath, "lakeDsn/2016_survey/cowan/Bathymetry/cowan_tin_ras")),
  bathy_delaware = raster(paste0(userPath, "lakeDsn/2016_survey/delaware/Bathymetry/delaw_tin_ras")),
  bathy_kiser = raster(paste0(userPath, "lakeDsn/2016_survey/kiserLake/Bathymetry/kisertin_ras")),
  bathy_knox = raster(paste0(userPath, "lakeDsn/2016_survey/knox/Bathymetry/knox_tin_ras"))
)

# lakes/sites where site depth wasn't measured
dat_2016_missing_depth <- dat_2016 %>%
  filter(lake_id %in% c(1001, 1002, 1012, 1013, 1016, 1017),
         is.na(site_depth),
         !is.na(lat),
         !is.na(long)) %>%
  st_as_sf(coords = c("long", "lat"), crs = "EPSG:4326") %>% 
  st_transform(., st_crs(bathy_2016$bathy_act))  # consistent with bathy

# make sure coordinate systems are consistent
st_crs(bathy_2016$bathy_act) == st_crs(dat_2016_missing_depth)

# overlay points on rasters and extract depth
depth_estimates <- map(bathy_2016, # list of bathymetry files
                       ~.x %>% raster::extract(dat_2016_missing_depth)) %>% # extract raster values for sites with missing data
  map_df(., ~cbind(.x)) %>% # collapse into a dataframe of columns
  # collapse into one column
  # https://stackoverflow.com/questions/75308080/row-wise-coalesce-over-all-columns
  mutate(site_depth = do.call(coalesce, across(everything())) %>% # creates matrix column
           as.vector(.) %>% # simplify matrix column to regular column
           "*"(-1)) %>% # multiply by neg 1 to convert to positive values.
  select(site_depth) %>%
  # join with unique IDs
  bind_cols(., 
            dat_2016_missing_depth %>% 
              st_drop_geometry %>%
              select(lake_id, site_id, visit))

# replace missing depth_values in dat_2016 with estimated depths
dat_2016 <- rows_update(dat_2016,
                        depth_estimates,
                        by = c("lake_id", "site_id", "visit")) %>%
  # A few random  missing depths
  rows_update(
    tribble(~lake_id, ~site_id, ~visit, ~site_depth,
            # Harsha: July/August 2017 observations at EUS3, co-located with site 6
            1031, 6, 1, mean(17, 13, 13, 13, 13, 13, 12, 13) * 0.3048, # ft -> m 
            # Harsha: estimated from visual inspection of bathymetry
            1031, 32, 1, 40 * 0.3048, # ft -> m
            # estimated from visual inspection of bathymetry
            1028, 5, 1, 6.5,
            # MJ Kirwan: estimated from visual inspection of bathymetry
            1024, 3, 1, 2,
            # kiser: estimated from visual inspection of bathymetry
            1016, 1, 1, 1.1,
            # cowan: estimated from visual inspection of bathymetry
            1012, 38, 1, 1.1,
            1012, 41, 1, 1.1,
            # cave run: estimated from visual inspection of bathymetry
            1010, 4, 1, 18,
            # caeser Cr: estimated from visual inspection of bathymetry
            1008, 5, 1, 15 * 0.3048), # ft -> m
    by = c("lake_id", "site_id", "visit")
  )
