#   QUICK DATA INSPECTION FOR OUTLIERS

names(all_obs)

# ok, 270 variables in file, wow.
# lets do a dot-plot of each measurement variable


# 1. # First set of plots for data that do not have a flag

# vector of data with no flags
dat_no_flag <- all_obs %>%
  select(matches(paste(c("sample_date", "lat", "long", "site_depth", "deep_sample_depth_m", "shallow_sample_depth_m", "ch4_diffusion_best", 
                       "ch4_ebullition", "ch4_total", "co2_diffusion_best", "co2_ebullition", "co2_total", 
                       "n2o_ebullition", "date_time"), collapse = "|"))) %>%
  select(!contains("units")) %>%
  {names(.)}

# # Excellent tool for inspecting data, but requires manual interaction
# # which complicates sourcing the script. Commenting out, but uncomment
# # when ready to review data
# 
# # Interactive plot.
# for (i in 1:length(dat_no_flag)) {
#   p1 <- ggplot(all_obs, aes_string("lake_id", dat_no_flag[i])) + geom_point() +
#     theme(axis.text.x = element_text(angle = 90))
#   print(p1)
#   readline(prompt="Press [enter] to proceed")
# }

# CH4 diffusion > 150 
# lake 265, sites 7 and 8, omit
# lake 326, site 7, omit
all_obs %>% filter(ch4_diffusion_best > 150) %>% 
  select(lake_id, site_id, ch4_diffusion_best)

all_obs <- all_obs %>%
  # omit bad diffusive emission rates
  mutate(ch4_diffusion_best = case_when(lake_id == "265" & (site_id %in% 7:8) ~ NA_real_,
                                        lake_id == "65" & (site_id %in% c(15,20)) ~ NA_real_,
                                        lake_id == "326" & site_id == 7 ~ NA_real_,
                                        TRUE ~ ch4_diffusion_best),
         # recalculate total
         ch4_total = ch4_diffusion_best + ch4_ebullition)

# 2. Data that do have a flag.  Color code points by flag value

# vector of column names that have an accompanying flag
dat_flag_names <- all_obs %>% 
  select(!matches(c("lake_id|site_id|eval_status|visit|units|flag|comment"))) %>%
  select(!matches(paste(dat_no_flag, collapse = "|"))) %>%
  # exclude a few variables that don't have flags
  select(-volumetric_ebullition, -contains("po4"), -air_temp,
         -co2_deployment_length, -ch4_deployment_length) %>%
  {names(.)}

# Excellent tool for inspecting data, but requires manual interaction
# which complicates sourcing the script. Commenting out, but uncomment
# when ready to review data

# Interactive plot
# for (i in 1:length(dat_flag_names)) {
#   p1 <- ggplot(all_obs, aes_string("lake_id", dat_flag_names[i], color = paste0(dat_flag_names[i], "_flags"))) + geom_point()
#   print(p1)
#   readline(prompt="Press [enter] to proceed")
# }


# 3. values by depth
dat_no_sonde <- sub('.*\\_', '', dat_flag_names) %>%
  unique %>%
  .[!(grepl(c("sonde|ph|cond|temp|turb|lab|ratio|ch4|co2|n2o"), .))] %>%
  replace(., . == "3", "no2_3") %>%
  c(., "ch4_sat_ratio", "co2_sat_ratio",   
    "n2o_sat_ratio")

# Excellent tool for inspecting data, but requires manual interaction
# which complicates sourcing the script. Commenting out, but uncomment
# when ready to review data

# for (i in 1:length(dat_no_sonde)) { 
#   
#   poo <- all_obs %>%
#     select(lake_id, visit, matches("deep|shallow") & !matches("date|units|flag|comment|depth|sonde|ph|do_mg|cond|temp|turb|lab")) %>%
#     filter(!is.na(deep_ba)) %>% # exclude empty row.  could filter on any analyte
#     pivot_longer(!c(lake_id, visit)) %>% 
#     mutate(depth = case_when(grepl("deep", name) ~ "deep",
#                              grepl("shallow", name) ~ "shallow"),
#            name = case_when(grepl("_no2_3", name) ~ "no2_3",  # line below will convert _no2_3 to 3.  Fix here
#                             name == "deep_ch4_sat_ratio" ~ "ch4_sat_ratio",
#                             name == "deep_co2_sat_ratio" ~ "co2_sat_ratio",
#                             name == "deep_dissolved_ch4" ~ "dissolved_ch4",
#                             name == "deep_dissolved_co2" ~ "dissolved_co2",
#                             name == "deep_dissolved_n2o" ~ "dissolved_n2o",
#                             name == "deep_n2o_sat_ratio" ~ "n2o_sat_ratio",
#                             name == "shallow_ch4_sat_ratio" ~ "ch4_sat_ratio",
#                             name == "shallow_co2_sat_ratio" ~ "co2_sat_ratio",
#                             name == "shallow_dissolved_ch4" ~ "dissolved_ch4",
#                             name == "shallow_dissolved_co2" ~ "dissolved_co2",
#                             name == "shallow_dissolved_n2o" ~ "dissolved_n2o",
#                             name == "shallow_n2o_sat_ratio" ~ "n2o_sat_ratio",
#                             TRUE ~ sub('.*\\_', '', name))) %>% # extract characters after last _
#     # filter(lake_id == "1", depth == "deep") %>%
#     # select(name) %>%
#     # janitor::get_dupes(lake_id, name, visit, depth)  #mg is still duplicated?
#     pivot_wider(id_cols = c(lake_id, visit, depth), names_from = name, values_from = value)
#   
#   # note use of .data[[]] to read values from string.  this replaces aes_string
#   p1 <- ggplot(poo, aes(lake_id, .data[[dat_no_sonde[i]]], color = depth)) + 
#     geom_point() +  #ggplot(poo, aes_string("lake_id", dat_no_sonde[i], color = "depth")) + geom_point()
#     #geom_jitter() +
#     theme(axis.text.x = element_text(angle = 90))
#   print(p1)
#   readline(prompt="Press [enter] to proceed")
# }

#mn, nh4, ni, op, p, tn, and tp tend to be > in deep than shallow

# 3. How many lakes have an ebullition value
# all. good
all_obs %>% group_by(lake_id) %>%
  summarize(freq = sum(!is.na(ch4_ebullition))) %>% 
  filter(freq == 0) %>%
  print(n=Inf)


# 4. How many lakes don't have CH4 diffusion
# only 1 lake: 253
all_obs %>% group_by(lake_id) %>%
  summarize(freq = sum(!is.na(ch4_diffusion_best))) %>% 
  filter(freq == 0) %>%
  print(n=Inf)
