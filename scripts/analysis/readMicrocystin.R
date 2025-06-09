# script for reading microcystin measured at Narragansett laboratory.

# 11 Jan 2023: Flags already added to data by Jeff Hollister. 
# Commenting out all code related to determining flags. (jwc) 

get_microcystin_data <- function(path) { 
  
  d <- read_csv(path, na = "NA", 
                col_types = "Dnncccncc") %>% #D=date,n=number,c=character
    janitor::clean_names() %>% 
    rename(sample_type = field_dups,
           lake_id = waterbody,
           site_id = site,
           microcystin = value,
           microcystin_units = units,
           microcystin_flags = flag) %>%
    mutate(sample_depth = 
             case_when(sample_type == "blank" ~ "blank", # depth == blank for all blanks
                       sample_type %in% c("unknown", "duplicate") ~ "shallow", # all unknowns collected near a-w interface
                       TRUE ~ "FLY YOU FOOLS!"), # error code
           # Create visit field
           visit = case_when(lake_id %in% c("281", "250") &
                               between(date,
                                       as.Date("2022-08-15"),
                                       as.Date("2022-09-23")) ~ 2,
                             lake_id %in% c("147", "148") &
                               dplyr::between(date,
                                              as.Date("2023-08-01"),
                                              as.Date("2023-08-30")) ~ 2,
                             TRUE ~ 1),
           # flags
           microcystin_flags = case_when(microcystin_flags == "below detection limit; below reporting limit" ~ "ND",
                                         microcystin_flags == "below reporting limit" ~ "L",
                                         TRUE ~ microcystin_flags),
           # fix 69 and 70 lake_id values
           lake_id = case_when(lake_id == 69 & 
                                 date == as.Date("2021-06-22") ~ "69_transitional",
                               lake_id == 69 & 
                                 date == as.Date("2021-06-24") ~ "69_lacustrine",
                               lake_id == 69 & 
                                 date == as.Date("2021-07-13") ~ "69_riverine",
                               lake_id == 70 & 
                                 date == as.Date("2021-06-25") ~ "70_lacustrine",
                               lake_id == 70 & 
                                 date == as.Date("2021-06-29") ~ "70_riverine",
                               lake_id == 70 & 
                                 date == as.Date("2021-07-12") ~ "70_transitional",
                               TRUE ~ as.character(lake_id))) %>%
    select(lake_id, site_id, sample_type, sample_depth, visit,
           microcystin, microcystin_units, microcystin_flags) 
  
  
  return(d)
  
}

path <- paste0(userPath, "data/algalIndicators/surge_microcystin.csv")
microcystin <- get_microcystin_data(path)

# check for dups
# none
microcystin %>% janitor::get_dupes(lake_id, lake_id, site_id, sample_depth, sample_type, visit)

