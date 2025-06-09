# load project libraries
source(paste0("scripts/masterLibrary.R"))

# load data
# Analysis
source(paste0( "scripts/analysis/readSurgeLakes.R")) # read in survey design file
source(paste0( "scripts/analysis/chemSampleList.R")) # creates chem.samples.foo, an inventory of all collected chem sample


# Read chemistry
# uncomment rows to bring in more data
source(paste0( "scripts/analysis/readFieldSheets.R")) # read surgeData...xlsx.  fld_sheet, dg_sheet
source(paste0( "scripts/analysis/readAnionsAda.R")) # read ADA lab anions
source(paste0( "scripts/analysis/readAnionsDaniels.R")) # read Kit Daniels anions
source(paste0( "scripts/analysis/readNutrientsAda.R")) # read nutrients ran in ADA lab
source(paste0( "scripts/analysis/readNutrientsAwberc.R")) # read AWBERC lab nutrient results
source(paste0( "scripts/analysis/readNutrientsR10_2018.R")) # read AWBERC nutrients for 2018 R10
source(paste0( "scripts/analysis/readOcAda.R")) # read ADA TOC/DOC data
source(paste0( "scripts/analysis/readOcMasi.R")) # read 2020 TOC run at MASI lab
source(paste0( "scripts/analysis/readTteb.R")) # TTEB metals, TOC, DOC
source(paste0( "scripts/analysis/readPigments.R")) # NAR chl, phyco
source(paste0( "scripts/analysis/readMicrocystin.R")) # microcystin
source(paste0( "scripts/analysis/readChlorophyllR10_2018.R")) # 2018 R10 chlorophyll
source("scripts/analysis/readGc.R") # gc_lakeid_agg
source("scripts/analysis/calculateDissolvedGas.R") # dissolved_gas
source(paste0( "scripts/analysis/mergeChemistry.R")) # merge all chem objects #chemistry all


# Data Setup---------------------------------------------

detlimits <- read_csv("SuRGE_Sharepoint/data/chemistry/dataPaperDetectionLimits.csv")

# chemistry_all contains all chemistry analytes, field duplicates, and blanks
# Exclude variables we don't want
chemdata <- chemistry_all %>% 
  # exclude phycocyanin and gases
  select(!(starts_with("phyc") | matches(c("ch4|co2|n2o")))) 


# Flags-------------------------------------------------

tally_flags <- function(data) { 
  
  data <- data %>%
  ungroup() %>%
    select(!c(site_id, sample_depth), -matches("units")) %>%
    filter(sample_type != "blank") %>%
    pivot_longer(ends_with("flags"), names_to = "flag_names", values_to = "flags") %>%
    mutate(flag_names = if_else(flag_names == "no2_3_flags", "no2.3_flags", flag_names), 
           # Remove 'tteb.' from analyte names
           flag_names = str_remove(flag_names, "tteb."), 
      analyte = word(flag_names, sep = "_")) %>%
    group_by(sample_type, analyte) %>%
    summarize(ND = sum(str_detect(flags, "ND"), na.rm = TRUE), 
              L = sum(str_detect(flags, "L"), na.rm = TRUE), 
              H = sum(str_detect(flags, "H"), na.rm = TRUE), 
              S = sum(str_detect(flags, "S"), na.rm = TRUE), 
              total = n()) %>%
    ungroup()
  
  return(data)
  
}

# Create summary tables of flags by analyte and sample type
flags <- chemdata %>% tally_flags()


# Field Blanks------------------------------------------

 summarize_blanks <- function(df) { 
   
   df <- chemdata %>%
     ungroup() %>%
     mutate(sample_type = case_when(sample_type == "duplicate" ~ "unknown", TRUE ~ sample_type)) %>%
     select(!c(site_id, sample_depth), -matches("flag|units")) %>%
     pivot_longer(!c(lake_id, sample_type, visit)) %>%
     filter(!(name == "chla_lab")) %>% # no field blanks for chlorophyll
     group_by(lake_id, name, sample_type, visit) %>%
     summarize(min = min(value, na.rm = TRUE), 
               max = max(value, na.rm = TRUE)) %>%
     ungroup() %>%
     pivot_wider(names_from = sample_type, values_from = c(min, max)) %>%
     filter(is.na(min_blank) == FALSE, is.infinite(min_blank) == FALSE) %>%
     select(-max_blank) %>%
     mutate(name = str_remove(name, "tteb."))
   
   df <- df %>%
     # apply conditional filtering to detlimits based on dataframe name
     
     # 1. Join with lake list to bring in year and lab
     #    Naturally joins on lake_id and visit
     left_join(lake.list.all %>% 
                 distinct(lake_id, visit, lab, year = sample_year)) %>%

     # 2. Create analyte_group field
     mutate(analyte_group = case_when(name %in% c("no2_3", "no3", "no2", "nh4", "op", "tn", "tp") ~ "nutrients",
                                      name %in% c("br", "cl", "so4", "f") ~ "anions",
                                      name %in% c("al", "as", "ba", "be", "ca", "cd", 
                                                  "cr", "cu", "fe", "k", "li", "mg", 
                                                  "mn", "na", "ni", "pb", "p", "sb", 
                                                  "si", "sn", "sr", "s", "v", "zn") ~ "metals",
                                      name %in% c("toc", "doc") ~ "organic",
                                      name == "microcystin" ~ "algal_indicators",
                                      TRUE ~ NA_character_)) %>%
     
     # 3. detection limits differ between CIN and ADA. Current "lab" field is for
     #    field crew (eg. R10, RTP, etc) not which lab ran chemistry. Create 
     #    "analytical_lab" field with either ADA and CIN as only legitimate values.
     mutate(analytical_lab = 
              case_when(year == 2020 & name %in% c("no2_3", "no3", "no2", "nh4", "op", "tn", "tp") ~ "ADA", # all 2020 nutrients sent to ADA
                        lab == "ADA" & name %in% c("no2_3", "no3", "no2", "nh4", "op", "tn", "tp", "br", "cl", "so4", "f", "toc", "doc") ~ "ADA", # ADA ran their own nutrients, anions, OC
                        TRUE ~ "CIN")) %>% # all others ran in CIN
     
     # 4. Join with detlimits to bring in detection limits
     #    naturally joins on name, analytical_lab, year
     left_join(detlimits %>%
                 select(name, mdl, ql, year, analytical_lab = lab)) %>% 
     
     # 5. select and rename
     select(-lab, -year) %>%
     rename(analyte = name, blank = min_blank, 
            minimum = min_unknown, maximum = max_unknown) %>%
     
     # 6. derived values
     mutate(mean_unknown = (minimum + maximum) / 2, 
            blank_prop = round((blank / mean_unknown), 2))
 }
 

  blanks <- summarize_blanks(chemdata) %>%
    group_by(analyte_group) %>%
    group_split()

# Field Duplicates------------------------------------------

## Field Duplicates Setup----------------------------------
  
# identify lakes, sites, and depths with duplicates
duplicate_ids <- chemdata %>% 
    filter(sample_type == "duplicate") %>%
  select(lake_id, site_id, sample_depth, visit) %>%
  mutate(id = paste0(lake_id, site_id, sample_depth, visit))

# pull out data from lakes, sites, and depths with duplicates
chemistry_all_dups <- chemdata %>%
  mutate(id = paste0(lake_id, site_id, sample_depth, visit)) %>% # create unique id
  filter(id %in% duplicate_ids$id) %>% # pull out duplicates and corresponding unknowns
  select(-contains("flag"), -contains("qual"), -contains("units"),
         -sample_type, -id, -no3) %>%
  pivot_longer(!c(lake_id, site_id, sample_depth, visit))

## Nutrients----------------------------------
nutrient_name <- detlimits %>% 
  filter(analyte_group == "nutrients") %>%
  select(name) %>% 
  pull()

# calculate mean relative percent difference among field replicates
nutrients_rpd <- chemistry_all_dups %>% 
  filter(name %in% nutrient_name) %>%
  group_by(lake_id, site_id, sample_depth, name, visit) %>%
  mutate(ad = abs(diff(value)), # difference between dup and unknown
         mean = mean(value, na.rm = TRUE), # mean of dup and unknown
         rpd = (ad/mean)*100) %>% # rpd
  distinct() 


## Anions----------------------------------
anion_name <- detlimits %>% 
  filter(analyte_group == "anions") %>%
  select(name) %>% 
  pull()

# calculate mean relative percent difference among field replicates
anions_rpd <- chemistry_all_dups %>% 
  filter(name %in% anion_name) %>%
  group_by(lake_id, site_id, sample_depth, name, visit) %>%
  mutate(ad = abs(diff(value)), # difference between dup and unknown
         mean = mean(value), # mean of dup and unknown
         rpd = (ad/mean)*100) %>% # rpd
  distinct() 

## Metals----------------------------------

metal_name <- detlimits %>% 
  filter(analyte_group == "metals") %>%
  select(name) %>% 
  pull()

# calculate mean relative percent difference among field replicates
metals_rpd <- chemistry_all_dups %>% 
  filter(name %in% metal_name) %>%
  group_by(lake_id, site_id, sample_depth, name, visit) %>%
  mutate(ad = abs(diff(value)), # difference between dup and unknown
         mean = mean(value), # mean of dup and unknown
         rpd = (ad/mean)*100) %>% # rpd
  distinct() 

## Organic Carbon----------------------------------

organic_name <- detlimits %>% 
  filter(analyte_group == "organic") %>%
  select(name) %>% 
  pull()

# calculate mean relative percent difference among field replicates
organic_rpd <- chemistry_all_dups %>% 
  filter(name %in% organic_name) %>%
  group_by(lake_id, site_id, sample_depth, name, visit) %>%
  mutate(ad = abs(diff(value)), # difference between dup and unknown
         mean = mean(value), # mean of dup and unknown
         rpd = (ad/mean)*100) %>% # rpd
  distinct() 


## Pigments----------------------------------

chlorophyll_rpd <- chemistry_all_dups %>% 
  filter(name == "chla_lab") %>%
  group_by(lake_id, site_id, sample_depth, name, visit) %>%
  mutate(ad = abs(diff(value)), # difference between dup and unknown
         mean = mean(value), # mean of dup and unknown
         rpd = (ad/mean)*100) %>% # rpd
  distinct()


dupes <- lst(anions_rpd, nutrients_rpd, chlorophyll_rpd, metals_rpd, organic_rpd)


# Tables-----------------------------------

# Flags table

flags_table <- tribble(
  ~Condition, ~ Value, ~Flag,
 "< MDL", "MDL",	"ND",
   "MDL and < RL",	"Measured concentration",	"L",
  "Holding time violation",	"Measured concentration",	"H",
  "Shipping issue",	"Measured concentration",	"S"
) 

technical_validation_data <- lst(dupes, blanks, flags, detlimits, chemistry_all)

saveRDS(technical_validation_data, 
        "scripts/analysis/data_paper/technical_validation_chemistry_data")

