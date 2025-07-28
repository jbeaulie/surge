morpho_vars <- c("surface_area",         
                 "shoreline_length",     
                 "shoreline_development",
                 "max_width", # Maximum shore to shore distance that is perpendicular to the maximum lake length            
                 "mean_width", # Lake surface area divided by fetch        
                 "max_length", # longest open water distance of a lake (fetch)        
                 "mean_depth",           
                 "max_depth",            
                 "volume",               
                 "littoral_fraction",    
                 "circularity", # circular lakes (value approaches 1) vs elongated lakes (value approaches 0).",          
                 "dynamic_ratio") # Surface area to mean depth ratio of the lake. Low values indicate lakes that are bowl shaped, whereas high values are associated to dish-like lakes.


# Dont forget about point specific sample depths "sample_depth"

source("scripts/analysis/aed_corvif.R")

dat %>% select(morpho_vars)
