# DEFINE MORPHO VARIABLES--------
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

# any missing values?
# missing littoral fraction for 69 () and 287 (Owhyee, OR)
dat %>% 
  select(lake_id, all_of(morpho_vars)) %>% 
  # filter missing values
  filter(rowSums(is.na(.)) > 0)

# INSPECT MORPHO VARIABLES FOR OUTLIERS-------------
# cleveland dotplots
p_morpho <- dat %>% 
  select(lake_id, all_of(morpho_vars)) %>% 
  # morpho variables are repeated for each site in a lake
  # filter to unique value to simplify the plot
  distinct() %>% 
  pivot_longer(cols = !lake_id, names_to = "morpho_var", values_to = "value") %>%
  mutate(f_lake_id = as.factor(lake_id)) %>% # better spacing along y-axis
  ggplot(aes(value, f_lake_id)) +
  geom_point() +
  facet_wrap(~ morpho_var, scales = "free_x") +
  # suppress y-axis tick labels
  theme(axis.ticks.y = element_blank(),
        axis.text.y = element_blank(),
        axis.title.y = element_blank())

ggplotly(p_morpho)


# INSPECT VARIABLES FOR COLLINEARITY-------------

# discussion from Cass Ruiz et al.2021 below
# littoral fraction and mean_depth were strongly correlated (rho = 0.99)
# and littoral fraction was omitted.
#Lake area and volume were also included in
#the elastic net models. Lake area was included because
#there is a size-scaling of lake morphometry, such that
#larger lakes are more fractal, less circular and more
#dish-like (figure 2). Thus, by including lake area in the
#models we can separately assess the influence of lake
#size and morphometry on the concentration of the
#different C species. Lake volume was included because
#larger lakes have a higher dilution capacity and generally
#present longer water residence times.

# here are the morpho variables used in Cass Ruiz
# surface_area and volume are correlated at rho=0.99
morpho %>%
  select(surface_area, volume, shoreline_development, circularity,
         dynamic_ratio) %>%
  cor(use = "pairwise.complete.obs") %>% 
  corrplot(method = "number") 


# use the variance inflation factor (Zuur pg.386-387, 478-479, 535-536) to assess collinearity
source("scripts/analysis/aed_corvif.R")

# Most >10. Zuur references cut offs of 3, 5, and 10. Clearly we have issues
dat %>% select(all_of(morpho_vars)) %>%
  corvif()

# pairplots
dat %>% select(all_of(morpho_vars)) %>%
  pairs(pch = 19, col = "orange", lower.panel = panel.smooth)


# Inspect for correlations.  There are lots.  
morpho %>%
  select(where(is.numeric), -lake_id) %>%
  cor(use = "pairwise.complete.obs") %>% 
  corrplot(method = "number") 


