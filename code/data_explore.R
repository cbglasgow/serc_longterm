# Data exploration of long-term GCREW (Global Change Research Wetland data)
# View meta-data in excel files for details on the data structure

# Load libraries  
library(tidyverse)

# Read in data
all_mass <- read_csv("data/4-co2xcomm_total_shoot_biomass_1987-2019_published_04-23-2020.csv")

all_mass %>% 
  # Select just biomass for now (ignore stem densities)
  select(Year:SCmass_m2, SPmass_m2, DImass_m2, OTHERmass_m2) %>% 
  # Reformat data from wide to long so that species is in one column
  gather(key = species, value = biomass_m2, SCmass_m2:OTHERmass_m2) -> all_mass_long

# To do next: (1) look at how biomass changes across time for species, (2)
# consider how proportions shift across time, (3) what are the differences
# across treatments/communities?

# Making a change to the code!!!
dim(all_mass)

# convert Chamber to character for graphical purposes
all_mass_long$Chamber <- as.character(all_mass_long$Chamber)


