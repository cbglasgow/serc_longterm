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

# Make plot looking at biomass change over time (by species)
# not sure if this is the best way to visualize
all_mass_long %>% filter(biomass_m2 > -1) %>%  # filter out missing data
  # y = biomass, x= year, color by species
  ggplot(aes(x = Year, y = biomass_m2, color = Treatment)) +
  # plot as line graph, add trendline
  geom_point() + geom_smooth(method = "lm", se = FALSE) +
  # facet by species
  facet_wrap(vars(species))

# find average biomass per species/treatment/community/year
all_mass_long %>% 
  # group by year, community, treatment, species
  group_by(Year, Community, Treatment, species) %>% 
  # calculate mean
  summarize(mean_bm = mean(biomass_m2)) -> all_mass_avg

# plot MX community over time
# filter by community, also filter out missing values
all_mass_avg %>% filter(Community == "MX") %>% filter(mean_bm > -1) %>% 
  # plot biomass over time, color by species
  ggplot(aes(x = Year, y = mean_bm, color = species)) +
  # plot as scatter plot, facet by treatment
  geom_point() + facet_wrap(vars(Treatment)) + 
  # add trend lines
  geom_smooth(method = "lm", se = FALSE)

# same plot as above, but with all communities
# easier to see using Zoom; 
all_mass_avg %>% filter(mean_bm > -1) %>%  # filter out missing values
  # plot biomass over time, color by species
  ggplot(aes(x = Year, y = mean_bm, color = species)) +
  # plot as scatter plot, facet by treatment and community
  geom_point() + facet_wrap(vars(Treatment, Community)) + 
  # add trend lines
  geom_smooth(method = "lm", se = FALSE)

# now look at densities
all_mass %>% 
  # select densities
  select(Year:Chamber, SCdensity_m2, SPdensity_m2, DIdensity_m2, OTHERdensity_m2) %>% 
  # Reformat data from wide to long so that species is in one column
  gather(key = species, value = density_m2, SCdensity_m2:OTHERdensity_m2) -> all_density_long

# plot densities over time 
all_density_long %>%   
  # y = density, x= year, color by species
  ggplot(aes(x = Year, y = density_m2, color = Treatment)) +
  # plot as line graph, add trendline
  geom_point() + geom_smooth(method = "lm", se = FALSE) +
  # facet by species
  facet_wrap(vars(species))

# find average density per species/treatment/community/year
all_density_long %>% 
  # group by year, community, treatment, species
  group_by(Year, Community, Treatment, species) %>% 
  # calculate mean
  summarize(mean_density = mean(density_m2)) -> all_density_avg

# plot mean density over time per community and treatment
all_density_avg %>%  
  # plot density over time, color by species
  ggplot(aes(x = Year, y = mean_density, color = species)) +
  # plot as scatter plot, facet by treatment and community
  geom_point() + facet_wrap(vars(Treatment, Community)) + 
  # add trend lines
  geom_smooth(method = "lm", se = FALSE)



