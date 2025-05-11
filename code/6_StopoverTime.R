# 6_StopoverTime.R -------------------------
#
# Code to 
#
# Input:  
#      
#
# Output:  
#
#
#

cat("\n\n\n\n  6_StopoverTime.R \n\n\n\n\n")

# Load packages ----------------------------
library(tidyverse)
library(ggplot2)
library(dplyr)
library(glue)

flight_spe <- read_csv("data/source/Speed_data/Speed_table_small.csv")

final2 <- readRDS("~/OneDrive/BirdMigrationSpeed_copy/final.rds") %>% 
  #"~/Library/CloudStorage/OneDrive-ThePennsylvaniaStateUniversity/BirdMigrationSpeed_copy/data/final.rds") %>% 
  mutate(species = as.factor(species), 
         cell = as.factor(cell),
         mig_cell = as.factor(mig_cell),
         sps_cell = as.factor(glue("{species}_{cell}"))
  ) %>% 
  group_by(species) %>% 
  summarise(mig_speed = mean(vArrMag, na.rm = T)) %>% 
  ungroup()

mass_tax <- read_csv("data/source/gcb14540-sup-0001-supinfo_mass.csv") %>% 
  dplyr::select(species, Distance_m)

flight_spe2 <- left_join(flight_spe, final2, by = "species") %>% 
  left_join(., mass_tax, by = "species") %>% 
  mutate(flight_speed = flight_speed * 24,
         distance_km  = Distance_m,  ## units are probably wrong in the horton GCB paper, km make more sense
         days_flight = distance_km/flight_speed,
         days_mig = distance_km/mig_speed,
         time_stop = days_mig - days_flight,
         percent_stop = time_stop/days_mig)

mean(flight_spe2$percent_stop)

flight_spe_exp <- flight_spe2 %>% 
  select(species, flight_speed, mig_speed, distance_km, percent_stop) %>% 
  mutate(percent_stop = round(percent_stop, 2),
         mig_speed = round(mig_speed,0),
         distance_km = round(distance_km,0),
         species = str_replace(species, "_", " "))

colnames(flight_spe_exp) <- c("Species name", "Flight Speed (km/day)", "Migratory Speed (km/day)", "Migration distance (km)", "Proportion of time at stopover site")
write_csv(flight_spe_exp, file = "data/out/stopover_percent.csv")
