# long versus short distance migrants (calendar vs weather birds) - questions --------------------------------
# long distance will have no idea of what going on far (use internal clocks), but also,
#  short distance that are in the equator don't have a lot of variation on day lenght or
#    environ conditions to follow
#  internal rhytmns initiate, photoperiod and environment adjusts speed (fine-tune).
#  when long distance leave, is not only to get good quality in the breeding grounds,
#    but also leaving before conditions get harsh
#  a tricky timing challenge to find the right balance between flexibly adjusting migration to the current
#    environment on the one hand, and keeping track of seasonally appropriate behaviour on the other
#  bad conditions on a site make birds move, not 'predicting' good ones in the future

## Paper:
# long: less synchrony, arrive later, move quicker, inflexible
# short: more synchrony, arrive earlier, move slower, flexible

library(lemon)
library(tidyverse)
library(purrr)
library(dplyr)

## who is long and short distance migrants? --------------
bird_mig <- read_csv("data/sps_list.csv") %>% 
  select(species,me) %>% 
  rename(species = species,
         migdist = me)

cells <- read_rds("data/cellcoor.rds")

## load and merge dataset
# fast slow year
final <- readRDS("data/birdgreen.rds") %>% 
  rename(cell_lat_s = cell_lat) %>% 
  left_join(., bird_mig, by = "species") %>% 
  select(-cell_lng) %>% 
  left_join(., cells, by = "cell") %>% 
  mutate(lat_g = ifelse(cell_lat<34, 'low', 
                        ifelse(cell_lat > 34 & cell_lat < 42, "mid", 
                               ifelse(cell_lat > 42, "high", "opps"))),
         year_g = ifelse(year %in% c(2010,2012), "early", 
                         (ifelse(year %in% c(2004,2006,2007,2011,2015,2017), "ave", 
                                 ifelse(year %in% c(2002,2003,2005,2008,2009,2013,2014,2016), "late", "ops")))),
         fast_g = ifelse(year %in% c(2009,2010,2011,2013,2014),"fast", 
                         ifelse(year %in% c(2002,2003,2005,2006,2007,2012,2015,2016,2017), "slow", 
                                ifelse(year %in% c(2004,2008), "ave", "opps"))))

final$lat_g <- factor(final$lat_g, ordered = TRUE,
                      levels = c("low","mid","high"))
final$year_g <- factor(final$year_g, ordered = TRUE,
                       levels = c("early","ave","late"))
final$fast_g <- factor(final$fast_g, ordered = TRUE,
                       levels = c("slow","ave","fast"))

## add Casey's overwinter latitude
winlat <- read_csv("data/Table_S1.csv") %>% 
  select(Species, Overwinterlatitude) %>% 
  mutate(species2 = Species) %>% 
  rename(species = Species, 
         winlat = Overwinterlatitude) %>% 
  mutate(species = sub(" ", "_", species))

final <- left_join(final, winlat, by = "species")

## add Casey's species sensitivity
sensi <- readRDS("data/data_sensi.RDS") %>% 
  select(sci_name,
         cell,
         beta_mean) %>% 
  rename(species = sci_name,
         sensi = beta_mean)

cellnumbs <- readRDS("data/cellnumbs.rds")
sensi <- left_join(sensi, cellnumbs, by = "cell") %>% 
  select(-cell) %>% 
  rename(cell = cell2)

final <- left_join(final, sensi, by = c("species","cell")) 

sort(colnames(final))

ggplot(final) + geom_boxplot(aes(x = as.factor(year), y = AnomVGr)) + 
  geom_hline(yintercept = 0, color = "red") 
# early late year
ggplot(final) + geom_boxplot(aes(x = as.factor(year), y = AnomDGr)) + 
  geom_hline(yintercept = 0, color = "red") 

## how are those groups tracking green up? -------------------
# smaller lags for short distance
ggplot(final) +
  geom_boxplot(aes(x = migdist, y = AnomLag)) +
  theme_bw() +
  facet_rep_wrap(~lat_g, ncol = 3, scales='free_x')

# short seem to vary way less in the arrival (low), seem to be better matched

## variation in speed in general and who moves faster? ------------------------
ggplot(final) +
  geom_boxplot(aes(x = migdist, y = log(vArrMag))) +
  theme_bw() +
  facet_rep_wrap(~lat_g, ncol = 3, scales='free_x')
# short seems slower, but they speed up going north
# long faster, speed up a little going north too
ggplot(final) +
  geom_point(aes(x = cell_lat, y = log(vArrMag),
                 colour = migdist, alpha = 0.7)) +
  theme_bw() +
  geom_smooth(aes(x = cell_lat, y = log(vArrMag), colour = migdist)) 
# do they have flexibility,
#  and does that flexibility translates into smaller mismatch
final %>% 
  group_by(species) %>% 
  summarise(var_speed = var(vArrMag, na.rm = T)) %>% 
  left_join(., final, by = 'species') %>% 
  ggplot() +
    geom_point(aes(x = log(var_speed), y = AnomLag,
                   colour = migdist, alpha = 0.9)) +
  theme_bw() +
  facet_rep_wrap(~lat_g, ncol = 3, scales='free_x')
# variance does not seem to be driving mismatch

## variation in speed with fast and slow green up years
ggplot(final) +
  geom_boxplot(aes(x = migdist, y = log(vArrMag))) +
  theme_bw() +
  facet_rep_wrap(~fast_g, ncol = 3, scales='free_x')
  
## variation in speed with early and late years
ggplot(final) +
  geom_boxplot(aes(x = migdist, y = log(vArrMag))) +
  theme_bw() +
  facet_rep_wrap(~year_g, ncol = 3, scales='free_x')




