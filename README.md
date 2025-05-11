# Shifting gears in a shifting climate: birds adjust migration speed in response to spring vegetation green-up

[https://doi.org/10.5061/dryad.ttdz08m6w](https://doi.org/10.5061/dryad.ttdz08m6w)

This repository contains code to calculate bird speed, fit models, and make figures and predictions of bird migration front speed. All code is available (numbered in order of execution), as well as all the data necessary to run the analysis, and all the output. Additional info at [https://github.com/br-amaral/BirdMigrationSpeed](https://github.com/br-amaral/BirdMigrationSpeed)

## Code

**1_GetEstimates.R:** code to import phenological mismatch data (results from Youngflesh et al 2021) and create matrices with bird and green-up velocity to run models in 3_ModelPlots.R

* *input*: 
  * data/source/arrival_master_2020-07-21.csv
  * data/source/MidGreenup_2020-08-06-forest.csv
* *output*:
  * data/velocityB.rds
  * data/birdgreen.rds
  * data/cellcoor.rds
  * data/cellnumbs.rds

**2_AddSpsData.R:** code to add species traits data from the literature and also calculate extra traits from the data

* *input*:
  * data/birdgreen.rds
  * data/cellnumbs.rds
  * data/source/traits/Table_S1.csv
  * data/source/traits/data_sensi.rds
  * data/source/traits/gcb14540-sup-0001-supinfo_mass.csv
  * data/source/traits/jane13345-sup-0002-tables1_diet.csv
  * data/source/traits/species_tax_ord.csv
  * data/source/traits/birds_HWI.csv
  * data/source/traits/sps_migtime.csv
* *output*:
  * data/final.rds

**3_ModelPlots.R:** code to analyze how bird arrival date speed is affected by green-up, bird traits and latitude, which factors contribute to anomalies in lag, and how green-up varies according to latitude. Model summaries and figures are created here.

* *source*:
  * 5_Maps.R
* *input*: 
  * data/final.rds
  * data/cellcoor.rds
  * data/cellnumbs.rds
  * data/velocityB.rds
* *output*:
  * data/res/mod_gu.rds
  * data/res/mod_lat_guspe.rds
  * data/res/mod_tra.rds
  * data/res/mod_lag.rds

**4_PredictSpsSpeed.R:** code to predict how much species speed will change with 1 sd of green-up date or speed

* *input*: 
  * data/res/mod_gu.rds
  * data/final.rds

**5_Maps.R:** code to make maps with migration speed and direction for species and green-up

* *input*:
  * data/species_Grid.RData
  * data/master_cell_grid.rds
  * data/final.rds
  * data/source/spskey.csv
  * data/cell_grid_master.rds

**6_StopoverTime.R:** code to calculate stopover time based on our speed estimates and bird flight speeds from the literature

* *input*:
  * data/final.rds
  * data/source/Speed_data/Speed_table_small.csv
  * data/source/traits/gcb14540-sup-0001-supinfo_mass.csv
* *output*:
  * data/stopover_percent.csv

**7_PhyloRes.R:** code to check for the effect of phylogenic relationships on our results

* *input*:
  * data/res/mod_tra.rds
  * data/source/philo_tree/tree-pruner-c9524fe9-2d15-4b45-a65a-03b4fa85e4e9/output.nex

map.R: function to create maps that is sourced in 5_Maps.R

* input:
  * data/data_arr.rds
  * data/source/traits/data_sensi.rds
  * data/species_Grid.RData
  * data/fit_df_tab5.rds
  * data/master_cell_grid.rds
  * data/for_green-up_dl.rds

**stopoverdata.R:** code with values from the literarture of species flight speed during migration

## Data

data/**birdgreen.rds**: tibble with bird speed calculations from 1_GetEstimates.R

data/**cell_grid_master.rds**: grid cell information

data/**cellcoor.rds**: tibble with XY coordinates of each cell, and a cell id number

data/**cellnumbs.rds**: tibble with cell numbers that start at 1

data/**data_arr.rds**: tibble with arrival date estimates (Youngflesh et al. [2021])

data/**final.rds**: tibble with bird and green-up information, plus species traits

data/**fit_df_tab5.rds**: average sensitivity for species

data/**for_green-up_dl.rds**: estimates of green-up date (Youngflesh et al. [2021])

data/**master_cell_grid.rds**: information about the grid cell locations

data/**species_Grid.RData**: map grid for each species (migratory and breeding cell information)

data/s**topover_percent.csv**: matrix with the stopover time for species according to their flight speed and our estimates

data/**velocityB.rds**: tibble with calculated bird speeds from 1_GetEstimates.R

data/source/philo_tree/tree-pruner-c9524fe9-2d15-4b45-a65a-03b4fa85e4e9/**output.nex**: phylogenetic tree for birds

data/source/Speed_data/**Speed_table_small.csv**: flight speed for birds from the literature

data/source/**spskey.csv**: species common and scientific names

data/source/traits/**birds_HWI.csv**: bird hand-wing index (Sheard et al. [2020])

data/source/traits/**data_sensi.rds**: bird sensitivity data (Youngflesh et al. [2021])

data/source/traits/**gcb14540-sup-0001-supinfo_mass.csv**: bird body mass data (Horton et al. [2019])

data/source/traits/**jane13345-sup-0002-tables1_diet.csv**: bird diet data (La Sorte & Graham [2021])

data/source/traits/**species_tax_ord.csv**: bird family data (BirdTree.org)

data/source/traits/**sps_migtime.csv**: Bird migration time data (Birds of the World [2022])

data/source/traits/**Table_S1.csv**: bird overwinter latitude data (Youngflesh et al. [2021])

data/res/**mod_gu.rds**: model two results

data/res/**mod_lag.rds**: model four results

data/res/**mod_lat_guspe.rds**: model one results

data/res/**mod_tra.rds**: model three results

## Figures

* **figure 1** - maps of migration speed
* **figure 2** - individual species speeds
* **figure 3a** - speed and green-up date (model 2)
* **figure 3b** - speed and green-up speed (model 2)
* **figure 3c** - speed and range type (model 2)
* **figure 4a** - speed and latitude (model 1 and 2)
* **figure 4b** - anomaly on lag and z-score (model 4)
* **figure 5** - first arrival and bird speed (model 3)

