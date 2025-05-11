## Shifting gears in a shifting climate: birds adjust migration speed in response to spring vegetation green-up

##### Bruna R. Amaral, Casey Youngflesh, Morgan Tingley and David A. Miller

Publication on <i>Diversity and Distributions</i> [![DOI]()] and [![PDF]()]

<img src="https://www.royensoc.co.uk/wp-content/uploads/2024/08/Dryad-Logo-no-text-1600x0-c-default.png" alt="GitHub Icon" width="22" height="22">Dryad: [https://doi.org/10.5061/dryad.ttdz08m6w](https://doi.org/10.5061/dryad.ttdz08m6w)

&nbsp;<img src="https://upload.wikimedia.org/wikipedia/commons/thumb/9/91/Octicons-mark-github.svg/2048px-Octicons-mark-github.svg.png" alt="GitHub Icon" width="17" height="17"> GitHub: [https://github.com/br-amaral/BirdMigrationSpeed](https://github.com/br-amaral/BirdMigrationSpeed)


This repository contains code to calculate bird migration and green-up speeds, fit models, and make figures and predictions of bird migration front speed. All the code (numbered in order of execution), data necessary to run the analysis, and output are provided.

##### Folder structure:
- <b>[code](#code)</b>: all the R script files use to run the analysis, numbered according to the ordered they should be executed. Details on the specific files used as input or output can be found in the<b>[data](#data)</b> folder description.

- <b>[data](#data)</b>: all the data used on the analysis, including our output. Divided in:
    - <b>[source](#source)</b>: files from different studies used in our analysis:
        - <b>[Speed_data](#Speed_data)</b>: folder with data from the literature of species active flight migratory speeds. 
        - <b>[philo_tree](#philo_tree)</b>: folder with the phylogenetic tree of all species in our study.
        - <b>[traits](#traits)</b>: folder with data on species gathered from the literature.
    - <b>[res](#res)</b>: model results output for the 4 analysis we conducted.

- <b>[figures](#figures)</b>: folder with figures from the manuscript, which also serve as a placeholder when running the code to generate the figures.
- <b>[tables](#tables)</b>: folder with tables from the manuscript.
- <b>[appendix](#appendix)</b>: folder with the appendix S1 from the manuscript.

### code

<b>[1_GetEstimates.R](code/1_GetEstimates.R)</b>: code to import phenological mismatch data (results from Youngflesh et al 2021,  available in their [Zenodo repository](https://zenodo.org/records/4532885)) and create matrices with bird and green-up velocity to run models in [3_ModelPlots.R](code/3_ModelPlots.R)

&nbsp;&nbsp;<u>Input:</u>

- [data/source/arrival_master_2020-07-21.csv](data/source/arrival_master_2020-07-21.csv)

- [data/source/MidGreenup_2020-08-06-forest.csv](data/source/MidGreenup_2020-08-06-forest.csv)

&nbsp;&nbsp;<u>Output:</u>

- [data/velocityB.rds](data/velocityB.rds)

- [data/birdgreen.rds](data/birdgreen.rds)

- [data/cellcoor.rds](data/cellcoor.rds)

- [data/cellnumbs.rds](data/cellnumbs.rds)

<b>[2_AddSpsData.R](code/2_AddSpsData.R)</b>: code to add species traits data from the literature and also calculate extra traits from the data

&nbsp;&nbsp;<u>Input:</u>

- [data/birdgreen.rds](data/birdgreen.rds)

- [data/cellnumbs.rds](data/cellnumbs.rds)

- [data/source/traits/Table_S1.csv](data/source/traits/Table_S1.csv)

- [data/source/traits/data_sensi.rds](data/source/traits/data_sensi.rds)

- [data/source/traits/gcb14540-sup-0001-supinfo_mass.csv](data/source/traits/gcb14540-sup-0001-supinfo_mass.csv)

- [data/source/traits/jane13345-sup-0002-tables1_diet.csv](data/source/traits/jane13345-sup-0002-tables1_diet.csv)

- [data/source/traits/species_tax_ord.csv](data/source/traits/species_tax_ord.csv)

- [data/source/traits/birds_HWI.csv](data/source/traits/birds_HWI.csv)

- [data/source/traits/sps_migtime.csv](data/source/traits/sps_migtime.csv)

&nbsp;&nbsp;<u>Output:</u>

- [data/final.rds](data/final.rds)

<b>[3_ModelPlots.R](code/3_ModelPlots.R)</b>: code to analyze how bird arrival date speed is affected by green-up, bird traits and latitude, which factors contribute to anomalies in lag, and how green-up varies according to latitude. Model summaries and figures are created here.

&nbsp;&nbsp;<u>Source:</u> 

 - <b>[5_Maps.R](code/5_Maps.R)</b> 

&nbsp;&nbsp;<u>Input:</u>

- [data/final.rds](data/final.rds)

- [data/cellcoor.rds](data/cellcoor.rds)

- [data/cellnumbs.rds](data/cellnumbs.rds)

- [data/velocityB.rds](data/velocityB.rds)

&nbsp;&nbsp;<u>Output:</u>

- [data/res/mod_gu.rds](data/res/mod_gu.rds)

- [data/res/mod_lat_guspe.rds](data/res/mod_lat_guspe.rds)

- [data/res/mod_tra.rds](data/res/mod_tra.rds)

- [data/res/mod_lag.rds](data/res/mod_lag.rds)

<b>[4_PredictSpsSpeed.R](code/4_PredictSpsSpeed.R)</b>: code to predict how much species speed will change with 1 sd of green-up date or speed

&nbsp;&nbsp;<u>Input:</u>
- [data/res/mod_gu.rds](data/res/mod_gu.rds)

- [data/final.rds](data/final.rds)

<b>[5_Maps.R](code/5_Maps.R)</b>: code to make maps with migration speed and direction for species and green-up

<u>Source:</u>
&nbsp;&nbsp;<b>[map.R](map.R)</b>: function to create maps 
&nbsp;&nbsp;&nbsp;&nbsp;<u>Input</u>:

&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;- [data/data_arr.rds](data/data_arr.rds)
&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;- [data/source/traits/data_sensi.rds](data/source/traits/data_sensi.rds)
&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;- [data/species_Grid.RData](data/species_Grid.RData)    
&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;- [data/fit_df_tab5.rds](data/fit_df_tab5.rds)
&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;- [data/master_cell_grid.rds](data/master_cell_grid.rds)
&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;- [data/for_green-up_dl.rds](data/for_green-up_dl.rds)

&nbsp;&nbsp;<u>Input:</u>
- [data/species_Grid.RData](data/species_Grid.RData)

- [data/master_cell_grid.rds](data/master_cell_grid.rds)

- [data/final.rds](data/final.rds)

- [data/source/spskey.csv](data/source/spskey.csv)

- [data/cell_grid_master.rds](data/cell_grid_master.rds)

<b>[6_StopoverTime.R](code/6_StopoverTime.R)</b>: code to calculate stopover time based on our speed estimates and bird flight speeds from the literature

&nbsp;&nbsp;<u>Input:</u>

- [data/final.rds](data/final.rds)

- [data/source/Speed_data/Speed_table_small.csv](data/source/Speed_data/Speed_table_small.csv)

- [data/source/traits/gcb14540-sup-0001-supinfo_mass.csv](data/source/traits/gcb14540-sup-0001-supinfo_mass.csv)

&nbsp;&nbsp;<u>Output:</u>

- [data/stopover_percent.csv](data/stopover_percent.csv)

<b>[7_PhyloRes.R](code/7_PhyloRes.R)</b>: code to check for the effect of phylogenic relationships on our results

&nbsp;&nbsp;<u>Input:</u>

- [data/res/mod_tra.rds](data/res/mod_tra.rds)

- [data/source/philo_tree/tree-pruner-c9524fe9-2d15-4b45-a65a-03b4fa85e4e9/output.nex](data/source/philo_tree/tree-pruner-c9524fe9-2d15-4b45-a65a-03b4fa85e4e9/output.nex)

<b>[SUP_sensiana](SUP_sensiana)</b>: code to run sensitivity analysis
&nbsp;&nbsp;<u>Input:</u>
- [data/source/arrival_master_2020-07-21.csv](data/source/arrival_master_2020-07-21.csv)
- [data/source/MidGreenup_2020-08-06-forest.csv](data/source/MidGreenup_2020-08-06-forest.csv)
- [data/birdgreen_st3000.rds](data/birdgreen_st3000.rds)
- [data/res/data_trait.rds](data/res/data_trait.rds)

### data

- [data/birdgreen.rds](data/birdgreen.rds): tibble with bird and green-up speed calculations from [1_GetEstimates.R](1_GetEstimates.R)

- [data/birdgreen_st3000.rds](data/birdgreen_st3000.rds): : tibble with bird and green-up speed calculations filtering species faster than 3000

- [data/cell_grid_master.rds](data/cell_grid_master.rds): grid cell information

- [data/cellcoor.rds](data/cellcoor.rds): tibble with XY coordinates of each cell, and a cell id number

- [data/cellnumbs.rds](data/cellnumbs.rds): tibble with cell numbers that start at 1

- [data/data_arr.rds](data/data_arr.rds): tibble with arrival date estimates (Youngflesh et al. [2021])

- [data/final.rds](data/final.rds): tibble with bird and green-up information, plus species traits

- [data/fit_df_tab5.rds](data/fit_df_tab5.rds): average sensitivity for species

- [data/for_green-up_dl.rds](data/for_green-up_dl.rds): estimates of green-up date (Youngflesh et al. [2021])

- [data/master_cell_grid.rds](data/master_cell_grid.rds): information about the grid cell locations

- [data/species_Grid.RData](data/species_Grid.RData): map grid for each species (migratory and breeding cell information)

- [data/stopover_percent.csv](data/stopover_percent.csv): matrix with the stopover time for species according to their flight speed and our estimates

- [data/velocityB.rds](data/velocityB.rds): tibble with calculated bird speeds from [1_GetEstimates.R](1_GetEstimates.R)

    ##### data/source
- [data/source/philo_tree/tree-pruner-c9524fe9-2d15-4b45-a65a-03b4fa85e4e9/output.nex](data/source/philo_tree/tree-pruner-c9524fe9-2d15-4b45-a65a-03b4fa85e4e9/output.nex): phylogenetic tree for birds

- [data/source/Speed_data/Speed_table_small.csv](data/source/Speed_data/Speed_table_small.csv): flight speed for birds from the literature (specific references on [Appendix S1]((appendix/ddi70033-sup-0001-AppendixS1.docx.doc)))

- [data/source/spskey.csv](data/source/spskey.csv): species common and scientific names

- [data/source/traits/birds_HWI.csv](data/source/traits/birds_HWI.csv): bird hand-wing index from [Sheard et al. 2020](https://doi.org/10.1038/s41467-020-16313-6)

- [data/source/traits/data_sensi.rds](data/source/traits/data_sensi.rds): bird sensitivity data from [Youngflesh et al. 2021](https://doi.org/10.1038/s41559-021-01442-y)

- [data/source/traits/gcb14540-sup-0001-supinfo_mass.csv](data/source/traits/gcb14540-sup-0001-supinfo_mass.csv): bird body mass data from [Horton et al. 2019](https://doi.org/10.1038/s41558-019-0648-9)

- [data/source/traits/jane13345-sup-0002-tables1_diet.csv](data/source/traits/jane13345-sup-0002-tables1_diet.csv): bird diet data from [La Sorte & Graham 2021](https://doi.org/10.1111/1365-2656.13345)

- [data/source/traits/species_tax_ord.csv](data/source/traits/species_tax_ord.csv): bird family data from [BirdTree.org](BirdTree.org)

- [data/source/traits/sps_migtime.csv](data/source/traits/sps_migtime.csv): Bird migration time data from [Birds of the World (2022)](https://birdsoftheworld.org/)

- [data/source/traits/Table_S1.csv](data/source/traits/Table_S1.csv): bird overwinter latitude data from [Youngflesh et al. 2021](https://doi.org/10.1038/s41559-021-01442-y)

    ##### data/res

- [data/res/mod_lat_guspe.rds](data/res/mod_lat_guspe.rds): model one results

- [data/res/mod_gu.rds](data/res/mod_gu.rds): model two results

- [data/res/mod_tra.rds](data/res/mod_tra.rds): model three results

- [data/res/mod_lag.rds](data/res/mod_lag.rds): model four results

### figures

 - [Figure 1](figures/fig1.png): Bird migration velocity (direction (a) and speed (b)) in the Eastern flyway in North America. (a) Arrows represent median bird migration direction according to latitude across years (2002 to 2017) for all species combined. Birds are mostly moving north, as seen in the arrows showing migration direction. (b) Median bird migration speed according to latitude across years (2002 to 2017) for all species combined is represented in the shades of purple and yellow. Birds move faster in higher latitudes as seen by the yellow781 coloured cells in the plot in the north. Species have different occurrence ranges and data available for different cells and years, the migration speed and direction in a cell might represent different sets of species.

 - [Figure 2](figures/fig2.png): Species and trait-specific bird front migration speed (km/day). Bird migration speed estimates in the Eastern flyway in North America for each species grouped mean estimates distribution (histogram, log scale y-axis). Each boxplot represents a species in a year and a cell (25th and 75th percentiles), and species are ordered according to their median speed. The squares on the bottom represent the data used in model 3 to estimate bird migration speed (details in Table 1, legend on the bottom right), except for species diet and migration time. Both variables were removed from the model due to the lack of many species in each category.

 - [Figure 3](figures/fig3.png): Mean bird migration speed (in pink, log(km/day)) varying according to latitude estimated by model 2 (details on Table 1). Vegetation green-up speed varied according to latitude (in green, log(km/day)) according to model 1. Bird speed tends to increase with latitude, while green-up speed decreases with latitude in the Eastern flyway in North America. Lines represent mean estimate, and the shaded region the 95% confidence interval.

 - [Figure 4](figures/fig4.png): Effect of vegetation green-up on bird migration speed (km/day) in the breeding (yellow and solid) and migratory range (orange and dashed) Eastern flyway in North America (model 2). Lines (a, b) and dots (c) represent the mean estimate and the shaded areas (a, b) and whiskers (c) represent the 95%  confidence interval. (a) Anomaly in green-up date (ordinal day, scaled) effect on bird migration speed. Early green-up years were associated with a faster migration pace. (b) Anomaly in green-up speed (km/day, base  10 logarithm and scaled) effect on bird migration speed. Fast green-up years were associated with a fast migration pace. (c) Bird migration speed varied according to bird range type. Birds moved slower in breeding cells than in migratory cells.

 - [Figure 5](figures/fig5.png): Bird traits that influenced migration speed in the Eastern flyway in North America. The pink line indicates the average relationships, while the shaded area indicates the 0.95 confidence interval. The first arrival date was positively associated with bird migration speed, with birds migrating faster when they arrive later.

 - [Figure 6](figures/fig6.png):  Coefficient effects of model 4 (details in Table 1) on anomaly on bird relative arrival (vegetation green-up arrival date minus bird arrival date, scaled). Positive values of anomaly on relative arrival (y-axis) represent birds arriving earlier than green-up, whereas negative values represent birds arriving behind green-up. The green dashed line represents the effect of the green-up date z-score on the relative arrival anomaly. Early green-up years are strongly associated with birds arriving relatively later than average, relative to green-up. The pink dashed line represents the effect of bird migration speed prior to arrival (km/day, z-score) on relative arrival anomaly. Birds are migrating slower on average (negative z-score), they are late in relation to green-up. The blue dashed line shows the effect of the first-arrival date (ordinal date z-score) on anomalies on relative arrival anomaly. First-arrival date and anomalies in relative arrival presented a very weak relationship. Even though both the green-up date and bird migration speed prior to arrival were associated with relative arrival anomaly, the effect size of the green-up date was almost twice the effect of bird migration speed prior to arrival.

### tables

- [Table 1](tables/tab1.csv): Generalised additive models and their response variables, predictors, and results. Coefficient estimates and confidence intervals are shown for the linear fixed effects of the four models that investigated variation in bird migratory and green-up speed. For the random effects and smooth terms, we show how much variance they are explaining, as well as the residual variance estimate. Bold estimates indicate coefficient estimates with 95% confidence intervals that do not overlap zero in the fixed effects; for the non-linear and non-fixed effects, bold font indicates a p-value smaller than 0.05 in the likelihood ratio test that uses a reference distribution to test a null hypothesis (that the variance is 0) using the estimated degrees of freedom and maximum degrees of freedom for each term.

### appendix

- [Appendix S1](appendix/ddi70033-sup-0001-AppendixS1.docx.doc)(ddi70033-sup-0001-AppendixS1.docx): file with supplementary tables, figures, and equations.