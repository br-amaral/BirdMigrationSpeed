Description od all columns of each file of the data:

### data/birdgreen_st3000.rds
**year (numeric):** year of sampling
**cell (numeric):** cell id number
**species (character):** bird species scientific name
**mig_cell (logical):** TRUE if the cell is within the breeding range of the species
**breed_cell (logical):** TRUE if the cell is within the migratory range of the species (note that some cells can be both breeding and migratory)
**cell_lat (numeric):** latitude scaled of cell centroid
**cell_lng (numeric):** longitude in degrees of cell centroid
**arr_GAM_mean (numeric):** estimate of the posterior mean arrival date (calendar day) of a species in a cell
**arr_GAM_sd (numeric):**  estimate of the posterior standard deviation arrival date (calendar day) of a species in a cell
**gr_mn (numeric):** posterior mean 'midgreen-up' date (calendar day) for all pixels classified as 'good' or 'best' that fall within cell that were classified as forest - green-up dates derived from the MCD12Q2 data product - land cover information derived from MCD12Q1 data product
**gr_ncell (numeric):** number of green-up pixels that met filtering criteria that fell within cell
**NArr (numeric):** number of neighboring cell with bird arrival data
**vArrMag (numeric):** bird velocity magnitude (speed, km/day) 
**vArrAng (numeric):** bird velocity angle (direction, radians) 
**angB (numeric):** bird velocity angle in degrees
**xB (numeric):** x coordinate of the direction of bird migration for a given cell 
**yB (numeric):** y coordinate of the direction of bird migration for a given cell 
**NGr (numeric):** number of neighboring cell with green-up data
**vGrMag (numeric):** green-up velocity magnitude (speed, km/day) 
**vGrAng (numeric):** green-up velocity angle (direction, radians) 
**angG (numeric):** green-up velocity angle in degrees
**xG (numeric):** x coordinate of the direction of grenn-up for a given cell 
**yG (numeric):** y coordinate of the direction of grenn-up for a given cell 
**lag (numeric):** bird arrival date minus green-up date (days)
**AnomDArr (numeric):** bird arrival date anomaly (scaled bird arrival date)
**AnomDGr (numeric):** green-up date anomaly (scaled green-up date)
**AnomVArr (numeric):** bird speed anomaly (scaled bird speed)
**AnomVGr (numeric):** green-up speed anomaly (scaled green-up speed )
**AnomLag (numeric):** lag anomaly (scaled lag)
**cell_lat2 (numeric):** latitude in degrees of cell centroid

### data/birdgreen.rds
**year (numeric):** year of sampling
**cell (numeric):** cell id number
**species (character):** bird species scientific name
**mig_cell (logical):** TRUE if the cell is within the breeding range of the species
**breed_cell (logical):** TRUE if the cell is within the migratory range of the species (note that some cells can be both breeding and migratory)
**cell_lat (numeric):** latitude scaled of cell centroid
**cell_lng (numeric):** longitude in degrees of cell centroid
**arr_GAM_mean (numeric):** estimate of the posterior mean arrival date (calendar day) of a species in a cell
**arr_GAM_sd (numeric):**  estimate of the posterior standard deviation arrival date (calendar day) of a species in a cell
**gr_mn (numeric):** posterior mean 'midgreen-up' date (calendar day) for all pixels classified as 'good' or 'best' that fall within cell that were classified as forest - green-up dates derived from the MCD12Q2 data product - land cover information derived from MCD12Q1 data product
**gr_ncell (numeric):** number of green-up pixels that met filtering criteria that fell within cell
**NArr (numeric):** number of neighboring cell with bird arrival data
**vArrMag (numeric):** bird velocity magnitude (speed, km/day) 
**vArrAng (numeric):** bird velocity angle (direction, radians) 
**angB (numeric):** bird velocity angle in degrees
**xB (numeric):** x coordinate of the direction of bird migration for a given cell 
**yB (numeric):** y coordinate of the direction of bird migration for a given cell 
**NGr (numeric):** number of neighboring cell with green-up data
**vGrMag (numeric):** green-up velocity magnitude (speed, km/day) 
**vGrAng (numeric):** green-up velocity angle (direction, radians) 
**angG (numeric):** green-up velocity angle in degrees
**xG (numeric):** x coordinate of the direction of grenn-up for a given cell 
**yG (numeric):** y coordinate of the direction of grenn-up for a given cell 
**lag (numeric):** bird arrival date minus green-up date (days)
**AnomDArr (numeric):** bird arrival date anomaly (scaled bird arrival date)
**AnomDGr (numeric):** green-up date anomaly (scaled green-up date)
**AnomVArr (numeric):** bird speed anomaly (scaled bird speed)
**AnomVGr (numeric):** green-up speed anomaly (scaled green-up speed )
**AnomLag (numeric):** lag anomaly (scaled lag)
**cell_lat2 (numeric):** latitude in degrees of cell centroid

### data/cellcoor.rds
**cell (numeric):** cell id number
**cell_lat (numeric):** latitude in degrees of cell centroid
**cell_lng (numeric):** longitude in degrees of cell centroid

### data/cellnumbs.rds
**cell (numeric):** cell id number
**cell2 (numeric):** cell id number starting from 1

### data/final.rds
**year (numeric):** year of sampling
**cell (numeric):** cell id number
**species (character):** bird species scientific name
**mig_cell (logical):** TRUE if the cell is within the breeding range of the species
**breed_cell (logical):** TRUE if the cell is within the migratory range of the species (note that some cells can be both breeding and migratory)
**cell_lat (numeric):** latitude scaled of cell centroid
**cell_lng (numeric):** longitude in degrees of cell centroid
**arr_GAM_mean (numeric):** estimate of the posterior mean arrival date (calendar day) of a species in a cell
**arr_GAM_sd (numeric):**  estimate of the posterior standard deviation arrival date (calendar day) of a **species in a cell
**gr_mn (numeric):** posterior mean 'midgreen-up' date (calendar day) for all pixels classified as 'good' or 'best' that fall within cell that were classified as forest - green-up dates derived from the MCD12Q2 data product - land cover information derived from MCD12Q1 data product
**gr_ncell (numeric):** number of green-up pixels that met filtering criteria that fell within cell
**NArr (numeric):** number of neighboring cell with bird arrival data
**vArrMag (numeric):** bird velocity magnitude (speed, km/day) 
**vArrAng (numeric):** bird velocity angle (direction, radians) 
**angB (numeric):** bird velocity angle in degrees
**xB (numeric):** x coordinate of the direction of bird migration for a given cell 
**yB (numeric):** y coordinate of the direction of bird migration for a given cell 
**NGr (numeric):** number of neighboring cell with green-up data
**vGrMag (numeric):** green-up velocity magnitude (speed, km/day) 
**vGrAng (numeric):** green-up velocity angle (direction, radians) 
**angG (numeric):** green-up velocity angle in degrees
**xG (numeric):** x coordinate of the direction of grenn-up for a given cell 
**yG (numeric):** y coordinate of the direction of grenn-up for a given cell 
**lag (numeric):** bird arrival date minus green-up date (days)
**AnomDArr (numeric):** bird arrival date anomaly (scaled bird arrival date)
**AnomDGr (numeric):** green-up date anomaly (scaled green-up date)
**AnomVArr (numeric):** bird speed anomaly (scaled bird speed)
**AnomVGr (numeric):** green-up speed anomaly (scaled green-up speed )
**AnomLag (numeric):** lag anomaly (scaled lag)
**cell_lat2 (numeric):** latitude in degrees of cell centroid
**winlat (numeric):** mean latitude in which species overwinter
**species2 (character):** species name with space and not underscore
**xi_mean (numeric):** posterior mean of estimate for species-specific sensitivity (days/day) to green-up for each cell
**sensi_mean (numeric):** mean species sensitivity (days/day) for all cells
**sensi_sd (numeric):** standard deviation of bird sensitivity (days/day)
**Order (character):** species taxonomic order
**Family (character):** species taxonomic family
**Body_mass_g (numeric):** species average body mass in grams
**Time (character):** time of the day that species migrate (diurnal or nocturnal)
**Diet (character):** species diet
**FamilyColor (character):** color according to species family
**TaxOrder (numeric):** number for each taxonimic family to order figures
**HWI (numeric):** hand-wing index for each species
**ea_lat_m (numeric):** mean first arrival date for a species accross all years for all cells
**ea_lat (numeric):** : mean first arrival date for a species in each cell
**ea_lat_yr (numeric):** mean first arrival date for a species at each year for all cells
**ea_lat_yr_ano (numeric):** anomaly of mean first arrival date for a species at each year for all cells
**ea_lat_ano (numeric):** anomaly of mean first arrival date for a species accross all years for all cells

### data/fit_df_tab5.rds
**species (character):** bird species scientific name
**slat (numeric):** scaled cell centroid latitude
**lat (numeric):** latitude in degrees of cell centroid
**sensim (numeric):** posterior mean of estimate for species-specific sensitivity (days/day) to green-up for each cell

### data/for_green-up_dl.rds
**year (numeric):** year of sampling
**cell (numeric):** cell id number
**cell_lat (numeric):** latitude in degrees of cell centroid
**cell_lng (numeric):** longitude in degrees of cell centroid
**gr_mn (numeric):** posterior mean 'midgreen-up' date (calendar day) for all pixels classified as 'good' or 'best' that fall within cell that were classified as forest - green-up dates derived from the MCD12Q2 data product - land cover information derived from MCD12Q1 data product
**gr_ncell (numeric):** number of green-up pixels that met filtering criteria that fell within cell
**gr_pcell (numeric):**
**gr_type (character):** type of greenup - MidGreenup here, which is 50% of maximum (see MCD12Q2 product guide for details)

### data/master_cell_grid.rds
**long (numeric):** longitude in degrees of cell centroid
**lat (numeric):** latitude in degrees of cell centroid
**order (numeric):** number of which of the 6 cornbers of the hexagon the coordinates are
**hole (logical):** indicates if there are any missing coordinates
**piece (numeric):** indicates if we all the points of the heaxon are present
**group (numeric):** group which a row belongs to
**cell (numeric):** cell id number

### data/species_Grid.RData
**species_Grid (character):** list with the names of all species grid cell maps that represent their distribution, and cells with data

### data/stopover_percent.csv
**Species name (character):** bird species scientific name with space
**Flight Speed (numeric):** bird active flight flight speed (km/day)
**Migratory Speed (numeric):** bird migratory flight speed (km/day)
**Migration distance (numeric):** distance which birds migrate from overwintering latitude (km)
**Proportion of time at stopover site (numeric):** proportion of time that birds spend not flying during migration

### data/velocityB.rds
**species (character):** bird species scientific name
**year (numeric):** year of sampling
**cell (numeric):** cell id number
**NArr (numeric):** number of neighboring cell with bird arrival data
**vArrMag (numeric):** bird velocity magnitude (speed, km/day) 
**vArrAng (numeric):** bird velocity angle (direction, radians) 
**angB (numeric):** bird velocity angle in degrees
**xB (numeric):** x coordinate of the direction of bird migration for a given cell 
**yB (numeric):** y coordinate of the direction of bird migration for a given cell 

### data/res/data_trait.rds
**year (numeric):** year of sampling
**cell (numeric):** cell id number
**species (character):** bird species scientific name
**mig_cell (logical):** TRUE if the cell is within the breeding range of the species
**breed_cell (logical):** TRUE if the cell is within the migratory range of the species (note that some cells can be both breeding and migratory)
**cell_lat (numeric):** latitude scaled of cell centroid
**cell_lng (numeric):** longitude in degrees of cell centroid
**arr_GAM_mean (numeric):** estimate of the posterior mean arrival date (calendar day) of a species in a cell
**arr_GAM_sd (numeric):**  estimate of the posterior standard deviation arrival date (calendar day) of a species in a cell
**gr_mn (numeric):** posterior mean 'midgreen-up' date (calendar day) for all pixels classified as 'good' or 'best' that fall within cell that were classified as forest - green-up dates derived from the MCD12Q2 data product - land cover information derived from MCD12Q1 data product
**gr_ncell (numeric):** number of green-up pixels that met filtering criteria that fell within cell
**NArr (numeric):** number of neighboring cell with bird arrival data
**vArrMag (numeric):** bird velocity magnitude (speed, km/day) 
**vArrAng (numeric):** bird velocity angle (direction, radians) 
**angB (numeric):** bird velocity angle in degrees
**xB (numeric):** x coordinate of the direction of bird migration for a given cell 
**yB (numeric):** y coordinate of the direction of bird migration for a given cell 
**NGr (numeric):** number of neighboring cell with green-up data
**vGrMag (numeric):** green-up velocity magnitude (speed, km/day) 
**vGrAng (numeric):** green-up velocity angle (direction, radians) 
**angG (numeric):** green-up velocity angle in degrees
**xG (numeric):** x coordinate of the direction of grenn-up for a given cell 
**yG (numeric):** y coordinate of the direction of grenn-up for a given cell 
**lag (numeric):** bird arrival date minus green-up date (days)
**AnomDArr (numeric):** bird arrival date anomaly (scaled bird arrival date)
**AnomDGr (numeric):** green-up date anomaly (scaled green-up date)
**AnomVArr (numeric):** bird speed anomaly (scaled bird speed)
**AnomVGr (numeric):** green-up speed anomaly (scaled green-up speed )
**AnomLag (numeric):** lag anomaly (scaled lag)
**cell_lat2 (numeric):** latitude in degrees of cell centroid
**winlat (numeric):** mean latitude in which species overwinter
**species2 (character):** species name with space and not underscore
**sensi (numeric):** posterior mean of estimate for species-specific sensitivity (days/day) to green-up for each cell
**sensi_mean (numeric):** mean species sensitivity (days/day) for all cells
**sensi_sd (numeric):** standard deviation of bird sensitivity (days/day)
**Order (character):** species taxonomic order
**Family (character):** species taxonomic family
**Body_mass_g (numeric):** species average body mass in grams
**Time (character):** time of the day that species migrate (diurnal or nocturnal)
**Diet (character):** species diet
**FamilyColor (character):** color according to species family
**TaxOrder (numeric):** number for each taxonimic family to order figures
**HWI (numeric):** hand-wing index for each species
**ea_lat_m (numeric):** mean first arrival date for a species accross all years for all cells
**ea_lat (numeric):** : mean first arrival date for a species in each cell
**ea_lat_yr (numeric):** mean first arrival date for a species at each year for all cells
**ea_lat_yr_ano (numeric):** anomaly of mean first arrival date for a species at each year for all cells
**ea_lat_ano (numeric):** anomaly of mean first arrival date for a species accross all years for all cells
**sps_cell (numeric):** unique combination of each species in each cell
**xi_mean (numeric):** posterior mean of species-level sensitivity
**xi_sd (numeric):** posterior sd of species-level sensitivity
**ea_lat2 (numeric):** mean first arrival date for a species in each cell in degrees

### data/res/mod_gu.rds
gam object with results for model 2

### data/res/mod_lag.rds
gam object with results for model 4

### data/res/mod_lat_guspe.rds
gam object with results for model 1

### data/res/mod_tra.rds
gam object with results for model 3

### data/source/arrival_master_2020-07-21.csv
**species (character):** bird species scientific name
**cell (numeric):** cell id number
**mig_cell (logical):** TRUE if the cell is within the breeding range of the species
**breed_cell (logical):** TRUE if the cell is within the migratory range of the species (note that some cells can be both breeding and migratory)
**cell_lat (numeric):** latitude  in degrees of cell centroid
**cell_lng (numeric):** longitude in degrees of cell centroid
**arr_GAM_mean (numeric):** estimate of the posterior mean arrival date (calendar day) of a species in a cell
**arr_GAM_sd (numeric):** estimate of the posterior standard deviation arrival date (calendar day) of a species in a cell
**valid_GAM (logical):** denotes whether a spcies-cell-year had a valid GAM estimate. Can be used to filter IAR-derived estimates for downstream analyses (see below).
**arr_IAR_mean (numeric):** posterior mean of IAR-derived arrival estimate
**arr_IAR_sd (numeric):** posterior mean of IAR-derived arrival estimate

### data/source/MidGreenup_2020-08-06-forest.csv
**year (numeric):** year of sampling
**cell (numeric):** cell id number
**cell_lat (numeric):** latitude  in degrees of cell centroid
**cell_lng (numeric):** longitude in degrees of cell centroid
**gr_mn (numeric):** posterior mean 'midgreen-up' date (calendar day) for all pixels classified as 'good' or 'best' that fall within cell that were classified as forest - green-up dates derived from the MCD12Q2 data product - land cover information derived from MCD12Q1 data product
**gr_ncell (numeric):** number of green-up pixels that met filtering criteria that fell within cell

### data/source/Speed_data/Speed_table_small.csv
**species (character):** bird species scientific name
**flight_speed (numeric):** bird active flight flight speed (km/day)

### data/source/spskey.csv
**species (character):** bird species common name
**sci_name (character):** bird species scientific name

### data/source/traits/birds_HWI.csv
**Species-ID (numeric):** species unique number ID
**Order (character):** species taxonomic order
**Species name (character):** bird species scientific name
**IUCN name (character):** bird species scientific name according to the IUCN
**Tree name (character):** bird species phylogenectic name
**HWI (numeric):** hand-wing index for each species
**species (character):** bird species scientific name

### data/source/traits/data_sensi.rds
**sci_name (character):** bird species scientific name
**species (character):** bird species common name
**cell (numeric):** cell id number
**cell_lat (numeric):** latitude in degrees of cell centroid
**cell_lng (numeric):** longitude in degrees of cell centroid
**beta_mean (numeric):** posterior mean of species/cell specific sensitivity of arrival to green-up
**beta_sd (numeric):** posterior sd of species/cell specific sensitivity of arrival to green-up
**xi_mean (numeric):** posterior mean of species-level sensitivity
**xi_sd (numeric):** posterior sd of species-level sensitivity
**gamma_mean (numeric):** posterior mean of the effect of latitude on xi
**gamma_sd (numeric):** posterior sd of the effect of latitude on xi

### data/source/traits/species_tax_ord.csv
**FamilyColor (character):** color according to species family
**species (character):** bird species scientific name
**TaxOrder (numeric):** number for each taxonimic family to order figures

### data/source/traits/sps_migtime.csv
**CommonName (character):** bird species common name
**species (character):** bird species scientific name
**Order (character):** species taxonomic order
**Family (character):** species taxonomic family
**Time (character):** time of the day that species migrate (diurnal or nocturnal)

### data/source/traits/Table_S1.csv
**Species (character):** bird species scientific name with space
**Common name (character):** bird species common name with space
**Order (character):** species taxonomic order
**Included (logical):** species included in the analysis (original, not this paper)
**Mig Pace (numeric):** how fast species moved (days / deg latitude)
**Arr date (numeric):** species mean arrival date (ordinal date)
**Overwinterlatitude (numeric):** mean latitude in which species overwinter
**PC1 (numeric):** trais PC1 score for each species (migration pace, arrival date, overwinter latitude)
**species (character):** bird species scientific name 