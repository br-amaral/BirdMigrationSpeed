
# load packages -----------------------------------------------------------

library(picante)
library(ape)
library(dplyr)

'%!in%' <- function(x,y)!('%in%'(x,y))

# read in data ------------------------------------------------------------
mod_tra <- readRDS(file = "data/res/mod_tra.rds")

xi_resid <- residuals.gam(mod_tra)

res_sps <- model.frame(mod_tra) %>% 
  select(sps_cell) 
res_sps$species <- NA

for(i in 1:nrow(res_sps)){
  res_sps$species[i] <- paste0(str_split(res_sps$sps_cell[i], '\\_')[[1]][1],
                               '_',
                               str_split(res_sps$sps_cell[i], '\\_')[[1]][2])
}

species <- res_sps$species

xi_resid2 <- cbind(species, xi_resid) %>% 
  as_tibble() %>% 
  mutate(species = replace(species, species == "Geothlypis_formosa","Oporornis_formosus"),
         species = replace(species, species == "Oreothlypis_ruficapilla","Vermivora_ruficapilla"),
         species = replace(species, species == "Parkesia_motacilla","Seiurus_motacilla"),
         species = replace(species, species == "Setophaga_americana","Parula_americana"),
         species = replace(species, species == "Setophaga_caerulescens","Dendroica_caerulescens"),
         species = replace(species, species == "Setophaga_cerulea","Dendroica_cerulea"),
         species = replace(species, species == "Setophaga_citrina","Wilsonia_citrina"),
         species = replace(species, species == "Setophaga_discolor","Dendroica_discolor"),
         species = replace(species, species == "Setophaga_dominica","Dendroica_dominica"),
         species = replace(species, species == "Setophaga_fusca","Dendroica_fusca"),
         species = replace(species, species == "Setophaga_magnolia","Dendroica_magnolia"),
         species = replace(species, species == "Setophaga_pensylvanica","Dendroica_pensylvanica"),
         species = replace(species, species == "Setophaga_petechia","Dendroica_petechia"),
         species = replace(species, species == "Setophaga_pinus","Dendroica_pinus"),
         species = replace(species, species == "Setophaga_striata","Dendroica_striata"),
         species = replace(species, species == "Setophaga_tigrina","Dendroica_tigrina"),
         species = replace(species, species == "Setophaga_virens","Dendroica_virens"),
         species = replace(species, species == "Vermivora_cyanoptera","Vermivora_pinus"),
         xi_resid = as.numeric(xi_resid)) %>% 
  group_by(species) %>% 
  summarise(mean_spe = mean(xi_resid))

# bird names (used in bird trees) 
bn2 <- xi_resid2 %>% select(species) %>% pull %>% sort() %>% unique() %>% as.character

bn3 <- data.frame(name = bn2, num = 1:length(bn2))

#read in tree (100 trees)
#tree downlaoded from: http://birdtree.org
# Ericson All Species: a set of 10000 trees with 9993 OTUs each
tree <- ape::read.nexus('data/source/philo_tree/tree-pruner-c9524fe9-2d15-4b45-a65a-03b4fa85e4e9/output.nex')

bn2[which(bn2 %!in% tree$tree_1605$tip.label)] == tree$tree_1605$tip.label[which(tree$tree_1605$tip.label %!in% bn2)] 

# calculate Blomberg's K for each tree -----------------------------------
out.df <- data.frame(K = rep(NA, length(tree), PIC.var.P = NA))
for (i in 1:length(tree)){
  #i <- 1
  tree_n <- tree[[i]]
  
  #get index for name order on tips
  j_idx <- dplyr::left_join(data.frame(name = tree_n$tip.label), bn3)
  #apply to residuals
  resid_srt <- xi_resid2[j_idx$num,]
  phy_res <- picante::phylosignal(resid_srt$mean_spe, tree_n)
  out.df$K[i] <- phy_res$K
  out.df$PIC.var.P[i] <- phy_res$PIC.variance.P
}

randtree <- rcoal(20)
randtraits <- rTraitCont(randtree)
phylosignal(randtraits[randtree$tip.label],randtree)

# summarize output --------------------------------------------------------

hist(out.df$PIC.var.P)
sum(out.df$PIC.var.P < 0.05)
out.df
