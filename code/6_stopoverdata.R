

library(glue)
library(tidyverse)

fli_spe <- as_tibble(matrix(data = c("Dumetella_carolinensis", 24.1,
                                     "Tyrannus_tyrannus", 24.1,
                                     "Spizella_passerina", 28.1,
                                     "Icterus_galbula", 28.3,
                                     "Setophaga_striata", 63.5,
                                     "Passerina_cyanea", 32.2,
                                     "Tachycineta_bicolor", 30.3,
                                     "Hirundo_rustica", 48.1,
                                     "Archilochus_colubris", 77.8),
                            ncol = 2, byrow = T))
colnames(fli_spe) <- c("species", "fli_spe")

mig_spe <- final2 %>% select(species, vArrMag) %>% group_by(species) %>% summarise(mig_spe = mean(vArrMag))

spee <- left_join(fli_spe, mig_spe, by = "species")
