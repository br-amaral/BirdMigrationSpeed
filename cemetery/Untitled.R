## script to analyze how bird arrival date speed is affected by green-up and bird traits

# Green-up effect on speed
mod_gu <- mgcv::gam(data = final %>% mutate(species = as.factor(species), cell = as.factor(cell)), 
                     log(vArrMag) ~ (AnomDGr + AnomVGr) * mig_cell + 
                       s(species, bs = "re") + s(year, bs = "re") + s(cell_lat, bs = "tp") + s(cell, bs = "re")) 

summary(mod_gu)

mod_yr5 <- lm(data = annual, ABS ~ AGS + AGD)


summary(mod_yr5)






log(vArrMag) ~xi_mean + ea_lat + Diet + Body_mass_g + winlat + Time+ s(cell_lat, bs ='tp') +  s(species, bs ='re') + s(year, bs='re') + s(cell, bs='re')