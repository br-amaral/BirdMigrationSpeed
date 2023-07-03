# Predict how much a species speed will change with 1 sd of green-up date or speed
## Green-up date -------------------------------------------------------------------------------------------
### average species: Troglodytes_aedon -----------------------------------------------------------------------
#### early years -------------------------------------------------------------------------------------------
new_data_early <- data.frame(AnomDGr = -10,
                             AnomVGr = 0,
                             mig_cell = c(T,F),
                             species = "Troglodytes_aedon",
                             year = 1,
                             cell_lat2 = 1,
                             sps_cell = 1,
                             cell = 1,
                             mig_cell = 1)

pred_early <- predict(mod_gu,
                      newdata = new_data_early,
                      se.fit = TRUE, iterms.type=2, re.form=NA, exclude = list("s(year)","s(cell_lat2)","s(sps_cell)", "s(cell)", "mig_cell"))

pred_early_tab <- as.data.frame(matrix(ncol = 3,
                                       data = c(pred_early$fit, 
                                                pred_early$fit + pred_early$se.fit,
                                                pred_early$fit - pred_early$se.fit),
                                       byrow = F)) 

colnames(pred_early_tab) <- c("mean", "up","low")

(pred_early_tabX <- pred_early_tab %>% 
  mutate(mean = exp(mean),
         up = exp(up),
         low = exp(low),
         mig_cell = c(T,F)))

#### average year -------------------------------------------------------------------------------------------
new_data_ave <- data.frame(AnomDGr = 0,
                           AnomVGr = 0,
                           mig_cell = c(T,F),
                           species = "Troglodytes_aedon",
                           year = 1,
                           cell_lat2 = 1,
                           sps_cell = 1,
                           cell = 1,
                           mig_cell = 1)

pred_ave <- predict(mod_gu,
                    newdata = new_data_ave,
                    se.fit = TRUE, iterms.type=2, re.form=NA,exclude = list("s(year)","s(cell_lat2)","s(sps_cell)", "s(cell)", "mig_cell"))

pred_ave_tab <- as.data.frame(matrix(ncol = 3,
                                     data = c(pred_ave$fit, 
                                              pred_ave$fit + pred_ave$se.fit,
                                              pred_ave$fit - pred_ave$se.fit),
                                     byrow = F)) 

colnames(pred_ave_tab) <- c("mean", "up","low")

(pred_ave_tabX <- pred_ave_tab %>% 
  mutate(mean = exp(mean),
         up = exp(up),
         low = exp(low),
         mig_cell = c(T,F)))

#### late year -------------------------------------------------------------------------------------------
new_data_late <- data.frame(AnomDGr = 10,
                            AnomVGr = 0,
                            mig_cell = c(T,F),
                            species = "Troglodytes_aedon",
                            year = 1,
                            cell_lat2 = 1,
                            sps_cell = 1,
                            cell = 1,
                            mig_cell = 1)

pred_late <- predict(mod_gu,
                     newdata = new_data_late,
                     se.fit = TRUE, iterms.type=2, re.form=NA,exclude = list("s(year)","s(cell_lat2)","s(sps_cell)", "s(cell)", "mig_cell"))

pred_late_tab <- as.data.frame(matrix(ncol = 3,
                                      data = c(pred_late$fit, 
                                               pred_late$fit + pred_late$se.fit,
                                               pred_late$fit - pred_late$se.fit),
                                      byrow = F)) 

colnames(pred_late_tab) <- c("mean", "up","low")

(pred_late_tabX <- pred_late_tab %>% 
  mutate(mean = exp(mean),
         up = exp(up),
         low = exp(low),
         mig_cell = c(T,F)))

### slow species: Tachycineta_bicolor ----------------------------------------------------------------------
#### early years -------------------------------------------------------------------------------------------
new_data_early <- data.frame(AnomDGr = -10,
                             AnomVGr = 0,
                             mig_cell = c(T,F),
                             species = "Tachycineta_bicolor",
                             year = 1,
                             cell_lat2 = 1,
                             sps_cell = 1,
                             cell = 1,
                             mig_cell = 1)

pred_early <- predict(mod_gu,
                      newdata = new_data_early,
                      se.fit = TRUE, iterms.type=2, re.form=NA,exclude = list("s(year)","s(cell_lat2)","s(sps_cell)", "s(cell)", "mig_cell"))

pred_early_tab <- as.data.frame(matrix(ncol = 3,
                                       data = c(pred_early$fit, 
                                                pred_early$fit + pred_early$se.fit,
                                                pred_early$fit - pred_early$se.fit),
                                       byrow = F)) 

colnames(pred_early_tab) <- c("mean", "up","low")

pred_early_tabX <- pred_early_tab %>% 
  mutate(mean = exp(mean),
         up = exp(up),
         low = exp(low),
         mig_cell = c(T,F))

#### average year -------------------------------------------------------------------------------------------
new_data_ave <- data.frame(AnomDGr = 0,
                           AnomVGr = 0,
                           mig_cell = c(T,F),
                           species = "Tachycineta_bicolor",
                           year = 1,
                           cell_lat2 = 1,
                           sps_cell = 1,
                           cell = 1,
                           mig_cell = 1)

pred_ave <- predict(mod_gu,
                    newdata = new_data_ave,
                    se.fit = TRUE, iterms.type=2, re.form=NA,exclude = list("s(year)","s(cell_lat2)","s(sps_cell)", "s(cell)", "mig_cell"))

pred_ave_tab <- as.data.frame(matrix(ncol = 3,
                                     data = c(pred_ave$fit, 
                                              pred_ave$fit + pred_ave$se.fit,
                                              pred_ave$fit - pred_ave$se.fit),
                                     byrow = F)) 

colnames(pred_ave_tab) <- c("mean", "up","low")

pred_ave_tabX <- pred_ave_tab %>% 
  mutate(mean = exp(mean),
         up = exp(up),
         low = exp(low),
         mig_cell = c(T,F))

#### late year -------------------------------------------------------------------------------------------
new_data_late <- data.frame(AnomDGr = 10,
                            AnomVGr = 0,
                            mig_cell = c(T,F),
                            species = "Tachycineta_bicolor",
                            year = 1,
                            cell_lat2 = 1,
                            sps_cell = 1,
                            cell = 1,
                            mig_cell = 1)

pred_late <- predict(mod_gu,
                     newdata = new_data_late,
                     se.fit = TRUE, iterms.type=2, re.form=NA,exclude = list("s(year)","s(cell_lat2)","s(sps_cell)", "s(cell)", "mig_cell"))

pred_late_tab <- as.data.frame(matrix(ncol = 3,
                                      data = c(pred_late$fit, 
                                               pred_late$fit + pred_late$se.fit,
                                               pred_late$fit - pred_late$se.fit),
                                      byrow = F)) 

colnames(pred_late_tab) <- c("mean", "up","low")

pred_late_tabX <- pred_late_tab %>% 
  mutate(mean = exp(mean),
         up = exp(up),
         low = exp(low),
         mig_cell = c(T,F))

### fast species: Empidonax_traillii ----------------------------------------------------------------------
#### early years ------------------------------------------------------------------------------------------
new_data_early <- data.frame(AnomDGr = -10,
                             AnomVGr = 0,
                             mig_cell = c(T,F),
                             species = "Empidonax_traillii",
                             year = 1,
                             cell_lat2 = 1,
                             sps_cell = 1,
                             cell = 1,
                             mig_cell = 1)

pred_early <- predict(mod_gu,
                      newdata = new_data_early,
                      se.fit = TRUE, iterms.type=2, re.form=NA,exclude = list("s(year)","s(cell_lat2)","s(sps_cell)", "s(cell)", "mig_cell"))

pred_early_tab <- as.data.frame(matrix(ncol = 3,
                                       data = c(pred_early$fit, 
                                                pred_early$fit + pred_early$se.fit,
                                                pred_early$fit - pred_early$se.fit),
                                       byrow = F)) 

colnames(pred_early_tab) <- c("mean", "up","low")

pred_early_tabX <- pred_early_tab %>% 
  mutate(mean = exp(mean),
         up = exp(up),
         low = exp(low),
         mig_cell = c(T,F))

#### average year -------------------------------------------------------------------------------------------
new_data_ave <- data.frame(AnomDGr = 0,
                           AnomVGr = 0,
                           mig_cell = c(T,F),
                           species = "Empidonax_traillii",
                           year = 1,
                           cell_lat2 = 1,
                           sps_cell = 1,
                           cell = 1,
                           mig_cell = 1)

pred_ave <- predict(mod_gu,
                    newdata = new_data_ave,
                    se.fit = TRUE, iterms.type=2, re.form=NA,exclude = list("s(year)","s(cell_lat2)","s(sps_cell)", "s(cell)", "mig_cell"))

pred_ave_tab <- as.data.frame(matrix(ncol = 3,
                                     data = c(pred_ave$fit, 
                                              pred_ave$fit + pred_ave$se.fit,
                                              pred_ave$fit - pred_ave$se.fit),
                                     byrow = F)) 

colnames(pred_ave_tab) <- c("mean", "up","low")

pred_ave_tabX <- pred_ave_tab %>% 
  mutate(mean = exp(mean),
         up = exp(up),
         low = exp(low),
         mig_cell = c(T,F))

#### late year -------------------------------------------------------------------------------------------
new_data_late <- data.frame(AnomDGr = 10,
                            AnomVGr = 0,
                            mig_cell = c(T,F),
                            species = "Empidonax_traillii",
                            year = 1,
                            cell_lat2 = 1,
                            sps_cell = 1,
                            cell = 1,
                            mig_cell = 1)

pred_late <- predict(mod_gu,
                     newdata = new_data_late,
                     se.fit = TRUE, iterms.type=2, re.form=NA,exclude = list("s(year)","s(cell_lat2)","s(sps_cell)", "s(cell)", "mig_cell"))

pred_late_tab <- as.data.frame(matrix(ncol = 3,
                                      data = c(pred_late$fit, 
                                               pred_late$fit + pred_late$se.fit,
                                               pred_late$fit - pred_late$se.fit),
                                      byrow = F)) 

colnames(pred_late_tab) <- c("mean", "up","low")

pred_late_tabX <- pred_late_tab %>% 
  mutate(mean = exp(mean),
         up = exp(up),
         low = exp(low),
         mig_cell = c(T,F))

# Green-up Speed -----------------------------------------------------------------------
### average species: Troglodytes_aedon -----------------------------------------------------------------------
#### slow years -------------------------------------------------------------------------------------------
new_data_slow <- data.frame(AnomDGr = 0,
                             AnomVGr = -0.7,
                             mig_cell = c(T,F),
                             species = "Troglodytes_aedon",
                             year = 1,
                             cell_lat2 = 1,
                             sps_cell = 1,
                             cell = 1,
                             mig_cell = 1)

pred_slow <- predict(mod_gu,
                      newdata = new_data_slow,
                      se.fit = TRUE, iterms.type=2, re.form=NA, exclude = list("s(year)","s(cell_lat2)","s(sps_cell)", "s(cell)", "mig_cell"))

pred_slow_tab <- as.data.frame(matrix(ncol = 3,
                                       data = c(pred_slow$fit, 
                                                pred_slow$fit + pred_slow$se.fit,
                                                pred_slow$fit - pred_slow$se.fit),
                                       byrow = F)) 

colnames(pred_slow_tab) <- c("mean", "up","low")

(pred_slow_tabX <- pred_slow_tab %>% 
    mutate(mean = exp(mean),
           up = exp(up),
           low = exp(low),
           mig_cell = c(T,F)))

#### average year -------------------------------------------------------------------------------------------
new_data_ave <- data.frame(AnomDGr = 0,
                           AnomVGr = 0,
                           mig_cell = c(T,F),
                           species = "Troglodytes_aedon",
                           year = 1,
                           cell_lat2 = 1,
                           sps_cell = 1,
                           cell = 1,
                           mig_cell = 1)

pred_ave <- predict(mod_gu,
                    newdata = new_data_ave,
                    se.fit = TRUE, iterms.type=2, re.form=NA,exclude = list("s(year)","s(cell_lat2)","s(sps_cell)", "s(cell)", "mig_cell"))

pred_ave_tab <- as.data.frame(matrix(ncol = 3,
                                     data = c(pred_ave$fit, 
                                              pred_ave$fit + pred_ave$se.fit,
                                              pred_ave$fit - pred_ave$se.fit),
                                     byrow = F)) 

colnames(pred_ave_tab) <- c("mean", "up","low")

(pred_ave_tabX <- pred_ave_tab %>% 
    mutate(mean = exp(mean),
           up = exp(up),
           low = exp(low),
           mig_cell = c(T,F)))

#### fast year -------------------------------------------------------------------------------------------
new_data_fast <- data.frame(AnomDGr = 0,
                            AnomVGr = 0.7,
                            mig_cell = c(T,F),
                            species = "Troglodytes_aedon",
                            year = 1,
                            cell_lat2 = 1,
                            sps_cell = 1,
                            cell = 1,
                            mig_cell = 1)

pred_fast <- predict(mod_gu,
                     newdata = new_data_fast,
                     se.fit = TRUE, iterms.type=2, re.form=NA,exclude = list("s(year)","s(cell_lat2)","s(sps_cell)", "s(cell)", "mig_cell"))

pred_fast_tab <- as.data.frame(matrix(ncol = 3,
                                      data = c(pred_fast$fit, 
                                               pred_fast$fit + pred_fast$se.fit,
                                               pred_fast$fit - pred_fast$se.fit),
                                      byrow = F)) 

colnames(pred_fast_tab) <- c("mean", "up","low")

(pred_fast_tabX <- pred_fast_tab %>% 
    mutate(mean = exp(mean),
           up = exp(up),
           low = exp(low),
           mig_cell = c(T,F)))

