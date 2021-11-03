library(phyloclim)
library(ape)

#Making PNOs - by variable for all models
#Path_bioclim - for projected NW layers - with correct heading
#Path_models - for projected models for each species (put into the same folder)
pno_ai <- pno(path_bioclim = "./Layers/3_clades/Projecting/North/Run7/North_ai_et0.asc", path_model = "./Models/3_clades/Run7_all/", subset = NULL, bin_number = 100)
pno_bdticm <- pno(path_bioclim = "./Layers/3_clades/Projecting/North/Run7/North_bdticm_trimmed.asc", path_model = "./Models/3_clades/Run7_all/", subset = NULL, bin_number = 100)
pno_bio1 <- pno(path_bioclim = "./Layers/3_clades/Projecting/North/Run7/North_bio1.asc", path_model = "./Models/3_clades/Run7_all/", subset = NULL, bin_number = 100)
pno_bio2 <- pno(path_bioclim = "./Layers/3_clades/Projecting/North/Run7/North_bio2.asc", path_model = "./Models/3_clades/Run7_all/", subset = NULL, bin_number = 100)
pno_bio3 <- pno(path_bioclim = "./Layers/3_clades/Projecting/North/Run7/North_bio3.asc", path_model = "./Models/3_clades/Run7_all/", subset = NULL, bin_number = 100)
pno_bio4 <- pno(path_bioclim = "./Layers/3_clades/Projecting/North/Run7/North_bio4.asc", path_model = "./Models/3_clades/Run7_all/", subset = NULL, bin_number = 100)
pno_bio5 <- pno(path_bioclim = "./Layers/3_clades/Projecting/North/Run7/North_bio5.asc", path_model = "./Models/3_clades/Run7_all/", subset = NULL, bin_number = 100)
pno_bio6 <- pno(path_bioclim = "./Layers/3_clades/Projecting/North/Run7/North_bio6.asc", path_model = "./Models/3_clades/Run7_all/", subset = NULL, bin_number = 100)
pno_bio7 <- pno(path_bioclim = "./Layers/3_clades/Projecting/North/Run7/North_bio7.asc", path_model = "./Models/3_clades/Run7_all/", subset = NULL, bin_number = 100)
pno_bio12 <- pno(path_bioclim = "./Layers/3_clades/Projecting/North/Run7/North_bio12.asc", path_model = "./Models/3_clades/Run7_all/", subset = NULL, bin_number = 100)
pno_bio15 <- pno(path_bioclim = "./Layers/3_clades/Projecting/North/Run7/North_bio15.asc", path_model = "./Models/3_clades/Run7_all/", subset = NULL, bin_number = 100)
pno_bio16 <- pno(path_bioclim = "./Layers/3_clades/Projecting/North/Run7/North_bio16.asc", path_model = "./Models/3_clades/Run7_all/", subset = NULL, bin_number = 100)
pno_bio17 <- pno(path_bioclim = "./Layers/3_clades/Projecting/North/Run7/North_bio17.asc", path_model = "./Models/3_clades/Run7_all/", subset = NULL, bin_number = 100)
pno_bio18 <- pno(path_bioclim = "./Layers/3_clades/Projecting/North/Run7/North_bio18.asc", path_model = "./Models/3_clades/Run7_all/", subset = NULL, bin_number = 100)
pno_bio19 <- pno(path_bioclim = "./Layers/3_clades/Projecting/North/Run7/North_bio19.asc", path_model = "./Models/3_clades/Run7_all/", subset = NULL, bin_number = 100)
pno_bldfie <- pno(path_bioclim = "./Layers/3_clades/Projecting/North/Run7/North_bldfie_trimmed.asc", path_model = "./Models/3_clades/Run7_all/", subset = NULL, bin_number = 100)
pno_canopy <- pno(path_bioclim = "./Layers/3_clades/Projecting/North/Run7/North_canopy_trimmed.asc", path_model = "./Models/3_clades/Run7_all/", subset = NULL, bin_number = 100)
pno_et <- pno(path_bioclim = "./Layers/3_clades/Projecting/North/Run7/North_et0_yr.asc", path_model = "./Models/3_clades/Run7_all/", subset = NULL, bin_number = 100)
pno_sndppt <- pno(path_bioclim = "./Layers/3_clades/Projecting/North/Run7/North_sndppt_trimmed.asc", path_model = "./Models/3_clades/Run7_all/", subset = NULL, bin_number = 100)

#list of pnos
# pno_ai
# pno_bdticm
# pno_bio1
# pno_bio2
# pno_bio3
# pno_bio4
# pno_bio16
# pno_bio17
# pno_bio18
# pno_bio19
# pno_bldfie
# pno_canopy
# pno_et 
# pno_sndppt

#Make pnos dataframes
pno_ai_df <- as.data.frame(pno_ai)
pno_bdticm_df <- as.data.frame(pno_bdticm)
pno_bio1_df <- as.data.frame(pno_bio1)
pno_bio2_df <- as.data.frame(pno_bio2)
pno_bio3_df <- as.data.frame(pno_bio3)
pno_bio4_df <- as.data.frame(pno_bio4)
pno_bio5_df <- as.data.frame(pno_bio5)
pno_bio6_df <- as.data.frame(pno_bio6)
pno_bio7_df <- as.data.frame(pno_bio7)
pno_bio12_df <- as.data.frame(pno_bio12)
pno_bio15_df <- as.data.frame(pno_bio15)
pno_bio16_df <- as.data.frame(pno_bio16)
pno_bio17_df <- as.data.frame(pno_bio17)
pno_bio18_df <- as.data.frame(pno_bio18)
pno_bio19_df <- as.data.frame(pno_bio19)
pno_bldfie_df <- as.data.frame(pno_bldfie)
pno_bdticm_df <- as.data.frame(pno_bdticm)
pno_canopy_df <- as.data.frame(pno_canopy)
pno_et_df <- as.data.frame(pno_et)
pno_sndppt_df <- as.data.frame(pno_sndppt)

#Change column names
model_names <- c("Variable", "North clade", "South clade", "Widespread clade")
colnames(pno_ai_df) <- model_names
colnames(pno_bdticm_df) <- model_names
colnames(pno_bio1_df) <- model_names
colnames(pno_bio2_df) <- model_names
colnames(pno_bio3_df) <- model_names
colnames(pno_bio4_df) <- model_names
colnames(pno_bio5_df) <- model_names
colnames(pno_bio6_df) <- model_names
colnames(pno_bio7_df) <- model_names
colnames(pno_bio12_df) <- model_names
colnames(pno_bio15_df) <- model_names
colnames(pno_bio16_df) <- model_names
colnames(pno_bio17_df) <- model_names
colnames(pno_bio18_df) <- model_names
colnames(pno_bio19_df) <- model_names
colnames(pno_bldfie_df) <- model_names
colnames(pno_canopy_df) <- model_names
colnames(pno_et_df) <- model_names
colnames(pno_sndppt_df) <- model_names


#Making a list format that phyloclim likes because she's picky ONLY USE FOR PNO's ONLY
pno_list <- list(pno_ai = pno_ai_df, pno_bdticm = pno_bdticm_df, pno_bio1 = pno_bio1_df, 
                 pno_bio2 = pno_bio2_df, pno_bio3 = pno_bio3_df, pno_bio4 = pno_bio4_df,
                 pno_bio5 = pno_bio5_df, pno_bio6 = pno_bio6_df, pno_bio7 = pno_bio7_df,
                 pno_bio12 = pno_bio12_df, pno_bio15 = pno_bio15_df,
                 pno_bio16 = pno_bio16_df, pno_bio17 = pno_bio17_df, pno_bio18 = pno_bio18_df,
                 pno_bio19 = pno_bio19_df, pno_bldfie = pno_bldfie_df, pno_canopy = pno_canopy_df, 
                 pno_et = pno_et_df, pno_sndppt = pno_sndppt_df)

 
# 

write.csv(pno_list, "./Models/3_clades/Run7_all/PNO_avg_all_run7.csv", row.names = FALSE)


#Figure out how to move and change the legend - for plotting multiple pnos at once
# pno_ai
# pno_bdticm
# pno_bio1
# pno_bio2
# pno_bio3
# pno_bio4
# pno_bio16
# pno_bio17
# pno_bio18
# pno_bio19
# pno_bldfie
# pno_canopy
# pno_et 
# pno_sndppt

pdf("./Output_figures/3_clades/Run7/ai_avg.pdf")
jpeg("./Output_figures/3_clades/Run7/ai_avg.jpg", res = 600, units = "in", width = 6, height = 6)
plotPNO(x = pno_list$pno_ai, legend.pos = "topright", xlab = "", wm = TRUE, tail_threshold = 0.005)
mtext("Aridity", side=1, line=3, cex=1.5)
dev.off()

pdf("./Output_figures/3_clades/Run7/bdticm_avg.pdf")
plotPNO(x = pno_list$pno_bdticm, legend.pos = "topright", xlab = "", wm = TRUE, tail_threshold = 0.005)
mtext("Depth to bedrock", side=1, line=3, cex=1.5)
dev.off()

pdf("./Output_figures/3_clades/Run7/bio1_avg.pdf")
plotPNO(x = pno_list$pno_bio1, legend.pos = "topleft", xlab = "", wm = TRUE, tail_threshold = 0.005)
mtext("Annual Mean Temperature (deg C*10)", side=1, line=3, cex=1.5)
dev.off()

pdf("./Output_figures/3_clades/Run7/bio2_avg.pdf")
plotPNO(x = pno_list$pno_bio2, legend.pos = "topright", xlab = "", wm = TRUE, tail_threshold = 0.005)
mtext("Mean Diurnal Range (Mean of monthly (max temp - min temp))", side=1, line=3, cex=1)
dev.off()

pdf("./Output_figures/3_clades/Run7/bio3_avg.pdf")
plotPNO(x = pno_list$pno_bio3, legend.pos = "topright", xlab = "", wm = TRUE, tail_threshold = 0.005)
mtext("Isothermality (BIO2/BIO7) (×100)", side=1, line=3, cex=1)
dev.off()

pdf("./Output_figures/3_clades/Run7/bio4_avg.pdf")
plotPNO(x = pno_list$pno_bio4, legend.pos = "topright", xlab = "", wm = TRUE, tail_threshold = 0.005)
mtext("Temperature Seasonality (standard deviation ×100)", side=1, line=3, cex=1)
dev.off()

pdf("./Output_figures/3_clades/Run7/bio5_avg.pdf")
plotPNO(x = pno_list$pno_bio5, legend.pos = "topright", xlab = "", wm = TRUE, tail_threshold = 0.005)
mtext("Max Temperature of Warmest Month", side=1, line=3, cex=1)
dev.off()

pdf("./Output_figures/3_clades/Run7/bio6_avg.pdf")
jpeg("./Output_figures/3_clades/Run7/bio6_avg.jpg", res = 600, units = "in", width = 6, height = 6)
plotPNO(x = pno_list$pno_bio6, legend.pos = "topleft", xlab = "", wm = TRUE, tail_threshold = 0.005)
mtext("Min Temperature of Coldest Month", side=1, line=3, cex=1)
dev.off()

pdf("./Output_figures/3_clades/Run7/bio7_avg.pdf")
jpeg("./Output_figures/3_clades/Run7/bio7_avg.jpg", res = 600, units = "in", width = 6, height = 6)
plotPNO(x = pno_list$pno_bio7, legend.pos = "topleft", xlab = "", wm = TRUE, tail_threshold = 0.005)
mtext("Temperature Annual Range (BIO5-BIO6)", side=1, line=3, cex=1)
dev.off()

pdf("./Output_figures/3_clades/Run7/bio12_avg.pdf")
plotPNO(x = pno_list$pno_bio12, legend.pos = "topright", xlab = "", wm = TRUE, tail_threshold = 0.005)
mtext("Annual Precipitation", side=1, line=3, cex=1)
dev.off()

pdf("./Output_figures/3_clades/Run7/bio15_avg.pdf")
plotPNO(x = pno_list$pno_bio15, legend.pos = "topright", xlab = "", wm = TRUE, tail_threshold = 0.005)
mtext("Precipitation Seasonality (Coefficient of Variation)", side=1, line=3, cex=1)
dev.off()

pdf("./Output_figures/3_clades/Run7/bio16_avg.pdf")
jpeg("./Output_figures/3_clades/Run7/bio16_avg.jpg", res = 600, units = "in", width = 6, height = 6)
plotPNO(x = pno_list$pno_bio16, legend.pos = "topright", xlab = "", wm = TRUE, tail_threshold = 0.005)
mtext("Precipitation of Wettest Quarter", side=1, line=3, cex=1)
dev.off()

pdf("./Output_figures/3_clades/Run7/bio17_avg.pdf")
jpeg("./Output_figures/3_clades/Run7/bio17_avg.jpg", res = 600, units = "in", width = 6, height = 6)
plotPNO(x = pno_list$pno_bio17, legend.pos = "topright", xlab = "", wm = TRUE, tail_threshold = 0.005)
mtext("Precipitation of Driest Quarter", side=1, line=3, cex=1)
dev.off()

pdf("./Output_figures/3_clades/Run7/bio18_avg.pdf")
plotPNO(x = pno_list$pno_bio18, legend.pos = "topright", xlab = "", wm = TRUE, tail_threshold = 0.005)
mtext("Precipitation of Warmest Quarter", side=1, line=3, cex=1)
dev.off()

pdf("./Output_figures/3_clades/Run7/bio19_avg.pdf")
plotPNO(x = pno_list$pno_bio19, legend.pos = "topright", xlab = "", wm = TRUE, tail_threshold = 0.005)
mtext("Precipitation of Coldest Quarter", side=1, line=3, cex=1)
dev.off()

pdf("./Output_figures/3_clades/Run7/bldfie_avg.pdf")
plotPNO(x = pno_list$pno_bldfie, legend.pos = "topleft", xlab = "", wm = TRUE, tail_threshold = 0.005)
mtext("Bulk density of fine earth", side=1, line=3, cex=1)
dev.off()

pdf("./Output_figures/3_clades/Run7/canopy_avg.pdf")
jpeg("./Output_figures/3_clades/Run7/canopy_avg.jpg", res = 600, units = "in", width = 6, height = 6)
plotPNO(x = pno_list$pno_canopy, legend.pos = "topright", xlab = "", wm = TRUE, tail_threshold = 0.005)
mtext("Canopy", side=1, line=3, cex=1)
dev.off()

pdf("./Output_figures/3_clades/Run7/et_avg.pdf")
jpeg("./Output_figures/3_clades/Run7/et_avg.jpg", res = 600, units = "in", width = 6, height = 6)
plotPNO(x = pno_list$pno_et, legend.pos = "topleft", xlab = "", wm = TRUE, tail_threshold = 0.005)
mtext("Evapotranspiration", side=1, line=3, cex=1)
dev.off()

pdf("./Output_figures/3_clades/Run7/sndppt_avg.pdf")
plotPNO(x = pno_list$pno_sndppt, legend.pos = "topleft", xlab = "", wm = TRUE, tail_threshold = 0.005)
mtext("Sand Content (50-2000 um) Mass Fraction (%)", side=1, line=3, cex=1)
dev.off()






#Plots for poster

pdf("./Output_figures/Poster/bio2_5sp.pdf")
plotPNO(x = keep_pnos$pno_bio2, legend.pos = "topright", wm = TRUE, xlab = "Mean Diurnal Range")
dev.off()

pdf("./Output_figures/Poster/bio16_5sp.pdf")
plotPNO(x = keep_pnos$pno_bio16, legend.pos = "topright", wm = TRUE, xlab = "Precipitation of Wettest Quarter (mm)", tail_threshold = 0.01)
dev.off()

pdf("./Output_figures/Poster/sndppt_5sp.pdf")
plotPNO(x = keep_pnos$pno_sndppt, legend.pos = "topright", wm = TRUE, xlab = "Sand content (50-2000 um) mass fraction\n in % at depth 0.00 m")
dev.off()

pdf("./Output_figures/Poster/bio1_5sp.pdf")
plotPNO(x = keep_pnos$pno_bio1, legend.pos = "topleft", wm = TRUE, xlab = "Annual Mean Temperature (deg C*10)")
dev.off()

#Plot your pno of choice
plotPNO(x = pno$pno_bio1, 
        xlab = "Annual mean temperature", wm = TRUE, legend.pos = locator(c(1,1)), tail_threshold = 0.01)

plotPNO(x = pno$pno_bio1, 
        xlab = "Annual mean temperature", wm = TRUE, legend.pos = "topleft", tail_threshold = 0.01)

plotPNO(x = pno$pno_bio2, 
        xlab = "Mean diurnal range", wm = TRUE, legend.pos = "bottomleft")

plotPNO(x = pno$pno_bio4, 
        xlab = "Max temperature warmest month", wm = TRUE, legend.pos = "none")

plotPNO(x = pno$pno_bio16, 
        xlab = "Precipitation wettest quarter", wm = TRUE, legend.pos = "none", tail_threshold = 0.01)

plotPNO(x = pno$pno_bio17, 
        xlab = "Precipitation driest quarter", wm = TRUE, legend.pos = "none", tail_threshold = 0.08)

plotPNO(x = pno$pno_bio18, 
        xlab = "Precipitation warmest quarter", wm = TRUE, legend.pos = "none", tail_threshold = 0.015)

plotPNO(x = pno$pno_sndppt, 
        xlab = "Sand content %", wm = TRUE, legend.pos = "none")

plotPNO(x = pno$pno_crfvol, 
        xlab = "Coarse fragments %", wm = TRUE, legend.pos = "none", tail_threshold = 0.005)

plotPNO(x = pno$pno_canopy, 
        xlab = "Canopy height", wm = TRUE, legend.pos = "none", tail_threshold = 0.001)

plotPNO(x = pno$pno_bldfie, 
        xlab = "Bulk density fine earth kg/m3", wm = TRUE, legend.pos = "none", tail_threshold = 0.001)

plotPNO(x = pno$pno_bdticm, 
        xlab = "Absolute depth to bedrock cm", wm = TRUE, legend.pos = "none", tail_threshold = 0.05)


#Read models to view
aegopogon <- raster("./Models/Post_processing/Avg_models/aegopogon_Reduced_avg.asc")
melastomoides <- raster("./Models/Post_processing/Avg_models/melastomoides_Reduced_avg.asc")

plot(aegopogon)
plot(melastomoides)
