library(phyloclim)
library(ape)

#Making a PNO
pnobio1 <- pno(path_bioclim = "~/Desktop/BioClim_Modeling_Layers/ALL_THE_LAYERS/Presentbio_30s_bil/croppedbio1.asc", path_model = "~/Desktop/Present_Distributions/Ascii/", subset = NULL, bin_number = 50)
pnobio2 <- pno(path_bioclim = "~/Desktop/BioClim_Modeling_Layers/ALL_THE_LAYERS/Presentbio_30s_bil/croppedbio2.asc", path_model = "~/Desktop/Present_Distributions/Ascii/", subset = NULL, bin_number = 50)
pnobio5 <- pno(path_bioclim = "~/Desktop/BioClim_Modeling_Layers/ALL_THE_LAYERS/Presentbio_30s_bil/croppedbio5.asc", path_model = "~/Desktop/Present_Distributions/Ascii/", subset = NULL, bin_number = 50)
pnobio9 <- pno(path_bioclim = "~/Desktop/BioClim_Modeling_Layers/ALL_THE_LAYERS/Presentbio_30s_bil/croppedbio9.asc", path_model = "~/Desktop/Present_Distributions/Ascii/", subset = NULL, bin_number = 50)
pnobio12 <- pno(path_bioclim = "~/Desktop/BioClim_Modeling_Layers/ALL_THE_LAYERS/Presentbio_30s_bil/croppedbio12.asc", path_model = "~/Desktop/Present_Distributions/Ascii/", subset = NULL, bin_number = 50)
pnobio13 <- pno(path_bioclim = "~/Desktop/BioClim_Modeling_Layers/ALL_THE_LAYERS/Presentbio_30s_bil/croppedbio13.asc", path_model = "~/Desktop/Present_Distributions/Ascii/", subset = NULL, bin_number = 50)

#Making a list format that phyloclim likes because she's picky ONLY USE FOR PNO's ONLY
pno <- list(pnobio1 = pnobio1, pnobio2 = pnobio2, pnobio5 = pnobio5, pnobio9 = pnobio9, pnobio12 = pnobio12, pnobio13 = pnobio13)
pno

#Plot your pno of choice
plotPNO(x = pno$pnobio1, 
        xlab = "Precipitation of Wettest Month (mm)", wm = TRUE, legend.pos = "topright")


#Convert PNO to dataframe and change something about the name of the files for them to be understoof
pnobio13_test <- as.data.frame(pnobio13)
names(pnobio13_test)[2:20] <- sapply(names(pnobio13_test)[2:20], function(x) {
  res <- unlist(strsplit(x, "_"))
  paste(res[3:length(res)], collapse="_")
})
#Ancestral Niche Prediction
# load phylogeny and PNOs of Oxalis sect. Palmatifoliae
tree<-read.tree("epithet.tre")
plot(tree)
# estimate ancestral tolerances
ac <- anc.clim(target = tree, pno = pnobio13_test, n = 100, method = "ML")
# visualize results
plotAncClim(ac, ylab = "Precipitation of Wettest Month (mm)")



#This function can be used to test for phylogenetic signal in patterns of niche overlap (Warren et al., 2008) 
#based on the age-range correlation (ARC) as implemented by Turelli & Fitzpatrick (2006).

# load PNOs for Oxalis sect. Palmatifoliae ...
data(PNO)
# ... and calculate niche overlap between species
no <- niche.overlap(pno$pnobio1)
# load phylogeny and PNOs of Oxalis sect. Palmatifoliae
data(tree)
# age-range correlation
x <- age.range.correlation(phy = tree, overlap = no, n = 100)
# plot average niche overlap versus node age
plot(x$age.range.correlation)
# add a regression line
abline(x$linear.regression$coefficients)
# add regression lines from Monte Carlo randomization
apply(x$MonteCarlo.replicates, 1, abline, lwd = 0.2, col = "grey50")






# JUNK LAND BELOW ENTER AT YOUR OWN RISK


# 290 collections of Palmatifoliae
# --------------------------------
data(sites)
plot(sites$long, sites$lat,
     xlab = "Longitude", ylab = "Latitude")

## PNO profiles along 19 bioclimatic variables
## -------------------------------------------
data(PNO); names(PNO)
temp <- names(PNO)[1]
plotPNO(PNO[[temp]], xlab = temp)

## phylogenetic hypothesis for Palmatifoliae
## -----------------------------------------
data(tree)
plot(tree)



data(tree)
plot(tree)
data(PNO)
head(PNO)
# choose summer precipitation for analysis
clim <- PNO$PrecipitationWarmestQuarter
# estimate ancestral tolerances
ac <- anc.clim(target = tree, pno = clim, n = 100)
# visualize results
plotAncClim(ac, ylab = "Precipitation of warmest quarter (mm)")



pnobulk <- pno(path_bioclim = "~/Desktop/BioClim_Modeling_Layers/ALL_THE_LAYERS/Presentbio_30s_bil/croppedbulk.asc", path_model = "~/Desktop/Present_Distributions/Ascii/", subset = NULL, bin_number = 50)
pnocarbon <- pno(path_bioclim = "~/Desktop/BioClim_Modeling_Layers/ALL_THE_LAYERS/Presentbio_30s_bil/croppedcarbon.asc", path_model = "~/Desktop/Present_Distributions/Ascii/", subset = NULL, bin_number = 50)
pnoclay <- pno(path_bioclim = "~/Desktop/BioClim_Modeling_Layers/ALL_THE_LAYERS/Presentbio_30s_bil/croppedclay.asc", path_model = "~/Desktop/Present_Distributions/Ascii/", subset = NULL, bin_number = 50)
pnopH <- pno(path_bioclim = "~/Desktop/BioClim_Modeling_Layers/ALL_THE_LAYERS/Presentbio_30s_bil/croppedpH.asc", path_model = "~/Desktop/Present_Distributions/Ascii/", subset = NULL, bin_number = 50)
pnosand <- pno(path_bioclim = "~/Desktop/BioClim_Modeling_Layers/ALL_THE_LAYERS/Presentbio_30s_bil/croppedsand.asc", path_model = "~/Desktop/Present_Distributions/Ascii/", subset = NULL, bin_number = 50)
pnosilt <- pno(path_bioclim = "~/Desktop/BioClim_Modeling_Layers/ALL_THE_LAYERS/Presentbio_30s_bil/croppedsilt.asc", path_model = "~/Desktop/Present_Distributions/Ascii/", subset = NULL, bin_number = 50)

colnames(pnobio2)[2:20] <-c("coccineum", "brevifolia", "canescens", "etonia", "glabra", "grandiflora", "verticillata", "frutescens", 
                            "christmanii", "cornutissima", "densiflora", "immaculata_var_immaculata", "immaculata_var_savannarum", "linearifolia_robustior", "linearifolia_var_linearifolia", "modesta",
                            "odorantissima", "radfordiana", "thinicola");
