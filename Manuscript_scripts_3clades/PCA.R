#Humboldt PCAs from Brown and Carnaval paper
library(devtools)
#install_github("jasonleebrown/humboldt")
library(humboldt)
library(raster)
library(rgdal); #For reading the M polygon
library(sf)
library(parallel)

##for environment data
data(env1)

##for species data
data(sp1)


#read cropped raster files
north_layers <- list.files(path = "./Layers/3_clades/North/All", pattern = ".asc", full.names = TRUE); #Gets a list of .asc files
north_stack <- stack(north_layers)
crs(north_stack) <- "+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0" #Defines projection of layers

south_layers <- list.files(path = "./Layers/3_clades/South/All", pattern = ".asc", full.names = TRUE); #Gets a list of .asc files
south_stack <- stack(south_layers)
crs(south_stack) <- "+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0" #Defines projection of layers

wide_layers <- list.files(path = "./Layers/3_clades/Wide/All", pattern = ".asc", full.names = TRUE); #Gets a list of .asc files
wide_stack <- stack(wide_layers)
crs(wide_stack) <- "+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0" #Defines projection of layers

##convert one of the rasters to a point dataframe to sample.  Use any raster input.
env.points.N<-rasterToPoints(north_stack[[1]], fun=NULL, spatial=FALSE)
env.points.S<-rasterToPoints(south_stack[[1]], fun=NULL, spatial=FALSE)
env.points.W<-rasterToPoints(wide_stack[[1]], fun=NULL, spatial=FALSE)

##rarefy points to appropriate analysis resolution.  Note it is best to start with climate data that is similar to the desired resolution.  Else this process can take a lot of time.  If climate is exactly the resolution desired (we recommend 10-40km2 for most studies), this step can be skipped.
#skipped this for time - resolution already reduced
env.sampling.res.N <-humboldt.occ.rarefy(env.points.N, colxy = 1:2, rarefy.dist = 40,  rarefy.units = "km", run.silent.rar = F)
env.sampling.res.S <-humboldt.occ.rarefy(env.points.S, colxy = 1:2, rarefy.dist = 40,  rarefy.units = "km", run.silent.rar = F)
env.sampling.res.W <-humboldt.occ.rarefy(env.points.W, colxy = 1:2, rarefy.dist = 40,  rarefy.units = "km", run.silent.rar = F)

##subset only the x and y data
#env.sampling.res<- env.sampling.res[,1:2]

env.sampling.res.N_2<- env.sampling.res.N[,1:2]
env.sampling.res.S_2<- env.sampling.res.S[,1:2]
env.sampling.res.W_2<- env.sampling.res.W[,1:2]

##Extract values to points from rasters
RAST_VAL.N<-data.frame(extract(north_stack, env.sampling.res.N_2))
RAST_VAL.S<-data.frame(extract(south_stack, env.sampling.res.S_2))
RAST_VAL.W<-data.frame(extract(wide_stack, env.sampling.res.W_2))


##merge sampled data to input
Env.N<-cbind(env.sampling.res.N_2,RAST_VAL.N)
Env.S<-cbind(env.sampling.res.S_2,RAST_VAL.S)
Env.W<-cbind(env.sampling.res.W,RAST_VAL.W)

##save the file as '.csv' for future analyses 
write.csv(Env.N, file = "./Layers/3_clades/PCA_layers/EnvN.csv")
write.csv(Env.S, file = "./Layers/3_clades/PCA_layers/EnvS.csv")
write.csv(Env.W, file = "./Layers/3_clades/PCA_layers/EnvW.csv")


##load environmental variables for all sites of the study area 1 (env1). Column names should be x,y,X1,X2,...,Xn)
##in this example all input datasets are tab-delimited text files, if using '.csv' files change the parameters below for import steps from 'sep="\t"' to 'sep=","' 
Env.N<-read.delim("./Layers/3_clades/PCA_layers/EnvN.csv",h=T,sep=",", row.names = 1)
Env.S<-read.delim("./Layers/3_clades/PCA_layers/EnvS.csv",h=T,sep=",", row.names = 1)
Env.W<-read.delim("./Layers/3_clades/PCA_layers/EnvW.csv",h=T,sep=",", row.names = 1)

#rename variables to match
colnames(Env.N) <- gsub("North_", "", colnames(Env.N))
colnames(Env.S) <- gsub("South_", "", colnames(Env.S))
colnames(Env.W) <- gsub("Wide_", "", colnames(Env.W))
colnames(Env.S)
str(Env.N)
str(Env.S)
str(Env.W)

#it is important for there not to be an extra column in the layer dataframes 
#Env.W <- Env.W[,-1]

## load environmental variables for all sites of the study area 2 (env2). Column names should be x,y,X1,X2,...,Xn)
#env2<-read.delim("env2.txt",h=T,sep="\t") 

## remove NAs and make sure all variables are imported as numbers
Env.N<-humboldt.scrub.env(Env.N)
Env.S<-humboldt.scrub.env(Env.S)
Env.W<-humboldt.scrub.env(Env.W)



##load occurrence sites for the species at study area 1 (env1). Column names should be sp,x,y
spN<-na.exclude(read.delim("./Data/3_clades/North_clade_cleaned_occurrences.csv",h=T,sep=","))
spS<-na.exclude(read.delim("./Data/3_clades/South_clade_cleaned_occurrences.csv",h=T,sep=","))
spW<-na.exclude(read.delim("./Data/3_clades/Wide_clade_cleaned_occurrences.csv",h=T,sep=","))


colnames(spN) <- c("sp", "y", "x")
colnames(spS) <- c("sp", "y", "x")
colnames(spW) <- c("sp", "y", "x")

spN_2 <- spN[,c(1,3,2)]
spS_2 <- spS[,c(1,3,2)]
spW_2 <- spW[,c(1,3,2)]


##load occurrence sites for the species at study area 2 (env2). Column names should be sp,x,y 
#sp2<-na.exclude(read.delim("sp2.txt",h=T,sep="\t"))

##its highly recommended that you using the function "humboldt.top.env" to select only the important environmental variables in humboldt.doitall. 
##This step can be skipped. If you downloaded tons of environmental data, you should use this step.  If you skip this step, input env1/env2 in place of reduc.vars$env1/reduc.vars$env2 

#first run used rarefy.dist = 50 (for all pdfs with no number specified in name) - use lower resolution which matches rarefy distance of 40
reduc.vars_NW<- humboldt.top.env(env1=Env.N,env2=Env.W,sp1=spN_2,sp2=spW_2,rarefy.dist=40, rarefy.units="km", env.reso=0.416669,learning.rt1=0.01,learning.rt2=0.01,e.var=(3:21),pa.ratio=4,steps1=50,steps2=50,method="contrib",contrib.greater=5)
reduc.vars_SW<- humboldt.top.env(env1=Env.S,env2=Env.W,sp1=spS_2,sp2=spW_2,rarefy.dist=40, rarefy.units="km", env.reso=0.416669,learning.rt1=0.01,learning.rt2=0.01,e.var=(3:21),pa.ratio=4,steps1=50,steps2=50,method="contrib",contrib.greater=5)
reduc.vars_NS<- humboldt.top.env(env1=Env.N,env2=Env.S,sp1=spN_2,sp2=spS_2,rarefy.dist=40, rarefy.units="km", env.reso=0.416669,learning.rt1=0.01,learning.rt2=0.01,e.var=(3:21),pa.ratio=4,steps1=50,steps2=50,method="contrib",contrib.greater=5)

reduc.vars_NW<- humboldt.top.env(env1=Env.N,env2=Env.W,sp1=spN_2,sp2=spW_2,rarefy.dist=40, rarefy.units="km", env.reso=0.360360,learning.rt1=0.01,learning.rt2=0.01,e.var=(3:21),pa.ratio=4,steps1=50,steps2=50,method="contrib",contrib.greater=5)
reduc.vars_SW<- humboldt.top.env(env1=Env.S,env2=Env.W,sp1=spS_2,sp2=spW_2,rarefy.dist=40, rarefy.units="km", env.reso=0.360360,learning.rt1=0.01,learning.rt2=0.01,e.var=(3:21),pa.ratio=4,steps1=50,steps2=50,method="contrib",contrib.greater=5)
reduc.vars_NS<- humboldt.top.env(env1=Env.N,env2=Env.S,sp1=spN_2,sp2=spS_2,rarefy.dist=40, rarefy.units="km", env.reso=0.360360,learning.rt1=0.01,learning.rt2=0.01,e.var=(3:21),pa.ratio=4,steps1=50,steps2=50,method="contrib",contrib.greater=5)

##Adjust the number of variables input for e.vars after reduction to only important variables
#num.var.e<-ncol(reduc.vars$env1)
num.var.e<-ncol(Env.N)
num.var.e2_reducNW <-ncol(reduc.vars_NW$env1)
num.var.e2_reducSW <-ncol(reduc.vars_SW$env1)
num.var.e2_reducNS <-ncol(reduc.vars_NS$env1)



#get resolution of the input layers
#resolution <- res(north_stack[[1]])


##run it first with full environmental for background tests and equivalence statistic (total equivalence or divergence in current distributions)
#for testing scripts
#full<-doitall2(inname="./Output_figures/3_clades/PCA/SvW_full_extent", env1=Env.S, env2=Env.W, sp1=spS_2, sp2=spW_2, rarefy.dist = 50, rarefy.units = "km", env.reso=0.416669, non.analogous.environments="YES", correct.env=F, env.trim=F,  pcx=1, pcy=2, col.env=e.var, e.var=c(3:num.var.e), R=100, kern.smooth=2, e.reps=100, b.reps=100, nae="YES",thresh.espace.z=0.001, p.overlap=T, p.boxplot=T, p.scatter=T, run.silent=F, ncores=1) #do not reduce envt ##reduce both envt to each other

#original line
#full<-humboldt.doitall(inname="./Output_figures/3_clades/PCA/testing_full_extent", env1=Env.N, env2=Env.S, sp1=spN_2, sp2=spS_2, rarefy.dist=50, rarefy.units="km", env.reso=0.416669, reduce.env=0, reductype="PCA", non.analogous.environments="YES", correct.env=T, env.trim=T,  env.trim.type="RADIUS", trim.buffer.sp1=500, trim.buffer.sp2=500, pcx=1, pcy=2, col.env=e.var, e.var=c(3:num.var.e), R=100, kern.smooth=1, e.reps=100, b.reps=100, nae="YES",thresh.espace.z=0.001, p.overlap=T, p.boxplot=F, p.scatter=F, run.silent=F, ncores=2)


#with all variables - full environmental (not trimmed)
full<-humboldt.doitall(inname="./Output_figures/3_clades/PCA/NvS_full_extent", env1=Env.N, env2=Env.S, sp1=spN_2, sp2=spS_2, rarefy.dist = 50, rarefy.units = "km", env.reso=0.416669, reduce.env=0, reductype="PCA", non.analogous.environments="YES", correct.env=T, env.trim=F,  pcx=1, pcy=2, col.env=e.var, e.var=c(3:num.var.e), R=100, kern.smooth=1, e.reps=100, b.reps=100, nae="YES",thresh.espace.z=0.001, p.overlap=T, p.boxplot=T, p.scatter=T, run.silent=F, ncores=2) #do not reduce envt ##reduce both envt to each other: reduce.env=0, reductype="PCA", 
full<-humboldt.doitall(inname="./Output_figures/3_clades/PCA/NvW_full_extent", env1=Env.N, env2=Env.W, sp1=spN_2, sp2=spW_2, rarefy.dist = 50, rarefy.units = "km", env.reso=0.416669, reduce.env=0, reductype="PCA", non.analogous.environments="YES", correct.env=T, env.trim=F,  pcx=1, pcy=2, col.env=e.var, e.var=c(3:num.var.e), R=100, kern.smooth=2, e.reps=100, b.reps=100, nae="YES",thresh.espace.z=0.001, p.overlap=T, p.boxplot=T, p.scatter=T, run.silent=F, ncores=2) #do not reduce envt ##reduce both envt to each other
full<-humboldt.doitall(inname="./Output_figures/3_clades/PCA/SvW_full_extent", env1=Env.S, env2=Env.W, sp1=spS_2, sp2=spW_2, rarefy.dist = 50, rarefy.units = "km", env.reso=0.416669, reduce.env=0, reductype="PCA", non.analogous.environments="YES", correct.env=T, env.trim=F,  pcx=1, pcy=2, col.env=e.var, e.var=c(3:num.var.e), R=100, kern.smooth=2, e.reps=100, b.reps=100, nae="YES",thresh.espace.z=0.001, p.overlap=T, p.boxplot=T, p.scatter=T, run.silent=F, ncores=2) #do not reduce envt ##reduce both envt to each other


#with reduced variables - full environmental (not trimmed)
full<-humboldt.doitall(inname="./Output_figures/3_clades/PCA/NvW_reduced_full_extent", env1=reduc.vars_NW$env1, env2=reduc.vars_NW$env2, sp1=spN_2, sp2=spW_2, rarefy.dist = 50, rarefy.units = "km", env.reso=0.416669, reduce.env=0, reductype="PCA", non.analogous.environments="YES", correct.env=T, env.trim=F,  pcx=1, pcy=2, col.env=e.var, e.var=c(3:num.var.e2_reducNW), R=100, kern.smooth=1, e.reps=100, b.reps=100, nae="YES",thresh.espace.z=0.001, p.overlap=T, p.boxplot=T, p.scatter=T, run.silent=F, ncores=2) #do not reduce envt ##reduce both envt to each other: reduce.env=0, reductype="PCA", 
full<-humboldt.doitall(inname="./Output_figures/3_clades/PCA/NvS_reduced_full_extent", env1=reduc.vars_NS$env1, env2=reduc.vars_NS$env2, sp1=spN_2, sp2=spS_2, rarefy.dist = 50, rarefy.units = "km", env.reso=0.416669, reduce.env=0, reductype="PCA", non.analogous.environments="YES", correct.env=T, env.trim=F,  pcx=1, pcy=2, col.env=e.var, e.var=c(3:num.var.e2_reducNS), R=100, kern.smooth=2, e.reps=100, b.reps=100, nae="YES",thresh.espace.z=0.001, p.overlap=T, p.boxplot=T, p.scatter=T, run.silent=F, ncores=2) #do not reduce envt ##reduce both envt to each other
full<-humboldt.doitall(inname="./Output_figures/3_clades/PCA/SvW_reduced_full_extent", env1=reduc.vars_SW$env1, env2=reduc.vars_SW$env2, sp1=spS_2, sp2=spW_2, rarefy.dist = 50, rarefy.units = "km", env.reso=0.416669, reduce.env=0, reductype="PCA", non.analogous.environments="YES", correct.env=T, env.trim=F,  pcx=1, pcy=2, col.env=e.var, e.var=c(3:num.var.e2_reducSW), R=100, kern.smooth=2, e.reps=100, b.reps=100, nae="YES",thresh.espace.z=0.001, p.overlap=T, p.boxplot=T, p.scatter=T, run.silent=F, ncores=2) #do not reduce envt ##reduce both envt to each other

#with reduced variables - full environmental (not trimmed) - using rarefy.dist of 40
sink(file ="./Output_figures/3_clades/PCA/full_NW_screen_output.txt")
full_NW<-humboldt.doitall(inname="./Output_figures/3_clades/PCA/NvW_reduced_40_full_extent", env1=reduc.vars_NW$env1, env2=reduc.vars_NW$env2, sp1=spN_2, sp2=spW_2, rarefy.dist = 40, rarefy.units = "km", env.reso=0.360360, reduce.env=0, reductype="PCA", non.analogous.environments="YES", correct.env=T, env.trim=F,  pcx=1, pcy=2, col.env=e.var, e.var=c(3:num.var.e2_reducNW), R=100, kern.smooth=1, e.reps=100, b.reps=100, nae="YES",thresh.espace.z=0.001, p.overlap=T, p.boxplot=T, p.scatter=T, run.silent=F, ncores=2) #do not reduce envt ##reduce both envt to each other: reduce.env=0, reductype="PCA", 
sink()
sink(file ="./Output_figures/3_clades/PCA/full_NS_screen_output.txt")
full_NS<-humboldt.doitall(inname="./Output_figures/3_clades/PCA/NvS_reduced_40_full_extent", env1=reduc.vars_NS$env1, env2=reduc.vars_NS$env2, sp1=spN_2, sp2=spS_2, rarefy.dist = 40, rarefy.units = "km", env.reso=0.360360, reduce.env=0, reductype="PCA", non.analogous.environments="YES", correct.env=T, env.trim=F,  pcx=1, pcy=2, col.env=e.var, e.var=c(3:num.var.e2_reducNS), R=100, kern.smooth=2, e.reps=100, b.reps=100, nae="YES",thresh.espace.z=0.001, p.overlap=T, p.boxplot=T, p.scatter=T, run.silent=F, ncores=2) #do not reduce envt ##reduce both envt to each other
sink()
sink(file ="./Output_figures/3_clades/PCA/full_SW_screen_output.txt")
full_SW<-humboldt.doitall(inname="./Output_figures/3_clades/PCA/SvW_reduced_40_full_extent", env1=reduc.vars_SW$env1, env2=reduc.vars_SW$env2, sp1=spS_2, sp2=spW_2, rarefy.dist = 40, rarefy.units = "km", env.reso=0.360360, reduce.env=0, reductype="PCA", non.analogous.environments="YES", correct.env=T, env.trim=F,  pcx=1, pcy=2, col.env=e.var, e.var=c(3:num.var.e2_reducSW), R=100, kern.smooth=2, e.reps=100, b.reps=100, nae="YES",thresh.espace.z=0.001, p.overlap=T, p.boxplot=T, p.scatter=T, run.silent=F, ncores=2) #do not reduce envt ##reduce both envt to each other
sink()
#ncol(Env.W)
#ncol(Env.S)


##run it a second time with a trimmed, shared-espace. Here the equivalence statistic tests for niche evolution or niche divergence. For comparing results, change only the following model parameters: reduce.env, non.analogous.environmental, env.trim, nae
shared_ae<-humboldt.doitall(inname="./Output_figures/3_clades/PCA/NvW_shared_espace_ae", env1=Env.N, env2=Env.W, sp1=spN_2, sp2=spW_2, rarefy.dist=50, rarefy.units="km", env.reso=0.416669, reduce.env=2, reductype="PCA", non.analogous.environments="NO", correct.env=T, env.trim=T, env.trim.type="RADIUS", trim.buffer.sp1=500, trim.buffer.sp2=500, pcx=1,pcy=2, col.env=e.var, e.var=c(3:num.var.e), R=100, kern.smooth=1, e.reps=100, b.reps=100, nae="NO",thresh.espace.z=0.001, p.overlap=T, p.boxplot=T, p.scatter=T,run.silent=F, ncores=2)
shared_ae<-humboldt.doitall(inname="./Output_figures/3_clades/PCA/NvS_shared_espace_ae", env1=Env.N, env2=Env.S, sp1=spN_2, sp2=spS_2, rarefy.dist=50, rarefy.units="km", env.reso=0.416669, reduce.env=2, reductype="PCA", non.analogous.environments="NO", correct.env=T, env.trim=T, env.trim.type="RADIUS", trim.buffer.sp1=500, trim.buffer.sp2=500, pcx=1,pcy=2, col.env=e.var, e.var=c(3:num.var.e), R=100, kern.smooth=1, e.reps=100, b.reps=100, nae="NO",thresh.espace.z=0.001, p.overlap=T, p.boxplot=T, p.scatter=T,run.silent=F, ncores=2)
shared_ae<-humboldt.doitall(inname="./Output_figures/3_clades/PCA/SvW_shared_espace_ae", env1=Env.S, env2=Env.W, sp1=spS_2, sp2=spW_2, rarefy.dist=50, rarefy.units="km", env.reso=0.416669, reduce.env=2, reductype="PCA", non.analogous.environments="NO", correct.env=T, env.trim=T, env.trim.type="RADIUS", trim.buffer.sp1=500, trim.buffer.sp2=500, pcx=1,pcy=2, col.env=e.var, e.var=c(3:num.var.e), R=100, kern.smooth=1, e.reps=100, b.reps=100, nae="NO",thresh.espace.z=0.001, p.overlap=T, p.boxplot=T, p.scatter=T,run.silent=F, ncores=2)

shared_ae<-humboldt.doitall(inname="./Output_figures/3_clades/PCA/NvW_reduced_shared_espace_ae", env1=reduc.vars_NW$env1, env2=reduc.vars_NW$env2, sp1=spN_2, sp2=spW_2, rarefy.dist=50, rarefy.units="km", env.reso=0.416669, reduce.env=2, reductype="PCA", non.analogous.environments="NO", correct.env=T, env.trim=T, env.trim.type="RADIUS", trim.buffer.sp1=500, trim.buffer.sp2=500, pcx=1,pcy=2, col.env=e.var, e.var=c(3:num.var.e2_reducNW), R=100, kern.smooth=1, e.reps=100, b.reps=100, nae="NO",thresh.espace.z=0.001, p.overlap=T, p.boxplot=T, p.scatter=T,run.silent=F, ncores=2)
shared_ae<-humboldt.doitall(inname="./Output_figures/3_clades/PCA/NvS_reduced_shared_espace_ae", env1=reduc.vars_NS$env1, env2=reduc.vars_NS$env2, sp1=spN_2, sp2=spS_2, rarefy.dist=50, rarefy.units="km", env.reso=0.416669, reduce.env=2, reductype="PCA", non.analogous.environments="NO", correct.env=T, env.trim=T, env.trim.type="RADIUS", trim.buffer.sp1=500, trim.buffer.sp2=500, pcx=1,pcy=2, col.env=e.var, e.var=c(3:num.var.e2_reducNS), R=100, kern.smooth=1, e.reps=100, b.reps=100, nae="NO",thresh.espace.z=0.001, p.overlap=T, p.boxplot=T, p.scatter=T,run.silent=F, ncores=2)
shared_ae<-humboldt.doitall(inname="./Output_figures/3_clades/PCA/SvW_reduced_shared_espace_ae", env1=reduc.vars_SW$env1, env2=reduc.vars_SW$env2, sp1=spS_2, sp2=spW_2, rarefy.dist=50, rarefy.units="km", env.reso=0.416669, reduce.env=2, reductype="PCA", non.analogous.environments="NO", correct.env=T, env.trim=T, env.trim.type="RADIUS", trim.buffer.sp1=500, trim.buffer.sp2=500, pcx=1,pcy=2, col.env=e.var, e.var=c(3:num.var.e2_reducSW), R=100, kern.smooth=1, e.reps=100, b.reps=100, nae="NO",thresh.espace.z=0.001, p.overlap=T, p.boxplot=T, p.scatter=T,run.silent=F, ncores=2)

#use rarefy.dist of 40
sink(file ="./Output_figures/3_clades/PCA/shared_NW_screen_output.txt")
shared_ae_NW<-humboldt.doitall(inname="./Output_figures/3_clades/PCA/NvW_reduced_40_shared_espace_ae", env1=reduc.vars_NW$env1, env2=reduc.vars_NW$env2, sp1=spN_2, sp2=spW_2, rarefy.dist=40, rarefy.units="km", env.reso=0.360360, reduce.env=2, reductype="PCA", non.analogous.environments="NO", correct.env=T, env.trim=T, env.trim.type="RADIUS", trim.buffer.sp1=500, trim.buffer.sp2=500, pcx=1,pcy=2, col.env=e.var, e.var=c(3:num.var.e2_reducNW), R=100, kern.smooth=1, e.reps=100, b.reps=100, nae="NO",thresh.espace.z=0.001, p.overlap=T, p.boxplot=T, p.scatter=T,run.silent=F, ncores=2)
sink()

sink(file ="./Output_figures/3_clades/PCA/shared_NS_screen_output.txt")
shared_ae_NS<-humboldt.doitall(inname="./Output_figures/3_clades/PCA/NvS_reduced_40_shared_espace_ae", env1=reduc.vars_NS$env1, env2=reduc.vars_NS$env2, sp1=spN_2, sp2=spS_2, rarefy.dist=40, rarefy.units="km", env.reso=0.360360, reduce.env=2, reductype="PCA", non.analogous.environments="NO", correct.env=T, env.trim=T, env.trim.type="RADIUS", trim.buffer.sp1=500, trim.buffer.sp2=500, pcx=1,pcy=2, col.env=e.var, e.var=c(3:num.var.e2_reducNS), R=100, kern.smooth=1, e.reps=100, b.reps=100, nae="NO",thresh.espace.z=0.001, p.overlap=T, p.boxplot=T, p.scatter=T,run.silent=F, ncores=2)
sink()


sink(file ="./Output_figures/3_clades/PCA/shared_SW_screen_output.txt")
shared_ae_SW<-humboldt.doitall(inname="./Output_figures/3_clades/PCA/SvW_reduced_40_shared_espace_ae", env1=reduc.vars_SW$env1, env2=reduc.vars_SW$env2, sp1=spS_2, sp2=spW_2, rarefy.dist=40, rarefy.units="km", env.reso=0.360360, reduce.env=2, reductype="PCA", non.analogous.environments="NO", correct.env=T, env.trim=T, env.trim.type="RADIUS", trim.buffer.sp1=500, trim.buffer.sp2=500, pcx=1,pcy=2, col.env=e.var, e.var=c(3:num.var.e2_reducSW), R=100, kern.smooth=1, e.reps=100, b.reps=100, nae="NO",thresh.espace.z=0.001, p.overlap=T, p.boxplot=T, p.scatter=T,run.silent=F, ncores=2)
sink()

#how to save output to file
sink(file ="./Output_figures/3_clades/PCA/screen_output.txt")
sink()

#get loadings for PCAs
#shared_ae_NW<-humboldt.doitall(inname="./Output_figures/3_clades/PCA/NvW_reduced_40_shared_espace_ae", env1=reduc.vars_NW$env1, env2=reduc.vars_NW$env2, sp1=spN_2, sp2=spW_2, rarefy.dist=40, rarefy.units="km", env.reso=0.360360, reduce.env=2, reductype="PCA", non.analogous.environments="NO", correct.env=T, env.trim=T, env.trim.type="RADIUS", trim.buffer.sp1=500, trim.buffer.sp2=500, pcx=1,pcy=2, col.env=e.var, e.var=c(3:num.var.e2_reducNW), R=100, kern.smooth=1, e.reps=100, b.reps=100, nae="NO",thresh.espace.z=0.001, p.overlap=T, p.boxplot=T, p.scatter=T,run.silent=F, ncores=2)
#shared_ae_NS<-humboldt.doitall(inname="./Output_figures/3_clades/PCA/NvS_reduced_40_shared_espace_ae", env1=reduc.vars_NS$env1, env2=reduc.vars_NS$env2, sp1=spN_2, sp2=spS_2, rarefy.dist=40, rarefy.units="km", env.reso=0.360360, reduce.env=2, reductype="PCA", non.analogous.environments="NO", correct.env=T, env.trim=T, env.trim.type="RADIUS", trim.buffer.sp1=500, trim.buffer.sp2=500, pcx=1,pcy=2, col.env=e.var, e.var=c(3:num.var.e2_reducNS), R=100, kern.smooth=1, e.reps=100, b.reps=100, nae="NO",thresh.espace.z=0.001, p.overlap=T, p.boxplot=T, p.scatter=T,run.silent=F, ncores=2)
#shared_ae_SW<-humboldt.doitall(inname="./Output_figures/3_clades/PCA/SvW_reduced_40_shared_espace_ae", env1=reduc.vars_SW$env1, env2=reduc.vars_SW$env2, sp1=spS_2, sp2=spW_2, rarefy.dist=40, rarefy.units="km", env.reso=0.360360, reduce.env=2, reductype="PCA", non.analogous.environments="NO", correct.env=T, env.trim=T, env.trim.type="RADIUS", trim.buffer.sp1=500, trim.buffer.sp2=500, pcx=1,pcy=2, col.env=e.var, e.var=c(3:num.var.e2_reducSW), R=100, kern.smooth=1, e.reps=100, b.reps=100, nae="NO",thresh.espace.z=0.001, p.overlap=T, p.boxplot=T, p.scatter=T,run.silent=F, ncores=2)

zz_NW <-humboldt.g2e(env1=reduc.vars_NW$env1, env2=reduc.vars_NW$env2, sp1=spN_2, sp2=spW_2, reduce.env = 2, reductype = "PCA", non.analogous.environments = "NO", env.trim= T, e.var=c(3:num.var.e2_reducNW),  col.env = e.var, trim.buffer.sp1 = 500, trim.buffer.sp2 = 500, rarefy.dist = 40, rarefy.units="km", env.reso=0.360360, kern.smooth = 1, R = 100, run.silent = F)
zz_NS <-humboldt.g2e(env1=reduc.vars_NS$env1, env2=reduc.vars_NS$env2, sp1=spN_2, sp2=spS_2, reduce.env = 2, reductype = "PCA", non.analogous.environments = "NO", env.trim= T, e.var=c(3:num.var.e2_reducNW),  col.env = e.var, trim.buffer.sp1 = 500, trim.buffer.sp2 = 500, rarefy.dist = 40, rarefy.units="km", env.reso=0.360360, kern.smooth = 1, R = 100, run.silent = F)
zz_SW <-humboldt.g2e(env1=reduc.vars_SW$env1, env2=reduc.vars_SW$env2, sp1=spS_2, sp2=spW_2, reduce.env = 2, reductype = "PCA", non.analogous.environments = "NO", env.trim= T, e.var=c(3:num.var.e2_reducNW),  col.env = e.var, trim.buffer.sp1 = 500, trim.buffer.sp2 = 500, rarefy.dist = 40, rarefy.units="km", env.reso=0.360360, kern.smooth = 1, R = 100, run.silent = F)

#Variables contributing to PCAs 1 and 2 with loadings???
write.csv(zz_NW$pca.cal$co, "./Output_figures/3_clades/PCA/NW_loadings.csv", row.names = T)
write.csv(zz_NS$pca.cal$co, "./Output_figures/3_clades/PCA/NS_loadings.csv", row.names = T)
write.csv(zz_SW$pca.cal$co, "./Output_figures/3_clades/PCA/SW_loadings.csv", row.names = T)


#test plot
humboldt.plot.contrib(zz_NW$pca.cal$co,zz_NW$pca.cal$eig)



