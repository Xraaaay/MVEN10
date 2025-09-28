library(SDMtune)
library(dismo)
library(terra)
library(geodata)
library(viridis)
library(sf)
library(ggplot2)

# ========== Prepare data for model ==========
# 1. Get a shapefile of Sweden
ext = c(11.0273686052, 24, 23.9033785336, 69.06) # xmin, xmax, ymin, ymax
plot(ext(ext))

bd  = gadm(country='Sweden',level=1,path = "./data",download = T)
bd_main = bd # saving in a new variable to use later

bd  = crop(bd,extent(ext))

# visualization
par(mfrow=c(1,2))
plot(bd_main,axes=T, main="Sweden län")
plot(bd,axes=T, main="Cropped shapefile")

# 2. Raster/Image data: Climate data
bio19 = geodata::worldclim_country(
  country='Sweden',
  var='bio',
  res=0.5,
  path = "./data",
  download=T
)

# crop the data to the extent of the study area
bio19 = crop(bio19,extent(ext)) 

# HOW TO PLOT RASTER AND SHAPEFILE
plot(bio19[[1]],main="Annual mean temperature",col=map.pal("viridis", 100))
plot(bd,add=T)

# first four temperature layers
plot(bio19[[1:4]],col=map.pal("viridis", 100))

# first four rainfall layers
plot(bio19[[12:16]],col=map.pal("viridis", 100))

# saving the results
pdf("bio1-4.pdf",width = 6,height = 4)
plot(bio19[[1:4]],col=map.pal("viridis", 100))
dev.off()

pdf("bio12-16.pdf",width = 6,height = 4)
plot(bio19[[12:16]],col=map.pal("viridis", 100))
dev.off()

# ========== Prepare data for Maxent ==========
# 1. Occurance data
# load species occurrence data
occ = read.csv("./data/berghumla.csv",sep=";")

# these functions convert the data to spatial data frame
occ = st_as_sf(occ,coords=c("Ost","Nord"),crs=3021) 
# and then transform the data to the appropriate projection (WGS84)
occ = st_transform(occ,4326)

# extract just the coordinates of each occurrence record
occ_coordinates = st_coordinates(occ)

# 2. Background data
bg = predicts::backgroundSample(n=1000, bio19[[1]])

# 3. Prepare data SWD object
data <- SDMtune::prepareSWD(
  species = "berghumla", # species name
  p = occ_coordinates, # coordinates
  a = bg, # back ground points
  env = bio19, # environmental data
)

# ========== Train a simple Maxent model ==========
mx1 = train(
  method = "Maxnet",  # the name of the model
  fc="lq",            # feature classes: linear, and quadratic
  reg=0.5,            # regularization parameters
  verbose = T,        # show message during training
  data = data,        # the data variable
  iter = 1000         # number of iterations the logarithm will run
)

# plot the response curve
plotResponse(mx1, var = c("wc2.1_30s_bio_1"), type = "logistic") # mean annual temperature
plotResponse(mx1, var = c("wc2.1_30s_bio_12"), type = "logistic") # mean annual precipitation

# Then we plot the ROC curve for our model which shows the training AUC
auc(mx1)        # just to get the model AUC
plotROC(mx1)    # to plot the AUC curve

map = predict(mx1, data=bio19, type="logistic")

# ========== Visualize results ==========
dev.off() # clear the plotting window first

# saving the results
pdf("predictions.pdf",width = 4,height = 6)
plotPred(map, 
         lt = "Climate\nsuitability",
         colorramp = c("#2c7bb6", "#abd9e9", "#ffffbf", "#fdae61", "#d7191c"),hr=T)+theme(aspect.ratio=2)
dev.off()


















