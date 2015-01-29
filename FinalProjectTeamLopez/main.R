cat('Vegetation change around fluxtowers from 2000-present')
cat('#TeamLopez™')
cat('® Peter Hooiveld & Bart Driessen')

rm(list=ls())
source('Functions/GetFluxtowerNames.R')
source('Functions/GetNDVIRasters.R')
source('Functions/GetNDVIQualityRasters.R')
source('Functions/TimeSeries.R')
source('Functions/BFASTperCell.R')
library("rgdal")
library("raster")
library("zoo") 
library("bfast")
library("gridExtra")
library("animation")

cat('Additional information about the functions can be derived from calling describe(function); e.g. describe(Get_NDVI_Rasters)\nThe most time-consuming functions are progress-documented.')

### Step 1: Downloading fluxtower data and converting it to raster stack.
fluxtowernames <- get_fluxtower_names("fluxtowersinfo")

### Fill in name of a fluxtower. The list of fluxtowers can be found here in the 
### third column ("fluxtowernames"):
View(fluxtowernames)
dataset <- "fn_nlloobos"

## Get NDVI and quality data in raster stack.
NDVI <- Get_NDVI_Rasters(dataset)
Quality <- Get_NDVI_Quality_Rasters(dataset)

### Step 2: Preform B-fast analysis for single cell in fluxtower raster dataset.
continue <- T
quartz(width=11)
startingdate = "X2000.02.18"
while(continue == T){
  par(mfrow=c(1,2))
  plot(NDVI[[startingdate]],main=startingdate)
  plot(BFASTpercell(NDVI$X2000.02.18),type="trend")
  answer <- readline("Do you want to analyse another pixel? yes / no: ")
  if(answer == 'y' | answer == 'yes'){
    continue <- T
  } else if(answer == 'n' | answer == 'no'){
    continue <- F
  } else{
    print('This is not a valid answer. Please answer with yes or no.')
    break
  }
}
dev.off()

## Step 3: Create animated gif that shows NDVI change during the years
saveGIF( {
  for( ii in 1:length(NDVI[1])) {
    plot(NDVI[[ii]],main= paste(names(NDVI[[ii]])),breaks=seq(0,10000,by=1000),col=rev(terrain.colors(11)))    
  }
}, movie.name = paste('OutputFiles/NDVI',dataset,'.gif',sep=""), ani.height=500, ani.width=500,interval=0.1)
