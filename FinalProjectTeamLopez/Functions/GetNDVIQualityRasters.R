# Install packages.
library("fields")
library("raster")
library("rgdal")
source('Functions/ReadingCoordsFromHTML.R')

### Define function name to download and read quality rasters:
Get_NDVI_Quality_Rasters <- function(fluxtower){
  input_txt_file <- paste("ftp://daac.ornl.gov/data/modis_ascii_subsets/C5_MOD13Q1/data/MOD13Q1.",
                          paste(fluxtower,".txt",sep = ""), sep = "")
  download.file(input_txt_file, paste('Data/',fluxtower,".txt",sep = ""))
  cat('Progress: 40%. MODIS data downloaded.\n')
  
  coordinate_matr <- html_to_coordlist(fluxtower)
  cat('Progress: 50%. Coordinate data downloaded.\n')
  
  data <- read.csv(paste("Data/",fluxtower,".txt",sep=""))
  cat('Progress: 60%. MODIS data read.\n')
  
  WGS84 <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"
  sinusoidal <- "+proj=sinu +lon_0=0 +x_0=0 +y_0=0 +a=6371007.181 +b=6371007.181 +units=m +no_defs"
  spatialpoints <- SpatialPoints(coordinate_matr)
  proj4string(spatialpoints) <- CRS(WGS84)
  Extent <- spTransform(spatialpoints, CRS(sinusoidal))
  cat('Progress: 70%. Extent of MODIS data reprojected. Starting for loop...\n')
  
  Datelist <- as.numeric()
  Qalist <- c()
  for(i in 1:as.integer((length(data[,1])-7)/12)){
    n = (i-1)*12+7
    Qa <- data[n,c(7:790)]
    Qa <- as.matrix(t(matrix(data=as.numeric(Qa), ncol=28,nrow=28)))
    Qa <- raster(Qa,extent(Extent)[1],extent(Extent)[2],extent(Extent)[3],extent(Extent)[4],CRS(sinusoidal))
    Qalist <- c(Qalist, Qa)
    Datelist <- append(Datelist, as.Date(strptime(paste(as.numeric(substr(as.character(data$HDFname[n]),10,13)), as.numeric(substr(as.character(data$HDFname[n]),14,16))), "%Y %j")))
  }
  stackQa <- stack(Qalist)
  cat('Progress: 90%. Extracted quality data from MODIS dataset and put quality data in a raster stack.\n')

  names(stackQa) <- Datelist
  cat('Progress: 100%. Converted names of quality raster stack to date.\nQuality raster stack completed.\n')

  return(stackQa)
}
describe <- function(obj) attr(obj, "help")
attr(Get_NDVI_Quality_Rasters, "help") <- "This function downloads MODIS Fluxtower .txt dataset, selects the quality data and converts it to a georeferenced raster stack. The expected input is flux tower name."

describe(Get_NDVI_Quality_Rasters)