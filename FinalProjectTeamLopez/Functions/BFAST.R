BFAST <- function(indexnumber,qualityStack, NDVIStack){
  
  ts.NDVIStack <- c()  
  for (i in 1:length(NDVIStack[1])){
    ts.NDVIStack <- c(ts.NDVIStack,NDVIStack[[i]]@data@values[indexnumber])
  }
  cat('Progress: 30%. Made a vector containing the NDVI values of a given pixel for all timesteps.\n')
  
  ts.qualityStack <- c()
  for (i in 1:length(qualityStack[1])){
    ts.qualityStack <- c(ts.qualityStack,qualityStack[[i]]@data@values[indexnumber])
  }
  cat('Progress: 60%. Made a vector containing the quality values of a given pixel for all timesteps.\n')

  ts.NDVIStack[ts.qualityStack > 1] <- NA
  cat('Progress: 70%. Deleted the NDVI values of timesteps in which the quality value of the pixel data was > 1.\n')
  
  ts.DATUM <- c()
  for(i in 1:length(NDVIStack[1])){
    DATUM <- substr(names(NDVIStack[[i]]), regexpr("X.*",names(NDVIStack[[i]]))+1, nchar(names(NDVIStack[[i]]))-0)
    ts.DATUM <- c(ts.DATUM,as.Date(DATUM, "%Y.%m.%d"))
  }
  ts.DATUM <- as.Date(ts.DATUM,origin = "1970-01-01")
  cat('Progress: 85%. Extracted dates from NDVI column names and converted it to an as.Date format.\n')

  ts.NDVIStacktime <- timeser(ts.NDVIStack/10000, ts.DATUM)
  ts.NDVIStacktime <- na.approx(ts.NDVIStacktime)
  NDVIStackbreaks <- bfast(ts.NDVIStacktime, season = "harmonic", max.iter = 1)
  cat('Progress: 100%. Converted the date and NDVI vectors to a timeseries and performed bfast analysis.\n')
  
  return(NDVIStackbreaks)
}

describe <- function(obj) attr(obj, "help")
attr(BFAST, "help") <- "This function performs a bfast analysis on the NDVI values of a given pixel within a given raster stack. The expected inputs are the indexnumber of the pixel, a quality raster stack and an NDVI raster stack."
describe(BFAST)