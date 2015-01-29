timeser <- function(index, dt) {
  z <- zoo(index, dt)
  yr <- as.numeric(format(time(z), "%Y"))
  jul <- as.numeric(format(time(z), "%j"))
  delta <- min(unlist(tapply(jul, yr, diff)))
  zz <- aggregate(z, yr + (jul - 1)/delta/23)
  (tso <- as.ts(zz))
  return(tso)
}

describe <- function(obj) attr(obj, "help")
attr(timeser, "help") <- "This functions is used to make a timeseries of a given vector of NDVI values. The expected inputs are a vector containing NDVI values of a certain pixel through time and a vector containing the corresponding dates."
describe(timeser)