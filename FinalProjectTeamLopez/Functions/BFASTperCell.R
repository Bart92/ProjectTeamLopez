source("Functions/BFAST.R")

BFASTpercell <- function(x) {
  val <- click(x, n = 1,xy=T,cell=T)
  colnr <- (val$cell[1]-1) %% 28 + 1
  rownr <- (val$cell[1]-1) %/% 28 + 1
  indexnr <- val$cell[1]
  rowcolnr <- data.frame(rownr=rownr,colnr=colnr,index=indexnr)
  cat("Cell selected. Starting bfast...\n")
  return(BFAST(indexnr,Quality,NDVI))#,main=paste("BFAST analysis for cell",indexnr,sep=" "),add=T)
}

describe <- function(obj) attr(obj, "help")
attr(BFASTpercell, "help") <- "This function opens an interactive user interface in which a user can select a cell. Subsequently, bfast analysis is performed on the selected cell. The expected input is the NDVI raster stack on which the analysis should be performed."
describe(BFASTpercell)