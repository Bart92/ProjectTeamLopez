require("XML")

html_to_coordlist <- function(filename){
  coordtable <- readHTMLTable(paste(paste("http://daac.ornl.gov/cgi-bin/MODIS/GR_col5_1/corners.1.pl?site=",filename,sep=""),"&res=250m",sep=""))
  coordtable <- as.data.frame(coordtable[1])
  coordtable <- as.character(coordtable[,2][5:8])
  coords <- c()
  for(i in 1:length(coordtable)){
    splittedcoords = strsplit(coordtable[i]," , ")
    for(j in 1:2){
      coords = c(coords, splittedcoords[[1]][j])
    }
  }
  coords_mat <- matrix(as.numeric(coords),ncol=2,nrow=4,byrow=T)
  coords_mat_new <- data.frame(x=coords_mat[,2],y=coords_mat[,1])
  return(coords_mat_new)
}

describe <- function(obj) attr(obj, "help")
attr(html_to_coordlist, "help") <- "This function takes the file name of a fluxtower, extracts the extent of the corresponding fluxtower online and puts it in a matrix. The expected input is a fluxtower filename as given by the get_fluxtower_names function (e.g.: fn_nlloobos)."
describe(html_to_coordlist)