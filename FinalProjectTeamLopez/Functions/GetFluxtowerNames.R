get_fluxtower_names <- function(outputfilename){
  input_txt_file <- "ftp://daac.ornl.gov/data/modis_ascii_subsets/5_MODIS_Subset_Sites_Information_Collection5.csv"                  
  download.file(input_txt_file, paste('Data/',outputfilename,".txt",sep = ""))
  fluxtowers <- read.csv(paste('Data/',outputfilename,".txt",sep=""))
  fluxtowers <-fluxtowers[0:3]
  names(fluxtowers)[3] <- "fluxtowername"
  return(fluxtowers)  
}
describe <- function(obj) attr(obj, "help")
attr(get_fluxtower_names, "help") <- "This function downloads and returns the names of the flux towers. The Site_ID should be used as fluxtower name."
describe(get_fluxtower_names)

