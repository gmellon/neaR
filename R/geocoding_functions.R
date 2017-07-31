
#Geocoding Data

#' Transform to Data Frame
#'
#' Converts object to dataframe, defaults to StringsAsFactors=F
#'
#' @return A data frame
#' @export

dtf <- function(..., StAsFa= FALSE) {
  data.frame(..., stringsAsFactors = StAsFa)
}

#' Create chunks for looped functions
#'
#' This function loads a file as a matrix. It assumes that the first column
#' contains the rownames and the subsequent columns are the sample identifiers.
#' Any rows with duplicated row names will be dropped with the first one being
#' kepted.
#'
#' @param x vector to be used in function
#' @param group.size Sets the size of the "chunk" to be run at one time
#' @return Chunk Index
#' @export



makeChunkIndex <- function(x, group.size) {
  chunks <- split(1:length(x), ceiling(seq_along(1:length(x)) / group.size) )
}

#' Geocodes data using RDSTK in a loop
#'
#' This function loads geocodes data using RDSTK, and operates in loops
#' of chunks set with the chunk index function. It is an intermediary function
#' used in other geocoding functions in this package
#'
#' @param addresses vector of addresses
#' @return dataframe of addresses, lat, and long
#' @export

address2LatLon <- function(addresses) {
  #creates a json object from vector
  addresses <- paste(addresses, collapse = "\",\"")
  addresses <- paste0("[\"", addresses, "\"]")
  # post json object to DSTK API 
  output <- httr::POST('http://www.datasciencetoolkit.org/street2coordinates',
                 body = addresses, encode = 'json')
  # turns json object to R List
  output <- httr::content(output, "parsed")
  # function to extract lat/long
  getLatLon <- function(x) {
    lat <- x["latitude"]
    lon <- x["longitude"]
    lon[is.null(lon)] <- NA
    lat[is.null(lat)] <- NA
    c(lat,lon)
  }
  
  #creates list with lat/long and transposes
  output <- t(sapply(output, getLatLon))
  #adds column names
  colnames(output) <- c("lat", "lon")
  #returns data frame, and adds address names as columns
  output <- dtf(address = rownames(output), output)
  return(output)
}


#' Pads zipcodes to 5 digits
#'
#' This function uses the stringr package to automate
#' padding zipcodes to 5 digits
#'
#' @param zip vector of zipcodes, five digit format
#' @return vector of padded zipcodes
#' @examples NEA$zip<-get_padded_zip(NEA$CoZip)
#' @export
#
get_padded_zip<-function(zip){
  zip<-stringr::str_pad(zip, 5, pad="0")
  return(zip)
}


#' Combines relevant columns to create a complete address
#'
#' This function combines the address component columns in a dataframe 
#' and returns an address vector. Also includes functions to remove
#' problematic characters from the text that will interfere with geocoding
#'
#' @param data name of dataset
#' @param address.vars vector of address variables i.e. c("Address1", "Address2", "City")
#' @return formatted vector of addresses
#' @examples NEA$full_address<-create_full_address(NEA, c("CoAddress1", "CoAddress2",
#' "CoCity", "CoState", "CoZip"))
#' @export
#

create_full_address<-function(data, address.vars) {
  address <- apply(as.matrix(data[, address.vars]), 1, paste, collapse = ", ")
  address <- gsub(", ,", ", ", address)
  address <- gsub(" +", " ", address)
  address <- gsub("[^[:graph:]]", " ", address)
  address <- gsub("\"", " ", address)
  address<- gsub("/", " ", address)
  return(address)
}


#' Primary Geocoding Function
#'
#' This function takes an address column that has been properly formatted
#' using get_padded_zip and then create_full_address and returns a dataframe of 
#' addresses and relevant lat/long
#'
#' @param address name of the address vector in the data set i.e. NEA$full_address
#' @return dataframe of addresses and geocodes, that can then be added to the original
#' data
#' @examples addresses<-get_geocode_data(NEA$full_address)
#' NEA$CoLongitude<-addresses$long
#' NEA$CoLatitude<-addresses$lat
#' @export
#
get_geocode_data<-function(address){
  
  address.unique <- unique(address)
  length(address.unique)
  
  add.chunks <- makeChunkIndex(address.unique, group.size = 100)
  add.coded <- as.list(rep(NA, length(add.chunks)))

    for(ii in which(is.na(add.coded))) {
    add.coded[[ii]] <- try(address2LatLon(address.unique[add.chunks[[ii]]]))
    if(class(add.coded[[ii]])=="try-error") {
      break()
    } 
  }
  
  add.coded <- do.call(rbind, add.coded)
  
  #takes list and turns into a vector
  add.coded$lat <- unlist(add.coded$lat)
  add.coded$lon<- unlist(add.coded$lon)
  
  add.coded.all <- add.coded[match(address, rownames(add.coded)), ]
  rownames(add.coded.all) <- NULL
  return(add.coded.all)
}
