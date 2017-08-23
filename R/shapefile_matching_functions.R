#' Get Dataframe of Matching Metropolitan Area Codes
#'
#' This function returns a dataframe with MSA data for a vector of Latitudes
#' and Longitudes in a dataframe. 
#' 
#' The package operates by matching Lat/Long to shapefiles of Metropolitan
#' and Micropolitan Statistical Areas - from CBSA files. Then, the package matches Lat/Long
#' that have not yet been coded to Metropolitan and Micropolitan NECTAS
#' 
#'  It returns a dataframe with the following components:
#'  GEOID: The GEOID of the MSA or NECTA
#'  NAME_LSAD: The Name of the MSA or NECTA
#'  LSAD: A Code indicating whether it is a Metro or Micro MSA or NECTA
#'  -M1 are Metropolitan MSAs
#'  -M2 are Micropolitan MSAs
#'  -M5 are Metropolitan NECTAS
#'  -M6 are Micropolitan NECTAS
#'  Source: A constructed variable, indicating if the boundaries were based
#'  on NECTA or MSAs  
#'
#' @param Latitude vector of Latitudes
#' @param Longitude Vector of Longitudes
#' @return A datafame with associated MSA info. Will be in correct order
#' as original Lat/Long to make it easy to append the data
#' @examples msas<-get_msa_data(NEA$CoLatitude, NEA$CoLongitude)
#' NEA$MSA_GEOID<-msa$csba_GEOID
#' NEA$MSA_NameLSAD<-msa$cbsa_NameLSAD
#' @export

get_msa_data<-function(Latitude, Longitude, year=2016){
  cbsa_sf<-tigris::core_based_statistical_areas(cb = F, year = year)
  necta_sf<-tigris::new_england(type="necta", cb=F, year= year)
  ID <- 1:length(Latitude)
  spatial<-dtf(Latitude=Latitude, Longitude=Longitude, ID=ID)
  complete_spatial<- spatial[!is.na(spatial$Latitude), ]
  sp::coordinates(complete_spatial) <- c("Longitude", "Latitude")
  sp::proj4string(complete_spatial)<-sp::proj4string(cbsa_sf)
  
  complete_spatial$cbsa_GEOID <- NA
  complete_spatial$cbsa_NAMELSAD <- NA
  complete_spatial$cbsa_LSAD <- NA 
  
  
  complete_spatial$cbsa_GEOID <- sp::over(complete_spatial,cbsa_sf)$GEOID
  complete_spatial$cbsa_NAMELSAD <- sp::over(complete_spatial,cbsa_sf)$NAMELSAD
  complete_spatial$cbsa_LSAD <- sp::over(complete_spatial,cbsa_sf)$LSAD
  
  matched_spatial <- dtf(ID = complete_spatial$ID, 
                         cbsa_GEOID=complete_spatial$cbsa_GEOID,
                         cbsa_NAMELSAD=complete_spatial$cbsa_NAMELSAD,
                         cbsa_LSAD=complete_spatial$cbsa_LSAD, source = "MSA")
  
  matched_spatial <- matched_spatial[!is.na(matched_spatial$cbsa_GEOID), ]  
  
  complete_spatial <- complete_spatial[which(is.na(complete_spatial$cbsa_GEOID)),]
  
  sp::proj4string(complete_spatial)<-sp::proj4string(necta_sf)
  
  complete_spatial$cbsa_GEOID <- sp::over(complete_spatial,necta_sf)$GEOID
  complete_spatial$cbsa_NAMELSAD <- sp::over(complete_spatial,necta_sf)$NAMELSAD
  complete_spatial$cbsa_LSAD <- sp::over(complete_spatial,necta_sf)$LSAD
  
  matched_spatial <- rbind(matched_spatial, dtf(ID = complete_spatial$ID, 
                                                cbsa_GEOID=complete_spatial$cbsa_GEOID,
                                                cbsa_NAMELSAD=complete_spatial$cbsa_NAMELSAD,
                                                cbsa_LSAD=complete_spatial$cbsa_LSAD, source = "NECTA"))
  
  output <- matched_spatial[match(spatial$ID, matched_spatial$ID), ]
  all(output$ID==ID, na.rm = T)
  all(is.na(Longitude)==is.na(output$ID))
  output$ID <- NULL
  return(output)
}


#' Get Dataframe of Matching Congressional District
#'
#' This function returns a dataframe with Congressional data for a vector of Latitudes
#' and Longitudes in a dataframe. 
#' 
#' The package operates by matching Lat/Long to shapefiles of 114th Congressional
#' Districts - valid from 2015-2017. 
#' 
#'  It returns a dataframe with the following components:
#'  GEOID: The GEOID of the CD
#'  NAME_LSAD: The Name of the CD

#' @param Latitude vector of Latitudes
#' @param Longitude Vector of Longitudes
#' @return A datafame with associated CD info. Will be in correct order
#' as original Lat/Long to make it easy to append the data
#' @examples cds<-get_cd_data(NEA$CoLatitude, NEA$CoLongitude)
#' NEA$CD_GEOID<-cds$CD_GEOID
#' NEA$CD_NameLSAD<-cds$CD_NameLSAD
#' @export

get_cd_data<-function(Latitude, Longitude){
  CD_sf <- tigris::congressional_districts(cb = F)
  ID <- 1:length(Latitude)
  spatial<-dtf(Latitude=Latitude, Longitude=Longitude, ID=ID)
  complete_spatial<- spatial[!is.na(spatial$Latitude), ]
  sp::coordinates(complete_spatial) <- c("Longitude", "Latitude")
  sp::proj4string(complete_spatial)<-sp::proj4string(CD_sf)
  
  complete_spatial$CD_GEOID <- NA
  complete_spatial$CD_NAMELSAD <- NA
  
  complete_spatial$CD_GEOID <- sp::over(complete_spatial,CD_sf)$GEOID
  complete_spatial$CD_NAMELSAD <- sp::over(complete_spatial,CD_sf)$NAMELSAD
  
  matched_spatial <- dtf(ID = complete_spatial$ID, 
                         CD_GEOID=complete_spatial$CD_GEOID,
                         CD_NAMELSAD=complete_spatial$CD_NAMELSAD)
  
  output <- matched_spatial[match(spatial$ID, matched_spatial$ID), ]
  all(output$ID==ID, na.rm = T)
  all(is.na(Longitude)==is.na(output$ID))
  output$ID <- NULL
  return(output)
}


#' Get Dataframe of Matching County Data
#'
#' This function returns a dataframe with County ID data for a vector of Latitudes
#' and Longitudes in a dataframe. 
#' 
#'  It returns a dataframe with the following components:
#'  GEOID: The GEOID of the County - i.e. FIPS Code
#'  NAME_LSAD: The Name of the County

#' @param Latitude vector of Latitudes
#' @param Longitude Vector of Longitudes
#' @param year vintage of shapefiles, defaults to 2015 if none specified

#' @return A datafame with associated Ccountyinfo. Will be in correct order
#' as original Lat/Long to make it easy to append the data
#' @examples counties<-get_cunty_data(NEA$CoLatitude, NEA$CoLongitude)
#' NEA$county_GEOID<-counties$county_GEOID
#' NEA$county_NameLSAD<-counties$county_NameLSAD
#' @export

get_county_data<-function(Latitude, Longitude, year=2015){
  county_sf<- tigris::counties(cb = F, year)
  
  ID <- 1:length(Latitude)
  spatial<-dtf(Latitude=Latitude, Longitude=Longitude, ID=ID)
  complete_spatial<- spatial[!is.na(spatial$Latitude), ]
  sp::coordinates(complete_spatial) <- c("Longitude", "Latitude")
  sp::proj4string(complete_spatial)<-sp::proj4string(county_sf)
  
  complete_spatial$county_GEOID <- NA
  complete_spatial$county_NAMELSAD <- NA
  
  complete_spatial$county_GEOID <- sp::over(complete_spatial,county_sf)$GEOID
  complete_spatial$county_NAMELSAD <- sp::over(complete_spatial,county_sf)$NAMELSAD
  
  matched_spatial <- dtf(ID = complete_spatial$ID, 
                         county_GEOID=complete_spatial$county_GEOID,
                         county_NAMELSAD=complete_spatial$county_NAMELSAD)
  
  output <- matched_spatial[match(spatial$ID, matched_spatial$ID), ]
  all(output$ID==ID, na.rm = T)
  all(is.na(Longitude)==is.na(output$ID))
  output$ID <- NULL
  return(output)
}



#' Get Dataframe of Matching Census Tract IDs
#' 
#' This function returns a dataframe with Census Tract ID data for a vector of Latitudes
#' and Longitudes in a dataframe. 
#' 
#' Census Tract shapefiles can only be downloaded at the state level;
#' as such, this function runs in a loop, downloading and matching
#' the shapefile for each state. All shapefiles are the most precise boundary option
#' with the exception of CA, which offers only smaller boundary data
#' 
#'  It returns a dataframe with the following components:
#'  CT_GEOID: The GEOID of the Census Tract
#'  CT_NAME_LSAD: The Name of the Census Tract
#'  
#'  It is IMPORTANT to note that the CT GEOID is an 11 character digit
#'  It is recommended that CT are matched at the same time that CT level 
#'  data will be matched. If the file is saved and reimported, the ID
#'  must be padded to 11 digits, and must be set to not read out as scientific 
#'  notation. 

#' @param Latitude vector of Latitudes
#' @param Longitude Vector of Longitudes
#' @return A datafame with associated CT info. Will be in correct order
#' as original Lat/Long to make it easy to append the data
#' @examples CT<-get_ct_data(NEA$CoLatitude, NEA$CoLongitude)
#' NEA$CT_GEOID<-CT$CT_GEOID
#' NEA$CT_NameLSAD<-CT$CT_NameLSAD
#' @export


get_ct_data<- function(Latitude, Longitude){

  ### Create Data Frame with State Fips and Names for Looping
  State_FIPS<-c(  "01" ,"02", "04", "05", "06", "08", "09", "10", "11" ,"12", "13", "15", "16",
                  "17", "18", "19", "20", "21", "22", "23", "24" ,"25",
                  "26", "27", "28", "29" ,"30" ,"31" ,"32" ,"33" ,"34",
                  "35" ,"36" ,"37" ,"38", "39" ,"40", "41" ,"42", "44", "45" ,"46" ,"47", "48",
                  "49" ,"50", "51" ,"53" ,"54", "55", "56")
  
  State<-c( "Alabama"          ,    "Alaska"        ,       "Arizona"       ,       "Arkansas"    ,        
            "California"     ,      "Colorado"       ,      "Connecticut"  ,        "Delaware"   ,         
            "District of Columbia", "Florida"        ,      "Geogia"   ,            "Hawaii"     ,         
            "Idaho"       ,         "Illinois"   ,          "Indiana"   ,           "Iowa"    ,            
            "Kansas"    ,           "Kentucky"    ,         "Louisiana"  ,          "Maine"      ,         
            "Maryland"    ,         "Massachusetts",       "Michigan" ,            "Minnesota"   ,        
            "Mississippi"     ,     "Missouri"    ,         "Montana"     ,         "Nebraska"  ,          
            "Nevada"  ,             "New Hampshire"    ,    "New Jersey"   ,        "New Mexico"  ,        
            "New York"     ,        "North Carolina" ,      "North Dakota"   ,      "Ohio"   ,             
            "Oklahoma"         ,    "Oregon"         ,      "Pennsylvania"    ,     "Rhode Island"  ,      
            "South Carolina"   ,    "South Dakota"   ,      "Tennessee"  ,          "Texas"       ,        
            "Utah"         ,        "Vermont"     ,         "Virginia"        ,     "Washington"    ,      
            "West Virginia"   ,     "Wisconsin"     ,       "Wyoming" )
  
  State_Abbreviation<- c( "AL", "AK", "AZ", "AR" ,"CA" ,"CO" ,"CT", "DE" ,"DC" ,"FL", "GA", "HI" ,
                          "ID" ,"IL", "IN", "IA", "KS", "KY" ,"LA" ,"ME" ,"MD" ,"MA",
                          "MI", "MN", "MS" ,"MO", "MT", "NE" ,"NV" ,"NH" ,"NJ" ,"NM", "NY", "NC" ,"ND", "OH" ,"OK",
                          "OR", "PA", "RI" ,"SC" ,"SD" ,"TN" ,"TX",
                          "UT", "VT" ,"VA", "WA" ,"WV" ,"WI" ,"WY" )
  
  state_fips<-dtf(State_FIPS, State, State_Abbreviation)
  state_fips$State_FIPS<- stringr::str_pad(state_fips$State_FIPS,2,pad="0")
  
  #set up master file
  
  ID <- 1:length(Latitude)
  spatial<-dtf(Latitude=Latitude, Longitude=Longitude, ID=ID)
  complete_spatial<- spatial[!is.na(spatial$Latitude), ]
  sp::coordinates(complete_spatial) <- c("Longitude", "Latitude")
  
  complete_spatial$CT_GEOID <- NA
  complete_spatial$CT_NAMELSAD <- NA
  
  for( i in c(1:4, 6:nrow(state_fips))) {
       census_tract_sf<-tigris::tracts(state_fips$State_FIPS[i] , cb=F)
    sp::proj4string(complete_spatial)<-sp::proj4string(census_tract_sf)
    complete_spatial$CT_GEOID[is.na(complete_spatial$CT_GEOID)] <- sp::over(complete_spatial[is.na(complete_spatial$CT_GEOID),], census_tract_sf)$GEOID
    complete_spatial$CT_NAMELSAD[is.na(complete_spatial$CT_NAMELSAD)] <- sp::over(complete_spatial[is.na(complete_spatial$CT_NAMELSAD),], census_tract_sf)$NAMELSAD
  }
  
  #california - done last because there was originally an embedded problem with the shapefile on
  # tigris. Has been fixed in more recent updates.
  
  census_tract_sf<-tigris::tracts(state="06" , cb=F)
  sp::proj4string(complete_spatial) <- sp::proj4string(census_tract_sf)
  complete_spatial$CT_GEOID[is.na(complete_spatial$CT_GEOID)] <- sp::over(complete_spatial[is.na(complete_spatial$CT_GEOID),], census_tract_sf)$GEOID
  
  matched_spatial <- data.frame(ID = complete_spatial$ID, 
                                CT_GEOID = complete_spatial$CT_GEOID, 
                                CT_NAMELSAD = complete_spatial$CT_NAMELSAD, stringsAsFactors = FALSE)
  
  output <- matched_spatial[match(spatial$ID, matched_spatial$ID), ]
  all(output$ID==ID, na.rm = T)
  all(is.na(Longitude)==is.na(output$ID))
  output$ID <- NULL
  return(output)
}
