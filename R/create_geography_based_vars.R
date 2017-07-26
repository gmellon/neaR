
#' Create Flag for International Records
#'
#' This function can be used to create a column that flags intl records. It assumes 
#' that the dataset includes a column with State Names 
#'
#' @param state_var Name of column in data set with state abbreviations
#' @param additions Additional State Names to add as international.
#' defaults to "FO", "AS", "FM", "GU","MH", "MP", "PW", "PR", "VI",
#'  "AE","AP", "AA", "CM" 
#' @return A new Boolean TRUE and FALSE Column where TRUE=International
#' @export

create_intl_flag<-function(state_var, additions = c() ){
  international<- state_var %in% c("FO", "AS", "FM", "GU",
                                   "MH", "MP", "PW", "PR", "VI", "AE",
                                   "AP", "AA", "CM", additions)
  international[is.na(state_var)]<-TRUE
  international[grepl("^ +$", state_var)]<-TRUE
  return(international)
}

#' Create Rural/Urban Identifier
#'
#' This function can be used to create a column that flags records as either urban
#' or rural. 
#' 
#' Urban records are records that fall into a Metropolitan or Micropolitan
#' statistical area. As a result, MSAs will need to be appended to the data before
#' this function is utilized. Records not falling into these MSAs are classified
#' as rural. Records without valid Lat/Long will result in NAs
#' 
#' Note, caution should be exercised with international records, they are defaulted
#' to "rural" since they will not match a MSA. After adding rural/urban
#' a international record flag should be used to subset the data. 
#'
#' @param MSA_catory Name of column with MSA categories - i.e. M1, M2. should be LSAD
#' @param Latitude Column/vector with latitude 
#' @return a new column with markers for Rural or Urban 
#' @export

create_boolean_urban<-function(MSA_category, Latitude){
  
  boolean_urban<- rep(NA, length(MSA_category))
  boolean_urban[which(MSA_category=="M2")]<-"Rural"
  boolean_urban[which(MSA_category=="M6")]<-"Rural"
  boolean_urban[which(MSA_category=="M5")]<-"Urban"
  boolean_urban[which(MSA_category=="M1")]<-"Urban"
  boolean_urban[is.na(MSA_category) & !is.na(Latitude)]<-"Rural" 
  return(boolean_urban)
}

#' Create Categories of Urban Records
#'
#' This function can be used to create a column that categorizes
#' records based on urban population size
#' 
#' It requires that the dataset already has a rural/urban marker, and that
#' it has a second column with MSA and NECTA population data - should
#' be downloaded and appended from the Census, and matched based on CBSA and 
#' NECTA ids. 
#' 
#' Note, caution should be exercised with international records, they are defaulted
#' to "rural" since they will not match a MSA. After adding rural/urban
#' a international record flag should be used to subset the data. 
#'
#' @param MSA_catory Name of column with MSA categories - i.e. M1, M2. should be LSAD
#' @param Latitude Column/vector with latitude 
#' @return a new column with markers for Rural or Urban 
#' @export

create_urban_type<-function(MSA_pop, Boolean_Urban_MSA){
  
  urban_type<- rep(NA, length(MSA_pop))
  urban_type[is.na(urban_type)& Boolean_Urban_MSA=="Urban" & MSA_pop<50000]<-"Less than 50,000"
  urban_type[is.na(urban_type)& Boolean_Urban_MSA=="Urban"& MSA_pop>=50000 & MSA_pop<250000]<-"50,000 to 249,999"
  urban_type[is.na(urban_type)& Boolean_Urban_MSA=="Urban"& MSA_pop>=250000 & MSA_pop<1000000]<-"250,000 to 999,999"
  urban_type[is.na(urban_type)& Boolean_Urban_MSA=="Urban"& MSA_pop>999999 & MSA_pop<4600000]<-"1 Million to Less than 4.6 Million"
  urban_type[is.na(urban_type)& Boolean_Urban_MSA=="Urban"& MSA_pop>=4600000]<-"4.6 Million and More"
  urban_type[is.na(urban_type)& Boolean_Urban_MSA=="Rural"]<-"Rural"
  return(urban_type)
}



#' Create Poverty/Not Poverty Flag
#'
#' This function can be used to create a column that categorizes
#' records based on their poverty rate. 
#' 
#' It requires that the dataset already has a column for poverty rate
#' for the census tract in which the record is located, 
#' which can be created using the relevant poverty packages in this
#' r package. 
#' 
#' Note, that areas without population or without poverty rates are labeled
#' "Missing Data"  
#'
#' @param pov_column Name of column with poverty rate - i.e. 20, 30 etc.
#' @param Latitude Column/vector with latitude 
#' @param cutoff cutoff for defining a high poverty neighborhood, defaults to 20

#' @return a new column with markers for Rural or Urban 
#' @export


create_poverty_flag <-function(pov_column, Latitude, cutoff=20){
  
  poverty<- rep(NA, length(pov_column))
  poverty[pov_column>=cutoff]<-'Poverty'
  povertyy[pov_column<cutoff]<-'Not Poverty'
  poverty[is.na(pov_column)]<-'Missing Data'
  return(poverty) }
