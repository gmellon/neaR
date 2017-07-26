#' Download CT Level data from Census API
#'
#' This function downloads a national-level file of tract-level data
#' from a specified census table. Data is downloaded in a loop
#'
#' @param year ending year of ACS data, defaults to 2015
#' @param survey survey used, defaults to acs5
#' @param table table used, defaults to "B17021_002E"

#' @return A dataframe with census data
#' @export

tract_level_data<-function(year="2015", survey="acs5", table="B17021_002E"){
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
  
  state.dfs <- as.list(rep(NA, nrow(state_fips)))
  for (i in 1:nrow(state_fips)){
    state.dfs[[i]] <- try(RJSONIO::fromJSON(paste0("http://api.census.gov/data/" ,year ,"/" ,survey, "/?get=NAME,", table,"&for=tract:*&in=state:", 
                                                   state_fips$State_FIPS[i])))
    names.temp <- state.dfs[[i]][[1]]
    state.dfs[[i]] <- state.dfs[[i]][-1]
    state.dfs[[i]] <- do.call(rbind, state.dfs[[i]])
    colnames(state.dfs[[i]]) <- names.temp
  }
  state.all <- do.call(rbind, state.dfs)
  state.all<-as.data.frame(state.all, stringsAsFactors=F)
  state.all$CT_GEOID<-paste0(state.all$state, state.all$county, state.all$tract)
  return(state.all)
}

#' Creates a poverty rate variable, and merges two files
#' with different census tables
#'
#' This function requires two datasets using the tract level data function:
#' -a dataframe that has the poverty table data  "B17021_002E"
#' -a second dataframe that has the population data for which poverty is 
#' known "B17021_001E"
#'
#' @param poverty dataframe with poverty table
#' @param population dataframe with population table
#' @return A dataframe with formatted/merged pop and poverty data
#' @export

get_poverty_rates<-function(poverty, population){
  poverty$B17021_001E<-NA
  poverty$B17021_001E[is.na(poverty$B17021_001E)] <- population$B17021_001E[
    match(poverty$CT_GEOID[is.na(poverty$B17021_001E)], 
          population$CT_GEOID)]
  
  poverty$B17021_002E<-as.numeric(poverty$B17021_002E)
  poverty$B17021_001E<-as.numeric(poverty$B17021_001E)
  poverty$pctpov <- round(100 * (poverty$B17021_002E / poverty$B17021_001E), 1)
  return(poverty)
}


#' Appends Poverty Data to main Data file
#'
#' This function makes it easy to append 2011-2015 poverty rate data
#' from the 5 year ACS to another dataset. To use, just create a new column
#' in the original data to store the poverty rate data, and identify which column
#' in the original data has the census tract ID variables. The function will do the
#' rest. 
#'
#' @param ID_var column in data frame with CT ids, i.e. NEA$CT_GEOID
#' @param pov_column the name of the column in the dataset to append
#' poverty data to
#' @return A vector of poverty data, in the same order as original dataset
#' @examples NEA$poverty_rate<-NA
#' NEA$poverty_rate<-append_poverty_data(NEA$CT_GEOID, NEA$poverty_rate)
#' @export

append_poverty_data<-function(ID_var, pov_column){
  
  poverty<-tract_level_data(table="B17021_002E")
  population<-tract_level_data(table="B17021_001E")
  full.poverty.data<-get_poverty_rates(poverty, population)
  
  pov_column[is.na(pov_column)]<-full.poverty.data$pctpov[match(
    ID_var[is.na(pov_column)],full.poverty.data$CT_GEOID)]
  return(pov_column)
}
