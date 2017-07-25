
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

