
#' Create New Discipline Variable
#'
#' This function is based on the "Discipline" variable in the NEA admin data
#' and recodes "Discipline" into more succinct categories. The New Discipline
#' Variable created is used in Tableau visualizations
#'
#' @param discipline Name of column in data set with "Discipline" variable
#' @return A new column with recoded disciplines for use in analysis
#' @export

create_discipline_tag<-function(discipline){
  new_discipline<-rep(NA, length(discipline))
  
  new_discipline[which(discipline %in% c("AccessAbility", 
                                         "AccessAbility / Research" , "Accessibility" )) ] <- "Accessibility"
  
  new_discipline[which(discipline %in% c("Artist Communities", 
                                         "Artists Communities")) ] <- "Artist Communities"
  
  new_discipline[which(discipline %in% c("Arts Education", 
                                         "Arts Education in American Communities", 
                                         "Arts Education Invitational Grants Initiative (AEIGI)" ,
                                         "Arts in Education", "Learning in the Arts for Children & Youth"))] <- "Arts Education"
  
  new_discipline[which(discipline %in% c("Challenge America", 
                                         "Challenge America Fast-Track", "Challenge America Fast-Track Review" ,
                                         "Challenge America: Reaching Every Community"))] <- "Challenge America"
  
  new_discipline[which(discipline %in% c("Creative Links: Positive Alternatives for Youth",
                                         "Leadership Special Initiatives", "National Initiatives",
                                         "Program Innovation", "Resources for Change:Technology",
                                         "Save America's Treasures"))] <- "Special Initiatives"
  
  new_discipline[which(discipline %in% c("Creativity Connects"))] <- "Creativity Connects"
  
  new_discipline[which(discipline %in% c("Dance"))] <- "Dance"
  
  new_discipline[which(discipline %in% c("Design"))] <- "Design"
  
  new_discipline[which(discipline %in% c("Federal Partnerships", "State & Regional"))] <- "State and Regional"
  
  new_discipline[which(discipline %in% c("Folk & Traditional Arts"))] <- "Folk and Traditional Arts"
  
  new_discipline[which(discipline %in% c("Literature"))] <- "Literature"
  
  new_discipline[which(discipline %in% c("Local Arts Agencies", "Local Arts Agency"))] <- "Local Arts Agencies"
  
  new_discipline[which(discipline %in% c("Media Arts"))] <- "Media Arts"
  
  new_discipline[which(discipline %in% c("Multi Disciplinary", "Multidisciplinary",
                                         "Presenting", "Presenting & Multidisciplinary Works"))] <- "Presenting and Multidisciplinary Works"
  
  new_discipline[which(discipline %in% c("Museum"))] <- "Museums"
  
  new_discipline[which(discipline %in% c("Music"))] <- "Music"
  
  new_discipline[which(discipline %in% c("Musical Theater", "Theater & Musical Theater"))] <- "Musical Theater"
  
  new_discipline[which(discipline %in% c("Theater"))] <- "Theater"
  
  new_discipline[which(discipline %in% c("Opera"))] <- "Opera"
  
  new_discipline[which(discipline %in% c("Research"))] <- "Research and Analysis"
  
  new_discipline[which(discipline %in% c("Visual Arts", "VISUAL ARTS"))] <- "Visual Arts"
  
  new_discipline[which(discipline %in% c("Arts Engagement in American Communities", 
                                         "International"))] <- "."
  
  return(new_discipline)
  
}
