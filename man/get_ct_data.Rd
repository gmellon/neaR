% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/shapefile_matching_functions.R
\name{get_ct_data}
\alias{get_ct_data}
\title{Get Dataframe of Matching Census Tract IDs}
\usage{
get_ct_data(Latitude, Longitude)
}
\arguments{
\item{Latitude}{vector of Latitudes}

\item{Longitude}{Vector of Longitudes}
}
\value{
A datafame with associated CT info. Will be in correct order
as original Lat/Long to make it easy to append the data
}
\description{
This function returns a dataframe with Census Tract ID data for a vector of Latitudes
and Longitudes in a dataframe.
}
\details{
Census Tract shapefiles can only be downloaded at the state level;
as such, this function runs in a loop, downloading and matching
the shapefile for each state. All shapefiles are the most precise boundary option
with the exception of CA, which offers only smaller boundary data

It returns a dataframe with the following components:
 CT_GEOID: The GEOID of the Census Tract
 CT_NAME_LSAD: The Name of the Census Tract
 
 It is IMPORTANT to note that the CT GEOID is an 11 character digit
 It is recommended that CT are matched at the same time that CT level 
 data will be matched. If the file is saved and reimported, the ID
 must be padded to 11 digits, and must be set to not read out as scientific 
 notation.
}
\examples{
CT<-get_ct_data(NEA$CoLatitude, NEA$CoLongitude)
NEA$CT_GEOID<-CT$CT_GEOID
NEA$CT_NameLSAD<-CT$CT_NameLSAD
}
