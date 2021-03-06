% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/geocoding_functions.R
\name{makeChunkIndex}
\alias{makeChunkIndex}
\title{Create chunks for looped functions}
\usage{
makeChunkIndex(x, group.size)
}
\arguments{
\item{x}{vector to be used in function}

\item{group.size}{Sets the size of the "chunk" to be run at one time}
}
\value{
Chunk Index
}
\description{
This function loads a file as a matrix. It assumes that the first column
contains the rownames and the subsequent columns are the sample identifiers.
Any rows with duplicated row names will be dropped with the first one being
kepted.
}
