% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/hillr_ensemble_stats.R
\name{hilltopEnsembleStatBkgnd}
\alias{hilltopEnsembleStatBkgnd}
\title{Hilltop Ensemble Statistics Background.}
\usage{
hilltopEnsembleStatBkgnd(xmldata)
}
\arguments{
\item{xmldata}{An XML document, returned from a url request using anyXmlParse.}
}
\value{
A single line dataframe of the statistics background information.
}
\description{
\code{hilltopEnsembleStatBackground} returns background stats information
}
\details{
Helper Function that takes the parsed xml from an Ensemble Statistics request.
Returns a single line dataframe of the Statistics background information This
needs to be combined with the stats themselves to get a full dataframe.
}
