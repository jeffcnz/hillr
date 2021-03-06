% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/hillr_lists.R
\name{hilltopMeasInfoList}
\alias{hilltopMeasInfoList}
\title{Hilltop Data Sources And Measurement Information At A Site.}
\usage{
hilltopMeasInfoList(xmldata)
}
\arguments{
\item{xmldata}{An XML document, returned from a url request using anyXmlParse.}
}
\value{
A dataframe of measurement information and associated datasources.
}
\description{
\code{hilltopMeasInfoList} helper function for measurement metadata.
}
\details{
Helper function.
Takes an xml document from a Hilltop MeasurementList at a Site request.
Returns a dataframe of the measurement information and datasources.
}
