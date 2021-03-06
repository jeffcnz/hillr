% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/hillr_endpointChecks.R
\name{is.validEndpoint}
\alias{is.validEndpoint}
\title{Checks that a Hilltop endpoint is valid.}
\usage{
is.validEndpoint(endpoint)
}
\arguments{
\item{endpoint}{string A valid hilltop endpoint.}
}
\value{
Logical TRUE / FALSE
}
\description{
\code{is.validEndpoint} returns a logical value
}
\details{
Helper function to check if a provided endpoint is a valid Hilltop endpoint.
Currelntly limited to checking that the first 4 characters are http and the
last are.hts?.
}
