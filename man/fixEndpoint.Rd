% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/hillr_endpointChecks.R
\name{fixEndpoint}
\alias{fixEndpoint}
\title{Makes sure that an endpoint ends in a "?"}
\usage{
fixEndpoint(endpoint)
}
\arguments{
\item{endpoint}{string A valid hilltop endpoint.}
}
\value{
A version of the provided string that ends in a "?"
}
\description{
\code{fixEndpoint} returns a string with a ? at the end
}
\details{
Helper function to ensure that a Hilltop endpoint ends in a "?".
}
