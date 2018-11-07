#' General function to get timeseries data from a Hilltop server.

#' Takes a valid Hilltop server endpoint, a site name, a measurement name  and
#' optional from, to or timeIntervaland arguments, an optional timeseries type
#' argument and returns a dataframe of the data for that site and measurement
#' for the requested time range from the endpoint. For standard time series,
#' statistics can be obtained, to request these a method, interval and
#' allignment argument need to be provided.

#' @inheritParams buildDataRequestUrl
#' @return dataframe A dataframe of the timeseries data.
#' @export

getHilltopData <- function(endpoint, site, measurement, from, to, timeInterval, tsType, alignment, method, interval) {
  # Build the url.
  dataUrl <- buildDataRequestUrl(endpoint = endpoint,
                                  site = site,
                                  measurement = measurement,
                                  from = from,
                                  to = to,
                                  timeInterval = timeInterval,
                                  tsType = tsType,
                                  alignment = alignment,
                                  method = method,
                                  interval = interval)
  # Parse the XML
  dataXml <- anyXmlParse(tempUrl)
  # Request the data
  dataDf <- hilltopMeasurement(tempXml)
  # Return the data
  return(dataDf)
}

#' General funtion to get a site list from a Hilltop server.
#'
#' Takes a valid hilltop endpoint, and returns a list of available sites
#' available from the endpoint.
#'
#' @inheritParams buildSiteListUrl
#'
#' @return A dataframe of site names
#'
#' @export
getHilltopSites <- function(endpoint) {
  # Build the url.
  sitesUrl <- buildSiteListUrl(endpoint = endpoint)
  # Parse the XML
  sitesXml <- anyXmlParse(sitesUrl)
  # Request the data
  sitesDf <- hilltopSiteList(tempXml)
  # Return the data
  return(sitesDf)
}

#' General funtion to get a measuremnt list for a site from a Hilltop server.
#'
#' Takes a valid hilltop endpoint and site, and returns a list of available measurements
#' available from the endpoint.
#'
#' @inheritParams buildMeasurementListUrl
#'
#' @return A dataframe of measurements and associated metadata
#'
#' @export
getHilltopMeasurements <- function(endpoint, site) {
  # Build the url.
  measurementsUrl <- buildMeasurementListUrl(endpoint = endpoint,
                                      site = site)
  # Parse the XML
  measurementsXml <- anyXmlParse(measurementsUrl)
  # Request the data
  measurementsDf <- hilltopDsMeasListFull(measurementsXml)
  # Return the data
  return(measurementsDf)
}
