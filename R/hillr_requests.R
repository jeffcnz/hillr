#' General function to get timeseries data from a Hilltop server.

#' Takes a valid Hilltop server endpoint, a site name, a measurement name  and
#' optional from, to or timeIntervaland arguments, an optional timeseries type
#' argument and returns a dataframe of the data for that site and measurement
#' for the requested time range from the endpoint. For standard time series,
#' statistics can be obtained, to request these a method, interval and
#' allignment argument need to be provided.

#' @inheritParams buildDataRequesttUrl
#' @return dataframe A dataframe of the timeseries data.
#' @export

getHilltopData <- function(endpoint, site, measurement, from, to, timeInterval, tsType, alignment, method, interval) {
  # Build the url.
  tempUrl <- buildDataRequesttUrl(endpoint = endpoint,
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
  tempXml <- anyXmlParse(tempUrl)
  # Request the data
  tempDf <- hilltopMeasurement(tempXml)
  # Return the data
  return(tempDf)
}


