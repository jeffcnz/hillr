# http://data.hbrc.govt.nz/EnviroData/Emar.hts?Service=Hilltop&Request=GetData&Site=Ngaruroro River at Fernhill&Measurement=Total Nitrogen&From=1/6/2014"

#' Create a url to request a site list from a Hilltop Server.
#'
#' Takes a valid Hilltop server endpoint and returns a url that requests the
#' site list from the endpoint.
#' @inheritParams fixEndpoint
#' @return string A Hilltop url request for a site list.
#' @export
buildSiteListUrl <- function(endpoint) {
  # check whether a '?' is the last charachter of the endpoint, if not add one
  # and check that the endpoint is valid.
  endpoint <- checkFixEndpoint(endpoint)
  # set the service as 'Hilltop'
  service <- "Hilltop"
  # set the request as 'SiteList'
  request <- "SiteList"
  # build the url
  hillUrl <- base::paste0(endpoint,
                    "Service=", service,
                    "&Request=", request)
  # return the url
  return(hillUrl)
}

#' Create a url to request a measurement list for a site from a Hilltop Server.
#'
#' Takes a valid Hilltop server endpoint and a site name and returns a url that
#' requests the measurement list for that site from the endpoint.
#' @inheritParams fixEndpoint
#' @param site string A site that has data available on the server.
#' @return string A Hilltop url request for a measurement list for the site.
#' @export
buildMeasurementListUrl <- function(endpoint, site) {
  # check whether a '?' is the last charachter of the endpoint, if not add one
  # and check that the endpoint is valid.
  endpoint <- checkFixEndpoint(endpoint)
  # set the service as 'Hilltop'
  service <- "Hilltop"
  # set the request as 'SiteList'
  request <- "MeasurementList"
  # build the url
  hillUrl <- base::paste0(endpoint,
                          "Service=", service,
                          "&Request=", request,
                          "&Site=", site)
  # return the url
  return(hillUrl)
}

#' Create a url to request data for a site and measurement from a Hilltop
#' Server.
#'
#' Takes a valid Hilltop server endpoint, a site name, a measurement name  and
#' optional from and to arguments and an optional timeseries type argument and
#' returns a url that requests the data for that site and measurement for the
#' requested time range from the endpoint. For standard time series, statistics
#' can be requested, to request these a method, interval and allignment argument
#' need to be provided.
#' @inheritParams fixEndpoint
#' @inheritParams buildMeasurementListUrl
#' @param measurement string A measurement that has data available for the
#'   requested site on the server.
#' @param from string (optional) The date (or date time combination) that data
#'   is required from (if not provided only the last measurement will be
#'   provided.)
#' @param to string (optional) The date (or date time combination) that data is
#'   required up to (if not provided data will be provided from the from date to
#'   the most recent data available.)
#' @param tsType string (optional) The type of time series being requested.
#'   Default is StdTimeSeries, but quality code data can be obtained using
#'   tsType = StdQualSeries.
#' @param alignment string (optional) A time or keyword that the data should be
#'   aligned to.
#' @param method string (optional) A statistical method to apply to the data.
#'   The options are Interpolate, Average, Total, Moving Average and EP.
#'   Interpolate - Provide data aligned with the requested interval. Average -
#'   Provide the average of the measurement across the interval. Total - Provide
#'   the total in the interval. Moving Average - Provide the moving average in
#'   the interval, timestamped to half of the interval. EP - Provide the
#'   exceedence percentile based on the whole available dataset.
#' @param interval string (optional) The interval for the statistic to be
#'   computed over.
#'
#' @return string A Hilltop url request for data for the site and measurement in
#'   the time period for the site.
#'
#' @export
buildDataRequesttUrl <- function(endpoint, site, measurement, from, to, tsType, alignment, method, interval) {
  # check whether a '?' is the last charachter of the endpoint, if not add one
  # and check that the endpoint is valid.
  endpoint <- checkFixEndpoint(endpoint)

  # set the service as 'Hilltop'
  service <- "Hilltop"

  # set the request as 'SiteList'
  request <- "GetData"

  #Check that a site name has been provided.
  if(missing(site)) {stop("No site name provided.")}

  #Check that a measurement name has been provided.
  if(missing(measurement)) {stop("No measurement name provided")}

  #Check whether a from date has been provided in the correct format and create an appropriate request string.
  if(missing(from)) {
    #Omit the from key value pair from the request.
    fromStr <- ""
  } else {
    #TO DO - Check that the date format is acceptable.
    #build the from key value pair
    fromStr <- paste0("&From=", from)
  }

  #Check whether a to date has been provided in the correct format and create an appropriate request string.
  if(missing(to)) {
    #Omit the from key value pair from the request.
    toStr <- ""
  } else {
    #TO DO - Check that the date format is acceptable.
    #TO DO - Check that the to date is after the from date.
    #build the to key value pair
    toStr <- paste0("&To=", to)
  }

  #Check whether a tsType has been provided in the correct format and create an appropriate request string.
  if(missing(tsType)) {
    #Omit the from key value pair from the request.
    tsStr <- ""
  } else {
    #Check that the tsType matches allowed values.
    allowedTsTypes <- c("StdTimeSeries", "StdQualSeries")
    if(tsType %in% allowedTsTypes) {
      #build the tsType key value pair
      tsStr <- paste0("&tsType=", tsType)
    } else {
      #Exit and indicate why the error occurred.
      stop(paste("Invalid tsType provided.  Allowed values are ", paste(allowedTsTypes, collapse = ", "),"."))
    }
  }

  #Check whether an alignment has been provided in the correct format and create an appropriate request string.
  if(missing(alignment)) {
    #Omit the from key value pair from the request.
    alignmentStr <- ""
  } else {
    #TO DO - Check that the alignment matches allowed values.
    alignmentStr <- paste0("&Alignment=", alignment)
  }

  #Check whether a valid method has been provided and create an appropriate request string.
  if(missing(method)) {
    #Omit the from key value pair from the request.
    methodStr <- ""
  } else {
    #Check that the tsType matches allowed values.
    allowedMethods <- c("Interpolate", "Average", "Total", "Moving Average", "EP")
    if(method %in% allowedMethods) {
      #build the tsType key value pair
      methodStr <- paste0("&Method=", method)
    } else {
      #Exit and indicate why the error occurred.
      stop(paste("Invalid method provided.  Allowed values are ", paste(allowedMethods, collapse = ", "),"."))
    }
  }

  #Check whether an interval has been provided in the correct format and create an appropriate request string.
  if(missing(interval)) {
    #Omit the from key value pair from the request.
    intervalStr <- ""
  } else {
    #TO DO - Check that the interval matches allowed values.
    intervalStr <- paste0("&Interval=", interval)
  }

  # build the url
  hillUrl <- base::paste0(endpoint,
                          "Service=", service,
                          "&Request=", request,
                          "&Site=", site,
                          "&Measurement=", measurement,
                          fromStr,
                          toStr,
                          tsStr,
                          alignmentStr,
                          methodStr,
                          intervalStr)
  # return the url
  return(hillUrl)
}


