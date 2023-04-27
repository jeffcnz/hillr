#' Create a url to request a site list from a Hilltop Server.
#'
#' \code{buildSiteListUrl} returns a url to request a site list
#'
#' Takes a valid Hilltop server endpoint and returns a url that requests the
#' site list from the endpoint.
#'
#' @inheritParams fixEndpoint
#'
#' @param location logical (optional) whether the Lat Long for the sites is
#'   returned
#'
#' @return string A Hilltop url request for a site list.
#'
#' @export
#'
#' @importFrom stringr str_replace_all
#'
buildSiteListUrl <- function(endpoint, location) {
  if(missing(location)) {location <- TRUE}
  # check whether a '?' is the last charachter of the endpoint, if not add one
  # and check that the endpoint is valid.
  endpoint <- checkFixEndpoint(endpoint)
  # set the service as 'Hilltop'
  service <- "Hilltop"
  # set the request as 'SiteList'
  request <- "SiteList"
  # set the location request string
  locStr <- if(location) {"&Location=LatLong"} else {""}
  # build the url
  hillUrl <- base::paste0(endpoint,
                    "Service=", service,
                    "&Request=", request,
                    locStr)
  # replace spaces with %20
  hillUrl <- utils::URLencode(hillUrl)
  # return the url
  return(hillUrl)
}

#' Create a url to request a measurement list for a site from a Hilltop Server.
#'
#' \code{buildMeasurementListUrl} returns a url to request a measuremnt list for a site.
#'
#' Takes a valid Hilltop server endpoint and a site name and returns a url that
#' requests the measurement list for that site from the endpoint.
#'
#' @inheritParams fixEndpoint
#'
#' @param site string A site that has data available on the server.
#'
#' @return string A Hilltop url request for a measurement list for the site.
#'
#' @export
#'
#' @importFrom stringr str_replace_all
#'
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
  # replace spaces with %20
  hillUrl <- utils::URLencode(hillUrl)
  # return the url
  return(hillUrl)
}

#' Create a url to request data for a site and measurement.
#'
#' \code{buildDataRequestUrl} returns a url to request data for a site and measurement
#'
#' Takes a valid Hilltop server endpoint, a site name, a measurement name  and
#' optional from, to or timeIntervaland arguments, an optional timeseries type
#' argument and returns a url that requests the data for that site and
#' measurement for the requested time range from the endpoint. For standard time
#' series, statistics can be requested, to request these a method, interval and
#' allignment argument need to be provided.
#'
#' @inheritParams fixEndpoint
#'
#' @inheritParams buildMeasurementListUrl
#'
#' @param measurement string A measurement that has data available for the
#'   requested site on the server.
#'
#' @param from string (optional) The date (or date time combination) that data
#'   is required from (if not provided only the last measurement will be
#'   provided.)
#'
#' @param to string (optional) The date (or date time combination) that data is
#'   required up to (if not provided data will be provided from the from date to
#'   the most recent data available.)
#'
#' @param timeInterval string (optional) A time interval that data is requested
#'   over, either of the form P1D, or P3M, or fromDateTime/toDateTime.
#'
#' @param tsType string (optional) The type of time series being requested.
#'   Default is StdTimeSeries, but quality code data can be obtained using
#'   tsType = StdQualSeries.
#'
#' @param alignment string (optional) A time or keyword that the data should be
#'   aligned to.
#'
#' @param method string (optional) A statistical method to apply to the data.
#'   The options are Interpolate, Average, Total, Moving Average, EP and Extrema.
#'   Interpolate - Provide data aligned with the requested interval.
#'   Average - Provide the average of the measurement across the interval.
#'   Total - Provide the total in the interval.
#'   Moving Average - Provide the moving average in the interval, timestamped to half of the interval.
#'   EP - Provide the exceedence percentile based on the whole available dataset.
#'   Extrema - Provide the Min, Mean, Max, Amount of missing record (s) ,Time of Min and Time of Max over the interval.
#'
#' @param interval string (optional) The interval for the statistic to be
#'   computed over.  eg 1 Day, 1 Month etc.
#'
#' @param gapTolerance string (optional) The gap tolerance interval for the statistic.
#'   Default is no gap tolerance, ie if there are gaps in the interval a 0 result is returned.
#'   If you want to ignore gaps completely then set the gap tolerance to the same as the Interval.
#'   Accepts either an interval such as 1 Day, or 1 Month, or the word Interval
#'   if you want to use the same period as provided in the interval parameter.
#'
#' @param showFinal string (optional) Options are Yes, or No, default is Yes.
#'   If Yes then if the data stops part way through the last interval then the total in the interval is provided
#'
#' @return string A Hilltop url request for data for the site and measurement in
#'   the time period for the site.
#'
#' @export
#'
#' @importFrom stringr str_replace_all
#'
buildDataRequestUrl <- function(endpoint,
                                site,
                                measurement,
                                from,
                                to,
                                timeInterval,
                                tsType,
                                alignment,
                                method,
                                interval,
                                gapTolerance,
                                showFinal) {
  # check whether a '?' is the last character of the endpoint, if not add one
  # and check that the endpoint is valid.
  endpoint <- checkFixEndpoint(endpoint)

  # set the service as 'Hilltop'
  service <- "Hilltop"

  # set the request as 'GetData'
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
    fromStr <- base::paste0("&From=", from)
  }

  #Check whether a to date has been provided in the correct format and create an appropriate request string.
  if(missing(to)) {
    #Omit the from key value pair from the request.
    toStr <- ""
  } else {
    #TO DO - Check that the date format is acceptable.
    #TO DO - Check that the to date is after the from date.
    #build the to key value pair
    toStr <- base::paste0("&To=", to)
  }

  #Check whether a timeInterval date has been provided in the correct format and create an appropriate request string.
  if(missing(timeInterval)) {
    #Omit the from key value pair from the request.
    timeIntStr <- ""
  } else {
    #TO DO - Check that the date format is acceptable.
    #TO DO - Check that the to date is after the from date.
    #build the to key value pair
    timeIntStr <- base::paste0("&TimeInterval=", timeInterval)
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
      tsStr <- base::paste0("&tsType=", tsType)
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
    alignmentStr <- base::paste0("&Alignment=", alignment)
  }

  #Check whether a valid method has been provided and create an appropriate request string.
  if(missing(method)) {
    #Omit the from key value pair from the request.
    methodStr <- ""
  } else {
    #Check that the tsType matches allowed values.
    allowedMethods <- c("Interpolate", "Average", "Total", "Moving Average", "EP", "Extrema")
    if(method %in% allowedMethods) {
      #build the tsType key value pair
      methodStr <- paste0("&Method=", method)
    } else {
      #Exit and indicate why the error occurred.
      stop(base::paste("Invalid method provided.  Allowed values are ", paste(allowedMethods, collapse = ", "),"."))
    }
  }

  #Check whether an interval has been provided in the correct format and create an appropriate request string.
  if(missing(interval)) {
    #Omit the from key value pair from the request.
    intervalStr <- ""
  } else {
    #TO DO - Check that the interval matches allowed values.
    intervalStr <- base::paste0("&Interval=", interval)
  }

  #Check whether a gap tolerance has been provided in the correct format and create an appropriate request string.
  if(missing(gapTolerance)) {
    #Omit the from key value pair from the request.
    gapToleranceStr <- ""
  } else if(gapTolerance == "Interval") {
    gapToleranceStr <- base::paste0("&GapTolerance=", interval)
  } else {
    #TO DO - Check that the gapTolerance interval matches allowed values.
    gapToleranceStr <- base::paste0("&GapTolerance=", gapTolerance)
  }

  #Check whether an showFinal parameter has been provided in the correct format and create an appropriate request string.
  if(missing(showFinal)) {
    #Default is to set showFinal as Yes.
    showFinalStr <- "&ShowFinal=Yes"
  } else {
    #TO DO - Check that the parameter matches allowed values.
    showFinalStr <- base::paste0("&ShowFinal=", showFinal)
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
                          timeIntStr,
                          alignmentStr,
                          methodStr,
                          intervalStr,
                          gapToleranceStr,
                          showFinalStr)
  # replace spaces with %20
  hillUrl <- utils::URLencode(hillUrl)
  # return the url
  return(hillUrl)
}


