#' Create a url to request a site list from a Hilltop Server.
#'
#' \code{buildSiteListUrl} returns a url to request a site list
#'
#' Takes a valid Hilltop server endpoint and returns a url that requests the
#' site list from the endpoint.
#'
#' @inheritParams fixEndpoint
#'
#' @param measurement string (optional) a measurement name that a site must have available.
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
buildSiteListUrl <- function(endpoint, measurement, location) {
  # Handle for optional parameters
  if(missing(measurement)) {measurement <- ""}
  if(missing(location)) {location <- TRUE}
  # check whether a '?' is the last character of the endpoint, if not add one
  # and check that the endpoint is valid.
  endpoint <- checkFixEndpoint(endpoint)
  # set the service as 'Hilltop'
  service <- "Hilltop"
  # set the request as 'SiteList'
  request <- "SiteList"
  # set the measurement request string
  measStr <- if(measurement != "") {
                      paste0("&Measurement=", measurement)
                      } else {""}
  # set the location request string
  locStr <- if(location) {"&Location=LatLong"} else {""}
  # build the url
  hillUrl <- base::paste0(endpoint,
                    "Service=", service,
                    "&Request=", request,
                    measStr,
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
  # check whether a '?' is the last character of the endpoint, if not add one
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
#' @param dateOnly string(optional) Whether the returned timestamp should be the date, with no time.
#'   Default is date and time. Allowed values are Yes and No (default No)
#'   This is useful when working with intervals ove a day or more as the returned value will be the end of the interval,
#'   ie midnight on the day after the last day of the period.  This can be confusing as a monthly total for March
#'   would be timestamped as midnight on 1 April. Setting dateOnly to Yes would return a date of 31 March.
#'
#' @param showFinal string (optional) Options are Yes, or No, default is Yes.
#'   If Yes then if the data stops part way through the last interval then the total in the interval is provided
#'
#' @param showQuality string(optional) Options are Yes or No, default is Yes.
#'   Whether Quality Codes are returned with the data.  If quality codes are available they are returned.
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
                                from=NULL,
                                to=NULL,
                                timeInterval=NULL,
                                tsType=NULL,
                                alignment=NULL,
                                method=NULL,
                                interval=NULL,
                                gapTolerance=NULL,
                                showFinal=NULL,
                                dateOnly=NULL,
                                showQuality="Yes") {
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
  if(is.null(from)) {
    #Omit the from key value pair from the request.
    fromStr <- ""
  } else {
    #TO DO - Check that the date format is acceptable.
    #build the from key value pair
    fromStr <- base::paste0("&From=", from)
  }

  #Check whether a to date has been provided in the correct format and create an appropriate request string.
  if(is.null(to)) {
    #Omit the from key value pair from the request.
    toStr <- ""
  } else {
    #TO DO - Check that the date format is acceptable.
    #TO DO - Check that the to date is after the from date.
    #build the to key value pair
    toStr <- base::paste0("&To=", to)
  }

  #Check whether a timeInterval date has been provided in the correct format and create an appropriate request string.
  if(is.null(timeInterval)) {
    #Omit the from key value pair from the request.
    timeIntStr <- ""
  } else {
    #TO DO - Check that the date format is acceptable.
    #TO DO - Check that the to date is after the from date.
    #build the to key value pair
    timeIntStr <- base::paste0("&TimeInterval=", timeInterval)
  }

  #Check whether a tsType has been provided in the correct format and create an appropriate request string.
  if(is.null(tsType)) {
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
  if(is.null(alignment)) {
    #Omit the from key value pair from the request.
    alignmentStr <- ""
  } else {
    #TO DO - Check that the alignment matches allowed values.
    alignmentStr <- base::paste0("&Alignment=", alignment)
  }

  #Check whether a valid method has been provided and create an appropriate request string.
  if(is.null(method)) {
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
  if(is.null(interval)) {
    #Omit the from key value pair from the request.
    intervalStr <- ""
  } else {
    #TO DO - Check that the interval matches allowed values.
    intervalStr <- base::paste0("&Interval=", interval)
  }

  #Check whether a gap tolerance has been provided in the correct format and create an appropriate request string.
  if(is.null(gapTolerance)) {
    #Omit the from key value pair from the request.
    gapToleranceStr <- ""
  } else if(gapTolerance == "Interval") {
    gapToleranceStr <- base::paste0("&GapTolerance=", interval)
  } else {
    #TO DO - Check that the gapTolerance interval matches allowed values.
    gapToleranceStr <- base::paste0("&GapTolerance=", gapTolerance)
  }

  #Check whether an showFinal parameter has been provided in the correct format and create an appropriate request string.
  if(is.null(showFinal)) {
    #Default is to set showFinal as Yes.
    showFinalStr <- "&ShowFinal=Yes"
  } else {
    #Check that the argument matches allowed values.
    allowedOptions <- c("Yes", "No")
    if(showFinal %in% allowedOptions) {
      #build the showFinal key value pair
      showFinalStr <- base::paste0("&ShowFinal=", showFinal)
    } else {
      #Exit and indicate why the error occurred.
      stop(base::paste("Invalid showFinal argument provided.  Allowed values are ", paste(allowedOptions, collapse = ", "),"."))
    }
  }

  #Check whether an dateOnly parameter has been provided in the correct format and create an appropriate request string.
  if(is.null(dateOnly)) {
    #Default is to omit.
    dateOnlyStr <- ""
  } else {
    #Check that the argument matches allowed values.
    dateOnlyOptions <- c("Yes", "No")
    if(dateOnly %in% dateOnlyOptions) {
      #build the dateOnly key value pair
      dateOnlyStr <- base::paste0("&DateOnly=", dateOnly)
    } else {
      #Exit and indicate why the error occurred.
      stop(base::paste("Invalid dateOnly argument provided.  Allowed values are ", paste(dateOnlyOptions, collapse = ", "),"."))
    }

  }

  #Check whether an showQuality parameter has been provided in the correct format and create an appropriate request string.
  if(missing(showQuality)){
    # If no showQual;ity parameter, then default to showing quality code
    showQualityStr <- "&ShowQuality=Yes"
  } else {
    #Check that the argument matches allowed values.
    showQualityOptions <- c("Yes", "No")
    if(showQuality %in% showQualityOptions) {
      #build the dateOnly key value pair
      showQualityStr <- base::paste0("&ShowQuality=", showQuality)
    } else {
      #Exit and indicate why the error occurred.
      stop(base::paste("Invalid showQuality argument provided.  Allowed values are ", paste(showQualityOptions, collapse = ", "),"."))
    }
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
                          showFinalStr,
                          dateOnlyStr,
                          showQualityStr)
  # replace spaces with %20
  hillUrl <- utils::URLencode(hillUrl)
  # return the url
  return(hillUrl)
}

#' Create a url to request ensemble statistics for a site and measurement.
#'
#' \code{buildEnsembleStatsRequestUrl} returns a url to request ensemble statisticss for a site and measurement
#'
#' Takes a valid Hilltop server endpoint, a site name, a measurement name  and
#' optional from, to or timeIntervaland arguments, and a Statistic
#' argument and returns a url that requests the ensemble statistics for that site and
#' measurement for the requested time range from the endpoint.
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
#' @param statistic string The statistic function to use.  Options are
#'   HourlyExtrema, DailyExtrema, MonthlyExtrema,
#'   MeanHourlyExtrema, MeanDailyExtrema, MeanMonthlyExtrema,
#'   ExtremeHourlyMean, ExtremeDailyMean, ExtremeMonthlyMean,
#'   HourlyPDF, DailyPDF, MonthlyPDF
#'
#' @param lowerPercentile string (optional) The lower percentile value to return from a PDF request.
#'
#' @param upperPercentile string (optional) The upper percentile value to return from a PDF request.
#'
#' @return string A Hilltop url request for data for the site and measurement in
#'   the time period for the site.
#'
#' @export
#'
#' @importFrom stringr str_replace_all
#'
buildEnsembleStatsRequestUrl <- function(endpoint,
                                site,
                                measurement,
                                from=NULL,
                                to=NULL,
                                timeInterval=NULL,
                                statistic="MonthlyPDF",
                                lowerPercentile=NULL,
                                upperPercentile=NULL) {
  # check whether a '?' is the last character of the endpoint, if not add one
  # and check that the endpoint is valid.
  endpoint <- checkFixEndpoint(endpoint)

  # set the service as 'Hilltop'
  service <- "Hilltop"

  # set the request as 'EnsembleStats'
  request <- "EnsembleStats"

  #Check that a site name has been provided.
  if(missing(site)) {stop("No site name provided.")}

  #Check that a measurement name has been provided.
  if(missing(measurement)) {stop("No measurement name provided")}

  #Check whether a from date has been provided in the correct format and create an appropriate request string.
  if(is.null(from)) {
    #Omit the from key value pair from the request.
    fromStr <- ""
  } else {
    #TO DO - Check that the date format is acceptable.
    #build the from key value pair
    fromStr <- base::paste0("&From=", from)
  }

  #Check whether a to date has been provided in the correct format and create an appropriate request string.
  if(is.null(to)) {
    #Omit the from key value pair from the request.
    toStr <- ""
  } else {
    #TO DO - Check that the date format is acceptable.
    #TO DO - Check that the to date is after the from date.
    #build the to key value pair
    toStr <- base::paste0("&To=", to)
  }

  #Check whether a timeInterval date has been provided in the correct format and create an appropriate request string.
  if(is.null(timeInterval)) {
    #Omit the from key value pair from the request.
    timeIntStr <- ""
  } else {
    #TO DO - Check that the date format is acceptable.
    #TO DO - Check that the to date is after the from date.
    #build the to key value pair
    timeIntStr <- base::paste0("&TimeInterval=", timeInterval)
  }


  #Check whether a valid statistic has been provided and create an appropriate request string.
  if(is.null(statistic)) {
    stop("No statistic provided.")
  } else {
    #Check that the statistic matches allowed values.
    allowedStats <- c("HourlyExtrema", "DailyExtrema", "MonthlyExtrema",
                      "MeanHourlyExtrema", "MeanDailyExtrema", "MeanMonthlyExtrema",
                      "ExtremeHourlyMean", "ExtremeDailyMean", "ExtremeMonthlyMean",
                      "HourlyPDF", "DailyPDF", "MonthlyPDF")
    if(statistic %in% allowedStats) {
      #build the statistics key value pair
      statsStr <- paste0("&Statistic=", statistic)
    } else {
      #Exit and indicate why the error occurred.
      stop(base::paste("Invalid statistic provided.  Allowed values are ", paste(allowedStats, collapse = ", "),"."))
    }
  }

  #Check whether a lowerPercentile has been provided in the correct format and create an appropriate request string.
  if(is.null(lowerPercentile)) {
    #Omit the from key value pair from the request.
    lowerPercentileStr <- ""
  } else {
    #TO DO - Check that the string is acceptable (a number between 1 and 99).
    #build the from key value pair
    lowerPercentileStr <- base::paste0("&LowerPercentile=", lowerPercentile)
  }

  #Check whether an upperPercentile has been provided in the correct format and create an appropriate request string.
  if(is.null(upperPercentile)) {
    #Omit the from key value pair from the request.
    upperPercentileStr <- ""
  } else {
    #TO DO - Check that the string is acceptable (a number between 1 and 99).
    #TO DO - Check that the lowerPercentile is less than the upperPercentile.
    #build the to key value pair
    upperPercentileStr <- base::paste0("&UpperPercentile=", upperPercentile)
  }






  # build the url
  hillUrl <- base::paste0(endpoint,
                          "Service=", service,
                          "&Request=", request,
                          "&Site=", site,
                          "&Measurement=", measurement,
                          fromStr,
                          toStr,
                          timeIntStr,
                          statsStr,
                          lowerPercentileStr,
                          upperPercentileStr)
  # replace spaces with %20
  hillUrl <- utils::URLencode(hillUrl)
  # return the url
  return(hillUrl)
}
