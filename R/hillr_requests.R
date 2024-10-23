#' General function to get timeseries data from a Hilltop server.
#'
#' \code{getHilltopData} retrieves time series data from a Hilltop server.
#'
#' Takes a valid Hilltop server endpoint, a site name, a measurement name  and
#' optional from, to or timeIntervaland arguments, an optional timeseries type
#' argument and returns a dataframe of the data for that site and measurement
#' for the requested time range from the endpoint. For standard time series,
#' statistics can be obtained, to request these a method, interval and
#' allignment argument need to be provided.
#' If statistics are requested the amount of gap in the data can be specified
#' as an interval.

#' @inheritParams buildDataRequestUrl
#'
#' @return dataframe A dataframe of the timeseries data.
#'
#' @export

getHilltopData <- function(endpoint,
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
                                  interval = interval,
                                  gapTolerance = gapTolerance,
                                  showFinal = showFinal,
                                  dateOnly = dateOnly,
                                  showQuality=showQuality)
  # Parse the XML
  dataXml <- tryCatch({hillXmlParse(dataUrl)}, error = function(err) {stop(err)})
  # Check for errors

  # Get the Hilltop data type
  hilltopDataType <- hillXmlDataType(dataXml)

  # Check the data type of the XML and process accordingly
  if (hilltopDataType %in% c("SimpleTimeSeries", "WQData", "AirQuality")) {
    # Request the data
    dataDf <- hilltopMeasurement(dataXml)
    # Handle the Extrema Request
    if(!is.null(method)){
      if(method == "Extrema") {
        colnames(dataDf) <- c('Time', 'Minimum', 'Mean', 'Maximum', 'Missing', 'Time of Minimum', 'Time of Maximum', 'Site', 'Measurement', 'Units')
      }
    }

    # Handle the Quality Code for continuous measurements being provided diffferently to discrete.
    # Continuous Data Quality Codes are provided in Column Q1, WQ Quality codes are in Field QualityCode
    if("Q1" %in% colnames(dataDf)) {
      # Change field name Q1 to QualityCode
      names(dataDf)[names(dataDf) == 'Q1'] <- "QualityCode"
    }
  } else if (hilltopDataType == 'DepthProfile') {
    # Get the depth profile data
    # First a different url is needed with anything after the to keyword removed.
    if(!all(sapply(list(timeInterval,
                       tsType,
                       alignment,
                       method,
                       interval,
                       gapTolerance,
                       showFinal,
                       dateOnly,
                       showQuality), is.null))) {
      dataUrl <- buildDataRequestUrl(endpoint = endpoint,
                                     site = site,
                                     measurement = measurement,
                                     from = from,
                                     to = to)
    }
    # Needs tidying up as is using url and XML2
    dataDf <- hilltopDepthProfile(dataUrl)

  } else dataDf <- NULL

  if(is.null(dataDf)) {stop("Error retrieving data")}


  # Return the data
  return(dataDf)
}

#' General funtion to get a site list from a Hilltop server.
#'
#' \code{getHilltopSites} returns a list of sites.
#'
#' Takes a valid hilltop endpoint, and returns a list of available sites
#' available from the endpoint.
#'
#' @inheritParams buildSiteListUrl
#'
#' @return A dataframe of site names
#'
#' @export
getHilltopSites <- function(endpoint, measurement, location) {
  # Build the url.
  sitesUrl <- buildSiteListUrl(endpoint = endpoint, measurement=measurement, location=location)
  # Parse the XML
  sitesXml <- hillXmlParse(sitesUrl)
  #Check for errors

  # Request the data
  sitesDf <- hilltopSiteList(sitesXml)
  # Return the data
  return(sitesDf)
}

#' General funtion to get a measuremnt list for a site from a Hilltop server.
#'
#' \code{getHilltopMeasurements} returns the available measurements at a site.
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
  measurementsXml <- hillXmlParse(measurementsUrl)
  # Check for errors

  # Request the data
  measurementsDf <- hilltopDsMeasListFull(measurementsXml)
  # Return the data
  return(measurementsDf)
}

#' General function to get ensemblestats data from a Hilltop server.
#'
#' \code{getHilltopEnsembleStats} retrieves time series data from a Hilltop server.
#'
#' Takes a valid Hilltop server endpoint, a site name, a measurement name  and
#' optional from, to or timeIntervaland arguments, a Statistic and
#' optional upper and lower percentile arguments and byYear argument.
#' Returns a dataframe of the relevant Ensemble Statistics for that site and measurement
#' for the requested time range from the endpoint.

#' @inheritParams buildEnsembleStatsRequestUrl
#'
#' @param byYear boolean A flag to indicate whether you want ensemble stats for each year.
#'   Note: If true then the returned data will be for full years
#'   based on the from year and to year where data is available
#'
#' @return dataframe A dataframe of the ensemble stats.
#'
#' @export
#'
#' @importFrom lubridate dmy year now ymd
#'
#' @importFrom dplyr bind_rows

getHilltopEnsembleStats <- function(endpoint,
                             site,
                             measurement,
                             from=NULL,
                             to=NULL,
                             timeInterval=NULL,
                             statistic="MonthlyPDF",
                             lowerPercentile=NULL,
                             upperPercentile=NULL,
                             byYear=FALSE) {
  if(!byYear) {
    # Ensemble data over the full time range
    # Build the url.
    eStatsUrl <- buildEnsembleStatsRequestUrl(endpoint = endpoint,
                                              site = site,
                                              measurement = measurement,
                                              from = from,
                                              to = to,
                                              timeInterval = timeInterval,
                                              statistic = statistic,
                                              lowerPercentile = lowerPercentile,
                                              upperPercentile = upperPercentile
    )


    # Parse the XML
    statsXml <- tryCatch({hillXmlParse(eStatsUrl)}, error = function(err) {stop(err)})
    # Check for errors

    #Get the stats
    statsData <- hilltopEnsembleStatFull(statsXml)
  } else {


    # Get the start year from the from date (default 2000)
    if(is.null(from)) {
      startYear <- 2000
    } else {
      startYear <- lubridate::year(lubridate::dmy(from))
    }

    # Get the end year from the end date (default now)
    if(is.null(to)) {
      endYear <- lubridate::year(lubridate::now())
    } else {
      endYear <- lubridate::year(lubridate::dmy(from))
    }

    #Initiate dataframe for returning the results
    statsData <- data.frame(Site=character(),
                            stringsAsFactors=FALSE)

    # For each year request the year of data and append to output

    for(yr in startYear:endYear) {
      message(paste("Retrieving", site, measurement, yr))

      #Initiate dataframe for holding the results from each year
      yrEnsemble <- data.frame(Site=character(),
                              stringsAsFactors=FALSE)

      yrEStatsUrl <- buildEnsembleStatsRequestUrl(endpoint = endpoint,
                                                site = site,
                                                measurement = measurement,
                                                from = base::paste0("1/1/",yr),
                                                to = base::paste0("31/12/",yr),
                                                timeInterval = NULL,
                                                statistic = statistic,
                                                lowerPercentile = lowerPercentile,
                                                upperPercentile = upperPercentile
      )


      # Parse the XML
      yrStatsXml <- tryCatch({hillXmlParse(yrEStatsUrl)},
                             error = function(err) {message(paste("Error retrieving", site, measurement, yr, err))})
      # Check for errors

      if(!is.null(yrStatsXml)) {
        #Get the stats
        yrEnsemble <- tryCatch({hilltopEnsembleStatFull(yrStatsXml)},
                               error = function(err) {message(paste("Error retrieving", site, measurement, yr, err))})

        #Append results
        #attach the yrEnsemble to the statsData
        if(!is.null(yrEnsemble)) {
          if(base::nrow(yrEnsemble)>0) {
            # Add a year column
            yrEnsemble$Year <- yr

            #Append results to master data frame.
            statsData <- dplyr::bind_rows(statsData, yrEnsemble)
            } else {next}
        } else {next}

      }
    }



    # Calculate the date from the periodID and the year
    if(base::grepl("Daily", statistic, fixed = TRUE)) {
      statsData$Date <- lubridate::dmy(base::paste0(statsData$periodID, "-", statsData$Year))
    } else {
      if(base::grepl("Monthly", statistic, fixed = TRUE)) {
        statsData$Date <- lubridate::dmy(base::paste0("1-", statsData$periodID, "-", statsData$Year))
      }
    }




    # Return the results
    return(statsData)
  }




  if(is.null(statsData)) {stop("Error retrieving data")}


  # Return the data
  return(statsData)
}
