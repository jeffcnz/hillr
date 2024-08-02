#' Multiple sites and measurements data to Dataframe.
#'
#' \code{fullGetHilltopData} returns a dataframe of requested measurements at
#' sites.
#'
#' Takes a valid Hilltop server endpoint, a vector of site names, a vector of
#' measurement names and optional from, to or timeIntervaland arguments, an
#' optional timeseries type argument, and an option argument identifying the
#' type of metadata to be returned in the output dataframe. The function returns
#' a dataframe of the data for the sites and measurements for the requested time
#' range from the endpoint. For standard time series, statistics can be
#' obtained, to request these a method, interval and allignment argument need to
#' be provided.  The option argument provided determines what metadata is
#' returned.
#'
#' If no option is provided, or the option is "None" then no additional metadata
#' will be provided, the fuction works like getHilltopData, but allows multiple
#' sites and or measurements to be provided.
#'
#' If the option is "WQ" then water quality sample metadata will be provided in
#' the output where it is available.
#'
#' If the option is "Standard" then the quality code information for standard
#' (continuous) is provided with the data.
#'
#' Note: Currently if measurements of mixed types are requested then only one
#' type of metadata can be associated with the results (ie Water Quality, or
#' Continuous). If both types are required with full metadata then seperate
#' requests will be required.
#'
#'
#' @inheritParams getHilltopData
#'
#' @param sites string vector The sites that data is to be requested from.
#'
#' @param measurements string vector The measurements that are to be requested.
#'
#' @param option string One of WQ, Standard or None.
#'
#' @return dataframe of measurements with optional metadata for requested sites.
#'
#' @export
#'
#' @importFrom lubridate dmy year
#'
#' @importFrom tidyr fill
#'
#' @importFrom dplyr bind_rows
#'
fullGetHilltopData <- function(endpoint,
                               sites,
                               measurements,
                               from=NULL,
                               to=NULL,
                               option=NULL,
                               timeInterval=NULL,
                               alignment=NULL,
                               method=NULL,
                               interval=NULL,
                               gapTolerance=NULL,
                               showFinal=NULL,
                               dateOnly=NULL,
                               showQuality="Yes") {
  #option one of WQ, Standard, None default None would be All, but if site has
  #continuous and WQ data then a WQ sample measurement will exist.  Need better
  #logic, use measurement list
  if(is.null(option)) {option = "None"}
  #check option in allowed values

  #check from and to dates provided

  #Initiate dataframe for returning the results
  resultsDf <- data.frame(Site=character(),
                          stringsAsFactors=FALSE)

  #For each site
  for(site in sites) {
    #Initiate dataframe for site data
    siteDf <- data.frame(Site=character(),
                         stringsAsFactors=FALSE)
    #for each measurement
    for(meas in measurements) {
      #get hilltop data for the site measurement
      message(paste("Getting", meas, "from", site))
      tempDf <- tryCatch({getHilltopData(endpoint = endpoint,
                                         site = site,
                                         measurement = meas,
                                         from = from,
                                         to = to,
                                         timeInterval = timeInterval,
                                         alignment = alignment,
                                         method = method,
                                         interval = interval,
                                         gapTolerance=gapTolerance,
                                         showFinal=showFinal,
                                         dateOnly=dateOnly,
                                         showQuality=showQuality)},
                         error=function(err){
                           message(paste("Error retrieving", site, meas, err))
                         })

      siteDf <- dplyr::bind_rows(siteDf, tempDf)

    }
    #do stuff before moving to next site

    #attach the siteDF to the resultsDf
    resultsDf <- dplyr::bind_rows(resultsDf, siteDf)
  }
  return(resultsDf)
}


#' Multiple sites and measurements Ensemble STatistics to Dataframe.
#'
#' \code{fullGetHilltopEnsembleStats} returns a dataframe of requested Ensemble
#' Stats for measurements at sites.
#'
#' Takes a valid Hilltop server endpoint, a vector of site names, a vector of
#' measurement names and optional from, to or timeIntervaland arguments,
#' a Statistic and optional upper and lower percentile arguments and byYear argument.
#' Returns a dataframe of the relevant Ensemble Statistics for the requested
#' sites and measurements for the requested time range from the endpoint.
#'
#' @inheritParams getHilltopEnsembleStats
#'
#' @param sites string vector The sites that data is to be requested from.
#'
#' @param measurements string vector The measurements that are to be requested.
#'
#' @return dataframe of measurements with optional metadata for requested sites.
#'
#' @export
#'
#' @importFrom dplyr bind_rows
#'
fullGetHilltopEnsembleStats <- function(endpoint,
                               sites,
                               measurements,
                               from=NULL,
                               to=NULL,
                               timeInterval=NULL,
                               statistic="MonthlyPDF",
                               lowerPercentile=NULL,
                               upperPercentile=NULL,
                               byYear=FALSE) {
  #check from and to dates provided

  #Initiate dataframe for returning the results
  resultsDf <- data.frame(Site=character(),
                          stringsAsFactors=FALSE)

  #For each site
  for(site in sites) {
    #Initiate dataframe for site data
    siteDf <- data.frame(Site=character(),
                         stringsAsFactors=FALSE)
    #for each measurement
    for(meas in measurements) {
      #get hilltop ensemble stats for the site measurement
      message(paste("Getting", meas, "from", site))
      tempDf <- tryCatch({getHilltopEnsembleStats(endpoint = endpoint,
                                         site = site,
                                         measurement = meas,
                                         from = from,
                                         to = to,
                                         timeInterval = timeInterval,
                                         statistic = statistic,
                                         lowerPercentile = lowerPercentile,
                                         upperPercentile = upperPercentile,
                                         byYear=byYear)},
                         error=function(err){
                           message(paste("Error retrieving", site, meas, err))
                         })

      siteDf <- dplyr::bind_rows(siteDf, tempDf)

    }
    #do stuff before moving to next site

    #attach the siteDF to the resultsDf
    resultsDf <- dplyr::bind_rows(resultsDf, siteDf)
  }
  return(resultsDf)
}
