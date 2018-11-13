#' Multiple sites and measurements to Dataframe.
#'
#' \code{fullGetHilltopData} returns a dataframe of requested measurements at sites.
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
fullGetHilltopData <- function(endpoint, sites, measurements, from, to, option, timeInterval, alignment, method, interval) {
  #option one of WQ, Standard, None default None would be All, but if site has
  #continuous and WQ data then a WQ sample measurement will exist.  Need better
  #logic, use measurement list
  if(missing(option)) {option = "None"}
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
                                         interval = interval)},
                         error=function(err){
                           message(paste("Error retrieving", site, meas, err))
                         })
      #get continuous quality data if option is All or Standard
      if(option %in% c("Standard")) {
        #get quality series, from time has to be a lot earlier than the time series requested
        #take 10 years off the from date
        newFrom <- lubridate::dmy(from) - lubridate::years(10)
        #Convert to text
        qualFrom <- as.character(newFrom, format = "%d/%m/%Y")

        qual <- tryCatch({getHilltopData(endpoint = endpoint,
                                         site = site,
                                         measurement = meas,
                                         from = qualFrom, #no to date so that it grabs all quality data
                                         tsType = "StdQualSeries")})
        #Subset quality frame so only get Time and Value
        qual <- tryCatch(subset(qual, select = c("Time", "Value")))
        #Change Value to QC
        if(exists("qual")) {
          colnames(qual)[which(names(qual) == "Value")] <- "QC"
          #Last qual time
          maxQt <- max(qual$Time)
        }

        #Merge with tempDf by time
        tempDf<-tryCatch({
          base::merge(tempDf, qual, by="Time", all = TRUE)},
          error=function(err) {message(paste("No Quality information, leaving blank"))})
        #If there's a QC column then fill the gaps
        if("QC" %in% colnames(tempDf)) {
          #Subset tempDf at the last time in the qual data frame.
          tempQ <- subset(tempDf, Time <= maxQt)
          tempNoQ <- subset(tempDf, Time > maxQt)
          #After that date there will be no QC
          #Before that date fill down QC
          tempQ <- tidyr::fill(tempQ, "QC")
          #remove blank measurements
          tempQ <- subset(tempQ, !is.na(Site))
          #recombine dataframes
          tempDf <- rbind(tempQ, tempNoQ)

        }

      }

      siteDf <- dplyr::bind_rows(siteDf, tempDf)

    }
    #do stuff before moving to next site
    #get WQ sample metadata if option All or WQ

    if(option %in% c("WQ")) {
      #get WQ sample metadata and attach it to siteDf
      message(paste("Getting WQ Sample metadata from", site))
      #getWQsample metadata
      wqSampDf <- tryCatch({getHilltopData(endpoint = endpoint,
                                           site = site,
                                           measurement = "WQ Sample",
                                           from = from,
                                           to = to,
                                           timeInterval = timeInterval)},
                           error=function(err){
                             message(paste("Error retrieving WQ sample metadata for", site, err))
                           })
      #drop duplicate columns, SIte, Measurement and Units
      if(exists("wqSampDf")) {
        wqSampDf <- subset(wqSampDf, select = -c(Site, Measurement, Units))
      }
      #wqSampDf <- tryCatch(subset(wqSampDf, select = -c(Site, Measurement, Units)))
      #merge water quality sample metadata with measurement data by Time
      siteDf<-tryCatch({
        base::merge(siteDf, wqSampDf, by="Time", all.x=TRUE)
      }, error=function(err) {message(paste("No WQ Sample information, leaving blank"))})
    }
    #attach the siteDF to the resultsDf
    resultsDf <- dplyr::bind_rows(resultsDf, siteDf)
  }
  return(resultsDf)
}
