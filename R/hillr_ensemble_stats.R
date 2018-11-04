#' Hilltop Ensemble Statistics Background.
#'
#' Helper Function that takes the parsed xml from an Ensemble Statistics request.
#' Returns a single line dataframe of the Statistics background information This
#' needs to be combined with the stats themselves to get a full dataframe.
#' @inheritParams is.hilltopXml
#' @return A single line dataframe of the statistics background information.
#' @export
#' @importFrom XML xpathApply xpathSApply xmlName xmlGetAttr xmlValue
hilltopEnsembleStatBkgnd <- function(xmldata){
  bgtemp <- base::do.call(base::rbind, XML::xpathApply(xmldata, "/HilltopServer", function(node) {
    xp <- "./*"
    attribute <- XML::xpathSApply(node, xp, XML::xmlName)
    value <- XML::xpathSApply(node, xp, function(x){
      if(XML::xmlName(x) %in% c("Hour", "Day", "Month")){XML::xmlGetAttr(x,"Name")}else{XML::xmlValue(x)}
    })
    base::data.frame(attribute, value, stringsAsFactors = FALSE)
  }))
  bgtemp <- base::subset(bgtemp, !attribute %in% c("Hour", "Day", "Month"))

  fintemp <- stats::setNames(base::data.frame(t(bgtemp[,-1])), bgtemp[,1])

  return(fintemp)
}

#' Hilltop Ensemble Statistics By Time Period.
#'
#' Helper function that takes parsed xml from an EnsembleStats Request. Returns
#' the statistics for each time period (depending whether hourly, monthly or
#' annual stats).
#' @inheritParams is.hilltopXml
#' @return A dataframe of the of the summary statistics for each time period.
#' @export
#' @importFrom XML xpathApply xpathSApply xmlName xmlGetAttr xmlValue
hilltopEnsembleStatByTimePeriod <- function(xmldata){
  estatperiod <- period(xmldata)

  #Get the stats for each time period entry
  Statistic <- XML::xpathApply(xmldata, "/HilltopServer/Statistic",  XML::xmlValue)
  estat <- base::do.call(base::rbind, XML::xpathApply(xmldata, base::paste("/HilltopServer/",estatperiod, sep = ""), function(node) {
    xp <- "./*"
    periodID <- XML::xpathSApply(node, ".", function(x){XML::xmlGetAttr(x, "Name")})

    attribute <- XML::xpathSApply(node, xp, XML::xmlName)
    value <- XML::xpathSApply(node, xp, XML::xmlValue)
    base::data.frame(periodID, attribute, value, stringsAsFactors = FALSE)
  }))

  estest <- reshape2::dcast(estat, periodID ~ attribute, value.var = "value")
  estest$Statistic <- Statistic

  return(estest)
}

#' Hilltop Ensemble Statistics By Time Period, With Summary Information.
#'
#' Takes the parsed xml from an EnsembleStats Request. Returns a dataframe of the
#' statistics for the period, along with the background information such as site
#' measurement units etc.
#' @inheritParams is.hilltopXml
#' @return A dataframe of the ensemble statistics along with the background metadata.
#' @export
hilltopEnsembleStatFull <- function(xmldata) {
  bg <- hilltopEnsembleStatBkgnd(xmldata)
  pe <- hilltopEnsembleStatByTimePeriod(xmldata)
  full <- base::merge(bg, pe, by = c("Statistic"), all = TRUE)
}
