#' Hilltop Ensemble Statistics Background.
#'
#' Helper Function that takes the parsed xml from an Ensemble Statistics request.
#' Returns a single line dataframe of the Statistics background information This
#' needs to be combined with the stats themselves to get a full dataframe.
#' @export
#' @importFrom XML xpathApply xpathSApply xmlName xmlGetAttr xmlValue
hilltopEnsembleStatBkgnd <- function(dataxml){
  bgtemp <- base::do.call(base::rbind, XML::xpathApply(dataxml, "/HilltopServer", function(node) {
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
#' @export
#' @importFrom XML xpathApply xpathSApply xmlName xmlGetAttr xmlValue
hilltopEnsembleStatByTimePeriod <- function(dataxml){
  estatperiod <- period(dataxml)

  #Get the stats for each time period entry
  Statistic <- XML::xpathApply(dataxml, "/HilltopServer/Statistic",  XML::xmlValue)
  estat <- base::do.call(base::rbind, XML::xpathApply(dataxml, base::paste("/HilltopServer/",estatperiod, sep = ""), function(node) {
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
#' @export
hilltopEnsembleStatFull <- function(dataxml) {
  bg <- hilltopEnsembleStatBkgnd(dataxml)
  pe <- hilltopEnsembleStatByTimePeriod(dataxml)
  full <- base::merge(bg, pe, by = c("Statistic"), all = TRUE)
}
