#' Get A List of Sites Available From A Hilltop Web Service.
#'
#' Takes an parsed xml document from a Hilltop SiteList request.
#' Returns a dataframe of the available sites and location if available.
#' @inheritParams is.hilltopXml
#' @return A dataframe of available sites (with locations if available).
#' @export
#' @importFrom XML xpathApply xmlName xmlGetAttr xmlChildren xpathSApply xmlValue
#' @importFrom reshape2 dcast
hilltopSiteList <- function(xmldata) {
  stemp <- base::do.call(base::rbind, XML::xpathApply(xmldata, "/HilltopServer/Site", function(node) {
    xp <- "./*"
    site <- XML::xmlGetAttr(node, "Name")
    if(base::length(XML::xmlChildren(node)) < 2) {
      attribute <- "NoLocation"
      value <- "NA"
    } else {
      attribute <- XML::xpathSApply(node, xp, XML::xmlName)
      value <- XML::xpathSApply(node, xp, XML::xmlValue)
    }
    base::data.frame(site, attribute, value, stringsAsFactors = FALSE)
  } ) )
  castsite <- reshape2::dcast(stemp, site ~ attribute, value.var = "value")
  if(!base::is.null(castsite$NoLocation)) {
    castsite <- base::subset(castsite, select = -c(NoLocation) )
  }
  return(castsite)
}

#' Hilltop Data Source Information And Measurements At A Site.
#'
#' Helper function.
#' Takes an xml document from a Hilltop MeasurementList at a Site request.
#' Returns a dataframe of the datasource information and measurements names.
#' @inheritParams is.hilltopXml
#' @return A dataframe of available measurements at a site, including the datasource information.
#' @export
#' @importFrom XML xpathApply xmlName xmlGetAttr xpathSApply xmlValue
#' @importFrom reshape2 dcast
hilltopDsMeasList <- function(xmldata) {
  dstemp <- base::do.call(base::rbind, XML::xpathApply(xmldata, "/HilltopServer/DataSource", function(node) {
    xp <- "./*"
    datasource <- XML::xmlGetAttr(node, "Name")
    type <- XML::xpathSApply(node, "./TSType", xmlValue)
    datasourceid <- base::paste(type, datasource)
    attribute <- XML::xpathSApply(node, xp, XML::xmlName)
    value <- XML::xpathSApply(node, xp, function(x) {
      if(XML::xmlName(x) == "Measurement") {XML::xmlGetAttr(x, "Name") } else {XML::xmlValue(x) }
    } )
    base::data.frame(datasourceid, datasource, attribute, value, stringsAsFactors = FALSE)
  } ) )
  ds <- base::subset(dstemp, attribute != "Measurement")
  meas <- base::subset(dstemp, attribute == "Measurement", select = c("datasourceid", "value") )
  base::colnames(meas) [which(names(meas) == "value") ] <- "MeasurementName"
  castds <- reshape2::dcast(ds, datasourceid + datasource ~ attribute, value.var = "value")
  castds <- base::merge(castds, meas, all = TRUE)
  castds <- base::subset(castds, select= -c(datasourceid) )

  return(castds)
}

#' Hilltop Data Sources And Measurement Information At A Site.
#'
#' Helper function.
#' Takes an xml document from a Hilltop MeasurementList at a Site request.
#' Returns a dataframe of the measurement information and datasources.
#' @inheritParams is.hilltopXml
#' @return A dataframe of measurement information and associated datasources.
#' @export
#' @importFrom XML xpathApply xmlName xmlGetAttr xpathSApply xmlValue
#' @importFrom reshape2 dcast
hilltopMeasInfoList <- function(xmldata) {
  dstemp <- base::do.call(base::rbind, XML::xpathApply(xmldata, "/HilltopServer/DataSource/Measurement", function(node) {
    xp <- "./*"
    datasource <- XML::xpathSApply(node, "..", function(x) {XML::xmlGetAttr(x, "Name") } )
    MeasurementName <- XML::xmlGetAttr(node, "Name")
    measurementid <- base::paste(datasource, MeasurementName)
    attribute <- XML::xpathSApply(node, xp, XML::xmlName)
    value <- XML::xpathSApply(node, xp, XML::xmlValue)
    base::data.frame(measurementid, datasource, MeasurementName, attribute, value, stringsAsFactors = FALSE)
  } ) )
  castmeas <- reshape2::dcast(dstemp, measurementid + datasource + MeasurementName ~ attribute, value.var = "value")
  castmeas <- base::subset(castmeas, select = -c(measurementid) )

  return(castmeas)
}

#' Hilltop Data Source Information And Measurement Information At A Site.
#'
#' Takes an xml document from a Hilltop MeasurementList at a Site request.
#' Returns a dataframe of all of the datasource and measurement information
#' combined.
#' @inheritParams is.hilltopXml
#' @return A dataframe of datasources and measurments at a site with all available datasource and measurement information.
#' @export
hilltopDsMeasListFull <- function(xmldata) {
  t <- hilltopDsMeasList(xmldata)
  m <- hilltopMeasInfoList(xmldata)
  full <- base::merge(t, m, by = c("datasource", "MeasurementName") , all = TRUE)
  return(full)
}

#' Old Function To Get Data SOurce And Measurement Information.
#' Helper function.
#' Takes an xml document from a Hilltop MeasurementList at a Site request.
#' Returns a dataframe of the measurement information and datasource info.
#' @inheritParams is.hilltopXml
#' @return A dataframe of datasources and measurments at a site with all available datasource and measurement information.
#' @export
#' @importFrom XML xpathApply xmlName xmlGetAttr xpathSApply xmlValue
#' @importFrom reshape2 dcast
hilltopMeasInfoListExtra <- function(xmldata) {
  dstemp <- base::do.call(rbind, XML::xpathApply(xmldata, "/HilltopServer/DataSource/Measurement", function(node) {
    xp <- "./*"
    datasource <- XML::xpathSApply(node, "..", function(x) {XML::xmlGetAttr(x, "Name") } )
    MeasurementName <- XML::xmlGetAttr(node, "Name")
    TSType <- XML::xpathSApply(node, "../TSType", XML::xmlValue)
    DataType <- XML::xpathSApply(node, "../DataType", XML::xmlValue)
    Interpolation <- XML::xpathSApply(node, "../Interpolation", XML::xmlValue)
    From <- XML::xpathSApply(node, "../From", XML::xmlValue)
    To <- XML::xpathSApply(node, "../To", XML::xmlValue)
    attribute <- XML::xpathSApply(node, xp, XML::xmlName)
    value <- XML::xpathSApply(node, xp, XML::xmlValue)
    base::data.frame(datasource, MeasurementName, TSType, DataType, Interpolation, From, To, attribute, value, stringsAsFactors = FALSE)
  } ) )
  castmeas <- reshape2::dcast(dstemp, datasource + TSType + DataType + Interpolation + From + To + MeasurementName ~ attribute, value.var = "value")

  return(castmeas)
}
