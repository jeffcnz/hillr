#' Get A List of Sites Available From A Hilltop Web Service.
#'
#' \code{hilltopSiteList} returns a dataframe of sites
#'
#' Takes an parsed xml document from a Hilltop SiteList request. Returns a
#' dataframe of the available sites and location if available.
#'
#' @inheritParams is.hilltopXml
#'
#' @return A dataframe of available sites (with locations if available).
#'
#' @export
#'
#' @importFrom XML xpathApply xmlName xmlGetAttr xmlChildren xpathSApply
#'   xmlValue
#'
#' @importFrom reshape2 dcast
#'
hilltopSiteList <- function(xmldata) {
  #identify the sites and parse the names and locations
  stemp <- base::do.call(base::rbind, XML::xpathApply(xmldata, "/HilltopServer/Site", function(node) {
    xp <- "./*"
    site <- XML::xmlGetAttr(node, "Name")
    #check whether there is location information
    if(base::length(XML::xmlChildren(node)) < 2) {
      #if no location information then set the attribute to "No Location"
      attribute <- "NoLocation"
      value <- "NA"
    } else {
      #if there is site information then extract it
      attribute <- XML::xpathSApply(node, xp, XML::xmlName)
      value <- XML::xpathSApply(node, xp, XML::xmlValue)
    }
    #create a 'long' format dataframe
    base::data.frame(site, attribute, value, stringsAsFactors = FALSE)
  } ) )
  #pivot the dataframe
  castsite <- reshape2::dcast(stemp, site ~ attribute, value.var = "value")
  #remove any sites without location information
  if(!base::is.null(castsite$NoLocation)) {
    castsite <- base::subset(castsite, select = -c(NoLocation) )
  }
  #return the list of sites

  return(castsite)
}

#' Hilltop Data Source Information And Measurements At A Site.
#'
#' \code{hilltopDsMeasList} returns the datasource metadata and measurement names
#'
#' Helper function.
#' Takes an xml document from a Hilltop MeasurementList at a Site request.
#' Returns a dataframe of the datasource information and measurements names.
#'
#' @inheritParams is.hilltopXml
#'
#' @return A dataframe of available measurements at a site, including the datasource information.
#'
#' @export
#'
#' @importFrom XML xpathApply xmlName xmlGetAttr xpathSApply xmlValue
#'
#' @importFrom reshape2 dcast
#'
hilltopDsMeasList <- function(xmldata) {
  #get the datasource metadata
  dstemp <- base::do.call(base::rbind, XML::xpathApply(xmldata, "/HilltopServer/DataSource", function(node) {
    xp <- "./*"
    #get the datasource name
    datasource <- XML::xmlGetAttr(node, "Name")
    #get the data type
    type <- XML::xpathSApply(node, "./TSType", xmlValue)
    #create a unique identifier
    datasourceid <- base::paste(type, datasource)
    #extract the metadata tags
    attribute <- XML::xpathSApply(node, xp, XML::xmlName)
    #extract the metadata values
    value <- XML::xpathSApply(node, xp, function(x) {
      if(XML::xmlName(x) == "Measurement") {XML::xmlGetAttr(x, "Name") } else {XML::xmlValue(x) }
    } )
    #create a dataframe
    base::data.frame(datasourceid, datasource, attribute, value, stringsAsFactors = FALSE)
  } ) )
  #extract the metadata information
  ds <- base::subset(dstemp, attribute != "Measurement")
  #extract the measurement information
  meas <- base::subset(dstemp, attribute == "Measurement", select = c("datasourceid", "value") )
  #change the column names
  base::colnames(meas) [which(names(meas) == "value") ] <- "MeasurementName"
  #pivot the datasource dataframe
  castds <- reshape2::dcast(ds, datasourceid + datasource ~ attribute, value.var = "value")
  #join it to the measurement information
  castds <- base::merge(castds, meas, all = TRUE)
  #remove the unique identifier field (temporarily created to allow association)
  castds <- base::subset(castds, select= -c(datasourceid) )
  #return the data
  return(castds)
}

#' Hilltop Data Sources And Measurement Information At A Site.
#'
#' \code{hilltopMeasInfoList} helper function for measurement metadata.
#'
#' Helper function.
#' Takes an xml document from a Hilltop MeasurementList at a Site request.
#' Returns a dataframe of the measurement information and datasources.
#'
#' @inheritParams is.hilltopXml
#'
#' @return A dataframe of measurement information and associated datasources.
#'
#' @export
#'
#' @importFrom XML xpathApply xmlName xmlGetAttr xpathSApply xmlValue
#'
#' @importFrom reshape2 dcast
#'
hilltopMeasInfoList <- function(xmldata) {
  #extract the measurement information from the xml
  dstemp <- base::do.call(base::rbind, XML::xpathApply(xmldata, "/HilltopServer/DataSource/Measurement", function(node) {
    xp <- "./*"
    #extract the datasource name
    datasource <- XML::xpathSApply(node, "..", function(x) {XML::xmlGetAttr(x, "Name") } )
    #extract the measurement name
    MeasurementName <- XML::xmlGetAttr(node, "Name")
    #create a temporary unique id
    measurementid <- base::paste(datasource, MeasurementName)
    #extract the measurement metadata tags and values
    attribute <- XML::xpathSApply(node, xp, XML::xmlName)
    value <- XML::xpathSApply(node, xp, XML::xmlValue)
    #create a dataframe
    base::data.frame(measurementid, datasource, MeasurementName, attribute, value, stringsAsFactors = FALSE)
  } ) )
  #pivot the results
  castmeas <- reshape2::dcast(dstemp, measurementid + datasource + MeasurementName ~ attribute, value.var = "value")
  #remove the temporary unique identifier
  castmeas <- base::subset(castmeas, select = -c(measurementid) )
  #return the data
  return(castmeas)
}

#' Hilltop Data Source Information And Measurement Information At A Site.
#'
#' \code{hilltopDsMeasListFull} returns all datasource and measurement info.
#'
#' Takes an xml document from a Hilltop MeasurementList at a Site request.
#' Returns a dataframe of all of the datasource and measurement information
#' combined.
#'
#' @inheritParams is.hilltopXml
#'
#' @return A dataframe of datasources and measurments at a site with all
#'   available datasource and measurement information.
#'
#' @export
#'
hilltopDsMeasListFull <- function(xmldata) {
  #get the measurements and full datasource info
  t <- hilltopDsMeasList(xmldata)
  #get the measurement info and datasources
  m <- hilltopMeasInfoList(xmldata)
  #combine them
  full <- base::merge(t, m, by = c("datasource", "MeasurementName") , all = TRUE)
  #return the data
  return(full)
}

#' Old Function To Get Data Surce And Measurement Information. Helper function.
#' Takes an xml document from a Hilltop MeasurementList at a Site request.
#' Returns a dataframe of the measurement information and datasource info.
#'
#' @inheritParams is.hilltopXml
#'
#' @return A dataframe of datasources and measurments at a site with all
#'   available datasource and measurement information.
#'
#' @importFrom XML xpathApply xmlName xmlGetAttr xpathSApply xmlValue
#'
#' @importFrom reshape2 dcast
#'
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
