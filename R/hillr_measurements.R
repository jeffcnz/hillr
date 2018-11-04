#' Hilltop Measurement To Data Frame.
#'
#' Helper function that reads the nodes within a the Measurement node of a Hilltop
#' XML response from a anyXmlParse(url) request such as dataxml<-anyXmlParse(url).
#' Returns a dataframe of the data for each timestamp. Handles missing results and
#' doesn't require prior knowledge of parameter names. Handles true measurements
#' and WQ Sample requests.
#' @export
#' @importFrom XML getNodeSet xpathApply xmlValue
#' @importFrom reshape2 dcast
hilltopMeasurementToDF <- function(dataxml) {
  idNodes <- XML::getNodeSet(dataxml, "//Measurement/Data/E")
  Times <- base::lapply(idNodes, XML::xpathApply, path = "./T", XML::xmlValue)
  values <- base::lapply(idNodes, XML::xpathApply, path = "./*", hilltopValueHelper)
  attributes <- base::lapply(idNodes, XML::xpathApply, path = "./*", hilltopAttributeHelper)
  data <- base::do.call(base::rbind.data.frame, base::Reduce(function(x,y) base::Map(base::cbind, x, y), base::list(Times, attributes, values)))
  base::names(data) <- c("Time", "Attribute", "Content")
  data <- data[!(data$Attribute == "NULL"), ]
  data <- base::data.frame(base::lapply(data, base::as.character), stringsAsFactors = FALSE)
  cdata <- reshape2::dcast(data, Time ~ Attribute, value.var = "Content")
  cdata$Time <- base::as.POSIXct(base::strptime(cdata$Time, format = "%Y-%m-%dT%H:%M:%S"))
  base::colnames(cdata)[base::colnames(cdata)=="I1"] <- "Value"
  return(cdata)
}

#' Hilltop Data Source To Data Frame.
#'
#' Helper function that reads the nodes within a the DataSource ItemInfo node of a
#' Hilltop XML response from a anyXmlParse(url) request such as
#' dataxml<-anyXmlParse(url). Returns a dataframe of the Info for each Item.
#' Handles missing results and doen't require prior knowledge of the items.
#' @export
#' @importFrom XML getNodeSet xpathApply xmlValue
#' @importFrom reshape2 dcast
hilltopDataSourceToDF<-function(dataxml) {
  idNodes <- XML::getNodeSet(dataxml, "//Measurement/DataSource")
  Item <- base::lapply(idNodes, XML::xpathApply, path = "./ItemInfo", XML::xmlGetAttr, "ItemNumber")
  values <- base::lapply(idNodes, XML::xpathApply, path = "./ItemInfo/*", hilltopValueHelper)
  attributes <- base::lapply(idNodes, XML::xpathApply, path = "./ItemInfo/*", hilltopAttributeHelper)
  data <- base::data.frame(Attribute = base::unlist(attributes), Content = base::unlist(values))
  data$Item <- base::unlist(Item)
  data <- data[!(data$Attribute == "NULL"), ]
  data <- base::data.frame(base::lapply(data, base::as.character), stringsAsFactors = FALSE)
  cdata <- reshape2::dcast(data, Item ~ Attribute, value.var = "Content")
  return(cdata)
}

#' Hilltop Measurement Request As A Data Frame.
#'
#' Main function that converts a Hilltop XML document from a anyXmlParse(url)
#' request such as dataxml<-anyXmlParse(url) for a water quality measurement into
#' a dataframe that contains the measurement information. It returns a dataframe
#' of the data for each timestamp, including DataSource Information and the Site
#' Name. This dataframe can be merged with a WQ Sample dataframe processed using
#' hilltopMeasurementToDF
#' @export
hilltopMeasurement<-function(dataxml){

  Site <- dataxml[["string(//Measurement/@SiteName)"]]
  df <- hilltopMeasurementToDF(dataxml)
  df$Site <- Site
  items <- hilltopDataSourceToDF(dataxml)
  df$Measurement <- items$ItemName
  df$Units <- if(base::is.null(items$Units)) {c("")} else {items$Units}
  return(df)
}





