#' Hilltop Measurement To Data Frame.
#'
#' \code{hilltopMeasurementToDF} returns a timeseries dataframe of measurements
#'
#' Helper function that reads the nodes within a the Measurement node of a Hilltop
#' XML response from a anyXmlParse(url) request such as xmldata<-anyXmlParse(url).
#' Returns a dataframe of the data for each timestamp. Handles missing results and
#' doesn't require prior knowledge of parameter names. Handles true measurements
#' and WQ Sample requests.
#'
#' @inheritParams is.hilltopXml
#'
#' @return A dataframe of data for each timestep.
#'
#' @export
#'
#' @importFrom XML getNodeSet xpathApply xmlValue
#'
#' @importFrom reshape2 dcast
#'
#' @importFrom dplyr group_by mutate
#'
hilltopMeasurementToDF <- function(xmldata) {
  #get the xml nodes that relate to the timeseries
  idNodes <- XML::getNodeSet(xmldata, "//Measurement/Data/E")
  #extract the times of the measurements
  Times <- base::lapply(idNodes, XML::xpathApply, path = "./T", XML::xmlValue)
  #extract the values of the measurements
  values <- base::lapply(idNodes, XML::xpathApply, path = "./*", hilltopValueHelper)
  #extract any other attributes
  attributes <- base::lapply(idNodes, XML::xpathApply, path = "./*", hilltopAttributeHelper)
  #create a 'long' format dataframe of the data
  data <- base::do.call(base::rbind.data.frame, base::Reduce(function(x,y) base::Map(base::cbind, x, y), base::list(Times, attributes, values)))
  #set the field names
  base::names(data) <- c("Time", "Attribute", "Content")
  #remove any rows where attribute is NULL
  data <- data[!(data$Attribute == "NULL"), ]
  #convert everything to character format
  data <- base::data.frame(base::lapply(data, base::as.character), stringsAsFactors = FALSE)
  #group duplicate entries for Time and Attribute, any duplicate values will be joined by a |.
  data <- data %>%
    dplyr::group_by(Time, Attribute) %>%
    dplyr::mutate(Content = paste(Content, collapse = " | "))
  #remove duplicates
  data <- base::unique(data)
  #reshape the dataframe to 'wide' format (pivot the data)
  cdata <- reshape2::dcast(data, Time ~ Attribute, value.var = "Content")
  #convert the time field to time format
  cdata$Time <- base::as.POSIXct(base::strptime(cdata$Time, format = "%Y-%m-%dT%H:%M:%S"))
  #give the value field a meaningful name.
  base::colnames(cdata)[base::colnames(cdata)=="I1"] <- "Value"
  #return the data
  return(cdata)
}

#' Hilltop Data Source To Data Frame.
#'
#' \code{hilltopDataSourceToDF} returns measurement metadata
#'
#' Helper function that reads the nodes within a the DataSource ItemInfo node of a
#' Hilltop XML response from a anyXmlParse(url) request such as
#' xmldata<-anyXmlParse(url). Returns a dataframe of the Info for each Item.
#' Handles missing results and doen't require prior knowledge of the items.
#'
#' @inheritParams is.hilltopXml
#'
#' @return A dataframe of the Hilltop datasource information.
#'
#' @export
#'
#' @importFrom XML getNodeSet xpathApply xmlValue
#'
#' @importFrom reshape2 dcast
#'
hilltopDataSourceToDF<-function(xmldata) {
  #extract the datasource nodes
  idNodes <- XML::getNodeSet(xmldata, "//Measurement/DataSource")
  #extract the metadata (allows for multiple items)
  Item <- base::lapply(idNodes, XML::xpathApply, path = "./ItemInfo", XML::xmlGetAttr, "ItemNumber")
  values <- base::lapply(idNodes, XML::xpathApply, path = "./ItemInfo/*", hilltopValueHelper)
  attributes <- base::lapply(idNodes, XML::xpathApply, path = "./ItemInfo/*", hilltopAttributeHelper)
  #create a dataframe of the metadata
  data <- base::data.frame(Attribute = base::unlist(attributes), Content = base::unlist(values))
  #add the item id
  data$Item <- base::unlist(Item)
  #remove empty attributes
  data <- data[!(data$Attribute == "NULL"), ]
  #convert everything to character
  data <- base::data.frame(base::lapply(data, base::as.character), stringsAsFactors = FALSE)
  #handle server errors where metadata isn't provided
  cdata <- if(base::length(data) > 0) {
    #If there is metadata then create a dataframe of it that can be associated
    #with the measurements
    reshape2::dcast(data, Item ~ Attribute, value.var = "Content")
  } else {
    #Extract the measurement name from the xml and return this in a dataframe
    #that can be associated with the measuremnts.
    itemName <- xmldata[["string(//DataSource/@Name)"]]
    base::data.frame(ItemName = itemName, stringsAsFactors = FALSE)
  }
  #return the metadata
  return(cdata)
}

#' Hilltop Measurement Request As A Data Frame.
#'
#' \code{hilltopMeasurement} returns a timeseries with measurement metadata.
#'
#' Main function that converts a Hilltop XML document from a anyXmlParse(url)
#' request such as xmldata<-anyXmlParse(url) for a water quality measurement
#' into a dataframe that contains the measurement information. It returns a
#' dataframe of the data for each timestamp, including DataSource Information
#' and the Site Name. Combines the results of the
#' hilltopMeasurementToDF(xmldata) and hilltopDataSourceToDF(xmldata) functions.
#' This dataframe can be merged with a WQ Sample dataframe processed using
#' hilltopMeasurementToDF.
#'
#' @inheritParams is.hilltopXml
#'
#' @return A dataframe of measurement data and metadata from the Hilltop
#'   datasource.
#'
#' @export
#'
hilltopMeasurement<-function(xmldata){
  #identify the site that the data is from
  Site <- xmldata[["string(//Measurement/@SiteName)"]]
  #get the measurement data
  df <- hilltopMeasurementToDF(xmldata)
  #add the site information
  df$Site <- Site
  #get the measurement metadata
  items <- hilltopDataSourceToDF(xmldata)
  #add the measurement name to the main dataset
  df$Measurement <- items$ItemName
  #add the measurement units to the dataset (handle missing metadata by adding an empty column)
  df$Units <- if("Units" %in% base::names(items)){
    #If a Units field is in the items dataframe then use it.
    if(base::is.null(items$Units)) {c("")} else {items$Units}
  } else {
    #Leave the units blank
    ""
  }
  #return the data
  return(df)
}





