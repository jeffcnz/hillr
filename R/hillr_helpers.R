#' Hilltop XML Value Identifier.
#'
#' Helper function to return the appropriate xml value depending whether the value
#' of interest is from a named node, or is a named parameter.
#' @param xmlNode The XML from a Hilltop XML node.
#' @return The value of the attribute described by the XML node.
#' @export
#' @importFrom XML xmlName xmlGetAttr xmlValue
hilltopValueHelper <- function(xmlNode) {
  if(XML::xmlName(xmlNode) != "T") {
    if(XML::xmlName(xmlNode) == "Parameter") {
      return(XML::xmlGetAttr(xmlNode, "Value"))
    } else {return(XML::xmlValue(xmlNode)) }
  }
}

#' Hilltop XML Attribute Identifier.
#'
#' Helper function to return the appropriate xml attribute name depending whether
#' the attribute of interest is from a named node, or is a named parameter.
#' @inheritParams hilltopValueHelper
#' @return The name of the attribute described by the XML node.
#' @export
#' @importFrom XML xmlName xmlGetAttr xmlValue
hilltopAttributeHelper <- function(xmlNode) {
  if(XML::xmlName(xmlNode) != "T") {
    if(XML::xmlName(xmlNode) == "Parameter") {
      return(XML::xmlGetAttr(xmlNode, "Name"))
    } else {return(XML::xmlName(xmlNode)) }
  }
}

#' Checks Whether XML Is Hilltop XML.
#'
#' Checks if an xml document is hilltop xml, returns True or False accordingly.
#' @param xmldata An XML document, returned from a url request using anyXmlParse.
#' @return logical TRUE / FALSE
#' @export
#' @importFrom XML xmlName xmlRoot
is.hilltopXml <- function(xmldata){
  server <- XML::xmlName(XML::xmlRoot(xmldata))
  if(base::length(base::grep("Hilltop",server))>0) {
    return(TRUE)
  } else {
    return(FALSE)
  }
}

#' Parser For Hilltop XML
#'
#' Helper function to parse data from a hilltop server. Takes a valid url as an
#' input and returns a parsed xml document ready for other functions. Uses
#' xmlParse from the XML library, but enables https requests to be processed as
#' well as http.
#' @param url A url that returns an XML document.
#' @return A parsed XML document
#' @export
#' @importFrom RCurl getURL
#' @importFrom XML xmlParse
anyXmlParse <- function(url) {
  if(base::length(base::grep("https",url))>0){
    doc <- RCurl::getURL(url, ssl.verifypeer = FALSE)
    return(XML::xmlParse(doc))
  } else {return(XML::xmlParse(url))}
}

#' Determine The TimePeriod Of Hilltop XML Ensemble Data.
#'
#' Helper function to determine what the measurement period of the EnsembleStats is.
#' @inheritParams is.hilltopXml
#' @return A string describing a time period.  Possible values are "Hour", "Day", or "Month".
#' @export
#' @importFrom  XML xpathApply xmlGetAttr
period <- function(xmldata) {

  if (base::length(XML::xpathApply(xmldata, "/HilltopServer/Hour", XML::xmlGetAttr, "Hour"))>0) {
    return("Hour")} else if
  (base::length(XML::xpathApply(xmldata, "/HilltopServer/Day", XML::xmlGetAttr, "Day"))>0) {
    return("Day")} else if (base::length(XML::xpathApply(xmldata, "/HilltopServer/Month", XML::xmlGetAttr, "Month"))>0) {
      return("Month")}
}

