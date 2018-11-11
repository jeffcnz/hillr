#' Hilltop XML Value Identifier.
#'
#' \code{hilltopValueHelper} returns the value of parameters or nodes.
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
#' \code{hilltopAttributeHelper} returns the attribute name for parameters or nodes.
#'
#' Helper function to return the appropriate xml attribute name depending whether
#' the attribute of interest is from a named node, or is a named parameter.
#'
#' @inheritParams hilltopValueHelper
#'
#' @return The name of the attribute described by the XML node.
#'
#' @export
#'
#' @importFrom XML xmlName xmlGetAttr xmlValue
#'
hilltopAttributeHelper <- function(xmlNode) {
  if(XML::xmlName(xmlNode) != "T") {
    if(XML::xmlName(xmlNode) == "Parameter") {
      return(XML::xmlGetAttr(xmlNode, "Name"))
    } else {return(XML::xmlName(xmlNode)) }
  }
}

#' Checks Whether XML Is Hilltop XML.
#'
#' \code{is.hilltopXml} Checks if an xml document is hilltop xml, returns True or False accordingly.
#'
#' @param xmldata An XML document, returned from a url request using anyXmlParse.
#'
#' @return logical TRUE / FALSE
#'
#' @export
#'
#' @importFrom XML xmlName xmlRoot
#'
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
#' \code{anyXmlParse} parses Hilltop XML from http or https requests.
#'
#' Helper function to parse data from a hilltop server. Takes a valid url as an
#' input and returns a parsed xml document ready for other functions. Uses
#' xmlParse from the XML library, but enables https requests to be processed as
#' well as http.
#'
#' @param url A url that returns an XML document.
#'
#' @return A parsed XML document
#'
#' @export
#'
#' @importFrom RCurl getURL
#'
#' @importFrom XML xmlParse
#'
anyXmlParse <- function(url) {
  if(base::length(base::grep("https",url))>0){
    doc <- RCurl::getURL(url, ssl.verifypeer = FALSE)
    return(XML::xmlParse(doc))
  } else {return(XML::xmlParse(url))}
}

#' Determine The TimePeriod Of Hilltop XML Ensemble Data.
#'
#' \code{period} Determine what the measurement period of the Ensemble Statistics is.
#'
#' @inheritParams is.hilltopXml
#'
#' @return A string describing a time period.  Possible values are "Hour", "Day", or "Month".
#'
#' @export
#'
#' @importFrom  XML xpathApply xmlGetAttr
#'
period <- function(xmldata) {

  if (base::length(XML::xpathApply(xmldata, "/HilltopServer/Hour", XML::xmlGetAttr, "Hour"))>0) {
    return("Hour")} else if
  (base::length(XML::xpathApply(xmldata, "/HilltopServer/Day", XML::xmlGetAttr, "Day"))>0) {
    return("Day")} else if (base::length(XML::xpathApply(xmldata, "/HilltopServer/Month", XML::xmlGetAttr, "Month"))>0) {
      return("Month")}
}

#' Return the last n charachters from a string.
#'
#' \code{stringEnd} return the last n charachters of a string.
#'
#'
#' @param string string The string that the substring is to be extracted from.
#' @param n integer The number of charachters from the end of the string to return.
#'
#' @return string The last n charachters from the input string.
#'
stringEnd <- function(string, n){
  lenStr <- nchar(string)
  substr(string, lenStr-n+1, lenStr)
}

#' Return any error message provided by the server.
#'
#' \code{xmlError} returns the error message from the server
#'
#' @inheritParams is.hilltopXml
#'
#' @return character The error message provided by the Hilltop server.
#'
#' @export
#'
#' @importFrom XML getNodeSet xmlValue
#'
xmlError <- function(xmldata) {
  errorMsg <- base::sapply(XML::getNodeSet(xmldata, path="//Error"), XML::xmlValue)
  return(errorMsg)
}

#' Parse hilltop XML, notifying errors
#'
#' \code{hillXmlParse} returns hilltop xml for processing
#'
#' @inheritParams anyXmlParse
#'
#' @return Parsed Hilltop XML.
#'
#' @export
#'
hillXmlParse <- function(url) {
  #parse the requested data
  xml <- anyXmlParse(url)
  #check for an error message
  error <- xmlError(xml)
  #if there's an error message return it, otherwise return the xml
  if(base::length(error) > 0) {
    stop(error)
  } else {
    return(xml)
  }
}
