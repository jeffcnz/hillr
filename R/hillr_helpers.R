#' Hilltop XML Value Identifier.
#'
#' Helper function to return the appropriate xml value depending whether the value
#' of interest is from a named node, or is a named parameter.
#' @export
#' @importFrom XML xmlName xmlGetAttr xmlValue
hilltopValueHelper <- function(x) {
  if(XML::xmlName(x) != "T") {
    if(XML::xmlName(x) == "Parameter") {
      return(XML::xmlGetAttr(x, "Value"))
    } else {return(XML::xmlValue(x)) }
  }
}

#' Hilltop XML Attribute Identifier.
#'
#' Helper function to return the appropriate xml attribute name depending whether
#' the attribute of interest is from a named node, or is a named parameter.
#' @export
#' @importFrom XML xmlName xmlGetAttr xmlValue
hilltopAttributeHelper <- function(x) {
  if(XML::xmlName(x) != "T") {
    if(XML::xmlName(x) == "Parameter") {
      return(XML::xmlGetAttr(x, "Name"))
    } else {return(XML::xmlName(x)) }
  }
}

#' Checks Whether XML Is Hilltop XML.
#'
#' Checks if an xml document is hilltop xml, returns True or False accordingly.
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
#' input and returns a parsed xml document ready for other functions. Handles
#' https requests as well as http.
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
#' @export
#' @importFrom  XML xpathApply xmlGetAttr
period <- function(dataxml) {

  if (base::length(XML::xpathApply(dataxml, "/HilltopServer/Hour", XML::xmlGetAttr, "Hour"))>0) {
    return("Hour")} else if
  (base::length(XML::xpathApply(dataxml, "/HilltopServer/Day", XML::xmlGetAttr, "Day"))>0) {
    return("Day")} else if (base::length(XML::xpathApply(dataxml, "/HilltopServer/Month", XML::xmlGetAttr, "Month"))>0) {
      return("Month")}
}

