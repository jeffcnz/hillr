# http://data.hbrc.govt.nz/EnviroData/Emar.hts?Service=Hilltop&Request=GetData&Site=Ngaruroro River at Fernhill&Measurement=Total Nitrogen&From=1/6/2014"

#' Create a url to request a site list from a Hilltop Server.
#'
#' Takes a valid Hilltop server endpoint and returns a url that requests the
#' site list from the endpoint.
#' @param endpoint A valid hilltop endpoint.
#' @return A Hilltop url request.
#' @export
buildSiteListUrl <- function(endpoint) {
  # check whether a '?' is the last charachter of the endpoint, if not add one
  # and check that the endpoint is valid.
  endpoint <- checkFixEndpoint(endpoint)
  # set the service as 'Hilltop'
  service <- "Hilltop"
  # set the request as 'SiteList'
  request <- "SiteList"
  # build the url
  hillUrl <- base::paste0(endpoint,
                    "Service=", service,
                    "&Request=", request)
  # return the url
  return(hillUrl)
}
