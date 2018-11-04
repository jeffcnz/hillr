#' Makes sure that an endpoint ends in a "?"
#'
#' Helper function to ensure that a Hilltop endpoint ends in a "?".
#' @param endpoint string A valid hilltop endpoint.
#' @return A version of the provided string that ends in a "?"
fixEndpoint <- function(endpoint) {
  #Check whether the last charachter is a ?
  if(stringEnd(endpoint,1) != "?") {
    #If the string doesn't end in a ? then add one and return the result.
    return(paste0(endpoint, "?"))
  } else {
    #If the string ends in a ? then return the string unchanged.
    return(endpoint)
  }
}

#' Checks that a Hilltop endpoint is valid.
#'
#' Helper function to check if a provided endpoint is a valid Hilltop endpoint.
#' Currelntly limited to checking that the first 4 characters are http and the
#' last are.hts?.
#' @inheritParams fixEndpoint
#' @return Logical TRUE / FALSE
is.validEndpoint <- function(endpoint) {
  #Does the endpoint string start with http.
  if(substr(endpoint, 0, 4) == "http") {
    #Does the endpoint string end with .hts?
    if(stringEnd(endpoint, 5) == ".hts?") {
      return(TRUE)
    } else {
      return(FALSE)
    }
  } else {
    return(FALSE)
  }
}

#' Checks and fixes a Hiltop endpoint.
#'
#' Helper function to check and fix a Hilltop endpoint string.
#' @inheritParams fixEndpoint
#' @return A valid Hilltop endpoint.
checkFixEndpoint <- function(endpoint) {
  #First make sure the endpoint ends in a ?
  endpoint <- fixEndpoint(endpoint)
  #Check endpoint is valid, if it is return it, otherwise show an error message.
  if(is.validEndpoint(endpoint)) {
    return(endpoint)
  } else {
      stop("Invalid endpoint.")
    }
}
