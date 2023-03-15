#' @name redcapConnection
#' @export redcapConnection
#' 
#' @title Connect to a REDCap Database
#' @description Creates an object of class \code{redcapApiConnection} for 
#' using the REDCap API 
#' 
#' @param url URL for a REDCap database API.  Check your institution's REDCap 
#'   documentation for this address.  Either \code{url} or \code{conn} must 
#'   be specified.
#' @param token REDCap API token
#' @param config A list to be passed to \code{httr::POST}.  This allows the 
#'   user to set additional configurations for the API calls, such as 
#'   certificates, ssl version, etc. For the majority of users, this does 
#'   not need to be altered.  See Details for more about this argument's 
#'   purpose and the \code{redcapAPI} wiki for specifics on its use.
#'   
#' @details
#' For convenience, you may consider using 
#' \code{options(redcap_api_url=[your URL here])} in your RProfile.
#' To obtain an API token for a project, do the following:\cr
#' Enter the 'User Right' section of a project\cr
#' Select a user\cr
#' Check the box for 'API Data Export' or 'API Data Import,' as appropriate.  A full tutorial on 
#' configuring REDCap to use the API can be found at \url{https://github.com/vubiostat/redcapAPI/wiki}
#' 
#' Tokens are specific to a project, and a token must be created for each 
#' project for which you wish to use the API.
#' 
#' The \code{config} argument is passed to the \code{httr::POST} argument of 
#' the same name.  The most likely reason for using this argument is that the 
#' certificate files bundled in \code{httr} have fallen out of date.  
#' Hadley Wickham is pretty good about keeping those certificates up 
#' to date, so most of the time this problem can be resolved by updating 
#' \code{httr} to the most recent version.  If that doesn't work, a 
#' certificate file can be manually passed via the \code{config} argument.  
#' The \code{redcapAPI} wiki has a more detailed tutorial on how to 
#' find and pass an SSL certificate to the API call 
#' (\url{https://github.com/vubiostat/redcapAPI/wiki/Manually-Setting-an-SSL-Certificate-File}).
#' 
#' Additional Curl option can be set in the \code{config} argument.  See the documentation
#' for \code{httr::config} and \code{httr:httr_options} for more Curl options.
#' 
#' @author Jeffrey Horner
#' 
#' @examples
#' \dontrun{
#' rcon <- redcapConnection(url=[YOUR_REDCAP_URL], token=[API_TOKEN])
#' 
#' options(redcap_api_url=[YOUR_REDCAP_URL])
#' rcon <- redcapConnection(token=[API_TOKEN])
#' 
#' exportRecords(rcon)
#' }
#' 

redcapConnection <- function(url = getOption('redcap_api_url'),
                             token,
                             config = httr::config())
{
  u <- url
  t <- token
  md <- NULL
  arm <- NULL
  evt <- NULL
  fldnm <- NULL
  map <- NULL
  users <- NULL
  vrsn <- NULL
  proj <- NULL
  
  metadata <- function(){ 
    if (is.null(md)){
      message("Fetching Meta Data Through the API")
      md <<- exportMetaData(redcapConnection(u, t))
    }
    md 
  }
  
  arms <- function(){ 
    if (is.null(arm))
    {
      message("Fetching Arms Through the API")
      arm <<- exportArms(redcapConnection(u, t)) 
    }
    arm }
  
  events <- function(){ 
    if (is.null(evt)){
      message("Fetching Events Through the API")
      evt <<- exportEvents(redcapConnection(u, t))
    }
    evt }
  
  field_names <- function(){ 
    if (is.null(fldnm)){
      message("Fetching Field Names Through the API")
      fldnm <<- exportFieldNames(redcapConnection(u, t))
    }
    fldnm 
  }
  
  mapping <- function(){ 
    if (is.null(map)){ 
      message("Fetching Arm-Event Mappings Through the API")
      map <<- exportFieldNames(redcapConnection(u, t))
    }
    map 
  }
  
  users <- function(){ 
    if (is.null(users)){
      message("Fetching Users Through the API")
      users <<- exportUsers(redcapConnection(u, t))
    }  
    users }
  
  version <- function(){ 
    if (is.null(vrsn)){
      message("Fetching Version Number Through the API")
      vrsn <<- exportVersion(redcapConnection(u, t))
    }
    vrsn 
  }
  
  project <- function(){
    if (is.null(proj)){
      message("Fetching the Project Information Through the API")
      proj <<- exportProjectInformation(redcapConnection(u, t))
    }
    proj
  }
  
  rc <- list(url = u, 
             token = t, 
             config = config, 
             metadata = metadata, 
             arms = arms, 
             events = events, 
             field_names = field_names, 
             mapping = mapping, 
             users = users, 
             version = version, 
             project_info = project)
  class(rc) <- "redcapApiConnection"
  rc
}
