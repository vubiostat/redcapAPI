#' @param error_handling \code{character(1)}, one of \code{c("error", "null")}.
#'   An option for how to handle errors returned by the API.
#'   see \code{\link{redcapError}}.
#' @param config \code{list} Additional configuration parameters to pass to
#'   \code{\link[httr]{POST}}. These are appended to any parameters in
#'   \code{rcon$config}.
#' @param api_param \code{list} Additional API parameters to pass into the
#'   body of the API call. This provides users to execute calls with options
#'   that may not otherwise be supported by \code{redcapAPI}.
