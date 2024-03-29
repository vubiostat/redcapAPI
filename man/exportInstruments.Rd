% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/exportInstruments.R
\name{exportInstruments}
\alias{exportInstruments}
\alias{exportInstruments.redcapApiConnection}
\title{Export Instruments Defined in a Project}
\usage{
exportInstruments(rcon, ...)

\method{exportInstruments}{redcapApiConnection}(
  rcon,
  ...,
  error_handling = getOption("redcap_error_handling"),
  config = list(),
  api_param = list()
)
}
\arguments{
\item{rcon}{A \code{redcapConnection} object.}

\item{...}{Arguments to pass to other methods}

\item{error_handling}{\code{character(1)}. One of \code{c("error", "null")}.
An option for how to handle errors returned by the API.
see \code{\link[=redcapError]{redcapError()}}.}

\item{config}{A named \code{list}. Additional configuration parameters to pass to
\code{\link[httr:POST]{httr::POST()}}. These are appended to any parameters in
\code{rcon$config}.}

\item{api_param}{A named \code{list}. Additional API parameters to pass into the
body of the API call. This provides users to execute calls with options
that may not otherwise be supported by \code{redcapAPI}.}
}
\value{
Returns a data frame with the columns:\tabular{ll}{
    \tab  \cr
   \code{instrument_name} \tab The REDCap generated instrument name. \cr
   \code{instrument_label} \tab The user provided instrument label. \cr
}
}
\description{
These methods enable the user to view the instruments defined
in the project.
}
\examples{
\dontrun{
unlockREDCap(connections = c(rcon = "project_alias"), 
             url = "your_redcap_url", 
             keyring = "API_KEYs", 
             envir = globalenv())
 
exportInstruments(rcon)
}

}
\seealso{
\code{\link[=exportMetaData]{exportMetaData()}},\cr
\code{\link[=importMetaData]{importMetaData()}}, \cr
\code{\link[=exportInstruments]{exportInstruments()}},\cr
\code{\link[=exportMappings]{exportMappings()}},\cr
\code{\link[=importMappings]{importMappings()}}, \cr
\code{\link[=exportPdf]{exportPdf()}}
}
