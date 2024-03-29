% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/getProjectIdFields.R
\name{getProjectIdFields}
\alias{getProjectIdFields}
\title{Return a vector of the Project ID Fields}
\usage{
getProjectIdFields(rcon)
}
\arguments{
\item{rcon}{A \code{redcapConnection} object.}
}
\value{
Returns a character vector with the field names that uniquely identify
an experimental unit.
}
\description{
Returns a character vector listing the project ID fields.
This will be at most a vector of length two. The first element will
be the first field in the meta data. The second, if provided, will
be the name of the secondary unique field specified in the project.
}
\examples{
\dontrun{
unlockREDCap(connections = c(rcon = "project_alias"), 
             url = "your_redcap_url", 
             keyring = "API_KEYs", 
             envir = globalenv())
             
getProjectIdFields(rcon)
}
}
