% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/switchDag.R
\name{switchDag}
\alias{switchDag}
\alias{switchDagArgs}
\alias{switchDag.redcapApiConnection}
\title{Switch Data Access Group Assignment for the Current User}
\usage{
switchDag(rcon, dag, ...)

\method{switchDag}{redcapApiConnection}(rcon, dag, ...)
}
\arguments{
\item{rcon}{A \code{redcapConnection} object.}

\item{dag}{\code{character(1)} A unique data access group to which to
assign the current user. Use \code{NA} to leave the user unassigned.}

\item{...}{Arguments to pass to other methods}
}
\value{
Invisibly returns \code{TRUE} when the call is completed successfully.
Otherwise an error is thrown.
}
\description{
This method enables the current API user to switch
(assign/reassign/unassign) their current Data Access Group assignment
if they have been assigned to multiple DAGs via the DAG Switcher page
in the project.
}
\examples{
\dontrun{
unlockREDCap(connections = c(rcon = "project_alias"), 
             url = "your_redcap_url", 
             keyring = "API_KEYs", 
             envir = globalenv())

# Switch the current user to the DAG "Facility Two"
switchDag(rcon, 
          dag = "facility_two")
}
}
\seealso{
\code{\link[=exportDags]{exportDags()}},\cr
\code{\link[=importDags]{importDags()}},\cr
\code{\link[=deleteDags]{deleteDags()}}, \cr
\code{\link[=exportUserDagAssignments]{exportUserDagAssignments()}}, \cr
\code{\link[=importUserDagAssignments]{importUserDagAssignments()}}
}
