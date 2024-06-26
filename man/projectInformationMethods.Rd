% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/docsProjectInformationMethods.R,
%   R/exportProjectInformation.R, R/importProjectInformation.R
\name{projectInformationMethods}
\alias{projectInformationMethods}
\alias{exportProjectInformation}
\alias{importProjectInformation}
\alias{exportProjectInformation.redcapApiConnection}
\alias{importProjectInformation.redcapApiConnection}
\title{Export and Import Project Settings}
\usage{
exportProjectInformation(rcon, ...)

importProjectInformation(rcon, data, ...)

\method{exportProjectInformation}{redcapApiConnection}(rcon, ...)

\method{importProjectInformation}{redcapApiConnection}(rcon, data, ...)
}
\arguments{
\item{rcon}{A \code{redcapConnection} object.}

\item{data}{\code{data.frame} with only one row and any subset of allowable fields to be
updated. See Details.}

\item{...}{Arguments to pass to other methods}
}
\value{
\code{exportProjectInformation} returns a data frame with the columns\tabular{ll}{
    \tab  \cr
   \code{project_id} \tab The internal ID number assigned to the project. \cr
   \code{project_title} \tab The project title given to the project. \cr
   \code{creation_time} \tab The date/time the project was created. \cr
   \code{production_time} \tab The date/time the project was moved into production status. \cr
   \code{in_production} \tab Boolean value indicating if the project is in production status. \cr
   \code{project_language} \tab The language associated with the project. \cr
   \code{purpose} \tab An integerish value identifying the purpose of the project. 0 = "Practice/Just for fun", 1 = "Other", 2 = "Research", 3 = "Quality Improvement", 4 = "Operational Support". \cr
   \code{purpose_other} \tab The user supplied character value given when the project purpose is 'Other'. \cr
   \code{project_notes} \tab The user supplied notes about the project. \cr
   \code{custom_record_label} \tab The user provided custom label for the record identifier field. \cr
   \code{secondary_unique_field} \tab The name of the secondary unique field, if this has been configured. \cr
   \code{is_longitudinal} \tab Boolean value indicating if the project is a longitudinal project. \cr
   \code{has_repeating_instruments_or_events} \tab Boolean value indicating if the repeating instruments or events module has been enabled. \cr
   \code{surveys_enabled} \tab Boolean value indicating if the surveys module has been enabled. \cr
   \code{scheduling_enabled} \tab Boolean value indicating if the scheduling module has been enabled. \cr
   \code{record_autonumbering_enabled} \tab Boolean value indicating if the record autonumbering feature has been enabled. \cr
   \code{randomization_enabled} \tab Boolean value indicating if the randomization module has been enabled. \cr
   \code{ddp_enabled} \tab Boolean value indicating if dynamic data pull has been enabled for a project (may only be enabled by a REDCap administrator). \cr
   \code{project_irb_number} \tab The user provided IRB number for the project. \cr
   \code{project_grant_number} \tab The user provided grant number for the project. \cr
   \code{project_pi_firstname} \tab The first name of the principal investigator. \cr
   \code{project_pi_lastname} \tab The last name of the principal investigator. \cr
   \code{display_today_now_button} \tab Boolean value indicating if the today/now button is displayed for date/time fields in the UI. \cr
   \code{missing_data_codes} \tab Character value giving the missing data codes enabled for the project. They are given in the format \verb{[code],[label]}, with each coding separated by a pipe character. \cr
   \code{external_modules} \tab Character value listing the external modules enabled. \cr
   \code{bypass_branching_erase_field_prompt} \tab Boolean value indicating if the box for "Prevent branching logic from hiding fields that have values" has been checked under "Additional Customizations." \cr
}


\code{importProjectInformation} invisibly returns the number of fields updated.
}
\description{
These methods enable the user to export or update project
level settings, such as the project title, if it is longitudinal,
if surveys are enabled, etc.
}
\details{
When importing, fields that are not editable will be quietly removed prior to
import. This allows the user to use the result of
\code{exportProjectInformation} as a template for the import.

For any values that are boolean, they should be represented as either a '0'
(no/false) or '1' (yes/true).

It is not required for \code{data} to have all of the fields, but only
the fields the user wishes to update (see examples).

The following project attributes can be updated:
\itemize{
\item project_title
\item project_language
\item purpose
\item purpose_other
\item project_notes
\item custom_record_label
\item secondary_unique_field
\item is_longitudinal
\item surveys_enabled
\item scheduling_enabled
\item record_autonumbering_enabled
\item randomization_enabled
\item project_irb_number
\item project_grant_number
\item project_pi_firstname
\item project_pi_lastname
\item display_today_now_button
\item bypass_branching_erase_field_prompt
}
}
\section{Functions}{
\itemize{
\item \code{exportProjectInformation()}: Export project settings.

\item \code{importProjectInformation()}: Import project settings.

}}
\examples{
\dontrun{
unlockREDCap(connections = c(rcon = "project_alias"), 
             url = "your_redcap_url", 
             keyring = "API_KEYs", 
             envir = globalenv())

# Export Project Information
exportProjectInformation(rcon)

# Import a new project title
NewData <- data.frame(project_title = "New Title Name")
importProjectInformation(rcon, 
                         data = NewData)
                         
# Enable surveys in the project
NewData <- data.frame(surveys_enabled = 1)
importProjectInformation(rcon, 
                         data = NewData)
                         
# Change multiple fields in the project settings
NewData <- data.frame(project_irb_number = "IRB-12345", 
                      display_today_now_button = 0, 
                      scheduling_enabled = 1)
importProjectInformation(rcon, 
                         data = NewData)
}

}
