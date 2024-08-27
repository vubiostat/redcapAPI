#' @name armsMethods
#' @title Export, Import, and Delete Arms from a Project
#' 
#' @description These methods enable the user to export the current arms
#'   from a project, import new arms, and modify or delete existing arms.
#'   
#' @inheritParams common-rcon-arg
#' @inheritParams common-dot-args
#' @inheritParams common-api-args
#' @param data A `data.frame` with two columns.  The first column 
#'   (`arm_num`) is an `integerish` value . The second (`name`) is
#'   a character value. For backward compatibility, 
#'   this may also be passed as `arms_data`.
#' @param override `logical(1)`. By default, data will add to or modify 
#'   existing arms data. When `TRUE`, all the existing arms data is 
#'   deleted and replaced with the contents of `data`.
#' @param arms `character` or `integerish` identifying the arm 
#'   numbers to export or delete.
#' 
#' @details 
#'   Exporting arms is not supported for classical REDCap projects. If 
#'   the user attempts to export arms for a classical project, a 
#'   data frame will be returned with zero rows.
#' 
#'   When importing, arms are added when the value of `arm_num`
#'   does not already exist in the project. 
#'   
#'   Arm names may be modified by altering the `name` value associated
#'   with an existing `arm_num` value. 
#'   
#'   Deleting arms--whether by `deleteArms` or `importArms` with 
#'   `override = TRUE`--is a destructive act that also deletes 
#'   events and records associated with the arm. This is irreversible 
#'   data loss. REDCap will only permit these actions to occur in projects
#'   in Development status.
#'   
#' @return
#' `exportArms` returns a `data.frame` with columns:
#' 
#' |           |                                           |
#' |-----------|-------------------------------------------|
#' | `arm_num` | The ID number for the arm in the project. | 
#' | `name`    | The display name of the arm.              |
#' 
#' `importArms` invisibly returns the number of arms imported.
#'   
#' `deleteArms` invisibly returns the number of arms deleted.
#'   
#' @examples
#' \dontrun{
#' unlockREDCap(connections = c(rcon = "project_alias"), 
#'              url = "your_redcap_url", 
#'              keyring = "API_KEYs", 
#'              envir = globalenv())
#'
#' # Export all of the Arms
#' exportArms(rcon)
#' 
#' # Export only a subset of arms
#' exportArms(rcon, 
#'            arms = c(1, 3))
#' 
#' 
#' # Import a new arms
#' # Assume arms 1, 2, and 3 exist in the project already
#' NewData <- data.frame(arm_num = 4, 
#'                       name = "Arm Four Name")
#' importArms(rcon, 
#'            data = NewData)
#'            
#' # Change the name of an existing arm
#' NewData <- data.frame(arm_num = 1, 
#'                       name = "New Arm Name")
#' importArms(rcon, 
#'            data = NewData)
#'            
#' # Delete all arms and replace with a new specification
#' NewData <- data.frame(arm_num = c(1, 2), 
#'                       name = c("Treatment Arm", "Control Arm"))
#' importArms(rcon, 
#'            data = NewData, 
#'            override = TRUE)
#'            
#' # Delete an existing arm
#' deleteArms(rcon, 
#'            arms = 4)
#'          
#' # Delete multiple existing arm
#' deleteArms(rcon, 
#'            arms = c(2, 3))
#' }

#' @usage NULL
#' @order 0
# dummy function to control the order of arguments in the help file.
armsMethods <- function(rcon, 
                        arms, 
                        data, 
                        override,  
                        ...)
{
  NULL
}
