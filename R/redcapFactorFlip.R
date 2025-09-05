#' @name redcapFactorFlip
#' @export redcapFactorFlip
#'
#' @title Convert REDCap factors between labeled and coded
#' @description Factors exported from REDCap can be toggled between the coded
#'   and labeled values with the use of the attributes assigned to the
#'   factors during export.
#'
#' @param v A factor exported from REDCap.  The REDCap type may be radio,
#'   dropdown, check, yesno, etc.
#'
#' @details Each factor type variable in REDCap is given the attributes
#' `redcapLabels` and `redcapLevels`.  With these attached to the
#' vector, switching between the coded and labeled values can be done with
#' ease.  This may be helpful when the coded value has importance,
#' such as 0/1 for death, or if a yes is worth six points (instead of one).
#'

redcapFactorFlip <- function(v){
  logMessage("Please use recastRecords. redcapFactorFlip will be removed in version 3.0.0")
  #* extract attributes to be applied later
  redcapLabels <- attributes(v)$redcapLabels
  redcapLevels <- attributes(v)$redcapLevels

  if (is.null(redcapLabels) | is.null(redcapLevels))
    logStop("This does not appear to be a REDCap factor.")

  #* labeled to coded
  if ("factor" %in% class(v)){
    v <- factor(as.character(v),
                redcapLabels,
                redcapLevels)
    v <- as.character(v)
    if (is.numeric(redcapLevels)) v <- as.numeric(v)
  }

  #* coded to labeled
  else{
    v <- factor(v,
                attributes(v)$redcapLevels,
                attributes(v)$redcapLabels)
  }

  #* reapply attributes
  attr(v, 'redcapLabels') <- redcapLabels
  attr(v, 'redcapLevels') <- redcapLevels
  return(v)
}
