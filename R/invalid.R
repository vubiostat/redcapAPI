
format.invalid <- function(x, ...)
{
  dt <- attr(x, "time")
  vr <- attr(x, "version")
  ft <- attr(x, "field_types")
  x <- split(x, x$field_name)
  paste0(
    "# Failed Validations from REDCap data\n",
    paste0(dt, "\n"),
    paste0("Package redcapAPI version ", packageVersion("redcapAPI"), "\n"),
    paste0("REDCap version ", vr, "\n\n"),
    paste0(unlist(sapply(seq_along(x), function(i) {
      data <- x[[i]]
      c(paste0("* Field[",ft[i], "] '", names(x)[i], "' has ", nrow(data), " failure", ifelse(nrow(data) > 1, "s", "")),
        if("record_id" %in% names(data)) 
           paste0("  * Row ", data$row, ", Record Id '", data$record_id, "', Value '", data$value, "'") else
           paste0("  * Row ", data$row, ", Value '", data$value, "'")
      )
    })), collapse="\n")
  )
}

print.invalid <- function(x, ...)
{
  cat(format(x), ...)
}

summary.invalid <- function(x, ...)
{
  paste0("There are ", nrow(x), " validation failures over the following fields: '",
    paste0(unique(x$field_name), collapse="', '"),
    "'."
  , collapse="")
}