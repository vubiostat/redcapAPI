
format.invalid <- function(x, ...)
{
  x <- split(x, x$field_name)
  paste(
    "# Failed Validations from REDCap data",
    paste(unlist(sapply(seq_along(x), function(i) {
      data <- x[[i]]
      c(paste("* Field '", names(x)[i], "' has ", length(data), " failures"),
        if("record_id" %in% names(data)) 
           paste0("  * Row ", data$row, ", Record Id '", data$record_id, "', Value '", data$value, "'") else
           paste0("  * Row ", data$row, ", Value '", data$value, "'")
      )
    })), collapse="\n"),
    collapse="\n"
  )
}