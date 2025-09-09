#' @name exportSAS
#' @title Export the REDCap data as a SAS importable set of files.
#'
#' @description This creates a csv for each form and a SAS script that will
#' load all the data into SAS.
#'
#' @inheritParams common-rcon-arg
#' @inheritParams common-dot-args
#' @inheritParams common-api-args
#' @param directory `NULL` or `character(1)`. Directory to write files into.
#' Defaults to current working directory.
#'
#' @details
#' This function is _experimental_ and needs feedback/suggestions to flesh
#' it out fully. If this feature is important to you, please consider
#' opening an issue on github to suggest improvements.
#'
#' This function relies on \code{\link{exportBulkRecords}} to do the
#' bulk of the export before formatting for SAS. \dots are supplied to
#' exportBulkRecords so full user inversion of control still applies.
#'
#' @return A vector of exported data set names.
#' @export
#'
#' @seealso
#' [exportBulkRecords()]
#'
#' @examples
#' \dontrun{
#' exportSAS(rcon)
#' }
exportSAS <- function(
  rcon,
  directory = NULL,
  ...)
{
  coll <- checkmate::makeAssertCollection()

  checkmate::assert_class(x = rcon,
                          classes = "redcapApiConnection",
                          add = coll)

  checkmate::assert_character(x = directory,
                              max.len = 1,
                              null.ok = TRUE,
                              any.missing = FALSE,
                              add = coll)

  checkmate::reportAssertions(coll)

  if(is.null(directory)) directory <- getwd()

  curtime <- format(Sys.time(), '%Y%m%d%H%M%S')
  cb <- assembleCodebook(rcon, expand_check = TRUE)
  # used for looking up field_label
  tmpcb <- unique(cb[,c('field_name','field_label','form_name')])

  e <- new.env()
  args <- list(...)
  # envir/cast/lcon are overwritten if provided by user
  args[['envir']] <- e
  args[['cast']] <- raw_cast
  args[['lcon']] <- list(data = rcon)
  # create datasets in environment "e"
  do.call(exportBulkRecords, args)
  ds_names <- ls(envir = e)
  ds_names_real <- sub('data_', '', ds_names)
  form_list <- vector('list', length(ds_names))

  for(i in seq_along(ds_names))
  {
    dat_i <- e[[ds_names[i]]]
    form_name_i <- ds_names_real[i]
    fld_name_i <- names(dat_i)
    sas_name_i <- fld_name_i
    # SAS names have limited character length to consider
    long_vars_ix <- which(nchar(fld_name_i) > 30)
    if(length(long_vars_ix)) {
      long_vars <- substr(fld_name_i[long_vars_ix], 1, 20)
      # create unique names, starting with XXX_v_0
      lv_n <- unsplit(tapply(long_vars, long_vars, seq_along), long_vars) - 1
      sas_name_i[long_vars_ix] <- paste0(long_vars, '_v_', lv_n)
      names(dat_i)[long_vars_ix] <- sas_name_i[long_vars_ix]
    }

    FldMd1 <- data.frame(
      form_name = form_name_i,
      field_name = fld_name_i,
      sas_name = sas_name_i,
      order = seq_len(ncol(dat_i))
    )
    # find "field_label"
    FldMd2 <- merge(FldMd1, tmpcb, all.x = TRUE)
    no_fl_ix <- which(is.na(FldMd2[,'field_label']))
    # look for fields with form name mismatch -- typically System Fields
    FldMd2[no_fl_ix,'field_label'] <- tmpcb[match(FldMd2[no_fl_ix,'field_name'], tmpcb$field_name), 'field_label']
    # worst-case use "field_name"
    no_fl_ix <- which(is.na(FldMd2[,'field_label']))
    FldMd2[no_fl_ix,'field_label'] <- FldMd2[no_fl_ix,'field_name']
    FldMd2 <- FldMd2[order(FldMd2$order),]
    rownames(FldMd2) <- NULL
    form_list[[i]] <- FldMd2

    # write as csv
    sas_name <- sprintf('%s_redcap_%s.csv', curtime, form_name_i)
    write.csv(dat_i, file = file.path(directory, sas_name), row.names = FALSE, na = '.')
  }
  FormFldOrder <- do.call(rbind, form_list)
  # provide format/label in DATA step
  form_fields <- split(FormFldOrder, FormFldOrder[,'form_name'])

  # create SAS script
  fld_format_in <- c(
    alpha_only = '$500.',
    bioportal = '$5000.',
    calc = 'best32.',
    checkbox = 'best32.',
    date_ = 'yymmdd10.',
    datetime_ = '$500.',
    datetime_seconds_ = '$500.',
    descriptive = '$5000.',
    dropdown = 'best32.',
    email = '$500.',
    file = '$500.',
    form_complete = 'best32.',
    integer = 'best32.',
    notes = '$5000.',
    number = 'best32.',
    number_1dp = 'best32.',
    number_1dp_comma_decimal = '$500.',
    number_2dp = 'best32.',
    number_2dp_comma_decimal = '$500.',
    phone = '$500.',
    radio = 'best32.',
    slider = 'best32.',
    system_field = '$500.',
    text = '$500.',
    time = 'time5.',
    time_mm_ss = '$500.',
    truefalse = 'best32.',
    zipcode = '$500.',
    yesno = 'best32.'
  )
  fld_format_out <- c(
    alpha_only = '$500.',
    bioportal = '$5000.',
    calc = 'best12.',
    checkbox = 'best12.',
    date_ = 'yymmdd10.',
    datetime_ = '$500.',
    datetime_seconds_ = '$500.',
    descriptive = '$5000.',
    dropdown = 'best12.',
    email = '$500.',
    file = '$500.',
    form_complete = 'best12.',
    integer = 'best12.',
    notes = '$5000.',
    number = 'best12.',
    number_1dp = 'best12.',
    number_1dp_comma_decimal = '$500.',
    number_2dp = 'best12.',
    number_2dp_comma_decimal = '$500.',
    phone = '$500.',
    radio = 'best12.',
    slider = 'best12.',
    system_field = '$500.',
    text = '$500.',
    time = 'time5.',
    time_mm_ss = '$500.',
    truefalse = 'best12.',
    yesno = 'best12.',
    zipcode = '$500.'
  )

  # handling categorical variables
  y <- cb[,c('form_name','field_name','value','label')]
  y <- y[!is.na(y[,'value']) & y[,'value'] != 'NA',]
  # use FormFldOrder to pull CSV name (sas_name)
  y1 <- merge(y, FormFldOrder, all.x = TRUE)
  # look for fields with form name mismatch
  no_order_ix <- which(is.na(y1[,'sas_name']))
  fld_mm_ix <- match(y1[no_order_ix,'field_name'], FormFldOrder$field_name)
  # pull sas_name and field_label
  y1[no_order_ix,'sas_name'] <- FormFldOrder[fld_mm_ix,'sas_name']
  y1[no_order_ix,'field_label'] <- FormFldOrder[fld_mm_ix,'field_label']
  # look again - any missing are invalid
  y1 <- y1[!is.na(y1[,'sas_name']),]
  # replace field_name with sas_name
  y1$field_name <- y1$sas_name
  # reorder by value (keep?)
  y1 <- y1[order(y1[,'field_name'], y1[,'value']),]
  # these are used with PROC FORMAT
  fld_values <- split(y1[,c('value','label','field_name')], y1[,'field_name'])
  # collapse common 0/1 checkbox labels into "simple_checkbox"
  fld_values_key <- vapply(fld_values, function(i) {
    fv_i <- i[,c('value','label')]
    paste(do.call(paste, c(fv_i, sep = '|')), collapse = ';')
  }, character(1))
  simple_checkbox <- names(fld_values_key)[fld_values_key == '0|Unchecked;1|Checked']

  # handling data type
  z <- unique(cb[,c('form_name','field_name','field_type')])
  # use "FormFldOrder" to pull CSV column order
  # "z" contributes field_type
  z1 <- merge(z, FormFldOrder, all.y = TRUE)
  # look for fields with form name mismatch
  no_type_ix <- which(is.na(z1[,'field_type']))
  fld_mm_ix <- match(z1[no_type_ix, 'field_name'], z$field_name)
  # pull field_type
  z1[no_type_ix,'field_type'] <- z[fld_mm_ix,'field_type']
  # unknown field type
  no_type_ix <- which(is.na(z1[,'field_type']))
  z1[no_type_ix,'field_type'] <- 'unknown'
  # re-order by column order
  z1 <- z1[order(z1$form_name, z1$order),]
  # replace field_name with sas_name
  z1$field_name <- z1$sas_name

  fld_types <- sort(unique(z1$field_type))
  fld_types_unk <- fld_types[!(fld_types %in% names(fld_format_in))]
  if(length(fld_types_unk)) {
    logWarning(sprintf('unknown field types found (using SAS format $5000.): %s', paste(fld_types_unk, collapse = ', ')))
    fld_format_in[fld_types_unk] <- '$5000.'
    fld_format_out[fld_types_unk] <- '$5000.'
  }
  z1$field_format_in <- unname(fld_format_in[z1$field_type])
  z1$field_format_out <- unname(fld_format_out[z1$field_type])
  # this provides (IN)FORMAT
  csv_fields <- split(z1, z1[,'form_name'])

  cat_fields <- names(fld_values)
  if(length(simple_checkbox) > 1) {
    # location of checkbox fields
    simple_checkbox_ix <- which(names(fld_values) %in% simple_checkbox)
    # remove all checkbox fields except the first one
    fld_values <- fld_values[-simple_checkbox_ix[-1]]
  }
  fld_labvals <- vapply(fld_values, function(i) {
    i <- unique(i)
    fld_name <- i$field_name[1]
    # this should only happen once
    if(fld_name %in% simple_checkbox) {
      fld_name <- 'simple_checkbox'
    }
    if(any(duplicated(i$value))) logWarning(sprintf('field %s has discrepant labels/values; fix manually', fld_name))
    # check if any values are non-numeric
    has_char_vals <- any(grepl('[^0-9.-]', i$value))
    if(has_char_vals) {
      val_labs <- paste(sprintf("'%s'='%s'", i$value, i$label), collapse = ' ')
      fld_name <- paste0('$', fld_name)
    } else {
      val_labs <- paste(sprintf("%s='%s'", i$value, i$label), collapse = ' ')
    }
    sprintf('  value %s_ %s;\n', fld_name, val_labs)
  }, character(1), USE.NAMES = FALSE)
  proc_format <- sprintf('proc format;\n%srun;\n', paste(fld_labvals, collapse = ''))

  data_set_labelformat <- vapply(form_fields, function(i) {
    frm_name <- i$form_name[1]
    # categorical variables require "format"
    cat_flds_i <- i$sas_name[i$sas_name %in% cat_fields]
    if(length(cat_flds_i)) {
      format_field <- ifelse(cat_flds_i %in% simple_checkbox, 'simple_checkbox', cat_flds_i)
      fld_form <- paste(sprintf('  format %s %s_.;', cat_flds_i, format_field), collapse = '\n')
      fld_form <- paste0(fld_form, '\n')
    } else {
      fld_form <- ''
    }

    # labels for variables
    fld_labs <- paste(sprintf("  label %s='%s';", i$sas_name, i$field_label), collapse = '\n')

    # data statement
    sprintf('data %s;\n  set %s;\n%s\n%srun;\n', frm_name, frm_name, fld_labs, fld_form)
  }, character(1), USE.NAMES = FALSE)
  data_labels <- paste(data_set_labelformat, collapse = '\n')

  data_read <- vapply(csv_fields, function(i) {
    frm_name <- i$form_name[1]
    # informat
    fld_informat <- paste(sprintf("  informat %s %s;", i$field_name, i$field_format_in), collapse = '\n')
    # format
    fld_format <- paste(sprintf("  format %s %s;", i$field_name, i$field_format_out), collapse = '\n')
    # input
    fld_input <- paste0('  ', i$field_name)
    ix <- grep('^[$]', i$field_format_in)
    fld_input[ix] <- paste(fld_input[ix], '$')
    fld_input <- paste(fld_input, collapse = '\n')

    # data statement
    sprintf("data work.%s; %%let _EFIERR_ = 0;
infile \"&mydir.%s_redcap_%s.csv\" delimiter = ',' MISSOVER DSD lrecl=32767 firstobs=2 ;
%s
%s
input
%s
;
if _ERROR_ then call symput('_EFIERR_',\"1\");
run;
", frm_name, curtime, frm_name, fld_informat, fld_format, fld_input)
  }, character(1), USE.NAMES = FALSE)
  data_infile <- paste(data_read, collapse = '\n')

  sas_import <- sprintf("%%let mydir=/set/path/;\nOPTIONS nofmterr;\n\n%s\n%s\n%s", proc_format, data_infile, data_labels)
  cat(sas_import, file = file.path(directory, sprintf('%s_redcap_to_sas.sas', curtime)))
  ds_names_real
}
