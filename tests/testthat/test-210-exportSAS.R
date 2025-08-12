context("ExportSAS")

load(file.path(test_path("testdata"),
               "test_redcapAPI_MetaData.Rdata"))
load(file.path(test_path("testdata"),
               "test_redcapAPI_Data.Rdata"))

importMetaData(rcon,
               test_redcapAPI_MetaData)

forms <- rcon$instruments()$instrument_name
Mappings <- data.frame(arm_num = rep(1, length(forms)),
                       unique_event_name = rep("event_1_arm_1", length(forms)),
                       form = forms)
importMappings(rcon,
               data = Mappings)

RepeatInst <- data.frame(event_name = "event_1_arm_1",
                         form_name = "repeating_instrument")

importRepeatingInstrumentsEvents(rcon,
                                 data = RepeatInst)

# castForImport only needed until 3.0.0
ImportData <- castForImport(test_redcapAPI_Data,
                            rcon,
                            validation = list(bioportal = valSkip),
                            cast = list(number_1dp = as.numeric,
                                        number_2dp = as.numeric,
                                        number_1dp_comma_decimal = as.numeric,
                                        number_2dp_comma_decimal = as.numeric,
                                        bioportal = as.character))

importRecords(rcon, ImportData)

test_that(
  "Return an error if rcon is not a redcapConnection",
  {
    local_reproducible_output(width = 200)
    expect_error(exportSAS("not an rcon"),
                 "Must inherit from class 'redcapApiConnection'")
  }
)

test_that(
  "Return an error if directory is not a character",
  {
    local_reproducible_output(width = 200)
    expect_error(exportSAS(rcon, 1), "Must be of type 'character'")
  }
)

test_that(
  "Return an error if directory is length > 1",
  {
    local_reproducible_output(width = 200)
    expect_error(exportSAS(rcon, letters[1:2]), "Must have length <= 1")
  }
)

test_that(
  "Produces the expected files",
  {
    d <- tempdir()
    expect_silent(result <- exportSAS(rcon, d))

    parts <- c("branching_logic", "calculated_fields", "dates_and_times",
      "files_notes_descriptions", "multiple_choice", "numbers",
      "randomization", "record_id", "repeating_instrument",
      "slider_fields", "text_fields")

    expect_contains(result, parts)
    files <- dir(d)
    fpart <- gsub(".*_redcap_|\\.csv|\\.sas", "", files)

    expect_contains(fpart, c(parts, "to_sas"))

    unlink(file.path(d, files[fpart %in% parts]))
  }
)


