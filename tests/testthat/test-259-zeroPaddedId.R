purgeProject(rcon, records=TRUE)
load(file.path(test_path("testdata"),
               "test_redcapAPI_MetaData.Rdata"))
load(file.path(test_path("testdata"),
               "test_redcapAPI_Data.Rdata"))
load(file.path(test_path("testdata"),
               "test_redcapAPI_Arms.Rdata"))
load(file.path(test_path("testdata"),
               "test_redcapAPI_Events.Rdata"))

forms <- c("record_id", "text_fields")
MetaData <- test_redcapAPI_MetaData[test_redcapAPI_MetaData$form_name %in% forms, ]

MetaData$field_name[1] <- "zero_padded_id"
MetaData$text_validation_type_or_show_slider_number[1] <- "integer"

importMetaData(rcon, MetaData)
importArms(rcon,     test_redcapAPI_Arms)
importEvents(rcon,   test_redcapAPI_Events)
importProjectInformation(rcon,
                         data.frame(is_longitudinal = 1,
                                    record_autonumbering_enabled = 0))

Mappings <- data.frame(arm_num = rep(1, length(forms)),
                       unique_event_name = rep("event_1_arm_1", length(forms)),
                       form = forms)
importMappings(rcon, Mappings)


names(test_redcapAPI_Data)[1] <- "zero_padded_id"
ImportData <- test_redcapAPI_Data[names(test_redcapAPI_Data) %in% MetaData$field_name]
ImportData <- ImportData[!is.na(ImportData$email_test), ]
ImportData$zero_padded_id <- sprintf("%06d", 1:nrow(ImportData))


test_that("cast preserves zeropadded id",{

  # castForImport only needed until 3.0.0
  castImport <- castForImport(ImportData,
                              rcon,
                              validation = list(bioportal = valSkip),
                              cast = list(number_1dp = as.numeric,
                                          number_2dp = as.numeric,
                                          number_1dp_comma_decimal = as.numeric,
                                          number_2dp_comma_decimal = as.numeric,
                                          bioportal = as.character))
  expect_equal(ImportData$zero_padded_id, castImport$zero_padded_id)
})

test_that("zeropadded id makes a round trip",{
  importRecords(rcon, ImportData)
  x <- exportRecordsTyped(rcon)
  expect_equal(ImportData$zero_padded_id, sort(as.vector(x$zero_padded_id)))
})


