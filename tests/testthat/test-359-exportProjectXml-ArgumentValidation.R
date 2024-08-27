context("Export Project XML Argument Validation")

xml_file <- tempfile(fileext = ".xml")

test_that(
  "Return an error when rcon is not a redcapConnection", 
  {
    expect_error(exportProjectXml(rcon = "not an rcon", 
                                  file = xml_file), 
                 "no applicable method for 'exportProjectXml'")
  }
)

test_that(
  "Return an error when file is not character(1)", 
  {
    expect_error(exportProjectXml(rcon, 
                                  file = c("file1", "file2")), 
                 "'file': Must have length 1")
    
    expect_error(exportProjectXml(rcon, 
                                  file = 123), 
                 "'file': Must be of type 'character'")
  }
)

test_that(
  "Return an error when return_metadata_only is not logical(1)", 
  {
    expect_error(exportProjectXml(rcon, 
                                  file = xml_file, 
                                  return_metadata_only = "TRUE"), 
                 "'return_metadata_only': Must be of type 'logical'")
    expect_error(exportProjectXml(rcon, 
                                  file = xml_file, 
                                  return_metadata_only = c(TRUE, FALSE)), 
                 "'return_metadata_only': Must have length 1")
  }
)

test_that(
  "Return an error when records is not character", 
  {
    expect_error(exportProjectXml(rcon, 
                                  file = xml_file, 
                                  records = TRUE), 
                 "'records': Must be of type 'character'")
  }
)

test_that(
  "Return an error when fields is not character", 
  {
    expect_error(exportProjectXml(rcon, 
                                  file = xml_file, 
                                  fields = TRUE), 
                 "'fields': Must be of type 'character'")
    
    expect_error(exportProjectXml(rcon, 
                                  file = xml_file, 
                                  fields = "not_a_field"), 
                 "'fields': Must be a subset of")
  }
)

test_that(
  "Return an error when records is not character", 
  {
    expect_error(exportProjectXml(rcon, 
                                  file = xml_file, 
                                  records = TRUE), 
                 "'records': Must be of type 'character'")
    
    expect_error(exportProjectXml(rcon, 
                                  file = xml_file, 
                                  events = "not_a_field"), 
                 "'events': Must be a subset of")
  }
)

test_that(
  "Return an error when survey is not logical(1)", 
  {
    expect_error(exportProjectXml(rcon, 
                                  file = xml_file, 
                                  survey = "TRUE"), 
                 "'survey': Must be of type 'logical'")
    expect_error(exportProjectXml(rcon, 
                                  file = xml_file, 
                                  survey = c(TRUE, FALSE)), 
                 "'survey': Must have length 1")
  }
)

test_that(
  "Return an error when dag is not logical(1)", 
  {
    expect_error(exportProjectXml(rcon, 
                                  file = xml_file, 
                                  dag = "TRUE"), 
                 "'dag': Must be of type 'logical'")
    expect_error(exportProjectXml(rcon, 
                                  file = xml_file, 
                                  dag = c(TRUE, FALSE)), 
                 "'dag': Must have length 1")
  }
)

test_that(
  "Return an error when export_files is not logical(1)", 
  {
    expect_error(exportProjectXml(rcon, 
                                  file = xml_file, 
                                  export_files = "TRUE"), 
                 "'export_files': Must be of type 'logical'")
    expect_error(exportProjectXml(rcon, 
                                  file = xml_file, 
                                  export_files = c(TRUE, FALSE)), 
                 "'export_files': Must have length 1")
  }
)

test_that(
  "Validate config, api_param", 
  {
    local_reproducible_output(width = 200)

    expect_error(exportProjectXml(rcon, 
                                  file = xml_file,
                                  config = list(1)), 
                 "'config': Must have names")
    expect_error(exportProjectXml(rcon, 
                                  file = xml_file,
                                  config = "not a list"), 
                 "'config': Must be of type 'list'")
    
    expect_error(exportProjectXml(rcon, 
                                  file = xml_file,
                                  api_param = list(1)), 
                 "'api_param': Must have names")
    expect_error(exportProjectXml(rcon, 
                                  file = xml_file,
                                  api_param = "not a list"), 
                 "'api_param': Must be of type 'list'")
  }
)
