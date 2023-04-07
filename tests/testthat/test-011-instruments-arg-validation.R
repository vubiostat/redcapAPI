context("instruments, import/export mappings, export PDF argument validations")

rcon <- redcapConnection(url = url, token = API_KEY)

#####################################################################
# exportInstruments

test_that(
  "Return an error when rcon is not a redcapConnection object", 
  {
    local_reproducible_output(width = 200)
    expect_error(exportInstruments("not an rcon"), 
                 "no applicable method for 'exportInstruments'")
  }
)

#####################################################################
# exportMappings

test_that(
  "Return an error when rcon is not a redcapConnection object", 
  {
    local_reproducible_output(width = 200)
    expect_error(exportMappings("not an rcon"), 
                 "no applicable method for 'exportMappings'")
  }
)

test_that(
  "Return an error when arms is not character",
  {
    local_reproducible_output(width = 200)
    expect_error(exportMappings(rcon, 
                                arms = 1:2), 
                 "Variable 'arms'[:] Must be of type 'character'")
  }
)

#####################################################################
# importMappings

# FIXME: Add tests after writing file

#####################################################################
# exportPDF

# FIXME: Add tests