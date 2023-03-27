context("deleteFromFileRepository.R")

rcon <- redcapConnection(url= url, 
                         token = API_KEY)

# Only the argument validations are tested in this file. 
# See test-fileRepositoryTests.R for functional tests

test_that(
  "Return an error if rcon is not a redcapApiConnection", 
  {
    local_reproducible_output(width = 200)
    expect_error(deleteFromFileRepository(mtcars, 
                                          doc_id = 123), 
                 "no applicable method for 'deleteFromFileRepository'")
  }
)

test_that(
  "Return an error if doc_id is not integerish(1)", 
  {
    local_reproducible_output(width = 200)
    expect_error(deleteFromFileRepository(rcon, 
                                          doc_id = c(123, 234)), 
                 "'doc_id'[:] Must have length 1")
    expect_error(deleteFromFileRepository(rcon, 
                                          doc_id = "123"), 
                 "'doc_id'[:] Must be of type 'integerish'")
  }
)

test_that(
  "Return an error if refresh is not logical(1)", 
  {
    local_reproducible_output(width = 200)
    expect_error(deleteFromFileRepository(rcon, 
                                          doc_id = 123, 
                                          refresh = c(TRUE, FALSE)), 
                 "'refresh'[:] Must have length 1")
    expect_error(deleteFromFileRepository(rcon, 
                                          doc_id = 123, 
                                          refresh = "TRUE"), 
                 "'refresh'[:] Must be of type 'logical'")
  }
)
