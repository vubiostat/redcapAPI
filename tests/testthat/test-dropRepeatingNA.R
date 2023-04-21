context("dropRepeatingNA")

rcon <- redcapConnection(url = url, token = API_KEY)
rec <- exportRecordsTyped(rcon)

# Note: This test assumes that there is at least one instance of a redcap_repeat_instrument field that is NA

# get nrow of rec
rows <- nrow(rec)

# drop all instances of redcap_repeat_instrument that are NA
rec_drop <- dropRepeatingNA(rec, rcon, quiet=FALSE)

# test that repeating instruments are dropped
test_that("NA records are dropped", expect_false(nrow(rec_drop) >= rows))