context("prepUserImportData.R")

User <- exportUsers(rcon)

#####################################################################
# Functional Tests                                               ####

test_that(
  "Use consolidate = TRUE", 
  {
    ThisUser <- User
    ThisUser$design <- sample(c("Access", "NoAccess"), 
                              nrow(ThisUser), 
                              replace = TRUE)
    ImportUser <- prepUserImportData(User, rcon)
    for (v in REDCAP_USER_TABLE_ACCESS_VARIABLES){
      expect_true(all(ImportUser[[v]] %in% c(0, 1)))
    }
  }
)

#####################################################################
# Argument Validation                                            ####

test_that(
  "Return an error if data is not a data frame",
  {
    local_reproducible_output(width = 200)
    expect_error(prepUserImportData("not data", 
                                    rcon = rcon), 
                 "'data': Must be of type 'data.frame'")
    
    BadData <- data.frame(username = "someone", 
                          not_a_field = 0)
    expect_error(prepUserImportData(BadData, 
                                    rcon = rcon), 
                 "has additional elements [{]'not_a_field'[}]")
  }
)

test_that(
  "Return an error if rcon is not a redCapConnection", 
  {
    local_reproducible_output(width = 200)
    expect_error(prepUserImportData(User, 
                                    rcon = "not a connection"), 
                 "Must inherit from class 'redcapConnection'")
  }
)

test_that(
  "Return an error if consolidate is not logical(1)", 
  {
    local_reproducible_output(width = 200)
    expect_error(prepUserImportData(User, 
                                    rcon = rcon, 
                                    consolidate = "TRUE"), 
                 "'consolidate': Must be of type 'logical'")
    
    expect_error(prepUserImportData(User, 
                                    rcon = rcon, 
                                    consolidate = c(TRUE, FALSE)), 
                 "'consolidate': Must have length 1")
  }
)
