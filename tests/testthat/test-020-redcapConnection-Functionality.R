context("redcapConnection Functionality")

# Look for a yaml config for automated environments
config_file <- file.path("..", paste0(basename(getwd()),".yml"))
API_KEY <-
  if(file.exists(config_file))
  {
    config <- read_yaml(config_file)
    config$redcapAPI$keys$TestRedcapAPI
  } else 
  {
    keyring::key_get('redcapAPI', testdb, 'API_KEYs')
  }


test_that("redcapApiConnection can be created",
          expect_class(
            redcapConnection(url = url, token = API_KEY),
            classes = c("redcapApiConnection", "redcapConnection")
          )
)


