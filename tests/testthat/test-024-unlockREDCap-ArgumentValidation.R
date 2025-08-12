context("unlockREDCap argument validation")

library(mockery)
library(curl)

test_that("unlockREDCap only accepts URL character(1)",
{
  local_reproducible_output(width = 200)

  expect_error(
    unlockREDCap(c(test_conn    = 'TestRedcapAPI',
                   sandbox_conn = 'SandboxAPI'),
                 keyring      = '<NAME_OF_KEY_RING_HERE>',
                 envir        = globalenv(),
                 url          = TRUE),
    "Variable 'url': Must be of type 'character', not 'logical'")

  expect_error(
    unlockREDCap(c(test_conn    = 'TestRedcapAPI',
                   sandbox_conn = 'SandboxAPI'),
                 keyring      = '<NAME_OF_KEY_RING_HERE>',
                 envir        = globalenv(),
                 url          = character(0)),
    "Variable 'url': Must have length 1, but has length 0")

  expect_error(
    unlockREDCap(c(test_conn    = 'TestRedcapAPI',
                   sandbox_conn = 'SandboxAPI'),
                 keyring      = '<NAME_OF_KEY_RING_HERE>',
                 envir        = globalenv(),
                 url          = c("a", "b")),
    "Variable 'url': Must have length 1, but has length 2")
})

test_that("unlockREDCap only accepts keyring character(1)",
{
  local_reproducible_output(width = 200)

  expect_error(
    unlockREDCap(c(test_conn    = 'TestRedcapAPI',
                   sandbox_conn = 'SandboxAPI'),
                 keyring      = TRUE,
                 envir        = globalenv(),
                 url          = "url"),
    "Variable 'keyring': Must be of type 'character', not 'logical'")

  expect_error(
    unlockREDCap(c(test_conn    = 'TestRedcapAPI',
                   sandbox_conn = 'SandboxAPI'),
                 envir        = globalenv(),
                 keyring      = character(0),
                 url          = "url"),
    "Variable 'keyring': Must have length 1, but has length 0")

  expect_error(
    unlockREDCap(c(test_conn    = 'TestRedcapAPI',
                   sandbox_conn = 'SandboxAPI'),
                 envir        = globalenv(),
                 keyring      = c("a", "b"),
                 url          = "url"),
    "Variable 'keyring': Must have length 1, but has length 2")
})

test_that("unlockREDCap only accepts envir environment",
{
  local_reproducible_output(width = 200)
  expect_error(
    unlockREDCap(c(test_conn    = 'TestRedcapAPI',
                   sandbox_conn = 'SandboxAPI'),
                 keyring      = '<NAME_OF_KEY_RING_HERE>',
                 envir        = TRUE,
                 url          = "url"),
    "Variable 'envir': Must inherit from class 'environment', but has class 'logical'")
})
