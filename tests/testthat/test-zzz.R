test_that(".onAttach emits a startup message mentioning reflowR and entry points", {
  captured <- character(0)
  withCallingHandlers(
    reflowR:::.onAttach(libname = NULL, pkgname = "reflowR"),
    packageStartupMessage = function(m) {
      captured <<- c(captured, conditionMessage(m))
      invokeRestart("muffleMessage")
    }
  )
  msg <- paste(captured, collapse = "")
  expect_true(nzchar(msg))
  expect_match(msg, "reflowR")
  expect_match(msg, "reflow_init")
  expect_match(msg, "reflow_schemes")
})
