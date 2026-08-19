test_that("valid bag inputs pass", {
  inputs <- list(
    pattern = "bag",
    measurements = list(width = 18, height = 40),
    fabric = list(width = 140)
  )
  result <- validate_inputs(inputs)
  expect_equal(result$pattern, "bag")
  expect_equal(result$measurements$width, 18)
})

test_that("unknown pattern is rejected", {
  inputs <- list(
    pattern = "toga",
    measurements = list(),
    fabric = list(width = 140)
  )
  expect_error(validate_inputs(inputs), "Unknown pattern")
})

test_that("non-positive measurement is rejected", {
  inputs <- list(
    pattern = "bag",
    measurements = list(width = 0, height = 35),
    fabric = list(width = 140)
  )
  expect_error(validate_inputs(inputs), "must be > 0")
})

test_that("missing fabric width is rejected", {
  inputs <- list(
    pattern = "bag",
    measurements = list(width = 40, height = 35),
    fabric = list()
  )
  expect_error(validate_inputs(inputs), "Fabric width")
})