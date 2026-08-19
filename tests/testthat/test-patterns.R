# Sanity checks for Stella's generate_bag() - only checking the shape
# of what it returns, not re-testing her geometry decisions.

test_that("generate_bag returns a Pattern object with a 'full' piece", {
  pattern <- generate_bag(measurements = list(width = 18, height = 40))

  expect_equal(pattern$id, "bag")
  expect_equal(pattern$units, "cm")
  expect_length(pattern$pieces, 1)
  expect_equal(pattern$pieces[[1]]$id, "full")

  bbox <- sf::st_bbox(pattern$pieces[[1]]$geometry)
  expect_equal(unname(bbox["xmax"] - bbox["xmin"]), 18)
  expect_equal(unname(bbox["ymax"] - bbox["ymin"]), 80)  # height is doubled internally
})