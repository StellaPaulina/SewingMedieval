# Sanity checks for Liba's bag_fabric_layout() - specifically the
# required fabric length calculation, which had a swapped-axis bug (see
# BUGFIX comments in R/layouts/bag_layout.R).
#
# NOTE: bag_fabric_layout() itself expects pattern$pieces$full (named
# list), but generate_bag() alone still returns pieces unnamed (see
# normalize_pattern_pieces() in run_pipeline.R) - so tests here go
# through that same normalization, exactly as run_pipeline() does.
#
# (Function is named bag_fabric_layout(), not bag_layout() -
# run_pipeline.R's calculate_layout() dispatcher calls it by that name;
# these tests were calling a name that doesn't exist and would have
# failed with "could not find function", fixed to match.)

test_that("bag_fabric_layout: tall/narrow piece needs no rotation, length = tall side", {
  pattern <- normalize_pattern_pieces(
    generate_bag(measurements = list(width = 18, height = 40))  # -> 18 x 80
  )
  layout <- bag_fabric_layout(pattern, fabric_width = 140)

  expect_equal(layout$placements[[1]]$rotation, 0)
  expect_equal(layout$fabric$length, 80)
})

test_that("bag_fabric_layout: wide/short piece rotates, length = long side", {
  pattern <- normalize_pattern_pieces(
    generate_bag(measurements = list(width = 100, height = 10))  # -> 100 x 20
  )
  layout <- bag_fabric_layout(pattern, fabric_width = 140)

  expect_equal(layout$placements[[1]]$rotation, 90)
  expect_equal(layout$fabric$length, 100)
})

test_that("bag_fabric_layout errors when the piece doesn't fit the fabric at all", {
  pattern <- normalize_pattern_pieces(
    generate_bag(measurements = list(width = 200, height = 100))  # -> 200 x 200
  )
  expect_error(bag_fabric_layout(pattern, fabric_width = 140), "too large")
})