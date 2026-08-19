# End-to-end pipeline test for the bag pattern - the one pattern we're
# focusing on getting fully working before wiring up underdress /
# circleskirt. Exercises validate_inputs() -> generate_pattern() ->
# calculate_layout() -> calculate_fabric() together, the same way
# app_server.R does via run_pipeline_safe().

test_that("run_pipeline() works end-to-end for a bag, no rotation needed", {
  inputs <- list(
    pattern = "bag",
    measurements = list(width = 18, height = 40),  # flat height = 80
    fabric = list(width = 140)
  )

  result <- run_pipeline(inputs)

  # Pattern: pieces should be a NAMED list (see normalize_pattern_pieces())
  expect_true("full" %in% names(result$pattern$pieces))
  expect_s3_class(result$pattern$pieces$full$geometry, "POLYGON")

  # Layout: bag is narrower (18) than fabric width (140) and taller (80)
  # than wide, so no rotation is expected and required length == 80.
  expect_equal(result$layout$placements[[1]]$rotation, 0)
  expect_equal(result$layout$fabric$length, 80)

  # Fabric: recommended = required + 10cm default margin, and must not
  # error on the (previously undefined) required_length reference.
  expect_equal(result$fabric$required_length, 80)
  expect_equal(result$fabric$recommended_length, 90)
})

test_that("run_pipeline() rotates a bag piece wider than it is tall", {
  inputs <- list(
    pattern = "bag",
    measurements = list(width = 100, height = 10),  # flat height = 20 -> wider than tall
    fabric = list(width = 140)
  )

  result <- run_pipeline(inputs)

  # dim_x = 100, dim_y = 20 -> rotates 90 degrees, required length
  # should be the LONG side (100), not the short side (20).
  expect_equal(result$layout$placements[[1]]$rotation, 90)
  expect_equal(result$layout$fabric$length, 100)
})

test_that("run_pipeline() rejects invalid inputs before touching A/B code", {
  inputs <- list(
    pattern = "bag",
    measurements = list(width = -5, height = 40),
    fabric = list(width = 140)
  )
  expect_error(run_pipeline(inputs), "must be > 0")
})

test_that("run_pipeline_safe() never throws, reports errors instead", {
  inputs <- list(pattern = "bag", measurements = list(width = -5, height = 40),
                  fabric = list(width = 140))
  out <- run_pipeline_safe(inputs)
  expect_null(out$result)
  expect_true(grepl("must be > 0", out$error))
})
