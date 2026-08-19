# Basic sanity checks for Stella's geometry utilities, used directly by
# fabric_layout_plot.R for placing bag pieces on the fabric.

test_that("rotate_geometry rotates a square 90 degrees about the origin", {
  square <- sf::st_polygon(list(matrix(
    c(0, 0,  10, 0,  10, 10,  0, 10,  0, 0),
    ncol = 2, byrow = TRUE
  )))
  rotated <- rotate_geometry(square, 90)
  bbox <- sf::st_bbox(rotated)

  expect_equal(unname(bbox["xmin"]), -10, tolerance = 1e-6)
  expect_equal(unname(bbox["xmax"]), 0,   tolerance = 1e-6)
  expect_equal(unname(bbox["ymin"]), 0,   tolerance = 1e-6)
  expect_equal(unname(bbox["ymax"]), 10,  tolerance = 1e-6)
})

test_that("move_geometry translates coordinates by dx/dy", {
  square <- sf::st_polygon(list(matrix(
    c(0, 0,  10, 0,  10, 10,  0, 10,  0, 0),
    ncol = 2, byrow = TRUE
  )))
  moved <- move_geometry(square, dx = 5, dy = -2)
  bbox <- sf::st_bbox(moved)

  expect_equal(unname(bbox["xmin"]), 5, tolerance = 1e-6)
  expect_equal(unname(bbox["ymin"]), -2, tolerance = 1e-6)
})