generate_pattern <- function(inputs) {

  # inputs already validated (hopefully)

  if (inputs$pattern == "bag") {
    return(generate_bag(
      measurements = inputs$measurements,
      options = inputs$options
    ))
  }

  if (inputs$pattern == "underdress") {
    return(generate_underdress(
      measurements = inputs$measurements,
      options = inputs$options
    ))
  }

  if (inputs$pattern == "circleskirt") {
    return(generate_circleskirt(
      measurements = inputs$measurements,
      options = inputs$options
    ))
  }

  stop("Unknown pattern")
}