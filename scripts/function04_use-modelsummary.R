library(parameters)

# We use the `modelsummary` package to make the tables. I can't get it to work
# out of the box with `brmsfit` models out of the box, even with broom.mixed,
# so, per
# https://vincentarelbundock.github.io/modelsummary/articles/modelsummary.html#adding-new-models-part-ii-
# I create my own brmsfit tidiers that solve the problem.

tidy.brmsfit <- function(x, ...) {
  s <- model_parameters(x, 
                        ci = .9, 
                        ci_method = "eti", 
                        effects = "fixed")
  ret <- data.frame(
    term = s[, 1],
    estimate = s[, 2],
    conf.low = s[, 4],
    conf.high = s[, 5]
  )
  ret
}

glance.brmsfit <- function(x, ...) {
  ret <- data.frame(
    n   = as.character(nobs(x)))
  ret
}
