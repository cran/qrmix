print.qrmix = function(x, ...)
{
  cat("Call:\n")
  print(x$call)
  cat("\nCoefficientes:\n")
  print(x$coefficients)
}
