print.summary.qrmix=function(x,...)
{
  cat("Call:\n")
  print(x$call)
  cat("\n")
  cat("Residuals:\n")
  print(summary(x$residuals))

  cat("\n")
  cat("Quantiles:\n")
  print(x$quantiles)

  cat("\n")
  cat("Sizes:\n")
  print(table(x$clusters))

  cat("\n")
  if(!is.null(x$quantreg))
  {
    print(x$quantreg)
  }else{
  print(x[grepl('^cluster',names(x))&(!grepl('s$',names(x)))])}
}
