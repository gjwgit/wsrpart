print.mrpart <- function(x, ...)
{
  cat("A multiple rpart model with", length(x),
      ifelse(length(x) == 1, "tree.\n\n", "trees.\n\n"))
  lapply(x, function(d) {cat("Variables Used: ", paste(d$vars.used, collapse=", "),
                             ".\n\n", sep="")
                         print(d$model)
                         cat("\nOOB Error is ", round(d$oob.error, 3), ".\n\n", sep="")
                         })
}
