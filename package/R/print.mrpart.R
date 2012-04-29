print.mrpart <-
function(x, ...)
{
  cat("A multiple rpart model with ", length(x), " tree",
      ifelse(length(x) == 1, "", "s"), ".\n\n", sep="")

  lapply(x, function(d)
         {
           # Work out the exdent for the formatted list of variable names.

           exdent <- 19 + nchar(as.character(length(d$vars.used)))
  
           cat("Variables used (", length(d$vars.used), "): ",
               paste(strwrap(paste(d$vars.used, collapse=", "),
                             width=60, exdent=exdent), collapse="\n"),
               ".\n\n", sep="")
           print(d$model, ...)
           cat("\nOOB Error: ", round(d$oob.error, 3), ".\n\n", sep="")
         })
}
