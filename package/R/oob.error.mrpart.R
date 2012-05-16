oob.error.mrpart <- function(object, data, formula)
 {
   oob <- sapply(object,function(m) m$oob.scores)

   oobscolist <- list(id = rownames(Reduce(rbind, oob)),
                      class = apply(Reduce(rbind, oob), 1,
                        function(x)names(which.max(x))))

   oobscolist$id <- as.integer(oobscolist$id)

   oobvotes <- table(oobscolist)

   oobmat <- matrix(as.vector(oobvotes),
                    nrow=length(levels(as.factor(oobscolist$id))))
   row.names(oobmat) <- levels(as.factor(oobscolist$id))
   colnames(oobmat) <- levels(as.factor(oobscolist$class))

   oobclass <- apply(oobmat, 1, function(x)names(which.max(x)))

   # Get target variable

   vars <- as.character(attr(terms(model.frame(formula, data)), "variables"))[-1L]
   target <- vars[[1]]

   forest.oob.error <-
     sum(oobclass != data[as.numeric(names(oobclass)), target])/length(oobclass)

   return(forest.oob.error)
}
