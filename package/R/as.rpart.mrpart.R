as.rpart <-
  function(x, ...)
{
  UseMethod("as.rpart")
}

as.rpart.mrpart <-
function(x, treenum=1, ...)
{
  x[[treenum]]$model
}
