library(gbm)


gbm_custom <- getModelInfo("gbm")$gbm


gbm_custom[["predictors"]] <- function(x, ...) {
  vi <- gbm::relative.influence(x, n.trees = x$tuneValue$n.trees)
  names(vi)[vi > 0]
}

gbm_custom[["varImp"]] <- function(object, numTrees = NULL, ...) {
  if(is.null(numTrees)) numTrees <- object$tuneValue$n.trees
  varImp <- gbm::relative.influence(object, n.trees = numTrees)
  out <- data.frame(varImp)
  colnames(out) <- "Overall"
  rownames(out) <- object$var.names
  out
}













