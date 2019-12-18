#FIXME: create a class for lists of dual objects and 
#FIXME: identify and set standard initial values for this class
#       (what to do if the user intentionally omits certain arguments)
#FIXME: create the related "vectorized" functions

## dual_vec needs to be another class... (useful for vectorized operations)
dual_vec <- function(reals, vectors, byrow = FALSE) {
  # uses reals and vectors
  stopifnot(length(reals) == ifelse(byrow, nrow(vectors), ncol(vectors)))
  if (byrow) vectors <- t(vectors)
  mapply(function(xx, yy) {
    dual(xx, vectors[, yy])
  }, reals, seq_len(length(reals)))
}
  
# @export 
basis <- function(length, position, mode = "double") {
  # mode only for "logical", "integer", "numeric", "double"
  stopifnot(mode %in% c("logical", "integer", "numeric", "double"))
  res <- vector(mode = mode, length = length)
  if (mode == "logical") res[position] <- TRUE
  if (mode == "integer") res[position] <- 1L
  if (mode %in% c("numeric", "double")) res[position] <- 1
  return(res)
}

# @export
dualize_vec <- function(vec) {
  mapply(function(xx, yy) {
      dual(xx, basis(length(vec), yy))
    }, vec, seq_len(length(vec)))
}

