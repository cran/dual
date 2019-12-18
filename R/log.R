#' @name log
#' @aliases log
#' @aliases log,dual-method
#' @aliases logb
#' @aliases logb,dual,numeric-method
#' @aliases logb,numeric,dual-method
#' @aliases logb,dual,dual-method
#' @aliases log10
#' @aliases log10,dual-method
#' @aliases log2
#' @aliases log2,dual-method
#' @aliases log1p
#' @aliases log1p,dual-method
#' @aliases exp
#' @aliases exp,dual-method
#' @aliases expm1
#' @aliases expm1,dual-method
#'
#' @title Logarithms and Exponentials
#' 
# @usage 
# log(x) # with base = exp(1)
# logb(x, base = exp(1))
# log10(x)
# log2(x)
# 
# log1p(x)
# 
# exp(x)
# expm1(x)
# 
#' @param x a dual object or numeric value.
#' @param base a dual object or a positive number. Defaults to \code{e=exp(1)}.
#' 
#' @return A dual object containing the transformed values according to the chosen function.
#' 
#' @docType methods
#' 
#' @examples 
#' x <- dual(sqrt(pi), 1:0)
#' y <- dual(pi * .75, 0:1)
#' log(x)
#' logb(x, base = 1.1)
#' logb(3.1, base = x)
#'
#' logb(x, y)
#' log10(x)
#' log2(x)
#' 
#' log1p(x)
#' 
#' exp(2*x)
#' expm1(2*x)
#' @rdname log
#' @exportMethod log
setMethod("log",
          signature(x = "dual"),
          function (x) 
          {
            ifu <- 1.0 / x@f
            ans <- dual(f = log(x@f), grad = x@grad * ifu)
            return(ans)
          }
)
#' @rdname log
#' @exportMethod logb
setMethod("logb",
          signature(x = "dual", base = "numeric"),
          function (x, base) 
          {
            if (length(base) > 1L) warning("Only the first element will be used")
            ifu <- 1.0 / x@f / log(base[1L])
            ans <- dual(f = log(x = x@f, base = base[1L]), 
                        grad = x@grad * ifu)
            return(ans)
          }
)
#' @rdname log
#' @exportMethod logb
setMethod("logb",
          signature(x = "numeric", base = "dual"),
          function (x, base) 
          {
            if (length(x) > 1L) warning("Only the first element will be used")
            tmp <- log(base@f)
            ifu <- -log(x[1L]) / (tmp * tmp) / base@f
            ans <- dual(f = log(x = x[1L], base = base@f), 
                        grad = base@grad * ifu)
            return(ans)
          }
)
#' @rdname log
#' @exportMethod logb
setMethod("logb",
          signature(x = "dual", base = "dual"),
          function (x, base) 
          {
            stopifnot(length(x@grad) == length(base@grad))
            lx <- log(x)
            lb <- log(base)
            return(lx / lb)
          }
)
#' @rdname log
#' @exportMethod log10
setMethod("log10",
          signature(x = "dual"),
          function (x) 
          {
            ifu <- 1.0 / x@f / log(10)
            ans <- dual(f = log10(x@f), grad = x@grad * ifu)
            return(ans)
          }
)
#' @rdname log
#' @exportMethod log2
setMethod("log2",
          signature(x = "dual"),
          function (x) 
          {
            ifu <- 1.0 / x@f / log(2)
            ans <- dual(f = log2(x@f), grad = x@grad * ifu)
            return(ans)
          }
)
#' @rdname log
#' @exportMethod log1p
setMethod("log1p",
          signature(x = "dual"),
          function (x) 
          {
            ifu <- 1.0 / (1.0 + x@f)
            ans <- dual(f = log1p(x@f), grad = x@grad * ifu)
            return(ans)
          }
)
#' @rdname log
#' @exportMethod exp
setMethod("exp",
          signature(x = "dual"),
          function (x) 
          {
            anf <- exp(x@f)
            ans <- dual(f = anf, grad = x@grad * anf)
            return(ans)
          }
)
#' @rdname log
#' @exportMethod expm1
setMethod("expm1",
          signature(x = "dual"),
          function (x) 
          {
            tmp <- exp(x@f)
            ans <- dual(f = expm1(x@f), grad = x@grad * tmp)
            return(ans)
          }
)
