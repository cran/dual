#' @name Error
#' @aliases Error
#' @aliases erf
#' @aliases erf,dual-mehtod
#' @aliases erfc
#' @aliases erfc,dual-mehtod
#' @aliases erfinv
#' @aliases erfinv,dual-mehtod
#' @aliases erfcinv
#' @aliases erfcinv,dual-mehtod
#' 
#' @title Special Functions of Mathematics
#' 
#' @description
#' Special mathematical functions related to the error function. 
#' 
#' The function \code{erfc(x)} is a variant of the cumulative normal (or Gaussian) distribution funciton.
#' 
#' The functions \code{erfinv(x)} and \code{erfcinv(x)} respectively implement the inverse functions of \code{erf(x)} and \code{erfc(x)}. 
#'  
# erf(x)
# erfc(x)
# erfinv(x)
# erfcinv(x)
#' 
#' @param x dual object.
#'
#' @return A dual object containing the transformed values according to the chosen function.
#' 
#' @examples 
#' x <- dual(0.5, 1)
#' erf(x)
#' erfc(x)
#' erfinv(x)
#' erfcinv(x)
#' 
#' @rdname Error
#' @export erf
erf <- function(x) UseMethod("erf", x)

#' @rdname Error
#' @exportMethod erf
setMethod("erf",
          signature(x = "dual"),
          function(x) {
            anf <- sqrt(2) * x@f
            ans <- dual(f = stats::pchisq(anf * anf, 1) * sign(x@f),
                        grad = sqrt(8) * stats::dnorm(anf) * x@grad)
            return(ans)
          })

#' @rdname Error
#' @export erfinv
erfinv <- function(x) UseMethod("erfinv", x)

#' @rdname Error
#' @exportMethod erfinv
setMethod("erfinv", 
          signature(x = "dual"),
          function(x) {
            if (abs(x@f) > 1) return(NA)
            anf <- dual(sqrt(stats::qchisq(abs(x@f), 1) * 0.5) * sign(x@f), 1)
            enf <- erf(anf)
            ans <- dual(f = anf@f, grad = x@grad / enf@grad)
            return(ans)
          })

#' @rdname Error
#' @export erfc
erfc <- function(x) UseMethod("erfc", x)

#' @rdname Error
#' @exportMethod erfc
setMethod("erfc",
          signature(x = "dual"),
          function(x) {
            anf <- sqrt(2) * x@f
            ans <- dual(f = 2 * stats::pnorm(-anf), 
                        grad = -sqrt(8) * stats::dnorm(-anf) * x@grad)
            return(ans)
          })

#' @rdname Error
#' @export erfcinv
erfcinv <- function(x) UseMethod("erfcinv", x)

#' @rdname Error
#' @exportMethod erfcinv
setMethod("erfcinv", 
          signature(x = "dual"),
          function(x) {
            if (x@f < 0) return(NA)
            if (x@f > 2) return(NA)
            anf <- dual(-stats::qnorm(x@f * 0.5) / sqrt(2), 1)
            enf <- erfc(anf)
            ans <- dual(f = anf@f, grad = x@grad / enf@grad)
            return(ans)
          })
