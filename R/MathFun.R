#' @name MathFun
#' @aliases MathFun
#' @aliases abs,dual-method
#' @aliases sqrt,dual-method
#' 
#' @title Miscellaneous Mathematical Functions
#' 
#' @description 
#' The function \code{abs(x)} computes the absolute value of \code{x}, 
#' while \code{sqrt(x)} computes the square root of \code{x}.
#' 
#' @param x a dual object or numeric value.
#' 
#' @return A dual object containing the transformed values according to the chosen function.
#' 
# @usage 
# abs(x)
# sqrt(x)
#
#' @docType methods
#' 
#' @examples 
#' x <- dual(4.3, 1:0)
#' y <- dual(7.6, 0:1)
#' abs(-2.2 * x + 0.321 * y)
#' sqrt(y - x)
#' 
#' @rdname MathFun
#' @exportMethod sqrt
setMethod("sqrt",
          signature(x = "dual"),
          function (x) 
          {
            sfx <- sqrt(x@f)
            ans <- dual(f = sfx, grad = (0.5 / sfx) * x@grad)
            return(ans)
          }
)
#sqrt(y-x)

#' @rdname MathFun
#' @exportMethod abs
setMethod("abs",
          signature(x = "dual"),
          function (x) 
          {
            ans <- dual(f = abs(x@f), grad = sign(x@f) * x@grad)
            return(ans)
          }
)
#abs((-2.2*x+0.321*y))
