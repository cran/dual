#' @name Arithmetic
#' @aliases Arithmetic
#' @aliases +
#' @aliases +,dual,missing-method
#' @aliases +,dual,numeric-method
#' @aliases +,numeric,dual-method
#' @aliases +,dual,dual-method
#' @aliases -
#' @aliases -,dual,missing-method
#' @aliases -,dual,numeric-method
#' @aliases -,numeric,dual-method
#' @aliases -,dual,dual-method
#' @aliases *
#' @aliases *,dual,numeric-method
#' @aliases *,numeric,dual-method
#' @aliases *,dual,dual-method
#' @aliases /
#' @aliases /,dual,numeric-method
#' @aliases /,numeric,dual-method
#' @aliases /,dual,dual-method
#' @aliases ^
#' @aliases ^,dual,numeric-method
#' @aliases ^,numeric,dual-method
#' @aliases ^,dual,dual-method
#'
#' @title Arithmetic Operators
#'
# @usage
#  + e1
#  - e1
#  e1 + e2
#  e1 - e2
#  e1 * e2
#  e1 / e2
#  e1 ^ e2
#
#' @description These unary and binary operators perform arithmetic on dual objects.
#'
#' @param e1 dual object or numeric value.
#' @param e2 dual object or numeric value.
#' 
#' @return The correspondent values of the arithmetic operation on \code{e1} and \code{e2} is returned.
#' 
#' @docType methods
#' 
#' @examples 
#' x <- dual(1.5, 1:0)
#' y <- dual(2.6, 0:1)
#' +x
#' -x
#' x - y
#' x * y
#' x / y
#' x ^ y
#' x + y
#' @rdname Arithmetic
#' @exportMethod +
setMethod("+", signature(e1 = "dual", e2 = "missing"), function (e1, e2) 
{
  return(e1)
}
)
#' @rdname Arithmetic
#' @exportMethod +
setMethod("+",
    signature(e1 = "dual", e2 = "numeric"),
    function (e1, e2) 
    {
      if (length(e2) > 1L) warning("Only the first element will be used")
      ans <- dual(f = e1@f + e2[1L], grad = e1@grad)
      return(ans)
    }
)
#' @rdname Arithmetic
#' @exportMethod +
setMethod("+",
    signature(e1 = "numeric", e2 = "dual"),
    function (e1, e2) 
    {
      if (length(e1) > 1L) warning("Only the first element will be used")
      ans <- dual(f = e1[1L] + e2@f, grad = e2@grad)
      return(ans)
    }
)
#' @rdname Arithmetic
#' @exportMethod +
setMethod("+",
  signature(e1 = "dual", e2 = "dual"),
  function (e1, e2) 
  {
    stopifnot(length(e1@grad) == length(e2@grad))
    ans <- dual(f = e1@f + e2@f, 
                grad = e1@grad + e2@grad)
    return(ans)
  }
)
# showMethods("+")
# +x
# x+x
# x+2
# 2+x
# c(1:3)+x
# x+c(1:3)

#' @rdname Arithmetic
#' @exportMethod -
setMethod("-", signature(e1 = "dual", e2 = "missing"), function (e1, e2) 
{
  ans <- dual(f = -e1@f, grad = -e1@grad)
  return(ans)
}
)
#' @rdname Arithmetic
#' @exportMethod -
setMethod("-",
          signature(e1 = "dual", e2 = "numeric"),
          function (e1, e2) 
          {
            if (length(e2) > 1L) warning("Only the first element will be used")
            ans <- dual(f = e1@f - e2[1L], grad = e1@grad)
            return(ans)
          }
)
#' @rdname Arithmetic
#' @exportMethod -
setMethod("-",
          signature(e1 = "numeric", e2 = "dual"),
          function (e1, e2) 
          {
            if (length(e1) > 1L) warning("Only the first element will be used")
            ans <- dual(f = e1[1L] - e2@f, grad = -e2@grad)
            return(ans)
          }
)
#' @rdname Arithmetic
#' @exportMethod -
setMethod("-",
          signature(e1 = "dual", e2 = "dual"),
          function (e1, e2) 
          {
            stopifnot(length(e1@grad) == length(e2@grad))
            ans <- dual(f = e1@f - e2@f, 
                        grad = e1@grad - e2@grad)
            return(ans)
          }
)
# showMethods("-")
# -x
# x-x
# x-2
# 2-x
# c(1:3)-x
# x-c(1:3)

#' @rdname Arithmetic
#' @exportMethod *
setMethod("*",
          signature(e1 = "dual", e2 = "numeric"),
          function (e1, e2) 
          {
            if (length(e2) > 1L) warning("Only the first element will be used")
            ans <- dual(f = e1@f * e2[1L], grad = e1@grad * e2[1L])
            return(ans)
          }
)
#' @rdname Arithmetic
#' @exportMethod *
setMethod("*",
          signature(e1 = "numeric", e2 = "dual"),
          function (e1, e2) 
          {
            if (length(e1) > 1L) warning("Only the first element will be used")
            ans <- dual(f = e1[1L] * e2@f, grad = e1[1L] * e2@grad)
            return(ans)
          }
)
#' @rdname Arithmetic
#' @exportMethod *
setMethod("*",
          signature(e1 = "dual", e2 = "dual"),
          function (e1, e2) 
          {
            stopifnot(length(e1@grad) == length(e2@grad))
            ans <- dual(f = e1@f * e2@f, 
                        grad = e1@grad * e2@f + e1@f * e2@grad)
            return(ans)
          }
)
#showMethods("*")
# x*y
# x*2
# 2*x
# 1:3*x
# x*1:3

#' @rdname Arithmetic
#' @exportMethod /
setMethod("/",
          signature(e1 = "dual", e2 = "numeric"),
          function (e1, e2) 
          {
            if (length(e2) > 1L) warning("Only the first element will be used")
            ie2 <- 1.0 / e2[1L]
            ans <- dual(f = e1@f * ie2, grad = e1@grad * ie2)
            return(ans)
          }
)
#' @rdname Arithmetic
#' @exportMethod /
setMethod("/",
          signature(e1 = "numeric", e2 = "dual"),
          function (e1, e2)
          {
            if (length(e1) > 1L) warning("Only the first element will be used")
            ie2 <- 1.0 / e2@f
            anf <- e1[1L] * ie2
            ans <- dual(f = anf, grad = -anf * ie2 * e2@grad)
            return(ans)
          }
)
#' @rdname Arithmetic
#' @exportMethod /
setMethod("/",
          signature(e1 = "dual", e2 = "dual"),
          function (e1, e2) 
          {
            stopifnot(length(e1@grad) == length(e2@grad))
            ie2 <- 1.0 / e2@f
            anf <- e1@f * ie2
            ans <- dual(f = anf, 
                        grad = (e1@grad - anf * e2@grad) * ie2)
            return(ans)
          }
)
#showMethods("/")
# x <- dual(f = 2, grad = 1:0)
# y <- dual(f = 4, grad = 0:1)
# x/y
# x/2
# 2/x
# 1:3/x
# x/1:3

#' @rdname Arithmetic
#' @exportMethod ^
setMethod("^",
          signature(e1 = "dual", e2 = "numeric"),
          function (e1, e2) 
          {
            if (length(e2) > 1L) warning("Only the first element will be used")
            ans <- dual(f = e1@f^e2[1L], grad = e2[1L] * e1@f^(e2[1L] - 1) * e1@grad)
            return(ans)
          }
)
# x^2.1
# x^(2:1)

#' @rdname Arithmetic
#' @exportMethod ^
setMethod("^",
          signature(e1 = "numeric", e2 = "dual"),
          function (e1, e2) 
          {
            if (length(e1) > 1L) warning("Only the first element will be used")
            anf <- e1[1L]^e2@f
            ans <- dual(f = anf, grad = anf * log(e1[1L]) * e2@grad)
            return(ans)
          }
)
# 2.1^x
# (2:1)^x

#' @rdname Arithmetic
#' @exportMethod ^
setMethod("^",
          signature(e1 = "dual", e2 = "dual"),
          function (e1, e2) 
          {
            stopifnot(length(e1@grad) == length(e2@grad))
            return(exp(e2 * log(e1)))
          }
)
#x^y
