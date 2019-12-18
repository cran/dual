#' @name Hyperbolic
#' @aliases Hyperbolic
#' @aliases cosh,dual-method
#' @aliases sinh,dual-method
#' @aliases tanh,dual-method
#' @aliases acosh,dual-method
#' @aliases asinh,dual-method
#' @aliases atanh,dual-method
#' 
#' @title Hyperbolic Functions
#' 
#' @description
#' These functions provide the obvious hyperbolic functions. 
#' They respectively compute the hyperbolic cosine, sine, tangent, 
#' and their inverses, arc-cosine, arc-sine, arc-tangent.
#' 
#' @param x a dual object
#' 
#' @return A dual object containing the transformed values according to the chosen function.
#'
#' @docType methods
#' 
#' @examples
#' x <- dual(0.5, 1)
#' cosh(x)
#' sinh(x)
#' tanh(x)
#' acosh(1 + x)
#' asinh(x)
#' atanh(x)
#' 
#' @rdname Hyperbolic
#' @exportMethod cosh
setMethod("cosh",
          signature(x = "dual"),
          function (x) 
          {
            ans <- dual(f = cosh(x@f), 
                        grad = sinh(x@f) * x@grad)
            return(ans)
          }
)
# cosh(x)

#' @rdname Hyperbolic
#' @exportMethod sinh
setMethod("sinh",
          signature(x = "dual"),
          function (x) 
          {
            ans <- dual(f = sinh(x@f), 
                        grad = cosh(x@f) * x@grad)
            return(ans)
          }
)
# sinh(x)

#' @rdname Hyperbolic
#' @exportMethod tanh
setMethod("tanh",
          signature(x = "dual"),
          function (x) 
          {
            ang <- 1 - tanh(x@f)^2
            ans <- dual(f = tanh(x@f), 
                        grad = ang * x@grad)
            return(ans)
          }
)
# tanh(x)

#' @rdname Hyperbolic
#' @exportMethod acosh
setMethod("acosh",
          signature(x = "dual"),
          function (x) 
          {
            ang <- 1 / sqrt(x@f * x@f - 1)
            ans <- dual(f = acosh(x@f), 
                        grad = ang * x@grad)
            return(ans)
          }
)
# acosh(x)

#' @rdname Hyperbolic
#' @exportMethod asinh
setMethod("asinh",
          signature(x = "dual"),
          function (x) 
          {
            ang <- 1 / sqrt(x@f * x@f + 1)
            ans <- dual(f = asinh(x@f), 
                        grad = ang * x@grad)
            return(ans)
          }
)
# asinh(x)

#' @rdname Hyperbolic
#' @exportMethod atanh
setMethod("atanh",
          signature(x = "dual"),
          function (x) 
          {
            ang <- 1 / (1 - x@f * x@f) 
            ans <- dual(f = atanh(x@f), 
                        grad = ang * x@grad)
            return(ans)
          }
)
# atanh(x)
