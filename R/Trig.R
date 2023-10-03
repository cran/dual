#' @name Trig
#' @aliases Trig
#' @aliases cos,dual-method
#' @aliases sin,dual-method
#' @aliases tan,dual-method
#' @aliases acos,dual-method
#' @aliases arccos,dual-method
#' @aliases asin,dual-method
#' @aliases arcsin,dual-method
#' @aliases atan,dual-method
#' @aliases arctan,dual-method
#' @aliases atan2,ANY,dual-method
#' @aliases atan2,dual,ANY-method
#' @aliases atan2,dual,dual-method
#' @aliases cospi,dual-method
#' @aliases sinpi,dual-method
#' @aliases tanpi,dual-method
#' 
#' @title Trigonometric Functions
#'
#' @description 
#' These functions give the obvious trigonometric functions.  They
#' respectively compute the cosine, sine, tangent, arc-cosine, arc-sine,
#' arc-tangent, and the two-argument arc-tangent.
#'
#' \code{cospi(x)}, \code{sinpi(x)}, and \code{tanpi(x)}, compute
#' \code{cos(pi*x)}, \code{sin(pi*x)}, and \code{tan(pi*x)}.
#'
# @usage
# cos(x)
# sin(x)
# tan(x)
# 
# acos(x)
# asin(x)
# atan(x)
# atan2(y, x)
# 
# cospi(x)
# sinpi(x)
# tanpi(x)
#' 
#' @param x dual object or numeric value.
#' @param y dual object or numeric value.
#' 
#' @return  A dual object containing the transformed values according to the chosen function.
#' 
#' @docType methods
#' 
#' @examples 
#' x <- dual(1, 1:0)
#' y <- dual(1, 0:1)
#' 
#' cos(x)
#' sin(x)
#' tan(x)
#' acos(x - 0.5)
#' asin(x - 0.5)
#' atan(x - 0.5)
#' atan2(x, y)
#' atan2(2.4, y)
#' atan2(x, 1.2)
#' cospi(1.2 * x)
#' sinpi(3.4 * x)
#' tanpi(5.6 * x)
#' @rdname Trig
#' @exportMethod cos
setMethod("cos",
          signature(x = "dual"),
          function (x) 
          {
            ans <- dual(f = cos(x@f), grad = -x@grad * sin(x@f))
            return(ans)
          }
)

#' @rdname Trig
#' @exportMethod sin
setMethod("sin",
          signature(x = "dual"),
          function (x) 
          {
            ans <- dual(f = sin(x@f), grad = x@grad * cos(x@f))
            return(ans)
          }
)

#' @rdname Trig
#' @exportMethod tan
setMethod("tan",
          signature(x = "dual"),
          function (x) 
          {
            return(sin(x) / cos(x))
          }
)
# cos(x)
# sin(x)
# tan(x)

#' @rdname Trig
#' @exportMethod acos
setMethod("acos",
          signature(x = "dual"),
          function (x) 
          {
            ifu <- -1.0 / sqrt(1.0 - x@f * x@f)
            ans <- dual(f = acos(x@f), grad = x@grad * ifu)
            return(ans)
          }
)

#' @rdname Trig
#' @exportMethod asin
setMethod("asin",
          signature(x = "dual"),
          function (x) 
          {
            ifu <- 1.0 / sqrt(1.0 - x@f * x@f)
            ans <- dual(f = asin(x@f), grad = x@grad * ifu)
            return(ans)
          }
)

#' @rdname Trig
#' @exportMethod atan
setMethod("atan",
          signature(x = "dual"),
          function (x) 
          {
            ifu <- 1.0 / (1.0 + x@f * x@f)
            ans <- dual(f = atan(x@f), grad = x@grad * ifu)
            return(ans)
          }
)
# acos(x)
# asin(x)
# atan(x)

#' @rdname Trig
#' @exportMethod atan2
setMethod("atan2",
          signature(y = "dual", x = "numeric"),
          function (y, x) 
          {
            if (length(x) > 1L) warning("Only the first element will be used")
            derf <- x[1L] / (y@f * y@f + x[1L] * x[1L])
            ans <- dual(f = atan2(y@f, x[1L]), grad = y@grad * derf)
            return(ans)
          }
)

#' @rdname Trig
#' @exportMethod atan2
setMethod("atan2",
          signature(y = "numeric", x = "dual"),
          function (y, x) 
          {
            if (length(y) > 1L) warning("Only the first element will be used")
            derg <- y[1L] / (y[1L] * y[1L] + x@f * x@f)
            ans <- dual(f = atan2(y[1L], x@f), grad = -derg * x@grad)
            return(ans)
          }
)

#' @rdname Trig
#' @exportMethod atan2
setMethod("atan2",
          signature(y = "dual", x = "dual"),
          function (y, x) 
          {
            stopifnot(length(y@grad) == length(x@grad))
            derg <- y@f * y@f
            derf <- x@f * x@f
            derg <- 1.0 / (derg + derf)
            derf <- x@f * derg
            derg <- y@f * derg
            ans <- dual(f = atan2(y@f, x@f), 
                        grad = y@grad * derf - derg * x@grad)
            return(ans)
          }
)
# atan2(x, y)
# atan2(2.4, y)
# atan2(x, 1.2)

#' @rdname Trig
#' @exportMethod cospi
setMethod("cospi",
          signature(x = "dual"),
          function (x) 
          {
            ans <- dual(f = cospi(x@f), grad = -x@grad * sinpi(x@f) * pi)
            return(ans)
          }
)

#' @rdname Trig
#' @exportMethod sinpi
setMethod("sinpi",
          signature(x = "dual"),
          function (x) 
          {
            ans <- dual(f = sinpi(x@f), grad = x@grad * cospi(x@f) * pi)
            return(ans)
          }
)

#' @rdname Trig
#' @exportMethod tanpi
setMethod("tanpi",
          signature(x = "dual"),
          function (x) 
          {
            return(sinpi(x) / cospi(x))
          }
)
# cospi(x)
# sinpi(x)
# tanpi(x)
