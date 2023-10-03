#' @name Logic
#' @aliases Logic
#' @aliases ==
#' @aliases ==,dual,numeric-method
#' @aliases ==,numeric,dual-method
#' @aliases ==,dual,dual-method
#' @aliases !=
#' @aliases !=,dual,numeric-method
#' @aliases !=,numeric,dual-method
#' @aliases !=,dual,dual-method
#' @aliases >
#' @aliases >,dual,numeric-method
#' @aliases >,numeric,dual-method
#' @aliases >,dual,dual-method
#' @aliases >=
#' @aliases >=,dual,numeric-method
#' @aliases >=,numeric,dual-method
#' @aliases >=,dual,dual-method
#' @aliases <
#' @aliases <,dual,numeric-method
#' @aliases <,numeric,dual-method
#' @aliases <,dual,dual-method
#' @aliases <=
#' @aliases <=,dual,numeric-method
#' @aliases <=,numeric,dual-method
#' @aliases <=,dual,dual-method
#'
#' @title Logic Operators for Comparing Dual Numbers
#' 
#' @description
#' These functions provide the operators for logical comparisons between dual 
#' numbers.  These operators are designed to test equality and 
#' inequalities (such as "not equal", "greater", "less", "greater or equal", 
#' "less or equal"). 
#'
# @usage
# x == y
# x != y
# x > y
# x >= y
# x < y
# x <= y
#' 
#' @param e1 dual object or numeric value.
#' @param e2 dual object or numeric value.
#' 
#' @return The correspondent boolean/logical value of the comparison between
#' \code{e1} and \code{e2} is returned.
#' 
#' @docType methods
#' 
#' @examples 
#' x <- dual(1.5, 1:0)
#' y <- dual(2.6, 0:1)
#' x == y
#' x != y
#' x > y
#' x >= y
#' x < y
#' x <= y
#' @rdname Logic
#' @exportMethod ==
setMethod("==",
    signature(e1 = "dual", e2 = "numeric"),
    function (e1, e2) 
    {
      if (length(e2) > 1L) warning("Only the first element will be used")
      ans <- (e1@f == e2[1L]) & all(e1@grad == 0)
      return(ans)
    }
)
#' @rdname Logic
#' @exportMethod ==
setMethod("==",
    signature(e1 = "numeric", e2 = "dual"),
    function (e1, e2) 
    {
      if (length(e1) > 1L) warning("Only the first element will be used")
      ans <- (e2@f == e1[1L]) & all(e2@grad == 0)
      return(ans)
    }
)
#' @rdname Logic
#' @exportMethod ==
setMethod("==",
    signature(e1 = "dual", e2 = "dual"),
    function (e1, e2) 
    {
      stopifnot(length(e1@grad) == length(e2@grad))
      ans <- (e2@f == e1@f) & all(e1@grad == e2@grad)
      return(ans)
    }
)
# showMethods("==")
# x == 1
# 2 == x
# x == x

#' @rdname Logic
#' @exportMethod !=
setMethod("!=",
    signature(e1 = "dual", e2 = "numeric"),
    function (e1, e2) 
    {
      if (length(e2) > 1L) warning("Only the first element will be used")
      ans <- (e1@f != e2[1L]) | any(e1@grad != 0)
      return(ans)
    }
)
#' @rdname Logic
#' @exportMethod !=
setMethod("!=",
    signature(e1 = "numeric", e2 = "dual"),
    function (e1, e2) 
    {
      if (length(e1) > 1L) warning("Only the first element will be used")
      ans <- (e2@f != e1[1L]) | any(e2@grad != 0)
      return(ans)
    }
)
#' @rdname Logic
#' @exportMethod !=
setMethod("!=",
    signature(e1 = "dual", e2 = "dual"),
    function (e1, e2) 
    {
      stopifnot(length(e1@grad) == length(e2@grad))
      ans <- (e2@f != e1@f) | any(e1@grad != e2@grad)
      return(ans)
    }
)
# showMethods("!=")
# x != 1
# 2 != x
# x != x

#' @rdname Logic
#' @exportMethod >
setMethod(">",
    signature(e1 = "dual", e2 = "numeric"),
    function (e1, e2) 
    {
      if (length(e2) > 1L) warning("Only the first element will be used")
      ans <- (e1@f > e2[1L]) | ((e1@f == e2[1L]) & all(e1@grad > 0))
      return(ans)
    }
)
#' @rdname Logic
#' @exportMethod >
setMethod(">",
    signature(e1 = "numeric", e2 = "dual"),
    function (e1, e2) 
    {
      if (length(e1) > 1L) warning("Only the first element will be used")
      ans <- (e1[1L] > e2@f) | ((e1[1L] == e2@f) & all(0 > e2@grad))
      return(ans)
    }
)
#' @rdname Logic
#' @exportMethod >
setMethod(">",
    signature(e1 = "dual", e2 = "dual"),
    function (e1, e2) 
    {
      stopifnot(length(e1@grad) == length(e2@grad))
      ans <- (e1@f > e2@f) | ((e1@f == e2@f) & all(e1@grad > e2@grad))
      return(ans)
    }
)
# showMethods(">")
# x > 1
# 2 > x
# x > x

#' @rdname Logic
#' @exportMethod >=
setMethod(">=",
    signature(e1 = "dual", e2 = "numeric"),
    function (e1, e2) 
    {
      if (length(e2) > 1L) warning("Only the first element will be used")
      ans <- (e1@f > e2[1L]) | ((e1@f == e2[1L]) & all(e1@grad >= 0))
      return(ans)
    }
)
#' @rdname Logic
#' @exportMethod >=
setMethod(">=",
    signature(e1 = "numeric", e2 = "dual"),
    function (e1, e2) 
    {
      if (length(e1) > 1L) warning("Only the first element will be used")
      ans <- (e1[1L] > e2@f) | ((e1[1L] == e2@f) & all(0 >= e2@grad))
      return(ans)
    }
)
#' @rdname Logic
#' @exportMethod >=
setMethod(">=",
    signature(e1 = "dual", e2 = "dual"),
    function (e1, e2) 
    {
      stopifnot(length(e1@grad) == length(e2@grad))
      ans <- (e1@f > e2@f) | ((e1@f == e2@f) & all(e1@grad >= e2@grad))
      return(ans)
    }
)
# showMethods(">=")
# x >= 1
# 2 >= x
# x >= x

#' @rdname Logic
#' @exportMethod <
setMethod("<",
    signature(e1 = "dual", e2 = "numeric"),
    function (e1, e2) 
    {
      if (length(e2) > 1L) warning("Only the first element will be used")
      ans <- (e1@f < e2[1L]) | any(e1@grad < 0)
      return(ans)
    }
)
#' @rdname Logic
#' @exportMethod <
setMethod("<",
    signature(e1 = "numeric", e2 = "dual"),
    function (e1, e2) 
    {
      if (length(e1) > 1L) warning("Only the first element will be used")
      ans <- (e1[1L] < e2@f) | any(0 < e2@grad)
      return(ans)
    }
)
#' @rdname Logic
#' @exportMethod <
setMethod("<",
    signature(e1 = "dual", e2 = "dual"),
    function (e1, e2) 
    {
      stopifnot(length(e1@grad) == length(e2@grad))
      ans <- (e1@f < e2@f) | any(e1@grad < e2@grad)
      return(ans)
    }
)
# showMethods("<")
# x < 1
# 2 < x
# x < x

#' @rdname Logic
#' @exportMethod <=
setMethod("<=",
    signature(e1 = "dual", e2 = "numeric"),
    function (e1, e2) 
    {
      if (length(e2) > 1L) warning("Only the first element will be used")
      ans <- (e1@f < e2[1L]) | any(e1@grad < 0) | 
             ((e1@f == e2[1L]) & all(e1@grad == 0))
      return(ans)
    }
)
#' @rdname Logic
#' @exportMethod <=
setMethod("<=",
    signature(e1 = "numeric", e2 = "dual"),
    function (e1, e2) 
    {
      if (length(e1) > 1L) warning("Only the first element will be used")
      ans <- (e1[1L] < e2@f) | any(0 < e2@grad) |
             ((e1[1L] == e2@f) & all(0 == e2@grad))
      return(ans)
    }
)
#' @rdname Logic
#' @exportMethod <=
setMethod("<=",
    signature(e1 = "dual", e2 = "dual"),
    function (e1, e2) 
    {
      stopifnot(length(e1@grad) == length(e2@grad))
      ans <- (e1@f < e2@f) | any(e1@grad < e2@grad) |
             ((e1@f == e2@f) & all(e1@grad == e2@grad))
      return(ans)
    }
)
# showMethods("<=")
# x <= 1
# 2 <= x
# x <= x

