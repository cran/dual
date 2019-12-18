#' @name dual-class
#' @aliases dual-class
#' @title Dual object class
#' An S4 Class for dual numbers
#' 
#' @slot f a single numeric value denoting the "Real" component of the dual number
#' @slot grad a numeric vector rappresenting the "Dual" components of the dual number
#' @exportClass dual
dual <- setClass("dual", slots = c(f = "numeric", grad = "numeric"),
                 prototype = methods::prototype(f = 0, grad = 0))

#' @rdname dual-class
#' @description The method \code{initialize} sets the initial values of a new object of the class \code{dual}.
#' @param .Object an object of class \code{dual} to be initialized
setMethod("initialize", "dual",
          function(.Object, f = numeric(0), grad = numeric(0)) {
            .Object@f <- f
            .Object@grad <- grad
            .Object
          })

#' @rdname dual-class
#' @aliases dual
#' @description The function \code{dual} generates an object of class \code{dual} for the representation of dual numbers.
#' @usage dual(f, grad)
#' @param f a single numeric value denoting the "Real" component of the dual number.
#' @param grad a numeric vector rappresenting the "Dual" components of the dual number.
#' @return an object of the class \code{dual}.
#' @examples
#' x <- dual(3, 0:1)
#' @export
dual <- function(f, grad) new("dual", f = f, grad = grad)

#' @rdname dual-class
#' @description The function \code{is.dual} returns \code{TRUE} if \code{x} is of the class \code{dual}. It retuns \code{FALSE} otherwise.
#' @aliases is.dual,ANY,dual-method
#' @usage is.dual(x)
#' @param x an object of class \code{dual}.
#' @return a logical value indicating if the object is of the class \code{dual} or not.
#' @examples
#' library(dual)
#' x <- new("dual", f = 1, grad = 1)
#' is.dual(3)
#' is.dual(x)
#' @export
"is.dual" <- function(x) {
  methods::is(x, "dual")
}

# "as.dual" <- function(x, grad) {
#   ans <- dual(f = x, grad = grad)
#   return(ans)
# }

#' @rdname dual-class 
#' @description The method \code{show} shows the content of a \code{dual} object.
#' @param object an object of class \code{dual} to be shown
setMethod("show",
          signature(object = "dual"),
          function (object) 
          {
            rn <- sprintf("Real: %f", object@f)
            dn <- sprintf("%f", object@grad)
            cat(rn, "\nDuals: ",
                paste(dn, collapse = " "), "\n", sep = "")
          }
)
