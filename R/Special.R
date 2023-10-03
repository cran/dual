#' @name Special
#' @aliases Special
#' @aliases beta,dual,dual-method
#' @aliases beta,numeric,dual-method
#' @aliases beta,dual,numeric-method
#' @aliases lbeta,dual,dual-method
#' @aliases lbeta,numeric,dual-method
#' @aliases lbeta,dual,numeric-method
#' @aliases gamma,dual-mehtod
#' @aliases lgamma,dual-mehtod
#' @aliases psigamma,dual,NULL-mehtod
#' @aliases digamma,dual-mehtod
#' @aliases trigamma,dual-mehtod
#' @aliases choose,dual,dual-method
#' @aliases choose,numeric,dual-method
#' @aliases choose,dual,numeric-method
#' @aliases lchoose,dual,dual-method
#' @aliases lchoose,numeric,dual-method
#' @aliases lchoose,dual,numeric-method
#' @aliases factorial,dual-mehtod
#' @aliases lfactorial,dual-mehtod
#' 
#' @title Special Functions of Mathematics
#' 
#' @description
#' Special mathematical functions related to the beta and gamma. 
#'  
# @usage 
# beta(a, b)
# lbeta(a, b)
# 
# gamma(x)
# lgamma(x)
# psigamma(x, deriv = 0)
# digamma(x)
# trigamma(x)
# 
# choose(n, k)
# lchoose(n, k)
# factorial(x)
# lfactorial(x)
# 
#' 
#' @param a non-negative numeric value or dual object with non-negative real part.
#' @param b non-negative numeric value or dual object with non-negative real part.
#' @param x dual object or numeric value.
#' @param n dual object or numeric value.
#' @param k dual object or numeric value.
#' @param deriv integer value.
#'
#' @return A dual object containing the transformed values according to the chosen function.
#' 
#' @docType methods
#' 
#' @examples 
#' x <- dual(0.5, 1)
#' a <- dual(1.2, 1:0)
#' b <- dual(2.1, 0:1)
#' 
#' beta(a, b)
#' beta(1, b)
#' beta(a, 1)
#' lbeta(a, b)
#' lbeta(1, b)
#' lbeta(a, 1)
#' 
#' gamma(x)
#' lgamma(x)
#' psigamma(x, deriv = 0)
#' digamma(x)
#' trigamma(x)
#' psigamma(x, 2)
#' psigamma(x, 3)
#' 
#' 
#' n <- 7.8 + a
#' k <- 5.6 + b
#' choose(n, k)
#' choose(5, k)
#' choose(n, 2)
#' 
#' lchoose(n, k)
#' lchoose(5, k)
#' lchoose(n, 2)
#' 
#' factorial(x)
#' lfactorial(x)
#' 
#' @rdname Special
#' @exportMethod beta
setMethod("beta",
          signature(a = "dual", b = "dual"),
          function(a, b) {
            stopifnot(length(a@grad) == length(b@grad))
            ans <- exp(lbeta(a, b))
            return(ans)
          })

#' @rdname Special
#' @exportMethod beta
setMethod("beta",
          signature(a = "dual", b = "numeric"),
          function(a, b) {
            ans <- exp(lbeta(a, b))
            return(ans)
          })

#' @rdname Special
#' @exportMethod beta
setMethod("beta",
          signature(a = "numeric", b = "dual"),
          function(a, b) {
            ans <- exp(lbeta(a, b))
            return(ans)
          })

#' @rdname Special
#' @exportMethod lbeta
setMethod("lbeta",
          signature(a = "dual", b = "dual"),
          function(a, b) {
            stopifnot(length(a@grad) == length(b@grad))
            ans <- lgamma(a) + lgamma(b) - lgamma(a + b)
            return(ans)
          })

#' @rdname Special
#' @exportMethod lbeta
setMethod("lbeta",
          signature(a = "dual", b = "numeric"),
          function(a, b) {
            if (length(b) > 1L) warning("Only the first element will be used")
            ans <- lgamma(a) + lgamma(b[1L]) - lgamma(a + b[1L])
            return(ans)
          })

#' @rdname Special
#' @exportMethod lbeta
setMethod("lbeta",
          signature(a = "numeric", b = "dual"),
          function(a, b) {
            if (length(a) > 1L) warning("Only the first element will be used")
            ans <- lgamma(a[1L]) + lgamma(b) - lgamma(a[1L] + b)
            return(ans)
          })

#' @rdname Special
#' @exportMethod gamma
setMethod("gamma",
          signature(x = "dual"),
          function(x) {
            anf <- gamma(x@f)
            ans <- dual(f = anf,
                        grad = digamma(x@f) * anf * x@grad)
            return(ans)
          })

#' @rdname Special
#' @exportMethod lgamma
setMethod("lgamma",
          signature(x = "dual"),
          function(x) {
            ans <- dual(f = lgamma(x@f),
                        grad = digamma(x@f) * x@grad)
            return(ans)
          })

#' @rdname Special
#' @exportMethod psigamma
setMethod("psigamma",
          signature(x = "dual"),
          function(x, deriv) {
            if(missing(deriv)) deriv <- 0L
            if (length(deriv) > 1L) warning("Only the first element will be used")
            ans <- dual(f = psigamma(x@f, deriv = deriv[1L]),
                        grad = psigamma(x@f, deriv = deriv[1L] + 1L) * x@grad)
            return(ans)
          })

#' @rdname Special
#' @exportMethod digamma
setMethod("digamma",
          signature(x = "dual"),
          function(x) {
            ans <- dual(f = digamma(x@f),
                        grad = trigamma(x@f) * x@grad)
            return(ans)
          })

#' @rdname Special
#' @exportMethod trigamma
setMethod("trigamma",
          signature(x = "dual"),
          function(x) {
            ans <- dual(f = trigamma(x@f),
                        grad = psigamma(x@f, 2L) * x@grad)
            return(ans)
          })

#' @rdname Special
#' @exportMethod choose
setMethod("choose",
          signature(n = "dual", k = "dual"),
          function(n, k) {
            stopifnot(length(n@grad) == length(k@grad))
            ans <- exp(lchoose(n, k))
            return(ans)
          })

#' @rdname Special
#' @exportMethod choose
setMethod("choose",
          signature(n = "numeric", k = "dual"),
          function(n, k) {
            ans <- exp(lchoose(n, k))
            return(ans)
          })

#' @rdname Special
#' @exportMethod choose
setMethod("choose",
          signature(n = "dual", k = "numeric"),
          function(n, k) {
            ans <- exp(lchoose(n, k))
            return(ans)
          })

#' @rdname Special
#' @exportMethod lchoose
setMethod("lchoose",
          signature(n = "dual", k = "dual"),
          function(n, k) {
            stopifnot(length(n@grad) == length(k@grad))
            ans <- lfactorial(k) + lfactorial(n - k)
            ans <- lfactorial(n) - ans;
            return(ans)
          })

#' @rdname Special
#' @exportMethod lchoose
setMethod("lchoose",
          signature(n = "numeric", k = "dual"),
          function(n, k) {
            if (length(n) > 1L) warning("Only the first element will be used")
            ans <- lfactorial(k) + lfactorial(n[1L] - k)
            ans <- lfactorial(n[1L]) - ans;
            return(ans)
          })

#' @rdname Special
#' @exportMethod lchoose
setMethod("lchoose",
          signature(n = "dual", k = "numeric"),
          function(n, k) {
            if (length(k) > 1L) warning("Only the first element will be used")
            ans <- lfactorial(k[1L]) + lfactorial(n - k[1L])
            ans <- lfactorial(n) - ans;
            return(ans)
          })

#' @rdname Special
#' @exportMethod factorial
setMethod("factorial",
          signature(x = "dual"),
          function(x) {
            anf <- gamma(x@f + 1)
            ans <- dual(f = anf,
                        grad = digamma(x@f + 1) * anf * x@grad)
            return(ans)
          })

#' @rdname Special
#' @exportMethod lfactorial
setMethod("lfactorial",
          signature(x = "dual"),
          function(x) {
            anf <- lgamma(x@f + 1)
            ans <- dual(f = anf,
                        grad = digamma(x@f + 1) * x@grad)
            return(ans)
          })
