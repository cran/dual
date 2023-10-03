# @useDynLib dual, .registration = TRUE
#' @importFrom methods is new
#' @importFrom stats dnorm pchisq pnorm
#'
#' @name dual-package
#' 
#' @docType package
#' 
#' @encoding UTF-8
#' 
#' @title
#' \packageTitle{dual}
#' 
#' @description
#' \if{html}{\figure{logo.svg}{options: style='float: right;'}}
#' Automatic differentiation is achieved by using dual 
#' numbers without providing hand-coded gradient functions. 
#' The output value of a mathematical function is returned 
#' with the values of its exact first derivative (or 
#' gradient). For more details see Baydin, Pearlmutter, 
#' Radul, and Siskind (2018) 
#' \url{https://jmlr.org/papers/volume18/17-468/17-468.pdf}.
#' 
#' @details
#' \tabular{ll}{
#' Package: \tab dual\cr
#' Type: \tab Package\cr
#' Version: \tab 0.0.5\cr
#' Date: \tab 2023-10-02\cr
#' License: \tab GPL-3\cr
#' }
#' 
#' For a complete list of exported functions, use \code{library(help = "dual")}.
#'
#' @author
#' Luca Sartore \email{drwolf85@@gmail.com}
#'
#' Maintainer: Luca Sartore \email{drwolf85@@gmail.com}
#' 
#' @references
#' Baydin, A. G., Pearlmutter, B. A., Radul, A. A., & Siskind, J. M. (2018). Automatic differentiation in machine learning: a survey. \emph{Journal of Marchine Learning Research}, \bold{18}, 1-43.
#' 
#' Cheng, H. H. (1994). Programming with dual numbers and its applications in mechanisms design. \emph{Engineering with Computers}, \bold{10}(4), 212-229.
#'
#' @keywords dual autodiff differentiation numeric
#' 
#' @examples
#' library(dual)
#' 
#' # Initilizing variables of the function
#' x <- dual(f = 1.5, grad = c(1, 0, 0))
#' y <- dual(f = 0.5, grad = c(0, 1, 0))
#' z <- dual(f = 1.0, grad = c(0, 0, 1))
#' # Computing the function and its gradient
#' exp(z - x) * sin(x)^y / x
#' 
#' # General use for computations with dual numbers
#' a <- dual(1.1, grad = c(1.2, 2.3, 3.4, 4.5, 5.6))
#' 0.5 * a^2 - 0.1
#' 
#' # Johann Heinrich Lambert's W-function
#' lambertW <- function(x) {
#'   w0 <- 1
#'   w1 <- w0 - (w0*exp(w0)-x)/((w0+1)*exp(w0)-(w0+2)*(w0*exp(w0)-x)/(2*w0+2))
#'   while(abs(w1-w0) > 1e-15) {
#'     w0 <- w1
#'     w1 <- w0 - (w0*exp(w0)-x)/((w0+1)*exp(w0)-(w0+2)*(w0*exp(w0)-x)/(2*w0+2))
#'   }
#'   return(w1)
#' }
#' lambertW(dual(1, 1))
#' 
NULL
