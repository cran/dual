# dual: an R package for dual numbers 

### Authors <img src="man/figures/logo.svg" align="right" alt="dual logo" />
[Luca Sartore](mailto://drwolf85@gmail.com)

Maintainer: [Luca Sartore](mailto://drwolf85@gmail.com)

[![CRAN version](https://www.r-pkg.org/badges/version/dual)](https://cran.r-project.org/package=dual)
[![CRAN release](https://www.r-pkg.org/badges/ago/dual)](https://cran.r-project.org/package=dual)
[![GPLv3 license](https://img.shields.io/badge/License-GPLv3-yellow.svg)](https://perso.crans.org/besson/LICENSE.html)
[![CRAN RStudio mirror downloads](https://cranlogs.r-pkg.org/badges/dual)](https://cran.r-project.org/package=dual)
[![Total Downloads from CRAN RStudio mirror](https://cranlogs.r-pkg.org/badges/grand-total/dual?color=orange)](https://cran.r-project.org/package=dual)

## Features of the package
Dual numbers are mainly used to implement automatic differentiation. The **dual** package provides mathematical functions that are able to handle computations with dual numbers. The package is useful to calculate exact derivatives in R without providing self-coded functions.

For a complete list of exported functions, use `library(help = "dual")` once the **dual** package is installed (see the `inst/INSTALL.md` file for a detailed description of the setup process).

### Example
```R
library(dual)
x <- dual(f = 1.5, grad = c(1:0, 0))
y <- dual(f = 0.5, grad = c(0:1, 0))
z <- dual(f = 1.0, grad = c(0, 0:1))
exp(z - x) * sin(x)^y / x

a <- dual(f = 1.1, grad = c(1.2, 2.3, 3.4, 4.5, 5.6))
0.5 * a^2 - 0.1

lambertW <- function(x) {
  w0 <- 1
  w1 <- w0 - (w0*exp(w0)-x)/((w0+1)*exp(w0)-(w0+2)*(w0*exp(w0)-x)/(2*w0+2))
  while(abs(w1-w0) > 1e-15) {
    w0 <- w1
    w1 <- w0 - (w0*exp(w0)-x)/((w0+1)*exp(w0)-(w0+2)*(w0*exp(w0)-x)/(2*w0+2))
  }
  return(w1)
}
lambertW(dual(1, 1))
```

## References

Baydin, A. G., Pearlmutter, B. A., Radul, A. A., & Siskind, J. M. (2018). Automatic differentiation in machine learning: a survey. *Journal of Machine Learning Research*, **18**, 1-43.

Cheng, H. H. (1994). Programming with dual numbers and its applications in mechanisms design. *Engineering with Computers*, **10**(4), 212-229.

Kisil, V. V. (2007). Erlangen program at large-2: inventing a wheel. The parabolic one. *arXiv: 0707.4020*.
