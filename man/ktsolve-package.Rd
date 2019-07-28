\name{ktsolve-package}
\docType{package}
\alias{ktsolve-package}
\title{
Configurable Function for Solving Families of Nonlinear Equations
}
\description{
This function is designed for use with an arbitrary set of equations with an arbitrary set of unknowns.  The user selects "fixed" values for enough unknowns to leave as many variables as there are equations, which in most cases means the system is properly defined and a unique solution exists. The function, the fixed values, and initial values for the remaining unknowns are fed to a nonlinear backsolver.  
The original version of "TK!Solver" was the inspiration for this function.
}
\details{
\tabular{ll}{
Package: \tab ktsolve\cr
Type: \tab Package\cr
Version: \tab 1.2\cr
Date: \tab 2019-07-08\cr
License: \tab GPL-3\cr
}
Note: \code{ktsolve} requires at least one of the "Suggests" packages (currently \code{BB},\code{rootSolve} and \code{nleqslv} ) to execute nonlinear back-solvers.
}
\author{
Carl Witthoft

Maintainer: Carl Witthoft,  carl@witthoft.com
}


