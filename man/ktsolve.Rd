\name{ktsolve}
\alias{ktsolve}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Configurable Function for Solving Families of Nonlinear Equations
Version: 1.4
%%  ~~function to do ... ~~
}
\description{This function is designed for use with an arbitrary set of equations with an arbitrary set of unknowns.  The user selects "fixed" values for enough unknowns to leave as many variables as there are equations, which in most cases means the system is properly defined and a unique solution exists. The function, the fixed values, and initial values for the remaining unknowns are fed to a nonlinear backsolver.  
As of version 1.3, supports  \code{BB} and \code{nleqslv}
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
ktsolve(yfunc, known = list(), guess, 
tool = c("BBsolve", "nleqslv","multiroot"), show = TRUE, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{yfunc}{ a function which accepts a vector of length n and produces an output vector of length n.  See the rules for constructing \code{yfunc} below.
%%     ~~Describe \code{yfunc} here~~
}
  \item{known}{
%%     ~~Describe \code{known} here~~
A list  of known values.  The elements must be named and the names must match variable names in yfunc.
}
  \item{guess}{
%%     ~~Describe \code{guess} here~~
A list or vector of initial guesses for the unknown values.  The elements must be named and the names must match variable names in yfunc. AND length(guess)  must be same as the number  of y[j] equations in yfunc, to avoid having an over- or under-defined system.  
}
  \item{tool}{ name of package which holds the solver to be used. Currently only \code{BB::BBsolve}, \code{nleqslv::nleqslv}, and \code{rootSolve::multiroot} are supported.  
%%     ~~Describe \code{tool} here~~
}
  \item{show}{ if TRUE, the solution values are printed in brief form to the console.
%%     ~~Describe \code{show} here~~
}
  \item{\dots}{ additional arguments to pass to the selected \code{tool} package.

%%     ~~Describe \code{\dots} here~~
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
%%  maybe \preformatted{}  is what's wanted.
The input form of  \code{yfunc}  is a function of the form:
\preformatted{ yfunc<-function(x) {
y<-vector()
y[1]<-f1(known,guess)
y[2]<-f2(known,guess)
.
.
.
y[n]<-fn(known,guess)
}
}
 	where y[j] are dummies which will be driven to zero,
 	and x is a dummy vector which is used (with the "guess" values) to run the solver.
 So, eqns in the form \code{A=f(x)} must be entered as  \code{y[j] <- f(x)-A }  
 
 For example,  \code{d = a + sqrt(b)} and  \code{a = sin(a/b) + g*exp(f*a) }  become
  
  \code{y[1]<- a - d +sqrt(b) } and \code{y[2]<- sin(a/b) +g*exp(f*a) -a }, 
  
 and e.g.
 \code{known <- list(a=3,d=5,g=.1) } are the fixed parameters and
 
 \code{guess <- list(b=1,f=1) }  are the initializers for the solver.
 
 Note that it is not necessary to have any \code{known} values if the function in question has as many (independent) equations as unknowns.  
 One of the handy things about \code{ktsolve} is the ease with which one can swap 'known' and 'guess' inputs to evaluate the system over different parameter sets.  
 %
 %% Put in a discussion of the ease with which one can change the 'known' vs. 'guess' for a given system.
}
\value{
%outs<-list(results=solution, tool=tool, yfunc=yfunc)
\item{results}{The output returned from the called solver package. As such, the contents and structure depend on which package was invoked via the \code{tools} argument.}
\item{tool}{Echoes back the selected solver  package used for reference.}
\item{yfunc}{Returns the modified \code{yfunc} as a \code{function} for the user to review and /or  use to process additional data, using the fit values generated.}

%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}

\author{ Carl Witthoft,  \email{carl@witthoft.com}
%%  ~~who you are~~
}
\note{
The original version of TK!Solver  provided a very nice GUI-based version of what I've done in \code{ktsolver}.  Over the years, it's turned into a very large, powerful, and,sadly, expensive application. You can find it at \url{https://www.uts.com/Products/Tksolver}
%%  ~~further notes~~
}
\seealso{ 
 \code{\link[BB]{BBsolve}} , \code{\link[nleqslv]{nleqslv}}  
}
\examples{
% Make SURE those are valid solutions 
zfunc<-function(x) {
	z<-vector()
z[1]<- 4*var1 -3*var2 +5*var3
z[2]<-8*var1 +5*var2 -2*var3
z
}

 known=list(var2=5)
 guess=list(var1=2,var3=0)
 solv1 <- ktsolve(zfunc,known=known,guess=guess)
#   Successful convergence.
# solution is:
#      var1      var3 
# -1.979167  4.583333 
# "known" inputs were:
#       var2
# known 5  
 eval(solv1$yfunc)(solv1$results$par)

 
 known=list(var1=5)
 guess=list(var2=2,var3=0)
 solv2<- ktsolve(zfunc,known=known,guess=guess)
#   Successful convergence.
# solution is:
#      var2      var3 
# -12.63158 -11.57895 
# "known" inputs were:
#       var1
# known 5   
eval(solv2$yfunc)(solv2$results$par)
 }

