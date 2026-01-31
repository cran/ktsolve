# Jan 2026 -- get rid of requiring the arg in yfunc() to be
#named "x" . Put multiroot back in as it's live on CRAN again

# Oct 2024: cleanups: use match.arg to simplify following stuff; merged the two "switch" funcs,  add rootSolve::multiroot to list of options; as it's back in
# 
#REvised April 2020 because rootSolve is being pulled off CRAN
# Revised June 2019 to add rootSolve::multiroot option for "tool" 
#revised Dec 2017 to use requireNamespace instead of require, per R CMD CHECK advice
#revised 23 Sept 2013 to fix search/replace strings in gsub


ktsolve <- function(yfunc, known=list(), guess, tool=c('BBsolve', 'nleqslv','multiroot'), show=TRUE, ...) {

if( !(is(yfunc,'function')) ) stop('yfunc type must be "function" ')
# verify no duplicated names in known vs. guess
if( length( intersect(names(known), names(guess)) ) ) {
	stop(paste('The name(s) ',paste(intersect(names(known), names(guess)),collapse=' '),' are in both "known" and "guess" ') )
	}

 tool<- match.arg(tool[1], c('BBsolve', 'nleqslv','root','multiroot'))
 if(tool == 'root') tool <- 'multiroot'
 
dontuse = c(-1) #  useful later

if (length(known)>0 ) {
	for (i in 1:length(known)) {
		if(length(grep(names(known)[i], body(yfunc)[dontuse])) < 1) message("Input '", names(known)[i], '" not found in function body')
		lookfor<-paste("\\b",names(known)[i],"\\b",sep="",collapse="")
		parse(text=gsub(lookfor,known[i],body(yfunc)[dontuse])) -> body(yfunc)[dontuse]
		}
	} 
	
# Replace each "guess" name with yfunc_argument_name[n] in yfunc's body 
# I want to get the argument as char
theArg <- (methods::formalArgs(yfunc)  )
if (length(theArg) != 1) stop("yfunc must have a single argument")
for (i in 1:length(guess)) {
	if(length(grep(names(guess)[i], body(yfunc)[dontuse]))<1) message("Guess '", names(guess)[i],"' not found in function body")
# now use theArg instead of "x" 
	subpat <- paste(theArg,'[', i, ']', sep='')
	lookfive<-paste("\\b",names(guess)[i],"\\b",sep="",collapse="")
	parse(text=gsub(lookfive, subpat, body(yfunc)[dontuse])) -> body(yfunc)[dontuse]
	}

# call solver tool with the values in 'guess' 
switch (tool,
	'BBsolve' = {
		requireNamespace('BB',quietly=TRUE)
		 #require(BB, quietly=TRUE, warn.conflicts=FALSE )
		thesolver = BB::BBsolve; 
		do.call(thesolver,list(unlist(guess), yfunc, ...) ) -> solution

		if(show) {
			cat('solution is:\n')
			print(solution$par)
			}
		},
	'nleqslv' = {
		requireNamespace('nleqslv',quietly=TRUE)
		thesolver = nleqslv::nleqslv 
		do.call(thesolver, list(unlist(guess), yfunc, ...) ) -> solution
		if(show) {
			cat('solution is:\n')
			print(solution$x)
			}
		},
	'multiroot' ={
# first convert yfunc into rootSolve format
		jfunc <- function(x) {
			z <- c()
		}
		qlen = length(body(yfunc))
		for (jeq in 3:(qlen-1)) {
			ptmp = parse(text = body(yfunc)[jeq])
			ztmp  = parse(text = ptmp[[1]][3])
			body(jfunc)[[2]][[3]][jeq-1] = as.call(ztmp)
		}
		thesolver = rootSolve::multiroot; 
		do.call(thesolver, list(jfunc, unlist(guess),  ...) ) -> solution
		if(show) {
			cat('solution is:\n')
			print(solution$root)
			}
		} ,
) #end of switch
if(show && length(known) > 0 ) {
	cat('"known" inputs were:\n')
	print(rbind(known))
	} # end of if(show)

outs<-list(results=solution, tool=tool, yfunc=yfunc)
return(invisible(outs))
}
