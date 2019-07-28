ktsolve <- function(yfunc, known=list(), guess, tool=c('BB', 'nleqslv','rootSolve'), show=TRUE, ...) {
# Revised June 2019 to add rootSolve::multiroot option for "tool" 
#revised Dec 2017 to use requireNamespace instead of require, per R CMD CHECK advice
#revised 23 Sept 2013 to fix search/replace strings in gsub

if( !(is(yfunc,'function')) ) stop('yfunc type must be "function" ')
# verify no duplicated names in known vs. guess
if( length( intersect(names(known), names(guess)) ) ) {
	stop(paste('The name(s) ',paste(intersect(names(known), names(guess)),collapse=' '),' are in both "known" and "guess" ') )
	}

 tool<-tool[1] #get rid of the extras! 
 # pick the solver 
switch(tool,
	'BB' = {
		requireNamespace('BB',quietly=TRUE)
		 #require(BB, quietly=TRUE, warn.conflicts=FALSE )
		thesolver = BB::BBsolve; 
		}		,
	'BBsolve' = {
		requireNamespace('BB',quietly=TRUE)
		 #require(BB, quietly=TRUE, warn.conflicts=FALSE )
		thesolver = BB::BBsolve; 
		}		,
	'nleqslv' = {
		requireNamespace('nleqslv',quietly=TRUE)
		thesolver = nleqslv::nleqslv}  , 
	'rootSolve' = {
		requireNamespace('rootSolve',quietly=TRUE)
		thesolver <- rootSolve::multiroot
		} ,
	'multiroot' = {
			requireNamespace('rootSolve',quietly=TRUE)
			tool = 'rootSolve'
			thesolver <- rootSolve::multiroot
		}  ,
	stop('Unknown solver package specified.') 
	)

eqnum = (length(body(yfunc)) - 3)
nullfoo <- switch( sign( length(unlist(guess)) - eqnum) +2  ,
	stop('Fewer guesses than equations.  System is underdefined.'),
	NULL, #i.e. sign(blahblah) is zero and we are OK
	stop('More guesses than equations.  System is overdefined.')
	)

dontuse = c(-1) #  keep as possibly useful later

if (length(known)>0 ) {
	for (i in 1:length(known)) {
		if(length(grep(names(known)[i], body(yfunc)[dontuse])) < 1) message("Input '", names(known)[i], '" not found in function body')
		lookfor<-paste("\\b",names(known)[i],"\\b",sep="",collapse="")
		parse(text=gsub(lookfor,known[i],body(yfunc)[dontuse])) -> body(yfunc)[dontuse]
		}
	} 
	
# Replace each "guess" name with x[n] in yfunc's body 
for (i in 1:length(guess)) {
	if(length(grep(names(guess)[i], body(yfunc)[dontuse]))<1) message("Guess '", names(guess)[i],"' not found in function body")
	subpat <- paste('x[', i, ']', sep='')
	lookfive<-paste("\\b",names(guess)[i],"\\b",sep="",collapse="")
	parse(text=gsub(lookfive, subpat, body(yfunc)[dontuse])) -> body(yfunc)[dontuse]
	}

# call solver tool with the values in 'guess' 
# Since only loaded namespace,  second items include the name-call

switch (tool,
	'BB' = {
		do.call(thesolver,list(unlist(guess), yfunc, ...) ) -> solution

		if(show) {
			cat('solution is:\n')
			print(solution$par)
			}
		},
	'nleqslv' = {
		do.call(thesolver, list(unlist(guess), yfunc, ...) ) -> solution
		if(show) {
			cat('solution is:\n')
			print(solution$x)
			}
		},
	'rootSolve' ={
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
