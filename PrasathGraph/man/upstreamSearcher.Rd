\name{upstreamSearcher}
\alias{upstreamSearcher}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
upStreamSearcher
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
upstreamSearcher(graph, node)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{graph}{
%%     ~~Describe \code{graph} here~~
}
  \item{node}{
%%     ~~Describe \code{node} here~~
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}
\references{
%% ~put references to the literature/web site here ~
}
\author{
%%  ~~who you are~~
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.

## The function is currently defined as
function (graph, node) 
{
    require(graph)
    inPath <- unlist(inEdges(node, graph))
    alreadyChecked <- node
    while (length(inPath) > 0) {
        addToInPath <- vector()
        for (i in 1:length(inPath)) {
            currNode <- inPath[i]
            if (currNode \%in\% alreadyChecked) {
                next
            }
            moreConnections <- unlist(inEdges(currNode, graph))
            moreConnections <- moreConnections[!(moreConnections \%in\% 
                alreadyChecked)]
            if (length(moreConnections)) {
                addToInPath <- c(addToInPath, moreConnections)
            }
            alreadyChecked <- c(alreadyChecked, currNode)
        }
        inPath <- addToInPath[!(addToInPath \%in\% alreadyChecked)]
    }
    alreadyChecked <- unname(alreadyChecked)
    return(alreadyChecked)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
