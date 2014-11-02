\name{pathwayHeatmap}
\alias{pathwayHeatmap}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
pathwayHeatmap
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
pathwayHeatmap(graph, layout = "fdp", de, cols, subGList = list(), nodeAttrs = list(), attrs = list())
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{graph}{
%%     ~~Describe \code{graph} here~~
}
  \item{layout}{
%%     ~~Describe \code{layout} here~~
}
  \item{de}{
%%     ~~Describe \code{de} here~~
}
  \item{cols}{
%%     ~~Describe \code{cols} here~~
}
  \item{subGList}{
%%     ~~Describe \code{subGList} here~~
}
  \item{nodeAttrs}{
%%     ~~Describe \code{nodeAttrs} here~~
}
  \item{attrs}{
%%     ~~Describe \code{attrs} here~~
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
function (graph, layout = "fdp", de, cols, subGList = list(), 
    nodeAttrs = list(), attrs = list()) 
{
    require(graph)
    deKID <- names(de)
    allKID <- all
    logfc <- de[match(nodes(graph), deKID)]
    names(logfc) <- nodes(graph)
    logfc[is.na(logfc)] <- 0
    incol <- round(logfc + trunc(length(cols)/2))
    incol[incol > length(cols)] <- length(cols)
    incol[incol < 0] <- 1
    logcol <- cols[incol]
    logcol[logfc == 0] <- "darkgrey"
    names(logcol) <- names(logfc)
    nAttrs <- nodeAttrs
    nAttrs$fillcolor <- logcol
    if (!length(globalAttrs)) {
        attrs = list(node = list(shape = "circle", fontsize = 20, 
            fixedsize = T), edge = list(arrowsize = "0.6"))
    }
    if (length(subGList)) {
        plot(graph, layout, subGList = subGList, nodeAttrs = nAttrs, 
            attrs = attrs)
    }
    else {
        plot(graph, layout, nodeAttrs = nAttrs, attrs = attrs)
    }
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
