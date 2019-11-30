\name{compute_inertia_ahc}
\alias{compute_inertia_ahc}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Function to compute the intra cluster inertia from AHC
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
compute_inertia_ahc(data, max_clusters = 10)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{data}{R data frame: data set
%%     ~~Describe \code{data} here~~
}
  \item{max_clusters}{numeric: number of maximum clusters we want to perform agglomerative clustering
%%     ~~Describe \code{max_clusters} here~~
}
}
\details{
Link to the github repository:

https://www.github.com/Redcart/helda

}
\value{vector of length max_clusters - 1. It stores all intra cluster inertia for AHC from 2 to max_clusters clusters.
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
Simon Corde

Link to the author's github repository:

https://www.github.com/Redcart

}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
