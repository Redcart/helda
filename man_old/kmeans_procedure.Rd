\name{kmeans_procedure}
\alias{kmeans_procedure}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Function to do k-means clustering on a data set
%%  ~~function to do ... ~~
}
\description{This function allows to perform clustering via the k-means algorithm. It gives the opportunity to make some criteria in term of
the final size of the output clusters. This can be useful when the data analyst wants to get final clusters that are not too big and not too small.
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
kmeans_procedure(data, columns, threshold_min, threshold_max, verbose = FALSE, seed = 42)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{data}{R data frame: data set
%%     ~~Describe \code{data} here~~
}
  \item{columns}{vector of numeric: numbers of columns in the data frame on which we intend to perform the clustering (numeric variables only)
%%     ~~Describe \code{columns} here~~
}
  \item{threshold_min}{numeric: minimum desired size of final clusters
%%     ~~Describe \code{threshold_min} here~~
}
  \item{threshold_max}{numeric: maximal desired size of final clusters
%%     ~~Describe \code{threshold_max} here~~
}
  \item{verbose}{logical: set to \code{TRUE} if information about the procedure is needed. Default to \code{FALSE}
%%     ~~Describe \code{verbose} here~~
}
  \item{seed}{numeric: seed for reproduction of results. Default to 42
%%     ~~Describe \code{seed} here~~
}
}
\details{
Link to the github repository:

https://www.github.com/Redcart/helda

}
\value{Returns a list
%%  ~Describe the value returned
%%  If it is a LIST, use
\item{clusters}{vector of numbers representing final cluster for each observation in the data set }
\item{frequencies}{vector of frequencies of final clusters}
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
data <- trees
str(data)

final <- kmeans_procedure(data = data, columns = 1:3, threshold_min = 3, threshold_max = 5)
final
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
