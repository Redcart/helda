\name{proc_freq}
\alias{proc_freq}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Function that reproduces the output of the SAS proc freq
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
proc_freq(variable, digits = 4)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{variable}{character: name of a categorical variable from an R data frame
%%     ~~Describe \code{variable} here~~
}
  \item{digits}{numeric: number of decimal for the rounding in the output figures
%%     ~~Describe \code{digits} here~~
}
}
\details{
Link to the github repository:

https://www.github.com/Redcart/helda

}
\value{Return a table with frequencies and percentages (regular and cumulative ones)
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
data <- iris

str(data)

result_array <- proc_freq(data$Species)
View(result_array)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
