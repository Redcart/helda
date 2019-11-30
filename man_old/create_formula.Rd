\name{create_formula}
\alias{create_formula}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Function that permits to automatically create formula from a data set
%%  ~~function to do ... ~~
}
\description{ This function generalizes the creation of formula needed for the training of machine learning models. It allows to do it quickly from the columns of data frame.
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
create_formula(data, position = 1)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{data}{R data frame: data set
%%     ~~Describe \code{data} here~~
}
  \item{position}{numeric: position of the target variable in the data frame (by default equal to 1)
%%     ~~Describe \code{position} here~~
}
}
\details{
Link to the github repository:

https://www.github.com/Redcart/helda

}
\value{The function outputs a string containing the formula such as Y ~ X1 + X2 + ... + XN
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
create_formula(mtcars)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
