\name{lift_curve}
\alias{lift_curve}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Function that permits to draw the lift curve with ggplot2 style
%%  ~~function to do ... ~~
}
\description{This function implement the draw of the lift curve. This curve is another way of evaluating the predicitve perfomance of classification
machine learning models like the ROC curve.
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
lift_curve(predictions, true_labels, positive_label)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{predictions}{vector of numeric: probabilities predicted by a classification machine leanrning model
%%     ~~Describe \code{predictions} here~~
}
  \item{true_labels}{vector of characters: true labels of the data set
%%     ~~Describe \code{true_labels} here~~
}
  \item{positive_label}{character: string representing the positive label
%%     ~~Describe \code{positive_label} here~~
}
}
\details{
Link to the github repository:

https://www.github.com/Redcart/helda

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
