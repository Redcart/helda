\name{lift_effect}
\alias{lift_effect}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Function to draw the lift effect with gplot style
%%  ~~function to do ... ~~
}
\description{This function allows to get on a graph the lift effect with the size of population. It allows to compare the classification machine learning model to the random one.
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
lift_effect(predictions, true_labels, positive_label)
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
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.

## The function is currently defined as
function (predictions, true_labels, positive_label)
{
    data <- data.frame(predictions = predictions, true_labels = true_labels)
    data_1 <- data \%>\% arrange(desc(predictions))
    data_2 <- data \%>\% arrange(desc(true_labels))
    n <- length(true_labels)
    step <- floor(n/100)
    points <- seq(1, n, step)
    quantiles <- quantile(0:n)
    lift_1 <- c()
    for (i in 1:100) {
        lift_1 <- c(lift_1, mean(data_1$true_labels[1:points[i]] ==
            positive_label))
    }
    plot_1 <- ggplot() + geom_line(aes(x = points, y = lift_1),
        color = "#56B4E9") + geom_hline(yintercept = mean(true_labels ==
        positive_label), lty = "dashed", color = "grey") + coord_cartesian(ylim = c(0.05,
        0.8)) + scale_x_continuous(breaks = as.vector(quantiles),
        labels = names(quantiles)) + ggtitle("Effect Lift Curve") +
        xlab("Cumulative Population") + ylab("\% True Positive Label") +
        theme_bw() + theme(plot.title = element_text(hjust = 0.5))
    return(plot_1)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
