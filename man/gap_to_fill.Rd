\name{gap_to_fill}
\alias{gap_to_fill}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Function to do the third and final step in filling gaps in time series
%%  ~~function to do ... ~~
}
\description{This function allows to fill intermediate gaps in time series. It uses linear interpolation.
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
end_start_to_fill(data, calendar, gap_variable, key_variable, time_variable, digits = 2)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{data}{R data frame: data set
%%     ~~Describe \code{data} here~~
}
  \item{calendar}{R data frame: complete empty calendar (performed by \code{\link{create_calendar_day}})
%%     ~~Describe \code{calendar} here~~
}
  \item{gap_variable}{character: name of the variable we want to fill the gaps
%%     ~~Describe \code{gap_variable} here~~
}
  \item{key_variable}{character: variable name that refers to the key variable in the panel (ID, ...)
%%     ~~Describe \code{key_variable} here~~
}
  \item{time_variable}{character: time variable name that permits to sort observation on a time scale
%%     ~~Describe \code{time_variable} here~~
}
  \item{digits}{numeric: number of decimals to keep for the rounding (default to 2)
%%     ~~Describe \code{digits} here~~
}
}
\details{
Link to the github repository:

https://www.github.com/Redcart/helda

}
\value{R data frame with five columns:
%%  ~Describe the value returned
%%  If it is a LIST, use
  \item{`time_variable`}{column of the time}
  \item{`key_variable`}{column of the ID in the panel}
  \item{`gap_variable`}{column of the variable to filled}
  \item{`gap_variable`_corrected_1}{gap variable filled for the start and end gaps}
    \item{`gap_variable`_corrected_2}{gap variable filled for the start, end and intermediate gaps}

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

\seealso{\code{\link{create_calendar_day}}

\code{\link{start_end_to_fill}}

}
\examples{
### creation of a data set

rep(c("Paris", "Madrid", "Berlin"), each = 10)

jeu_donnees <- data.frame("country" = rep(c("France", "Spain", "Germany"), each = 10),
                          "capital" = rep(c("Paris", "Madrid", "Berlin"), each = 10),
                          "year" = 2009:2018,
                          "gdp" = c(NA, NA, 200, 300, 500, 1000, NA, NA, NA, 500,
                                      0, NA, NA, NA, NA, NA, NA, 800, 1200, 1500,
                                      100, 200, 400, 700, 700, 800, 600, 500, NA, NA))


jeu_donnees <- na.omit(jeu_donnees)# we artificially create some gaps in the time series

### Check

data_to_check_1 <- create_calendar_day(data = jeu_donnees, key_variable = "country", time_variable = "year", start_year = 2009, end_year = 2018)

data_to_check_2 <- end_start_to_fill(data = jeu_donnees, calendar = data_to_check_1, gap_variable = "gdp", key_variable = "country", time_variable = "year", digits = 2)

data_to_check_3 <- gap_to_fill(data = data_to_check_2, gap_variable = "gdp_corrected_1", key_variable = "country", time_variable = "year", digits = 1)

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
%%\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
%%\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
