% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/compute_global_inertia.R
\name{compute_global_inertia}
\alias{compute_global_inertia}
\title{Inertia of a data frame}
\usage{
compute_global_inertia(data)
}
\arguments{
\item{data}{a R data frame (all columns are required to be numeric types).}
}
\value{
a numeric value representing the total inertia.
}
\description{
This function allows to compute the inertia of a R data frame
}
\examples{
result <- compute_global_inertia(mtcars)
result
}
\references{
Link to the author's github package repository:
\url{https://github.com/Redcart/helda}
}
\author{
Simon CORDE
}
\keyword{data}
\keyword{frame}
\keyword{inertia}
