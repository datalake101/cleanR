% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/cleanR.R
\name{dupliFind}
\alias{dupliFind}
\title{Show index number and names of columns with the same names}
\usage{
dupliFind(df)
}
\arguments{
\item{df}{A data frame.}
}
\value{
A message indicating whether duplicate columns were found and their index number and names if found.
}
\description{
Show index number and names of columns with the same names
}
\examples{
dupliFind(mtcars)
}
