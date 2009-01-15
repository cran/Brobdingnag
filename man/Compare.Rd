\name{Compare-methods}
\docType{methods}
\alias{Compare-methods}
\alias{Compare,brob,ANY-method}
\alias{Compare,brob,brob-method}
\alias{Compare,brob,glub-method}
\alias{Compare,ANY,brob-method}
\alias{Compare,ANY,glub-method}
\alias{Compare,glub,glub-method}
\alias{Compare,glub,ANY-method}
\alias{Compare,glub,brob-method}
\title{Methods for Function Compare in Package Brobdingnag}
\description{
  Methods for comparision (greater than, etc) in package Brobdingnag
}
\note{
  As for \code{min()} and \code{max()}, comparison is not entirely
  straightforward in the presence of \code{NA}s.
}
\examples{
  a <- as.brob(10)^(0.5 + 97:103)
  a < 1e100
  }
\keyword{methods}
\keyword{math}
