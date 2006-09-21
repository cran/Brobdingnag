\name{Complex}
\docType{methods}
\alias{Re}
\alias{Im}
\alias{Re<-}
\alias{Im<-}
\alias{Mod}
\alias{Mod,brob-method}
\alias{Mod,glub-method}
\alias{Conj}
\alias{Conj,brob-method}
\alias{Conj,glub-method}
\alias{Arg}
\alias{Arg,brob-method}
\alias{Arg,glub-method}
\alias{Re,brob-method}
\alias{Re,glub-method}
\alias{Im,brob-method}
\alias{Im,glub-method}
\alias{Re<-,glub-method}
\alias{Im<-,brob-method}
\alias{Im<-,glub-method}
\alias{Complex-methods}
\alias{Complex,glub-method}
\alias{Complex,brob-method}
\title{Real and imaginary manipulation}
\description{
  Get or set real and imaginary components of \code{brob}s or
  \code{glub}s.
}
\usage{
\S4method{Re}{glub}(x)
\S4method{Im}{glub}(x)
\S4method{Mod}{glub}(x)
\S4method{Conj}{glub}(x)
\S4method{Arg}{glub}(x)
Re(x) <- value
Im(x) <- value
}
\arguments{
  \item{x}{object of class \code{glub}}
  \item{value}{object of class \code{numeric} or \code{brob}}
}
\value{
Functions \code{Re()} and \code{Im()} return an object of class
\code{brob}; functions \code{Re<-()} and \code{Im<-()} return an object
of class \code{glub}
}
\author{Robin K. S. Hankin}
\examples{
a <- cbrob(1:10,brob(1e100))
Im(a) <- 11:1
a


}
\keyword{math}
