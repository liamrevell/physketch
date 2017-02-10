\name{phylo.tracer}
\alias{phylo.tracer}
\title{Method to trace a tree in a raster image}
\usage{
phylo.tracer(img=NULL, file="", gridlines=TRUE)
}
\arguments{
	\item{img}{a raster image.}
	\item{file}{a file to be read into R.}
	\item{gridlines}{logical argument indicating whether or not grid lines should be plotted.}
}
\description{
	This function allows the user to trace a raster image of a phylogenetic tree & produce an object of class \code{"phylo"}.
}
\details{
	More details of the function here.
}
\value{
	An object of class \code{"phylo"}.
}
\references{
	Revell, L. J. (2012) phytools: An R package for phylogenetic comparative biology (and other things). \emph{Methods Ecol. Evol.}, \bold{3}, 217-223.
	
	Revell, L. J. (2017). physketch: Drawing phylogenetic objects free-hand. R package. http://github.com/liamrevell/physketch
}
\author{Liam Revell \email{liam.revell@umb.edu}}
\seealso{
	\code{\link{draw.ultrametric}}
}
\keyword{phylogenetics}

