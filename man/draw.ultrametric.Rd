\name{draw.ultrametric}
\alias{draw.ultrametric}
\title{Method to draw a cladogram or ultrametric phylgram}
\usage{
draw.ultrametric(ingroup, outgroup=NULL, depth=1.0, method=c("phylogram","cladogram"))
}
\arguments{
	\item{ingroup}{ingroup taxa of the tree to be drawn.}
	\item{outgroup}{single outgroup taxon. If there is no single outgroup, then this can just be any tip separated from \code{ingroup[1]} by the root.}
	\item{depth}{total depth of the tree.}
	\item{method}{type of tree to create.}
}
\description{
	This function uses \code{\link{bind.tip}} internally to let the user interactively create a tree which is returned to R as an object of class \code{"phylo"}.
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
	\code{\link{bind.tip}}, \code{\link{phylo.tracer}}
}
\keyword{phylogenetics}

