## function to draw a cladogram or ultrametric phylogram in R
## written by Liam J. Revell 2017

draw.ultrametric<-function(ingroup,outgroup=NULL,depth=1.0,
	method=c("phylogram","cladogram")){
	method<-method[1]
	if(is.null(outgroup)) out<-"OUTGROUP"
	else out<-outgroup
	tree<-pbtree(n=2,tip.label=c(ingroup[1],out),scale=depth)
	if(method=="cladogram"){
		for(i in 2:length(ingroup)) 
			tree<-bind.tip(tree,ingroup[i],interactive=TRUE)
		tree$edge.length<-NULL
	} else if(method=="phylogram"){
		dev.hold()
		plotTree(tree,mar=c(0.1,0.1,3.1,0.1))
		v<-seq(0,depth,by=depth/10)
		axis(3,at=v)
		abline(v=v,lty="dashed",col=make.transparent("grey",0.7))
		dev.flush()
		cat(paste("Click where you would like to bind the tip \"",
			ingroup[2],"\"\n",sep=""))
		flush.console()
		for(i in 2:length(ingroup)){
			obj<-get.treepos(message=FALSE)
			tree<-bind.tip(tree,ingroup[i],where=obj$where,
				position=obj$pos)
			dev.hold()
			plotTree(tree,mar=c(0.1,0.1,3.1,0.1))
			axis(3,at=v)
			abline(v=v,lty="dashed",col=make.transparent("grey",0.7))
			dev.flush()
			if(i<length(ingroup))
				cat(paste("Click where you would like to bind the tip \"",
					ingroup[i+1],"\"\n",sep=""))
			flush.console()
		}
	}
	tree
}
