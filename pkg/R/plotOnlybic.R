#----------------

#' A function to plot summaries only for biclust data.
#' @dset stands for the dataset,
#' @bres stands for biclust result, should be one of fabia, biclust or isa2
#' @fit a parameter takes string; 'all'(all plots in one frame), 'mean', 'median', 'variance' or 'mad'.
#' @gby, group by 'conditions' or 'genes'

#-----------------

plotOnlybic<-function(ball,fit="all",gby){
	
	if(fit=="all"){
		#biclust genes
		sname<-c("Median","Mean","Variance","MAD")
		par(mfrow=c(2,2))
		for(i in 1:length(ball)){
			plot(ball[[i]],type="n",main=sname[i],ylab="",xlab=gby)
			lines(ball[[i]],type="l",col=i+1)

		}
	}
	par(mfrow=c(1,1))
	if(fit=="median")
	{
		plot(ball[[1]],type="n",main="Median",ylab="",xlab="Condtions")
		lines(ball[[1]],type="l",col=2)
	}
	if(fit=="mean")
	{
		plot(ball[[2]],type="n",main="Mean",ylab="",xlab=gby)
		lines(ball[[2]],type="l",col=3)

	}
	if(fit=="variance")
	{
		plot(ball[[3]],type="n",main="Variance",ylab="",xlab=gby)
		lines(ball[[3]],type="l",col=4)

	}
	if(fit=="mad")
	{
		plot(ball[[4]],type="n",main="MAD",ylab="",xlab=gby)
		lines(ball[[4]],type="l",col=5)

	}
}
