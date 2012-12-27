#-------------------

#' a function supports for explore bic.
#' used to plot for 'all', 'mean','variance','median', or 'mad'
#' @sbic,@obic; are summary of the bic and outside bicluster respectively
#' @pfor; which plot to figure;'all', 'mean','variance','median', or 'mad'

#-------------------

explorePlot<-function(sbic,obic,pfor=c("all","mean","median","variance","mad","quant")){
  #check for which plot the user is looking for
  pfor<-match.arg(pfor)
  if(pfor=="all"){
	mname<-c("Mean","Median","Variance","MAD")
	par(mfrow=c(2,2))
	for(i in 1:4){
		plot(c(sbic[[i]],obic[[i]]),type="n",col=4,ylab="",xlab="",main=mname[i])
		lines(c(sbic[[i]],obic[[i]]),type="l",col=1)
		lines(sbic[[i]],col=2)
	}
  }
  par(mfrow=c(1,1))
  if(pfor=="quant"){
	par(mfrow=c(1,1))
	bquant1<-sbic[[5]][[1]];oquant1<-obic[[5]][[1]]
	bquant2<-sbic[[5]][[2]];oquant2<-obic[[5]][[2]]
	bquant3<-sbic[[5]][[3]];oquant3<-obic[[5]][[3]]
	
	plot(c(bquant3,oquant3),lty=1,type="n",lwd=1,xlab="",ylab="")
	lines(c(bquant3,oquant3),lty=1,type="l",col=2,lwd=1)
	lines(c(bquant2,oquant2),lty=1,type="l",col=3,lwd=1)
	lines(c(bquant1,oquant1),lty=1,type="l",col=4,lwd=1)
	legend("topleft",c("75%","50%","25%"),col=c(2,3,4),lty=c(1,1,1))
  }
	if(pfor=="mean"){  
	plot(c(sbic[[1]],obic[[1]]),type="n",xlab="",ylab="",main=pfor)
	lines(c(sbic[[1]],obic[[1]]),type="l",lty=1,lwd=1)
	lines(sbic[[1]],col=2,lty=1)
	}
	 if(pfor=="median"){
		plot(c(sbic[[2]],obic[[2]]),type="n",xlab="",ylab="",main=pfor)
		lines(c(sbic[[2]],obic[[2]]),type="l",lty=1,lwd=1)
		lines(sbic[[2]],col=2,lty=1)
	 }
	  if(pfor=="variance"){
		plot(c(sbic[[3]],obic[[3]]),type="n",xlab="",ylab="",main=pfor)
		lines(c(sbic[[3]],obic[[3]]),type="l",lty=1,lwd=1)
		lines(sbic[[3]],col=2,lty=1)
	 }
	  if(pfor=="mad"){
		plot(c(sbic[[4]],obic[[4]]),type="n",xlab="",ylab="",main=pfor)
		lines(c(sbic[[4]],obic[[4]]),type="l",lty=1,lwd=1)
		lines(sbic[[4]],col=2,lty=1)
	 }
}
  