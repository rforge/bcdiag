#--------------------------

#' profile plot for bic vs outside bic either by gene or condtion.
#' plots either for a single or for all in one data frame.
#' profile plot of the biclust genes and the remain genes 
#' @ dset a parameter for the dataset
#' @ bres a parameter of the bicluster object
#' @ plot.bic a vector of different plot names.
#' @teta; @ph; values -360<teta/ph< 360, rotates the 3D plot
#' @mname; method name;'biclust','fabia' or 'isa2'
#' @bnum; biclust number; must be an existed biclust. 

#-------------------------------------#
profileBic <- function(dset,bres,mname=c("fabia","isa2","biclust"),bplot="all",gby="genes",bnum=1,teta=120,ph=30){
	
	# Small extra for the GUI
	if(bplot=="threeD"){bplot <- "3D"}	

	#check if the bic number to plotted is specified and
	if(any(!bplot %in% c("all","boxplot","lines","3D","histogram"))) {
		stop("`bplot' must be one of `boxplot', `lines', `3D','histogram' or `all'")
	}
	if(any(!mname %in% c("fabia","isa2","biclust"))) {
		stop("`m.name' must be one of `fabia', `biclust'")
	}
	if(any(!gby %in% c("genes","conditions"))) {
		stop("`gby' must be one of `genes', `conditions'")
	}
	if(any(!mname %in% c("fabia","isa2","biclust"))){
		stop("`mname' must be one of `fabia',`isa2' or biclust")
	} 
	par(mfrow=c(1,1))
	ind.gc<-indexedBic(dset,bres,mname,bnum)# returns the required indecies based on thier method names
	indg<-ind.gc[[1]]
	indc<-ind.gc[[2]]
	
	# gby conditions	
	if(gby=="conditions"){
		#group the genes in to two.
		cnams <- colnames(dset)
		grp <- rep(1, length(cnams))
		grp[indc] <- 2
		d<-dset[indg, order(grp, decreasing=TRUE)]
		dbc<-dset[indg,(grp==2)]
		
		if(bplot=="lines"){
			matplot(y =t(d),type ="n",col="green3", xlab="Condtions",ylab="Expression", axes=T, pch = rep(1, ncol(d)))
			matlines(y = t(d), type = "l",lty = rep(1, nrow(d)) ,col="green3", lwd = 1, pch = 1)
			matlines(y = t(dbc), type = "l",lty = rep(1, nrow(d)) ,col="red", lwd = 1, pch = 1)
			legend("topright",c( "Biclust conditions","Outside Condtions"), col=c("red","green3"),lty=c(1,1))
			box()
		}
		
		if(bplot=="boxplot"){
			boxplot.matrix(d,col="green3",main="",axes=T,lty=1)
			boxplot.matrix( dbc,add=T,col="red",axes=F,lty=1)
			legend("topright",c( "Bic conditions","Outbic Condtions"), col=c("red","green3"),lty=c(1,1))
			box()
			
		}
			
		if(bplot=="histogram"){
			#hist 
			hist(d,col="green3",xlab="",main="",lty=1)
			hist( dbc,col="red",add=T,xlab="",main="",lty=1)
			legend("topright",c( "Bic conditions","Outbic Condtions"), col=c("red","green3"),lty=c(1,1))
			box()
			
		}
			
		if(bplot=="3D"){
			d1<-c(1:nrow(d))
			d2<-c(1:ncol(d))
			
			fill <- matrix("green3", nrow = nrow(d), ncol = ncol(d))
			fill[,(grp==2)] <- "red";fill<-sort(fill, decreasing=T)
			persp(d1,d2,d,theta = teta, phi = ph, expand = 0.5, col = fill,
				 ltheta = 120, shade = 0.75, ticktype = "detailed",
				,xlab="Genes",ylab="Condtions",zlab="Gene expression")
			legend("topright",c( "Bic conditions","Outbic Condtions"), col=c("red","green3"),lty=c(1,1))
			box()

		}
		if(bplot=="all"){
			par(mfrow=c(2,2))
			profileAll(dset,indg,indc,grp,gby)
		}
	}
	
	#gby genes
	if(gby=="genes"){
		#group the genes in to two.
		rnams <- rownames(dset)
		grp <- rep(1, length(rnams))
		grp[indg] <- 2
		d<-dset[order(grp, decreasing=T),indc]
		dbc<-dset[(grp==2),indc]
		
		if(bplot=="lines"){
		matplot(y = d, type = "n",xlab="Genes",ylab="Expression", axes=T, pch = rep(1, ncol(dset)))
		matlines(y = d, type = "l", col="black",lty = rep(1, nrow(dset)),lwd = 1, pch = 1) 
		matlines(y = dbc, type = "l",col="red",lty = rep(1, nrow(dset)) , lwd = 1, pch = 1)
		legend("topright",c( "Bic genes","Outbic genes"), col=c("red","black"),lty=c(1,1))
		axis(2)
		box()
		}
		
		if(bplot=="boxplot"){
		#boxplot
		boxplot.matrix(t(d),col="black",main="",xlab="",axes=T)
		lines(t(dbc),col="red",xlab="")
		legend("topright",c( "Bic genes","Outbic genes"), col=c("red","black"),lty=c(1,1))
		box()
		}
			
		if(bplot=="histogram"){
		#histogram
		hist(d,col="black",xlab="",main="")
		hist(dbc,col="red",add=T,xlab="",main="")
		legend("topright",c( "Bic genes","Outbic genes"), col=c("red","black"),lty=c(1,1))
		box()
		}
			
		if(bplot=="3D"){
			d1<-c(1:nrow(d))
			d2<-c(1:ncol(d))
			fill <- matrix("black", nrow = nrow(d), ncol = ncol(d))
			fill[(grp==2),] <- "red";fill<-sort(fill, decreasing=T)
			persp(d2,d1,t(d),theta = teta, phi = ph, expand = 0.5, col = fill,
				 ltheta = 120, shade = 0.75, ticktype = "detailed",
				,xlab="Condtions",ylab="Genes",zlab="Gene expression")
			legend("topright",c( "Bic genes","Outbic genes"), col=c("red","black"),lty=c(1,1))
			box()
		}
		if(bplot=="all"){
			par(mfrow=c(2,2))
			profileAll(dset,indg,indc,grp,gby)
		}
	}
}
