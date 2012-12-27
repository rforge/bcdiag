#-----------------

#' supportive function for profileBic
#' plots profile in four different types of plots.
#' outcome: four plots in one: profile lines,boxplot,histogram and 3d plots.
#' @ indg; index for the biclust genes.
#' @ indc; index for the biclust conditions.
#' @gby; group by 'genes' or 'conditions'

#------------------
profileAll<-function(dset,indg,indc,grp,gby="genes",teta=120,ph=30){
	
	if(gby=="conditions"){
		
		#group the genes in to two.
		cnams <- colnames(dset)
		grp <- rep(1, length(cnams))
		grp[indc] <- 2
		d<-dset[indg, order(grp, decreasing=TRUE)]
		dbc<-dset[indg,(grp==2)]		
		#lines 
		matplot(y =t(d),type ="n",col="green3", xlab="Condtions",ylab="Expression", axes=T, pch = rep(1, ncol(d)))
		matlines(y = t(d), type = "l",lty = rep(1, nrow(d)) ,col="green3", lwd = 1, pch = 1)
		matlines(y = t(dbc), type = "l",lty = rep(1, nrow(d)) ,col=2, lwd = 1, pch = 1)
		axis(2)
		box()
		
		#boxplot 
		boxplot.matrix(d,col="green3",main="",xlab="",axes=T,lty=1)
		boxplot.matrix(dbc,add=T,col="red",axes=F,xlab="",lty=1)
		box()
		
		#hist 
		hist(d,col="green3",xlab="",main="",lty=1)
		hist( dbc,col="red",add=T,xlab="",main="",lty=1)
		box()
		
		#3D 
		d1<-c(1:nrow(d))
		d2<-c(1:ncol(d))
		
		fill <- matrix("green3", nrow = nrow(d), ncol = ncol(d))
		fill[,(grp==2)] <- "red";fill<-sort(fill, decreasing=T)
		persp(d1,d2,d,theta = teta, phi = ph, expand = 0.5, col = fill,
			 ltheta = 120, shade = 0.75, ticktype = "detailed",
			,xlab="Genes",ylab="Condtions",zlab="Gene expression")
		box()
	}
	if(gby=="genes"){
		
		#group the genes in to two.
		rnams <- rownames(dset)
		grp <- rep(1, length(rnams))
		grp[indg] <- 2
		d<-dset[order(grp, decreasing=TRUE),indc]	
		dbc<-dset[(grp==2),indc]
					
		#lines
		matplot(y = d, type = "n",xlab="",ylab="Expression", axes=T, pch = rep(1, ncol(dset)))
		matlines(y = d, type = "l", col="black",lty = rep(1, nrow(dset)),lwd = 1, pch = 1) 
		matlines(y = dbc, type = "l",col="red",lty = rep(1, nrow(dset)) , lwd = 1, pch = 1)
		box()
		
		#boxplot
		boxplot.matrix(t(d),col="black",main="",xlab="",axes=T)
		lines(t(dbc),type="l",col="red")
		box()
		
		#histogram
		hist(d,col="black",xlab="",main="")
		hist(dbc,col="red",add=T,xlab="",main="")
		box()
		
		#3D
		d1<-c(1:nrow(d))
			d2<-c(1:ncol(d))
			fill <- matrix("black", nrow = nrow(d), ncol = ncol(d))
			fill[(grp==2),] <- "red";fill<-sort(fill,decreasing=T)
			persp(d1,d2,d,theta = teta, phi = ph, expand = 0.5, col = fill,
				 ltheta = 120, shade = 0.75, ticktype = "detailed",
				,xlab="Genes",ylab="Condtions",zlab="Gene expression")
			box()
	
	}
	
}
