#----------------------

#' a function supports for explore bic function
#' calculates the 'mean','median','variance','quantile' and 'mad'
#' @bic.mat, matrix data either for biclust or outside biclust


#----------------------

exploreCalc<-function(bic.mat){
	#matrix for quantiles
	quantile.25<-matrix(0,nrow(bic.mat),1)
	quantile.50<-matrix(0,nrow(bic.mat),1)
	quantile.75<-matrix(0,nrow(bic.mat),1)
	
	#matrix for mean, median, variance and mad
	mean.res<-matrix(0,nrow(bic.mat),1)
	median.res<-matrix(0,nrow(bic.mat),1)
	var.res<-matrix(0,nrow(bic.mat),1)
	mad.res<-matrix(0,nrow(bic.mat),1)
	
	for(j in 1:nrow(bic.mat)){
		bic.rows<-bic.mat[j,]
		quantile.25[j,]<-quantile(bic.rows,probs=25/100)
		quantile.50[j,]<-quantile(bic.rows,probs=50/100)
		quantile.75[j,]<-quantile(bic.rows,probs=75/100)
		#dimnames(quantile.res)<-list(rownames(bic.mat),names(quantile.res))
		mean.res[j,]<-mean(bic.rows)
		median.res[j,]<-median(bic.rows)
		var.res[j,]<-var(bic.rows)
		mad.res[j,]<-mad(bic.rows)
	}
	dimnames(mean.res)<-list(rownames(bic.mat),"Mean")
	dimnames(median.res)<-list(rownames(bic.mat),"Median")
	dimnames(var.res)<-list(rownames(bic.mat),"Variance")
	dimnames(mad.res)<-list(rownames(bic.mat),"MAD")
	
	dimnames(quantile.25)<-list(rownames(bic.mat),"Bic 25%")
	dimnames(quantile.50)<-list(rownames(bic.mat),"Bic 50%")
	dimnames(quantile.75)<-list(rownames(bic.mat),"Bic 75%")
	quant.res<-list(quantile.25,quantile.50,quantile.75)
	
	#return the calculated summary
	return(list(mean.res,median.res,var.res,mad.res,quant.res))

}
