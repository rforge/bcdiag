#----------------------

#' a supportive function for profileplot.bic,
#' indexedBic: a function to check the name of the method
#' and returns the list parameter of the required indcies of the biclust
#' @ bres, the biclust object
#' @ mname, name of the method to be applied for the biclust
#' @ dset,bres,mname,bnum has similar explanation as summary.bic function
#' outcome : returns the two indcies; the indg and indc
#' @ indg; index for the biclust genes.
#' @ indc; index for the biclust conditions.

#----------------------
indexedBic<-function(dset,bres,mname=c("fabia","isa2","biclust"),bnum){
	# which biclust object is it; 
	check<-match.arg(mname)
	l<-bnum
	if(check=="fabia"){
		#Extract biclusters:
		require(fabia)
		resf <- extractBic(bres)
		#get the biclust index inside the dset 
		#~ same as biclust of NumberxCol and RowxNumber format:
		
		bg<-resf$numn[1,]$numng
		bc<-resf$numn[1,]$numnp
		
		
		# tc<-resf$bic[l,]$biypn
		# bc<-rep(0,length(tc))
		
		indg<-bg
		indc<-bc
	# for(i in 1:length(tc)){
		# bc[i]<-grep(tc[i], colnames(dset)) }
		# index genes
		# tr<-resf$bic[l,]$bixn
		# br<-rep(0,length(tr))
		# for(i in 1:length(tr)){
		# br[i]<-grep(tr[i], rownames(dset)) }
	}
	if(check=="isa2"){
		#convert to biclust and get the biclust indecies
		require(isa2)
		resi<-isa.biclust(bres)
		indg<-which(resi@RowxNumber[,l])
		indc<-which(resi@NumberxCol[l,])
	
	}
	if(check=="biclust"){
		require(biclust)
		indg<-which(bres@RowxNumber[,l])
		indc<-which(bres@NumberxCol[l,])
	
	}
	return(list(indg,indc))
}
