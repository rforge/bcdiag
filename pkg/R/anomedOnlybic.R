#-----------------

#' A function for anova and median polish
#' @dest; dataset,@bres; biclust result,
#' @fit; one of 'aplot','mplot','anovbplot', 'mpolishbplot' or 'boxplot'
#' @mname; method name;'biclust','fabia' or 'isa2'
#' @bnum; biclust number; must be an existed biclust. 

#----------------

anomedOnlybic <- function(dset,bres,fit="boxplot",mname="biclust",bnum=1){
	if(any(!fit %in% c("aplot","mplot","anovbplot","mpolishbplot","boxplot"))){
		stop("`fit' must be one of 'aplot','mplot','anovbplot','mpolishbplot',`boxplot'")
	} 
	if(any(!mname %in% c("fabia","isa2","biclust"))){
		stop("`mname' must be one of `fabia',`isa2' or biclust")
	} 
	indgc<-indexedBic(dset,bres,mname,bnum)# returns the required indecies based on thier method names
	indg<-indgc[[1]]
	indc<-indgc[[2]]
	bic<-dset[indg,indc]
	
	
	# anova
	vbic<-as.vector(bic)
	gn<-rep(c(1:nrow(bic)),ncol(bic))
	cn<-sort(rep(c(1:ncol(bic)),nrow(bic)))
	bvgc<-cbind(vbic,gn,cn)
	fitan<-aov(vbic~as.factor(gn)+as.factor(cn))
	if(fit=="aplot"){
		 par(mfrow=c(2,2))
		 plot(fitan)
		
	}
	
	# median polish
	fitmp<-medpolish(bic, eps = 0.01, maxiter = 10, trace.iter = TRUE,na.rm = FALSE)
	if(fit=="mplot")
	{
		par(mfrow=c(1,1))
		plot(fitmp)
	}
	
	if(fit=="boxplot"){
		par(mfrow=c(2,2))
		#residual boxplot for anova
		boxplot(split(fitan$resid,as.factor(cn)),col=3,main="Resid vs Condition(ANOVA)")
		boxplot(split(fitan$resid,as.factor(gn)),add=F,col=2,main="Resid vs Genes(ANOVA)")

		#residual boxplot for median polish 
		boxplot(split(fitmp$resid,as.factor(cn)),col=3,main="Resid vs Condtions(Mpolish)")
		boxplot(split(fitmp$resid,as.factor(gn)),col=2,main="Resid vs Genes(Mpolish)")

	}
	if(fit=="anovbplot"){
		#residual boxplot for anova
		par(mfrow=c(2,1))
		boxplot(split(fitan$resid,as.factor(cn)),col=3,main="Resid vs Condition(ANOVA)")
		boxplot(split(fitan$resid,as.factor(gn)),add=F,col=2,main="Resid vs Genes(ANOVA)")

	}
	if(fit=="mpolishbplot"){
		#residual boxplot for median polish 
		par(mfrow=c(2,1))
		boxplot(split(fitmp$resid,as.factor(cn)),col=3,main="Resid vs Condtions(Mpolish)")
		boxplot(split(fitmp$resid,as.factor(gn)),col=2,main="Resid vs Genes(Mpolish)")
	}
}
