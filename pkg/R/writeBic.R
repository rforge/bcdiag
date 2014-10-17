 #----------------------

#'  writebic function: writes biclust outputs from fabia,isa2 and biclust methods
#'  a function Writes bicluster results to a file
#'@ dset stands for the dataset
#'@ fileName a parameter for the name of the file(possible format, txt,bic)
#'@ bicResult, object of the biclust outcome
#'@ bicname, name given to the biclust result any title is possible value
#'@ m is the name of the biclust method applied
#'@ outcome is a text file with a list of the total number outcome obtained,
#' the name of genes and conditions of the of each biclust.

#-------------------------

writeBic<-function (dset,fileName, bicResult, bicname, mname = c("fabia","isa2","biclust"),append = TRUE, delimiter = " "){
	#check the method name
	if(any(!mname %in% c("fabia","isa2","biclust"))){
		stop("`mname' must be one of `fabia',`isa2' or 'biclust'")
	} 
	#names of the data set
	geneNames<-dimnames(dset)[1][[1]]
	arrayNames<-dimnames(dset)[2][[1]]
	check<- match.arg(mname)
	if(check=="fabia") {
		#Extract biclusters:
		resb <-extractBic(bicResult)
		write(c(resb$np,bicname), file = fileName, append = append)
		for (i in 1:resb$np) {
			listar = matrix(resb$bic[i,]$bixn)
			listac = matrix(resb$bic[i,]$biypn)
			if(dim(listar)[1]>0 & dim(listac)[1]>0){
				write(c(length(listar), length(listac)), file = fileName, 
						ncolumns = 2, append = TRUE, sep = delimiter)
				write(listar, file = fileName, ncolumns = length(listar), 
						append = TRUE, sep = delimiter)
				write(listac, file = fileName, ncolumns = length(listac), 
						append = TRUE, sep = delimiter)
				write("\n", file = fileName, append = TRUE)
			}			
		}
	}
	if(check=="isa2"){
		bicResult<-isa2biclust(bicResult)
		write(c(bicResult@Number,bicname), file = fileName, append = append)
		for (i in 1:bicResult@Number) {
			listar = row(matrix(bicResult@RowxNumber[, i]))[bicResult@RowxNumber[,i] == T]
			listac = row(matrix(bicResult@NumberxCol[i, ]))[bicResult@NumberxCol[i,] == T]
			write(c(length(listar), length(listac)), file = fileName, 
				ncolumns = 2, append = TRUE, sep = delimiter)
			write(geneNames[listar], file = fileName, ncolumns = length(listar), 
				append = TRUE, sep = delimiter)
			write(arrayNames[listac], file = fileName, ncolumns = length(listac), 
				append = TRUE, sep = delimiter)
		}
	}
	if(check=="biclust"){
		
		write(c(bicResult@Number,bicname), file = fileName, append = append)
		for (i in 1:bicResult@Number) {
			listar = row(matrix(bicResult@RowxNumber[, i]))[bicResult@RowxNumber[,i] == T]
			listac = row(matrix(bicResult@NumberxCol[i, ]))[bicResult@NumberxCol[i,] == T]
			write(c(length(listar), length(listac)), file = fileName, 
				ncolumns = 2, append = TRUE, sep = delimiter)
			write(geneNames[listar], file = fileName, ncolumns = length(listar), 
				append = TRUE, sep = delimiter)
			write(arrayNames[listac], file = fileName, ncolumns = length(listac), 
				append = TRUE, sep = delimiter)
		}
	}
}
