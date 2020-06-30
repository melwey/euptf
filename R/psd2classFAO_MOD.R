psd2classFAO_MOD <-
function(sand=0,silt=0,clay=0,OC=0,option=FALSE,tol=1){
# Check input arguments
	N <- length(sand)
	if (length(clay)!=N | length(OC)!= N) {
		simpleError("all input arguments must be vectors of the same length")
	}
# Class defintion:
# Coarse
	indC <- clay < 18 & sand >= 65
# Medium
	indM <- (clay >= 18 & clay < 35  & sand >= 15) | (clay < 18 & sand >= 15 & sand < 65) 
# Medium Fine
	indMF <- clay < 35 & sand < 15
# Fine
	indF <- clay >= 35 & clay < 60
# Very Fine
	indVF <- clay >= 60
# Organic (2013/05/07: i can't find the definition anymore ... :s)
	indO <- (OC >= 18 & clay>=60) | (OC>= 12 & is.na(clay)) | (OC >= (12+clay*0.1) & clay < 60 & clay>0) 
# NA: sum != 100
	indNA <- rowSums(cbind(sand,silt,clay)) < 100-tol | rowSums(cbind(sand,silt,clay)) > 100+tol
	
# output
	value <- rep(NA,N)
	if (option){
	# character output
	value[indC]<-"coarse";value[indM]<-"medium";value[indMF]<-"medium fine";value[indF]<-"fine";value[indVF]<-"very fine";value[indO]<-"organic"
	} else{
	value[indC]<-1;value[indM]<-2;value[indMF]<-3;value[indF]<-4;value[indVF]<-5;value[indO]<-9
	}
	value[indNA & (!indO | is.na(indO))] <- NA
	return(value)
	
}
