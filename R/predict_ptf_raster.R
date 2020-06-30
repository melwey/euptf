# apply euptf spatially on a raster stack
predict.ptf.raster <- function(dataStack, PTF, filename, format="GTiff", overwrite = !file.exists(filename), transfo=NULL){
	require(raster); require(euptf); data(ptf.data)
	if (class(dataStack) != "RasterStack"){stop("dataStack should be a valid RsaterStack")}
	if(! all(names(dataStack) %in% c(names(ptf.data), "TEXT_US", "TEXT_FAO_MOD"))){stop(paste("names of the layers in dataStack must be taken from this list: ", paste(c(names(ptf.data), "TEXT_US", "TEXT_FAO_MOD"), collapse=", ")))}
	# run PTF on blocks
  if (PTF %in% paste('PTF',18:19,sep='')){
  out <- brick(dataStack,values=FALSE,nl=7)
  } else { if (PTF %in% paste('PTF',20:22,sep='')){
  out <- brick(dataStack,values=FALSE,nl=4)
  } else {  
	out <- raster(dataStack)
  }}
	bs <- blockSize(out)
	out <- writeStart(out, filename, overwrite = TRUE)
	for (i in 1:bs$n){
		nd <- as.data.frame(getValues(dataStack, row = bs$row[i], nrows = bs$nrows[i]))
		# ensure the right format for the factors variables
		if (any(names(nd) == "TOPSOIL")){nd$TOPSOIL <- factor(nd$TOPSOIL, levels = 1:2, labels = c("top","sub"))}
		if (any(names(nd) == "TEXT_FAO_MOD")){nd$TEXT_FAO_MOD <- factor(nd$TEXT_FAO_MOD, levels = c(1:5,9), labels = c("coarse", "fine", "medium", "medium fine", "very fine", "organic"))}
		if (any(names(nd) == "TEXT_US")){nd$TEXT_US <- factor(nd$TEXT_US, levels = 1:13, c("C", "SiC", "SC", "SiCL", "CL", "SCL", "Si", "SiL", "L", "SL", "LS", "S", "O"))}
    # transform variables
    if (!is.null(transfo)){
    for (itr in 1:length(transfo)){
      nd[,names(transfo)[itr]] <- eval(parse(text=paste("nd[,'",names(transfo)[itr],"']",transfo[[itr]],sep="")))
    }
    }
		# na.rm
		ok <- complete.cases(nd)
		# run ptf on complete cases
		b <- matrix(NA, nrow=bs$nrows[i]*ncol(out), ncol=nlayers(out))
		b[ok,] <- predict.ptf(nd[ok,], PTF)
		out <- writeValues(out, b, bs$row[i])
	}
	out <- writeStop(out)
	return(out)
}
