pred_cPTF_MVG_US <-
function(newdata,nms=c("SAMPLE_ID","TOPSOIL","TEXT_US")){
# newdata must have columns nms
# nms is a character vector of the column names for the sample identifier, the topsoil/subsoil distinction (TRUE/FALSE or "top"/"sub"), and the USDA texture class ("C","CL","L","LS","O","S","SC","SCL","Si","SiC","SiCL","SiL","SL")
	if (is.logical(newdata[,nms[2]])){
	topsoil <- rep("",length(newdata[,nms[2]]))
	topsoil[newdata[,nms[2]]] <- "top"
	topsoil[!newdata[,nms[2]]] <- "sub"
	newdata[,nms[2]] <- topsoil
	}
	# PTF19 is either loaded by predict.ptf or here
	if (!exists("PTF19")){
	data(PTF19,package="euptf",envir=environment())}
	cPTF_US <- PTF19
	output <- merge(newdata[,nms],cPTF_US,by.x=nms[2:3],by.y=c("TOPSOIL","TEXT_US"),all.x=TRUE)
	output <- output[match(newdata[,nms[1]],output[,nms[1]]),]
	nms2 <- names(output)
	output <- as.matrix(output[,!(nms2 %in% nms)])
	return(output)
}
