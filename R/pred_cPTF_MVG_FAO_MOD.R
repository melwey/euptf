pred_cPTF_MVG_FAO_MOD <-
function(newdata,nms=c("SAMPLE_ID","TOPSOIL","TEXT_FAO_MOD")){
# newdata must have columns nms
# nms is a character vector of the column names for the sample identifier, the topsoil/subsoil distinction (TRUE/FALSE or "top"/"sub"), and the modified FAO texture class ("coarse","medium","medium fine", "fine","very fine","organic")
	# PTF18 is either loaded by predict.ptf or here
	if (!exists("PTF18")){
	data(PTF18,package="euptf",envir=environment())}
	if (is.logical(newdata[,nms[2]])){
	topsoil <- rep("",length(newdata[,nms[2]]))
	topsoil[newdata[,nms[2]]] <- "top"
	topsoil[!newdata[,nms[2]]] <- "sub"
	newdata[,nms[2]] <- topsoil
	}
	cPTF_FAO_MOD <- PTF18
	output <- merge(newdata[,nms],cPTF_FAO_MOD,by.x=nms[2:3],by.y=c("TOPSOIL","TEXT_FAO_MOD"),all.x=TRUE)
	output <- output[match(newdata[,nms[1]],output[,nms[1]]),]
	nms2 <- names(output)
	output <- as.matrix(output[,!(nms2 %in% nms)])
	return(output)
}
