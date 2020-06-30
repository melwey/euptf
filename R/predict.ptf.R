predict.ptf <-
function(newdata,ptf,h=NULL,...){
# wrapper function for all PTFs
# newdata: data frame. Columns with predictors must respect the names used in the functions. Follow example: data(ptf.data)
# ptf: character string. name of ptf to be run (use ChoosePTF_gWidgets to help you in the choice)


# Load PTF:
data(list=ptf,package="euptf",envir=environment()) 
ptf <- eval(as.name(ptf))

# assess type of PTF
if (is.data.frame(ptf)){
# cPTF
if ("TEXT_FAO_MOD" %in% names(ptf)){
output <- pred_cPTF_MVG_FAO_MOD(newdata,...)
} else {
output <- pred_cPTF_MVG_US(newdata,...)}
} else{
if (length(ptf)==4){
# VG parameter estimation
# set transformation of parameter
tr <- c("x","x","10^(x)","10^(x)+1")
output <- matrix(NA,nrow=nrow(newdata),ncol=4,dimnames=list(NULL,c("thr","ths","alp","n")))
output[,1] <- predict(ptf[[1]],newdata)
for (i in 2:4){
x <- predict.lrptf(ptf[[i]],newdata)
output[,i] <- eval(parse(text=tr[i]))
}
} else {
if (class(ptf)=="rpart"){
# rpart object from package mvpart
# 18/02/2015: package mvpart not supported on CRAN anymore.
# Use of rpart instead
require("rpart")
names(newdata) <- sub("_MOD","",names(newdata))
# set factors
# 07/03/2016: correction of factor's levels for TEXT_FAO, TEXT_US and TOPSOIL
if ("TEXT_FAO" %in% names(newdata)){
	if (!is.factor(newdata$TEXT_FAO)){
    # set levels correctly
		newdata$TEXT_FAO <- factor(newdata$TEXT_FAO, levels=c("coarse", "fine", "medium", "medium fine", "organic", "very fine"))
		} else {
    # make sure levels are correct: as string, then as factor with correct levels
    tmp <- as.character(newdata$TEXT_FAO)
    newdata$TEXT_FAO <- factor(tmp, levels=c("coarse", "fine", "medium", "medium fine", "organic", "very fine"))
    }
	}
if ("TEXT_US" %in% names(newdata)){
	if (!is.factor(newdata$TEXT_US)){
		newdata$TEXT_US <- factor(newdata$TEXT_US,levels=c("C",    "CL",   "L",    "LS",   "O",    "S",    "SC",   "SCL",  "Si",   "SiC",  "SiCL", "SiL",  "SL"))
		} else {
    # make sure levels are correct: as string, then as factor with correct levels
    tmp <- as.character(newdata$TEXT_US)
    newdata$TEXT_US <- factor(tmp,levels=c("C",    "CL",   "L",    "LS",   "O",    "S",    "SC",   "SCL",  "Si",   "SiC",  "SiCL", "SiL",  "SL"))
    }
	}
tmp <- as.character(newdata$TOPSOIL)
newdata$TOPSOIL <- factor(tmp,levels=c('sub','top'))
output <- predict(ptf,newdata)
} else {
# lr point ptf
output <- predict.lrptf(ptf,newdata)
}}}

if (!is.null(h)){
if (is.matrix(output)){
# run MV and output ptf results at requested h
if (nrow(output)==1){
param <- matrix(output[,colnames(output) %in% c("thr","ths","alp","n","m","K0","L")],nrow=nrow(output))
} else {
param <- as.matrix(output[,colnames(output) %in% c("thr","ths","alp","n","m","K0","L")])
}
output <- array(unlist(apply(param,1,MV,h)),dim=c(length(h),4,nrow(param)),dimnames=list(NULL,c("h","theta","Se","K"),NULL))
} else {
warning("h ignored")
}}
return(output)}
