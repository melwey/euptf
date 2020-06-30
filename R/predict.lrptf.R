predict.lrptf <-
function(lrptf,newdata,na.action=na.pass,ensemble=FALSE){
if (!is.logical(newdata$TOPSOIL)){
topsoil <- newdata$TOPSOIL
newdata$TOPSOIL <- newdata$TOPSOIL == "top"}
if (grepl("pseudo.boot.fun",as.character(lrptf$call)[3])){
tt <- terms(lrptf$formula)
Terms <- delete.response(tt)
m <- model.frame(Terms,newdata,na.action=na.action)
X <- model.matrix(Terms,m)
if (ensemble){
beta <- lrptf$t[,1:(ncol(lrptf$t)-3)]
attr(beta,"colnames") <- attr(lrptf$t,"colnames")[1:(ncol(lrptf$t)-3)]
predictor <- X %*% t(beta)
} else {
beta <- lrptf$median_model[1:(length(lrptf$median_model)-3)]
predictor <- drop(X[, , drop = FALSE] %*% beta)
}
}
if (grepl("pseudo.boot.stpw",as.character(lrptf$call)[3])){
predictor <- matrix(NA,nrow=nrow(newdata),ncol=lrptf$R)
for (i in 1:lrptf$R){
tmp <- attr(lrptf$t[[i]]$stpw.anova,"heading")
tt <- terms(as.formula(paste(tmp[5:(length(tmp)-1)],collapse="")))
Terms <- delete.response(tt)
m <- model.frame(Terms,newdata,na.action=na.action)
X <- model.matrix(Terms,m)
beta <- lrptf$t[[i]]$stpw.coef
predictor[,i] <- drop(X[, , drop = FALSE] %*% beta)
}
}
return(predictor)
}
