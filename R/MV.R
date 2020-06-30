MV <- function(par,h=NULL,theta=NULL){
	if (is.list(par)){par<-unlist(par)}
	if (length(par)<5){
		par[5] <- 1-1/par[4]
	}
	if (length(par)<6){
		par[6:7]<-NA
	}
	thr<-par[1];ths<-par[2];alp<-par[3];n<-par[4];m<-par[5];K0<-par[6];L<-par[7]
	if (is.null(theta)){
		Se <- (1 + (alp * h)^n)^(-m)
		theta <- thr + (ths-thr) * Se
	} else {
		Se <- (theta - thr)/(ths-thr)
		h <- 1/alp * (1 + Se^(-n/(n-1)))^(1/n)
	}
	K <- K0 * Se^L * (1-(1-Se^(1/m))^m)^2
	output <- data.frame(h=h,theta=theta,Se=Se,K=K)
	return(output)
}
