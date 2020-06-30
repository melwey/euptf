psd2classUS <-
function (sa,si,cl,oc=NULL,option=FALSE,tol=1) {
# set number of entries
n <- length(sa);
# check size
if (n!=length(si) | n!=length(cl)) {
	stop("arguments must be the same size")
}
# initiate output
clas <- sigle <- rep(NA,n)
# Sand
ind <- si+1.5*cl<15
clas[ind] = 12
sigle[ind] = "S"
# Loamy Sand
ind <- si+1.5*cl>=15 & si+1.5*cl<30
clas[ind] = 11
sigle[ind] = "LS"
# Sandy Loam
ind <- (cl>=7 & cl<=20 & sa>52 & si+2*cl>=30) | (cl<7 & si<50 & si+2*cl>=30)
clas[ind] = 10
sigle[ind] = "SL"
# Loam
ind <- cl>=7 & cl<=27 & si>=28 & si<50 & sa<=52
clas[ind] = 9
sigle[ind] = "L"
# Silt Loam
ind <- (si>=50 & cl>=12 & cl<27) | (si>=50 & si<80 & cl<12)
clas[ind] = 8
sigle[ind] = "SiL"
# Silt
ind <- si>=80 & cl<12
clas[ind] = 7
sigle[ind] = "Si"
# Sandy Clay Loam
ind <- cl>=20 & cl<35 & si<28 & sa>45
clas[ind] = 6
sigle[ind] = "SCL"
# Clay Loam
ind <- cl>=27 & cl<40 & sa>20 & sa<=45
clas[ind] = 5
sigle[ind] = "CL"
# Silt Clay Loam
ind <- cl>=27 & cl<40 & sa<=20
clas[ind] = 4
sigle[ind] = "SiCL"
# Sandy Clay
ind <- cl>=35 & sa>=45
clas[ind] = 3
sigle[ind] = "SC"
# Silt Clay
ind <- cl>=40 & si>=40
clas[ind] = 2
sigle[ind] = "SiC"
# Clay
ind <- cl>=40 & sa<=45 & si<40
clas[ind] = 1
sigle[ind] = "C"
# Texture not summing to 100
ind <- cl+si+sa < 100-tol | cl+si+sa > 100+tol | is.na(cl+si+sa)
clas[ind] <- NA
sigle[ind] <- NA
# organic soils
if (!is.null(oc)){
# Organic (from FAO WRB. definition of organic (histic) horizons)
	indO <- (oc >= 18 & cl>=60) | (oc>= 12 & is.na(cl)) | (oc >= (12+cl*0.1) & cl < 60) 
clas[indO] <- 13
sigle[indO] <- "O"
}
if (option){return(sigle)
} else {return(clas)
}}
