SimDimor <-
function(NIDL=30, NIDS=30,Nst=1,Nan=1,Norg.st=1,Norg.an=1,
LSTmeanX=20, LSTsdX=0.5, LANmeanX=8,LANsdX=0.5,
SSTmeanX=8, SSTsdX=0.5, SANmeanX=20, SANsdX=0.5,
LSTmeanY=20, LSTsdY=0.5, LANmeanY=8 ,LANsdY=0.5,
SSTmeanY=8, SSTsdY=0.5, SANmeanY=20, SANsdY=0.5,
LSTmeanZ=20, LSTsdZ=0.5, LANmeanZ=8 ,LANsdZ=0.5,
SSTmeanZ=8, SSTsdZ=0.5, SANmeanZ=20, SANsdZ=0.5, pop_code="test1"){

if(length(LSTmeanX)!=Nst)
stop("Length of LSTmeanX do not match the defined Nst. Every stamen group must be defined by different mean values. To produce multiple stamens within the same distribution use Norg.st")
if(length(LSTmeanY)!=Nst)
stop("Length of LSTmeanY do not match the defined Nst. Every stamen group must be defined by different mean values. To produce multiple stamens within the same distribution use Norg.st")
if(length(LSTmeanZ)!=Nst)
stop("Length of LSTmeanZ do not match the defined Nst. Every stamen group must be defined by different mean values. To produce multiple stamens within the same distribution use Norg.st")
if(length(SSTmeanX)!=Nst)
stop("Length of LSTmeanX do not match the defined Nst. Every stamen group must be defined by different mean values. To produce multiple stamens within the same distribution use Norg.st")
if(length(SSTmeanY)!=Nst)
stop("Length of LSTmeanY do not match the defined Nst. Every stamen group must be defined by different mean values. To produce multiple stamens within the same distribution use Norg.st")
if(length(SSTmeanZ)!=Nst)
stop("Length of LSTmeanZ do not match the defined Nst. Every stamen group must be defined by different mean values. To produce multiple stamens within the same distribution use Norg.st")


if(length(LSTsdX)!=Nst)
stop("Length of LSTsdX do not match the defined Nst. Every stamen group must be defined by different sd values. To produce multiple stamens within the same distribution use Norg.st")
if(length(LSTsdY)!=Nst)
stop("Length of LSTsdY do not match the defined Nst. Every stamen group must be defined by different sd values. To produce multiple stamens within the same distribution use Norg.st")
if(length(LSTsdZ)!=Nst)
stop("Length of LSTsdZ do not match the defined Nst. Every stamen group must be defined by different sd values. To produce multiple stamens within the same distribution use Norg.st")
if(length(SSTsdX)!=Nst)
stop("Length of LSTsdX do not match the defined Nst. Every stamen group must be defined by different sd values. To produce multiple stamens within the same distribution use Norg.st")
if(length(SSTsdY)!=Nst)
stop("Length of LSTsdY do not match the defined Nst. Every stamen group must be defined by different sd values. To produce multiple stamens within the same distribution use Norg.st")
if(length(SSTsdZ)!=Nst)
stop("Length of LSTsdZ do not match the defined Nst. Every stamen group must be defined by different sd values. To produce multiple stamens within the same distribution use Norg.st")


if(length(LANmeanX)!=Nan)
stop("Length of LANmeanX do not match the defined Nan. Every stamen group must be defined by different mean values. To produce multiple anthers within the same distribution use Norg.an")
if(length(LANmeanY)!=Nan)
stop("Length of LANmeanY do not match the defined Nan. Every stamen group must be defined by different mean values. To produce multiple anthers within the same distribution use Norg.an")
if(length(LANmeanZ)!=Nan)
stop("Length of LANmeanZ do not match the defined Nan. Every stamen group must be defined by different mean values. To produce multiple anthers within the same distribution use Norg.an")
if(length(SANmeanX)!=Nan)
stop("Length of LANmeanX do not match the defined Nan. Every stamen group must be defined by different mean values. To produce multiple anthers within the same distribution use Norg.an")
if(length(SANmeanY)!=Nan)
stop("Length of LANmeanY do not match the defined Nan. Every stamen group must be defined by different mean values. To produce multiple anthers within the same distribution use Norg.an")
if(length(SANmeanZ)!=Nan)
stop("Length of LANmeanZ do not match the defined Nan. Every stamen group must be defined by different mean values. To produce multiple anthers within the same distribution use Norg.an")


if(length(LANsdX)!=Nan)
stop("Length of LANsdX do not match the defined Nan. Every stamen group must be defined by different sd values. To produce multiple anthers within the same distribution use Norg.an")
if(length(LANsdY)!=Nan)
stop("Length of LANsdY do not match the defined Nan. Every stamen group must be defined by different sd values. To produce multiple anthers within the same distribution use Norg.an")
if(length(LANsdZ)!=Nan)
stop("Length of LANsdZ do not match the defined Nan. Every stamen group must be defined by different sd values. To produce multiple anthers within the same distribution use Norg.an")
if(length(SANsdX)!=Nan)
stop("Length of LANsdX do not match the defined Nan. Every stamen group must be defined by different sd values. To produce multiple anthers within the same distribution use Norg.an")
if(length(SANsdY)!=Nan)
stop("Length of LANsdY do not match the defined Nan. Every stamen group must be defined by different sd values. To produce multiple anthers within the same distribution use Norg.an")
if(length(SANsdZ)!=Nan)
stop("Length of LANsdZ do not match the defined Nan. Every stamen group must be defined by different sd values. To produce multiple anthers within the same distribution use Norg.an")


NID<-NIDL+NIDS

X<-c()
Y<-c()
Z<-c()


X<-c(X,sapply(1:Nst,function(j) c(rnorm(NIDL*Norg.st,LSTmeanX[j],LSTsdX[j]))),sapply(1:Nan,function(j) c(rnorm(NIDL*Norg.an,LANmeanX[j],LANsdX[j]))))
X<-c(X,sapply(1:Nst,function(j) c(rnorm(NIDS*Norg.st,SSTmeanX[j],SSTsdX[j]))),sapply(1:Nan,function(j) c(rnorm(NIDS*Norg.an,SANmeanX[j],SANsdX[j]))))

Y<-c(Y,sapply(1:Nst,function(j) c(rnorm(NIDL*Norg.st,LSTmeanY[j],LSTsdY[j]))),sapply(1:Nan,function(j) c(rnorm(NIDL*Norg.an,LANmeanY[j],LANsdY[j]))))
Y<-c(Y,sapply(1:Nst,function(j) c(rnorm(NIDS*Norg.st,SSTmeanY[j],SSTsdY[j]))),sapply(1:Nan,function(j) c(rnorm(NIDS*Norg.an,SANmeanY[j],SANsdY[j]))))

Z<-c(Z,sapply(1:Nst,function(j) c(rnorm(NIDL*Norg.st,LSTmeanZ[j],LSTsdZ[j]))),sapply(1:Nan,function(j) c(rnorm(NIDL*Norg.an,LANmeanZ[j],LANsdZ[j]))))
Z<-c(Z,sapply(1:Nst,function(j) c(rnorm(NIDS*Norg.st,SSTmeanZ[j],SSTsdZ[j]))),sapply(1:Nan,function(j) c(rnorm(NIDS*Norg.an,SANmeanZ[j],SANsdZ[j]))))


if(Nst==1 & Nan==1)
SEXORG <- rep(c(rep("ST",Norg.st),rep("AN",Norg.st)),NID)

if(Nst!=1 & Nan==1)
SEXORG <- rep(c(paste(rep("ST",Nst*Norg.st),1:Nst,sep=""),rep("AN",Norg.st)),NID)

if(Nst==1 & Nan!=1)
SEXORG <- rep(c(rep("ST",Norg.st),paste(rep("AN",Nan*Norg.st),1:Nan,sep="")),NID)

if(Nst!=1 & Nan!=1)
SEXORG <- rep(c(paste(rep("ST",Nst*Norg.st),1:Nst,sep=""),paste(rep("AN",Nan*Norg.st),1:Nan,sep="")),NID)


M<-matrix(
c(
rep(pop_code,NID*(Nst*Norg.st+Nan*Norg.an)),
c(rep("L",NIDL*(Nst*Norg.st+Nan*Norg.an)),rep("S",NIDS*(Nst*Norg.st+Nan*Norg.an))),
sort(rep(1:NID,Nst*Norg.st+Nan*Norg.an)),
c(1:(NID*(Nst*Norg.st+Nan*Norg.an))),
SEXORG,
X,
Y,
Z
),ncol=8,
dimnames=list(c(),c("pop_code","morph","ID_indiv","ID_sexorg","sexorg","x","y","z")))

M<-as.data.frame(M)

M[,6]<-as.numeric(M[,6])
M[,7]<-as.numeric(M[,7])
M[,8]<-as.numeric(M[,8])

M
}
