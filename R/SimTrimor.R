SimTrimor<-function(NIDL=30,NIDM=30,NIDS=30,
LUPmeanX=0, LUPsdX=1, LBWmeanX=0, LBWsdX=1, LLWmeanX=0, LLWsdX=1,
MUPmeanX=0, MUPsdX=1, MBWmeanX=0, MBWsdX=1, MLWmeanX=0, MLWsdX=1,
SUPmeanX=0, SUPsdX=1, SBWmeanX=0, SBWsdX=1, SLWmeanX=0, SLWsdX=1,

LUPmeanY=0, LUPsdY=1, LBWmeanY=0, LBWsdY=1, LLWmeanY=0, LLWsdY=1,
MUPmeanY=0, MUPsdY=1, MBWmeanY=0, MBWsdY=1, MLWmeanY=0, MLWsdY=1,
SUPmeanY=0, SUPsdY=1, SBWmeanY=0, SBWsdY=1, SLWmeanY=0, SLWsdY=1,

LUPmeanZ=0, LUPsdZ=1, LBWmeanZ=0, LBWsdZ=1, LLWmeanZ=0, LLWsdZ=1,
MUPmeanZ=0, MUPsdZ=1, MBWmeanZ=0, MBWsdZ=1, MLWmeanZ=0, MLWsdZ=1,
SUPmeanZ=0, SUPsdZ=1, SBWmeanZ=0, SBWsdZ=1, SLWmeanZ=0, SLWsdZ=1,

pop_code="test1"){

Nst<-1
Nan<-2
NID<-sum(NIDL,NIDM, NIDS)
M<-matrix(
c(
rep(pop_code,NID*(Nst+Nan)),
c(rep("L",NIDL*(Nst+Nan)),rep("M",NIDM*(Nst+Nan)),rep("S",NIDS*(Nst+Nan))),
c(rep(c("up","bw","low"),NIDL),
  rep(c("bw","up","low"),NIDM),
  rep(c("low","up","bw"),NIDS)),
sort(rep(1:NID,Nst+Nan)),
c(1:(NID*(Nst+Nan))),
rep(c(rep("ST",Nst),rep("AN",Nan)),NID),
rep(0,NID*(Nst+Nan)),
rep(0,NID*(Nst+Nan)),
rep(0,NID*(Nst+Nan))
),ncol=9,
dimnames=list(c(),c("pop_code","morph","level","ID_indiv","ID_sexorg","sexorg","x","y","z")))

M<-as.data.frame(M)


X<-rep(NA,nrow(M))
X[which(M[,2]=="L" & M[,3]=="up")] <- rnorm(length(X[which(M[,2]=="L" & M[,3]=="up")]),  LUPmeanX,LUPsdX)
X[which(M[,2]=="L" & M[,3]=="bw")] <- rnorm(length(X[which(M[,2]=="L" & M[,3]=="bw")]),  LBWmeanX,LBWsdX)
X[which(M[,2]=="L" & M[,3]=="low")] <- rnorm(length(X[which(M[,2]=="L" & M[,3]=="low")]),LLWmeanX,LLWsdX)

X[which(M[,2]=="M" & M[,3]=="up")] <- rnorm(length(X[which(M[,2]=="M" & M[,3]=="up")]),  MUPmeanX,MUPsdX)
X[which(M[,2]=="M" & M[,3]=="bw")] <- rnorm(length(X[which(M[,2]=="M" & M[,3]=="bw")]),  MBWmeanX,MBWsdX)
X[which(M[,2]=="M" & M[,3]=="low")] <- rnorm(length(X[which(M[,2]=="M" & M[,3]=="low")]),MLWmeanX,MLWsdX)

X[which(M[,2]=="S" & M[,3]=="up")] <- rnorm(length(X[which(M[,2]=="S" & M[,3]=="up")]),  SUPmeanX,SUPsdX)
X[which(M[,2]=="S" & M[,3]=="bw")] <- rnorm(length(X[which(M[,2]=="S" & M[,3]=="bw")]),  SBWmeanX,SBWsdX)
X[which(M[,2]=="S" & M[,3]=="low")] <- rnorm(length(X[which(M[,2]=="S" & M[,3]=="low")]),SLWmeanX,SLWsdX)

M[,7]<-X


Y<-rep(NA,nrow(M))
Y[which(M[,2]=="L" & M[,3]=="up")] <- rnorm(length(Y[which(M[,2]=="L" & M[,3]=="up")])  ,LUPmeanY,LUPsdY)
Y[which(M[,2]=="L" & M[,3]=="bw")] <- rnorm(length(Y[which(M[,2]=="L" & M[,3]=="bw")])  ,LBWmeanY,LBWsdY)
Y[which(M[,2]=="L" & M[,3]=="low")] <- rnorm(length(Y[which(M[,2]=="L" & M[,3]=="low")]),LLWmeanY,LLWsdY)

Y[which(M[,2]=="M" & M[,3]=="up")] <- rnorm(length(Y[which(M[,2]=="M" & M[,3]=="up")])  ,MUPmeanY,MUPsdY)
Y[which(M[,2]=="M" & M[,3]=="bw")] <- rnorm(length(Y[which(M[,2]=="M" & M[,3]=="bw")])  ,MBWmeanY,MBWsdY)
Y[which(M[,2]=="M" & M[,3]=="low")] <- rnorm(length(Y[which(M[,2]=="M" & M[,3]=="low")]),MLWmeanY,MLWsdY)
                                                                                                
Y[which(M[,2]=="S" & M[,3]=="up")] <- rnorm(length(Y[which(M[,2]=="S" & M[,3]=="up")])  ,SUPmeanY,SUPsdY)
Y[which(M[,2]=="S" & M[,3]=="bw")] <- rnorm(length(Y[which(M[,2]=="S" & M[,3]=="bw")])  ,SBWmeanY,SBWsdY)
Y[which(M[,2]=="S" & M[,3]=="low")] <- rnorm(length(Y[which(M[,2]=="S" & M[,3]=="low")]),SLWmeanY,SLWsdY)

M[,8]<-Y

Z<-rep(NA,nrow(M))
Z[which(M[,2]=="L" & M[,3]=="up")] <- rnorm(length(Z[which(M[,2]=="L" & M[,3]=="up")]),   LUPmeanZ,LUPsdZ)
Z[which(M[,2]=="L" & M[,3]=="bw")] <- rnorm(length(Z[which(M[,2]=="L" & M[,3]=="bw")]),   LBWmeanZ,LBWsdZ)
Z[which(M[,2]=="L" & M[,3]=="low")] <- rnorm(length(Z[which(M[,2]=="L" & M[,3]=="low")]), LLWmeanZ,LLWsdZ)
                                                                                                       
Z[which(M[,2]=="M" & M[,3]=="up")] <- rnorm(length(Z[which(M[,2]=="M" & M[,3]=="up")]),   MUPmeanZ,MUPsdZ)
Z[which(M[,2]=="M" & M[,3]=="bw")] <- rnorm(length(Z[which(M[,2]=="M" & M[,3]=="bw")]),   MBWmeanZ,MBWsdZ)
Z[which(M[,2]=="M" & M[,3]=="low")] <- rnorm(length(Z[which(M[,2]=="M" & M[,3]=="low")]), MLWmeanZ,MLWsdZ)

Z[which(M[,2]=="S" & M[,3]=="up")] <- rnorm(length(Z[which(M[,2]=="S" & M[,3]=="up")]),  SUPmeanZ,SUPsdZ)
Z[which(M[,2]=="S" & M[,3]=="bw")] <- rnorm(length(Z[which(M[,2]=="S" & M[,3]=="bw")]),  SBWmeanZ,SBWsdZ)
Z[which(M[,2]=="S" & M[,3]=="low")] <- rnorm(length(Z[which(M[,2]=="S" & M[,3]=="low")]),SLWmeanZ,SLWsdZ)

M[,9]<-Z


M
}
