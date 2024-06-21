## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----setup--------------------------------------------------------------------
library(FlowerMate)

## ----eval=F-------------------------------------------------------------------
#  install.packages(FlowerMate)
#  library(FlowerMate)

## -----------------------------------------------------------------------------

## this is just you to get this same random example in your computer
set.seed(1234)
SimDimor(NIDL = 2, NIDS = 2)


## -----------------------------------------------------------------------------

## this is just you to get this same random example in your computer
set.seed(1234)
SimDimor(NIDL = 2, NIDS = 2,
LSTmeanZ = 0, LSTsdZ = 0, LANmeanZ = 0, LANsdZ = 0,
SSTmeanZ = 0, SSTsdZ = 0, SANmeanZ = 0, SANsdZ = 0)

## this is just you to get this same random example in your computer
set.seed(1234)
SimDimor(NIDL = 2, NIDS = 2,LSTmeanX = 0, LSTsdX = 0,
LANmeanX = 0, LANsdX = 0, SSTmeanX = 0, SSTsdX = 0, SANmeanX = 0, SANsdX = 0,
LSTmeanZ = 0, LSTsdZ = 0, LANmeanZ = 0, LANsdZ = 0,
SSTmeanZ = 0, SSTsdZ = 0, SANmeanZ = 0, SANsdZ = 0)


## -----------------------------------------------------------------------------


set.seed(1111)
enantiostylous.dat<- SimDimor (NIDL=20, NIDS=20,Nst=1,Nan=3,Norg.st=1,Norg.an=1,
LSTmeanX=-10, LSTsdX=2, LANmeanX=c(9,7,-7),LANsdX=c(2,2,2),
SSTmeanX=9, SSTsdX=2, SANmeanX=c(-9,-6,6), SANsdX=c(2,2,2),
LSTmeanY=9, LSTsdY=2, LANmeanY=c(7,3,3) ,LANsdY=c(2,2,2),
SSTmeanY=8, SSTsdY=2, SANmeanY=c(6,2,3), SANsdY=c(2,2,2),
LSTmeanZ=15, LSTsdZ=2, LANmeanZ=c(16,15,15) ,LANsdZ=c(2,2,2),
SSTmeanZ=16, SSTsdZ=2, SANmeanZ=c(15,15,15), SANsdZ=c(2,2,2),
pop_code="enantiostylous")

set.seed(2222)
distylous.dat <- SimDimor (NIDL=20, NIDS=20,Nst=1,Nan=1,Norg.st=1,Norg.an=1,
LSTmeanX=0, LSTsdX=0, LANmeanX=0,LANsdX=0,
SSTmeanX=0, SSTsdX=0, SANmeanX=0, SANsdX=0,
LSTmeanY=8, LSTsdY=1, LANmeanY=4 ,LANsdY=1,
SSTmeanY=5, SSTsdY=1, SANmeanY=9, SANsdY=1,
LSTmeanZ=0, LSTsdZ=0, LANmeanZ=0 ,LANsdZ=0,
SSTmeanZ=0, SSTsdZ=0, SANmeanZ=0, SANsdZ=0,
pop_code="distylous")

set.seed(3333)
styledimor.2antherwhorl.dat <- SimDimor (NIDL=20, NIDS=20,Nst=1,Nan=2,Norg.st=1,Norg.an=1,
LSTmeanX=0, LSTsdX=0, LANmeanX=c(0,0),LANsdX=c(0,0),
SSTmeanX=0, SSTsdX=0, SANmeanX=c(0,0), SANsdX=c(0,0),
LSTmeanY=8, LSTsdY=1, LANmeanY=c(6,5) ,LANsdY=c(1,1),
SSTmeanY=4, SSTsdY=1, SANmeanY=c(6,5), SANsdY=c(1,1),
LSTmeanZ=0, LSTsdZ=0, LANmeanZ=c(0,0) ,LANsdZ=c(0,0),
SSTmeanZ=0, SSTsdZ=0, SANmeanZ=c(0,0), SANsdZ=c(0,0),
pop_code="styledimorphic")

set.seed(4444)
tristylous.dat <-SimTrimor(NIDL=20,NIDM=20,NIDS=20,
LUPmeanX=0, LUPsdX=0, LBWmeanX=0 ,LBWsdX=0, LLWmeanX=0, LLWsdX=0,
MUPmeanX=0, MUPsdX=0, MBWmeanX=0 ,MBWsdX=0, MLWmeanX=0, MLWsdX=0,
SUPmeanX=0, SUPsdX=0, SBWmeanX=0 ,SBWsdX=0, SLWmeanX=0, SLWsdX=0,
LUPmeanY=12, LUPsdY=2, LBWmeanY=8 ,LBWsdY=2, LLWmeanY=4, LLWsdY=2,
MUPmeanY=12, MUPsdY=2, MBWmeanY=8 ,MBWsdY=2, MLWmeanY=4, MLWsdY=2,
SUPmeanY=12, SUPsdY=2, SBWmeanY=8 ,SBWsdY=2, SLWmeanY=4, SLWsdY=2,
LUPmeanZ=0, LUPsdZ=0, LBWmeanZ=0 ,LBWsdZ=0, LLWmeanZ=0, LLWsdZ=0,
MUPmeanZ=0, MUPsdZ=0, MBWmeanZ=0 ,MBWsdZ=0, MLWmeanZ=0, MLWsdZ=0,
SUPmeanZ=0, SUPsdZ=0, SBWmeanZ=0 ,SBWsdZ=0, SLWmeanZ=0, SLWsdZ=0,
pop_code="tristylous")

## -----------------------------------------------------------------------------
inaccuracy(enantiostylous.dat)
inaccuracy(distylous.dat)
inaccuracy(styledimor.2antherwhorl.dat)
inaccuracy(tristylous.dat)

## -----------------------------------------------------------------------------
inaccuracy(enantiostylous.dat)
inaccuracy(enantiostylous.dat, useonly.dim = c("x"))
inaccuracy(enantiostylous.dat, useonly.dim = c("y"))

## -----------------------------------------------------------------------------
inaccuracy(enantiostylous.dat, verbose=TRUE)
inaccuracy(distylous.dat, verbose=TRUE)
inaccuracy(styledimor.2antherwhorl.dat, verbose=TRUE)
inaccuracy(tristylous.dat, verbose=TRUE)

## ---- error=TRUE--------------------------------------------------------------
inaccuracy(enantiostylous.dat, intramorph=TRUE)
inaccuracy(distylous.dat, intramorph=TRUE)
inaccuracy(styledimor.2antherwhorl.dat, intramorph=TRUE)
inaccuracy(tristylous.dat, intramorph=TRUE)

## ---- error=TRUE--------------------------------------------------------------
# This will create new objects and insert NA in the first value of every y column

enantiostylous.dat_NA <- enantiostylous.dat
distylous.dat_NA <- distylous.dat
styledimor.2antherwhorl.dat_NA <- styledimor.2antherwhorl.dat
tristylous.dat_NA <- tristylous.dat


enantiostylous.dat_NA[1,"y"] <- distylous.dat_NA[1,"y"] <- styledimor.2antherwhorl.dat_NA[1,"y"] <- tristylous.dat_NA[1,"y"]<- NA

inaccuracy(enantiostylous.dat)
inaccuracy(distylous.dat)
inaccuracy(styledimor.2antherwhorl.dat)
inaccuracy(tristylous.dat)

inaccuracy(enantiostylous.dat, na.rm=T)
inaccuracy(distylous.dat, na.rm=T)
inaccuracy(styledimor.2antherwhorl.dat, na.rm=T)
inaccuracy(tristylous.dat, na.rm=T)


## ---- error=FALSE-------------------------------------------------------------
inaccuracy(enantiostylous.dat)
inaccuracy(enantiostylous.dat,useonly.vert=c("ST","AN1","AN3"))

## -----------------------------------------------------------------------------
## This code will generate a five-populations input data:

SEEDS<-4321:4325
exampleDataset<-c()
for(i in 1:length(SEEDS))
{
set.seed(SEEDS[i])
exampleDataset <- rbind(exampleDataset,SimDimor(NIDL = 10,NIDS = 10,
Nst = 1, Nan = 1, LSTmeanX = 22, LSTsdX = 0.7, LANmeanX = 6, LANsdX = 0.2,
SSTmeanX = 6, SSTsdX = 0.2, SANmeanX = 22, SANsdX = 0.7, LSTmeanY = 20,
LSTsdY = 0.5, LANmeanY = 8, LANsdY = 0.5, SSTmeanY = 8, SSTsdY = 0.5,
SANmeanY = 20, SANsdY = 0.5, LSTmeanZ = 18, LSTsdZ = 0.3, LANmeanZ = 9,
LANsdZ = 0.3, SSTmeanZ = 9, SSTsdZ = 0.3, SANmeanZ = 18, SANsdZ = 0.3,
pop_code=paste("pop",i,sep="")))
}

## Basic analysis
inaccuracy(exampleDataset)
## Verbose
inaccuracy(exampleDataset,verbose=TRUE)
## Subsetting dimensions
inaccuracy(exampleDataset,useonly.dim = c("x","y"))

