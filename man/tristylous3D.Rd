\name{tristylous3D}
\alias{tristylous3D}
\title{1D Tristylous populations example dataset}
\description{Simulated data for five tristylous populations with 1D data}
\usage{
data(tristylous3D)}
\details{
The dataset contains information from 450 sexual organs from 150 individuals from 5 populations (30 individuals per population). For every sexual organ, the dataset contains eight columns with the population code, morph, individual ID, sexual organ ID, sexual organ type, and coordinates x, y and z. This dataset has been obtained using the SimDimor function recursively as shown in the example section.
}
\value{
A data frame with 450 rows and 8 columns.
}

\author{
A.J. Muñoz-Pajares, V.I. Simón-Porcar
}
\examples{

## This dataset has been generated using:

SEEDS<-12341:12345

exampleDataset<-c()

for(i in 1:length(SEEDS))
{
set.seed(SEEDS[i])
exampleDataset <- rbind(exampleDataset,SimTrimor(NIDL=10, NIDM=10, NIDS=10,
LUPmeanX=8, LUPsdX=0.5, LBWmeanX=4, LBWsdX=0.5, LLWmeanX=2, LLWsdX=0.5,
MUPmeanX=8, MUPsdX=0.5, MBWmeanX=4, MBWsdX=0.5, MLWmeanX=2, MLWsdX=0.5,
SUPmeanX=8, SUPsdX=0.5, SBWmeanX=4, SBWsdX=0.5, SLWmeanX=2, SLWsdX=0.5,
LUPmeanY=2, LUPsdY=0.5, LBWmeanY=2, LBWsdY=0.5, LLWmeanY=2, LLWsdY=0.5,
MUPmeanY=2, MUPsdY=0.5, MBWmeanY=2, MBWsdY=0.5, MLWmeanY=2, MLWsdY=0.5,
SUPmeanY=2, SUPsdY=0.5, SBWmeanY=2, SBWsdY=0.5, SLWmeanY=2, SLWsdY=0.5,
LUPmeanZ=6, LUPsdZ=0.5, LBWmeanZ=3, LBWsdZ=0.5, LLWmeanZ=2, LLWsdZ=0.5,
MUPmeanZ=6, MUPsdZ=0.5, MBWmeanZ=3, MBWsdZ=0.5, MLWmeanZ=2, MLWsdZ=0.5,
SUPmeanZ=6, SUPsdZ=0.5, SBWmeanZ=3, SBWsdZ=0.5, SLWmeanZ=2, SLWsdZ=0.5))
}


## To load and test the inaccuracy function:

data(tristylous3D)
# inaccuracy(tristylous3D)

}
