\name{tristylous1D}
\alias{tristylous1D}
\title{1D Tristylous populations example dataset}
\description{Simulated data for five tristylous populations with 1D data}
\usage{
data(tristylous1D)}
\details{
The dataset contains information from 450 sexual organs from 150 individuals from 5 populations (30 individuals per population). For every sexual organ, the dataset contains eight columns with the population code, morph, individual ID, sexual organ ID, sexual organ type, and coordinates x, y and z. Because the dataset contains information in only one dimension, x and z columns values are 0. This dataset has been obtained using the SimDimor function recursively as shown in the example section.
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
LUPmeanX=0, LUPsdX=0, LBWmeanX=0, LBWsdX=0, LLWmeanX=0, LLWsdX=0,
MUPmeanX=0, MUPsdX=0, MBWmeanX=0, MBWsdX=0, MLWmeanX=0, MLWsdX=0,
SUPmeanX=0, SUPsdX=0, SBWmeanX=0, SBWsdX=0, SLWmeanX=0, SLWsdX=0,
LUPmeanY=2, LUPsdY=0.5, LBWmeanY=2, LBWsdY=0.5, LLWmeanY=2, LLWsdY=0.5,
MUPmeanY=2, MUPsdY=0.5, MBWmeanY=2, MBWsdY=0.5, MLWmeanY=2, MLWsdY=0.5,
SUPmeanY=2, SUPsdY=0.5, SBWmeanY=2, SBWsdY=0.5, SLWmeanY=2, SLWsdY=0.5,
LUPmeanZ=0, LUPsdZ=0, LBWmeanZ=0, LBWsdZ=0, LLWmeanZ=0, LLWsdZ=0,
MUPmeanZ=0, MUPsdZ=0, MBWmeanZ=0, MBWsdZ=0, MLWmeanZ=0, MLWsdZ=0,
SUPmeanZ=0, SUPsdZ=0, SBWmeanZ=0, SBWsdZ=0, SLWmeanZ=0, SLWsdZ=0))
}


## To load and test the inaccuracy function:

data(tristylous1D)
# inaccuracy(tristylous1D)

}
