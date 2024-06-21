\name{distylous3D}
\alias{distylous3D}
\title{3D Distylous populations example dataset}
\description{Simulated data for five distylous populations with 3D data}
\usage{
data(distylous3D)}
\details{
The dataset contains information from 200 sex organs from 100 individuals from 5 populations (20 individuals per population). For every sex organ, the dataset contains eight columns with the population code, morph, individual ID, sex organ ID, sex organ type, and coordinates x, y and z. This dataset has been obtained using the SimDimor function recursively as shown in the example section.
}
\value{
A data frame with 200 rows and 8 columns.
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
exampleDataset <- rbind(exampleDataset,SimDimor(NIDL = 10,NIDS = 10,
Nst = 1, Nan = 1, LSTmeanX = 0, LSTsdX = 0, LANmeanX =, LANsdX = 0,
SSTmeanX = 0, SSTsdX = 0, SANmeanX = 0, SANsdX = 0, LSTmeanY = 20,
LSTsdY = 0.5, LANmeanY = 8, LANsdY = 0.5, SSTmeanY = 8, SSTsdY = 0.5,
SANmeanY = 20, SANsdY = 0.5, LSTmeanZ = 0, LSTsdZ = 0, LANmeanZ =0,
LANsdZ = 0, SSTmeanZ = 0, SSTsdZ = 0, SANmeanZ = 0, 
SANsdZ = 0, pop_code=paste("pop",i,sep="")))
}

## To load and test the inaccuracy function:

data(distylous3D)
inaccuracy(distylous3D)

}
