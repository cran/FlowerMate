\name{FlowerMate-package}
\alias{FlowerMate-package}
\alias{FlowerMate}
\docType{package}
\title{Multidimensional reciprocity and inaccuracy indexes in style-polymorphic plant populations}
\description{FlowerMate computes the original reciprocity indexes of Sánchez et al. (2008, 2013) and Armbruster et al (2017), and their multidimensional variants, developed by Simón-Porcar et al. (2023). These indexes are applicable to common heterostylous populations and to any other type of stylar dimorphic and trimorphic populations, such as stylar-dimorphic, three-dimensional heterostylous and enantiostylous plants.}

\details{
FlowerMate includes a main function, inaccuracy, that computes either the one-, two- or three-dimensional versions of the Reciprocity and Inaccuracy Indexes, either for dimorphic or trimorphic populations, depending on the data input and options chosen. The input format includes one row per measured sex organ and the eight following columns: population code, floral morph, Individual ID number, Sex organ ID number, Sex organ type (stigma or anther), and x, y, z coordinates. The user can select the coordinates to compute, hence obtaining either uni-, bi- or three-dimensional indexes. In stylar polymorphic species with same sex organs at stricklingly different locations (e.g. enantiostylous flowers with both left- and right-facing stamens), the user can select different data subsets for computation. inaccuracy computes the Reciprocity and Inaccuracy Indexes also in presence of missing data, warning that the values may not be comparable across populations (e.g. if the distribution of missing data is strongly biased across morphs or dimensions). There are short and extended output options for inaccuracy, providing either the Reciprocity and Inaccuracy values alone or accompanied of all the intermediate components calculated for each population. FlowerMate also includes functions to simulate style-polymorphic populations with the average coordinates and variance specified for each sex organ level and morph.}

\author{
A. J. Muñoz-Pajares and V. I. Simón-Porcar
}

\references{
Armbruster, W. S., Bolstad, G. H., Hansen, T. F., Keller, B., Conti, E., & Pélabon, C. (2017). The measure and mismeasure of reciprocity in heterostylous flowers. New Phytologist, 215(2), 906-917.

Sánchez JM, Ferrero V, Navarro L. 2008. A new approach to the quantification of degree of reciprocity in distylous (sensu lato) plant populations. Annals of Botany 102: 463–472.

Sánchez JM, Ferrero V, Navarro L. 2013. Quantifying reciprocity in distylous and tristylous plant populations. Plant Biology 15: 616–620.

Simón-Porcar VI, Muñoz-Pajares AJ, Arroyo J, Johnson SD (2023). Multidimensional reciprocity and inaccuracy indexes in style-polymorphic plant populations. Applications in Plant Sciences in Press.
}

