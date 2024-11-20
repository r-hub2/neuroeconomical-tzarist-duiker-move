\name{getDataRepositoryData}
\alias{getDataRepositoryData}
\alias{getDataRepositoryData,character-method}

\docType{methods}

\title{Download data from the Movebank Data Repository}

\description{Download data from the \href{https://www.movebank.org/node/15294}{Movebank Data Repository} via DOI

}
\usage{
getDataRepositoryData(x, ...)
}

\arguments{
  \item{x}{character string of the DOI of data stored on the \href{https://www.movebank.org/node/15294}{Movebank Data Repository}}
	  \item{...}{Currently not used}
}
\details{
	This function downloads data stored in the \href{https://www.movebank.org/node/15294}{Movebank Data Repository} via the DOI. The output is MoveStack object containing the location data from all available sensors in the study. The non-location senor data are stored in the \code{UnUsedRecords} slots. Datasets without location data are excluded.\cr\cr
	If duplicated timestamps are present in the data, the first one is chosen by default. To use a more informed approach you can download the data of interest from the Movebank Data Repository, read it in with \code{read.csv} and use the function \link{getDuplicatedTimestamps} to located the duplicated timestamps and then decide which one to keep. And then use the function \link{move} to create a Move or MoveStack object from the cleaned .csv file.
}

\note{
Visit the dataset's repository page at http://dx.doi.org/<doi> for citations and a readme that might contain additional details needed to understand the data. If analyzing these published datasets, always consult the related papers and cite the paper and dataset. If preparing analysis for publication, also contact the data owner if possible for their contribution.
}

\value{
  \link[=Move-class]{move} or \link[=MoveStack-class]{moveStack} object
}

\author{
	Anne Scharf
}
\seealso{
  \link{getMovebankData}, \link{getMovebankNonLocationData}, \link{getMovebank}, \link{move}
}
\examples{
\dontrun{
getDataRepositoryData("doi:10.5441/001/1.2k536j54")
}
}

