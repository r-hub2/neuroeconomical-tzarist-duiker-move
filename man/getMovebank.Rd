\name{getMovebank}

\alias{getMovebank}
\alias{getMovebank,character,missing-method}
\alias{getMovebank,character,MovebankLogin-method}

\docType{methods}

\title{Download Data from Movebank}

\description{An enhanced function to download information of studies, animals, deployments and tags, and sensor measurements from \href{https://www.movebank.org}{Movebank}. Many of the options of this function have been included as separate more user friendly functions listed in the \emph{See Also} section below. }

\usage{
\S4method{getMovebank}{character,MovebankLogin}(entity_type , login, ...) 
}


\arguments{
	\item{entity_type}{character. The entity type to download from movebank, possible options are: "tag_type", "study", "tag", "individual", "deployment" or "event". See 'Details' for more information.}
	\item{login}{a \code{\link[=MovebankLogin-class]{MovebankLogin}} object, if empty you'll be asked to enter your username and password.}
	\item{...}{Arguments passed on to the Movebank \href{https://github.com/movebank/movebank-api-doc/blob/master/movebank-api.md}{API}:
	\describe{
		\item{\code{i_am_owner}}{logical. If TRUE all studies the user is a data manager for will be returned. Optional.}
	\item{\code{study_id}}{numeric. It is the \emph{Movebank ID} of the study. It can be obtained on the \emph{Study Details} page on Movebank or with \code{\link{getMovebankID}}.}
	\item{\code{individual_id}}{numeric. It is the internal individual Movebank identifier. A single individual or a vector of several individuals from the same study can be specified. It corresponds to the \emph{id} values of \code{getMovebank("individual", login, study_id)}. Optional.}
	\item{\code{deployment_id}}{numeric. It is the deployment Movebank identifier. A single deployment or a vector of several deployments from the same study can be specified. It corresponds to the \emph{id} values of \code{getMovebank("deployment", login, study_id)}. Optional.}
	\item{\code{sensor_type_id}}{numeric. It is the numeric id of the sensor type. A single sensor type or a vector of several sensor types can be specified. The corresponding numeric id for each sensor type can be found through \code{getMovebank("tag_type", login)}. To obtain the sensor types available in the study use \code{\link{getMovebankSensors}}. Optional.}
	\item{\code{attributes}}{character. A single attribute, a vector of attributes or "all" can be specified. Optional. See 'Details' for more information.}
 \item{\code{timestamp_start, timestamp_end}}{character or POSIXct. Starting and/or ending timestamp to download the data for a specific time period. Timestamps have to be provided in format 'yyyyMMddHHmmssSSS'. If POSIXct then it is converted to character using UTC as a time zone, note that this can change the time. Optional.}
 }
 }
}


\details{
	\itemize{
		\item \code{getMovebank("tag_type", login)}: returns all sensor types in Movebank and their corresponding sensor id. See also \link{getMovebankSensors}.
		\item \code{getMovebank("study", login)}: returns all studies where the user has permission to see the data. You may have permission to see only the study details, view some or all tracks but not download data, or view and download some or all data. Also, there are studies that you do not have permission to see at all, these studies will not be included in this list. See also \link{getMovebankStudies}.
		\item \code{getMovebank("study", login, i_am_owner=T)}: returns all studies where the user is a data manager. 
		\item \code{getMovebank("study", login, study_id)}:  returns a summary of information about one or more studies. See also \link{getMovebankStudy}.
		\item \code{getMovebank("tag", login, study_id)}: returns tag reference information from a study. See also \link{getMovebankReferenceTable}.
		\item \code{getMovebank("individual", login, study_id)}: returns animal reference information from a study. See also \link{getMovebankAnimals}, \link{getMovebankReferenceTable}.
		\item \code{getMovebank("deployment", login, study_id)}: returns deployment reference information from a study. See also \link{getMovebankReferenceTable}.
		\item \code{getMovebank("event", login, study_id,...)}: returns the sensor measurements from a study. See also \link{getMovebankData}, \link{getMovebankLocationData}, \link{getMovebankNonLocationData}.
	}

	The default columns of \code{getMovebank("event", login,study_id,...)} are \emph{timestamp, location_lat, location_long, individual_id, tag_id}. If the downloaded study only contains GPS data, these default columns are suitable, but for all other sensors, additional columns are required. The columns available vary among sensor type and tag manufacturer, the complete list of available attributes for a specific study can be obtained with:
	\code{getMovebankSensorsAttributes(study, login=login)}. If \code{attributes="all"} than all attributes that are present in the study will be downloaded.

	The definitions of the content of the columns is detailed in the \href{https://www.movebank.org/node/2381}{Attribute Dictionary on Movebank}
}


\note{
	\itemize{
		\item 'id' in \code{getMovebank("study", login, ...)} are the values required in \code{study_id}
		\item 'id' in \code{getMovebank("individual", login, study_id)} are the values required in \code{individual_id}
		\item 'id' in \code{getMovebank("deployment", login, study_id)} are the values required in \code{deployment_id}
		\item 'id' in \code{getMovebank("tag_type", login)} are the values required in \code{sensor_type_id}
		\item 'id' in \code{getMovebank("tag", login, study_id)} corresponds to 'tag_id'
	}

	See the 'browseMovebank' vignette for more information about security and how to use Movebank from within R. 

	If the data include double timestamps you can use the \code{\link{getDuplicatedTimestamps}} function to identify them and decide which one to keep.
}


\value{
	'\code{data.frame}'

}

\seealso{ 
	\link{movebankLogin}, \link{getMovebankData}, \link{getMovebankLocationData}, \link{getMovebankNonLocationData}, \link{getMovebankReferenceTable}, \link{getMovebankAnimals}, \link{getMovebankID}, \link{getMovebankSensors}, \link{getMovebankSensorsAttributes}, \link{getMovebankStudies}, \link{getMovebankStudy}, \link{searchMovebankStudies} 

}

\author{Marco Smolla & Anne Scharf}


\examples{
\dontrun{

## first create the login object 
login <- movebankLogin()

anne## get Movebank ID from study
studyID <- getMovebankID(study="MPIAB white stork lifetime tracking data (2013-2014)", 
                         login=login)
studyID2 <- getMovebankID(study="Ocelots on Barro Colorado Island, Panama", login=login)

## get a summary of information about the two studies
getMovebank("study", login=login, study_id=c(studyID,studyID2)) 

## get tag reference information from the study
head(getMovebank("tag", login=login, study_id=studyID))

## get animal reference information from the study
head(getMovebank("individual", login=login, study_id=studyID))

## get deployments reference information from the study
head(getMovebank("deployment", login=login, study_id=studyID))

## get the sensor measurements from the study
## find out which sensors were used in this study
unique(getMovebankSensors(study=studyID,login=login)$sensor_type_id)
## get movebank ID of one individual of this study
indID <- getMovebank("individual", login=login, study_id=studyID)$id[50]
## the correspondence table between the individual ID and the 
## animal names can be obtained like this
head(getMovebank("individual", login=login, 
                 study_id=studyID)[, c("id", "local_identifier")])

## get GPS and accelerometer data within a time period
## to download all attributes for all sensors included in the study
attrib <- "all"
## get measurments for a given time period, in this case for GPS and 
## accelerometer, and between "2013-06-25 03:55:00.000" and "2013-06-26 10:25:00.000"
getMovebank("event", login=login, study_id=studyID, sensor_type_id=c(653,2365683), 
            individual_id=indID, attributes=attrib, timestamp_start="20130625035500000", 
            timestamp_end="20130626102500000 ")

## get all GPS data for 2 individuals
## get movebank ID of another individual of this study
indID2 <- getMovebank("individual", login=login, study_id=studyID)$id[35]
## get GPS measurements for these two individuals with all available attributes
head(storks <- getMovebank("event", login=login, study_id=studyID, 
                           sensor_type_id=653, individual_id=c(indID,indID2), 
                           attributes="all"))

## create moveStack
## get the names of the individuals as they appear on Movebank
(individualNames <- getMovebank("individual", login=login, 
                                study_id=studyID)[c(35,50), c("id", "local_identifier")])
head(storks2 <- merge(storks,individualNames,by.x="individual_id", by.y="id"))

myMoveStack <- move(x=storks2$location_long, y=storks2$location_lat,
                    time=as.POSIXct(storks2$timestamp, format="%Y-%m-%d %H:%M:%S", tz="UTC"),
                    data=storks2, 
                    proj=CRS("+proj=longlat +ellps=WGS84"), 
                    animal=storks2$local_identifier)

plot(myMoveStack, type="l")
}
}
