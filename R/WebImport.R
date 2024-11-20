setOldClass("request")
setClass(
  Class = "MovebankLogin",
  contains = "request",
  validity = function(object) {
    if (nchar(object$headers["user"]) == 0 || nchar(object$headers["password"] == 0)) {
      return(TRUE)
    }
  }
)
## Browsing Movebank data base
setGeneric("movebankLogin", function(username, password, ...) standardGeneric("movebankLogin"))
setMethod(
  f = "movebankLogin",
  signature = c(username = "character", password = "character"),
  definition = function(username, password) {
    return(new("MovebankLogin", add_headers(user = username, password = password)))
  }
)

setMethod(
  f = "movebankLogin",
  signature = c(username = "character", password = "missing"),
  definition = function(username, password) {
    pwd <- readline("password:")
    return(movebankLogin(username = username, password = pwd))
  }
)

setMethod(
  f = "movebankLogin",
  signature = c(username = "missing", password = "missing"),
  definition = function(username, password) {
    user <- readline("username:")
    return(movebankLogin(username = user))
  }
)


## construct URLs and download from Movebank
setGeneric("getMovebank", function(entity_type, login, ...) standardGeneric("getMovebank"))
setMethod(
  f = "getMovebank",
  signature = c(entity_type = "character", login = "MovebankLogin"),
  definition = function(entity_type, login, ...) {
    tmp <- list(...)
    if ("timestamp_start" %in% names(tmp)) {
      if (inherits(tmp[["timestamp_start"]], "POSIXt")) {
        tmp[["timestamp_start"]] <- sub("\\.", "", strftime(format = "%Y%m%d%H%M%OS3", tmp[["timestamp_start"]], tz = "UTC"))
      }
    }
    if ("timestamp_end" %in% names(tmp)) {
      if (inherits(tmp[["timestamp_end"]], "POSIXt")) {
        tmp[["timestamp_end"]] <- sub("\\.", "", strftime(format = "%Y%m%d%H%M%OS3", tmp[["timestamp_end"]], tz = "UTC"))
      }
    }
    url <- paste("https://www.movebank.org/movebank/service/direct-read?entity_type=", entity_type, sep = "")
    if (length(tmp) != 0) {
      tmp <- lapply(tmp, paste, collapse = "%2C")
      url <- paste(url, sep = "&", paste(names(tmp), tmp, collapse = "&", sep = "="))
    }
    f <- GET(url, config = login)
    if (grepl("location_long", url)) {
      cols <- c(location_long = "numeric", location_lat = "numeric")
    } else {
      cols <- NA
    }
    cont <- content(f, as = "text", encoding = "UTF-8")
    if (grepl(
      pattern = "The requested download may contain copyrighted material. You may only download it if you agree with the terms listed below. If study-specific terms have not been specified, read the \"General Movebank Terms of Use\".",
      cont[1]
    )) {
      stop("You need a permission to access this data set. Go to www.movebank.org and accept the license terms when downloading the data set (you only have to do this once per data set).")
    }
    data <- read.csv(textConnection(cont), colClasses = cols)
    if (any(grepl(pattern = "The.request.has.not.been.applied.because.it.lacks.valid.authentication.credentials.for.the.target.resource", x = colnames(data)))) stop("There are no credentials")
    if (any(grepl(pattern = "The.server.understood.the.request.but.refuses.to.authorize", x = colnames(data)))) stop("There are no valid credentials")

    if (any(grepl(pattern = "You.may.only.download.it.if.you.agree.with.the.terms", x = colnames(data)))) stop("You need a permission to access this data set. Go to www.movebank.org and accept the license terms when downloading the data set (you only have to do this once per data set).")
    if (any(grepl(pattern = "X.html..head..title.Apache.Tomcat", colnames(data)))) stop("It looks like you are not allowed to download this data set, either by permission but maybe also an invalid password. Or there is a sensor for which no attributes are available.")
    if (any(grepl(pattern = "are.not.available.for.download", colnames(data)))) stop("You have no permission to download this data set.")
    if (any(grepl(pattern = "503 Service Temporarily Unavailable", unlist(head(data))))) stop("Movebank is (temporarily) unavailable")
    if (any(grepl(pattern = "No.data.are.available.for.download", colnames(data)))) stop("Api reports: No data are available for download")
    if (any(grepl(pattern = "By accepting this document the user agrees to the following", data[, 1]))) stop("It looks like you are not allowed to download this data set, have you agreed to the license terms in the web interface?")
    if (any(grepl(pattern = "The requested URL.s length exceeds the capacity", data[, 1]))) stop("The requested URL's length exceeds the capacity limit for this server. This can for example occur when too many indviduals are requested")


    return(data)
  }
)

setMethod(
  f = "getMovebank",
  signature = c(entity_type = "character", login = "missing"),
  definition = function(entity_type, login, ...) {
    d <- movebankLogin()
    getMovebank(entity_type = entity_type, login = d, ...)
  }
)


# names of the studies
setGeneric("searchMovebankStudies", function(x, login) standardGeneric("searchMovebankStudies"))
setMethod(
  f = "searchMovebankStudies",
  signature = c(x = "character", login = "MovebankLogin"),
  definition = function(x, login) {
    data <- getMovebank("study", login, sort = "name", attributes = "id%2Cname%2Ci_am_owner%2Ci_can_see_data%2Cthere_are_data_which_i_cannot_see")
    res <- as.character(data$name)[grepl(x, data$name, useBytes = TRUE)]
    #    names(res) <- paste("##### Results for ",x,"#####")
    if (length(res) > 0) {
      return(res)
    } else {
      "No study matches your search criteria"
    }
  }
)

setMethod(
  f = "searchMovebankStudies",
  signature = c(x = "character", login = "missing"),
  definition = function(x, login) {
    login <- movebankLogin()
    searchMovebankStudies(x = x, login = login)
  }
)



# get all study names
setGeneric("getMovebankStudies", function(login) standardGeneric("getMovebankStudies"))
setMethod(
  f = "getMovebankStudies",
  signature = c(login = "missing"),
  definition = function(login) {
    login <- movebankLogin()
    getMovebankStudies(login = login)
  }
)

setMethod(
  f = "getMovebankStudies",
  signature = c(login = "MovebankLogin"),
  definition = function(login) {
    data <- getMovebank("study", login, sort = "name", attributes = "id%2Cname%2Ci_am_owner%2Ci_can_see_data%2Cthere_are_data_which_i_cannot_see")
    return(data$name)
  }
)


# names of the sensors
setGeneric("getMovebankSensors", function(study, login) standardGeneric("getMovebankSensors"))
setMethod(
  f = "getMovebankSensors",
  signature = c(study = "ANY", login = "missing"),
  definition = function(study, login) {
    login <- movebankLogin()
    getMovebankSensors(study = study, login = login)
  }
)
setMethod(
  f = "getMovebankSensors",
  signature = c(study = "missing", login = "missing"),
  definition = function(study, login) {
    login <- movebankLogin()
    getMovebankSensors(login = login)
  }
)

setMethod(
  f = "getMovebankSensors",
  signature = c(study = "missing", login = "MovebankLogin"),
  definition = function(study, login) {
    data <- getMovebank("tag_type", login)
    return(data)
  }
)

setMethod(
  f = "getMovebankSensors",
  signature = c(study = "character", login = "MovebankLogin"),
  definition = function(study, login) {
    study <- getMovebankID(study, login)
    callGeneric()
  }
)
setMethod(
  f = "getMovebankSensors",
  signature = c(study = "numeric", login = "MovebankLogin"),
  definition = function(study, login) {
    data <- getMovebank("sensor", login, tag_study_id = study)
    return(data)
  }
)



setGeneric("getMovebankSensorsAttributes", function(study, login, ...) standardGeneric("getMovebankSensorsAttributes"))
setMethod(
  f = "getMovebankSensorsAttributes",
  signature = c(study = "ANY", login = "missing"),
  definition = function(study, login, ...) {
    login <- movebankLogin()
    getMovebankSensorsAttributes(study = study, login = login, ...)
  }
)
setMethod(
  f = "getMovebankSensorsAttributes",
  signature = c(study = "character", login = "MovebankLogin"),
  definition = function(study, login, ...) {
    study <- getMovebankID(study, login, ...)
    callGeneric()
  }
)
setMethod(
  f = "getMovebankSensorsAttributes",
  signature = c(study = "numeric", login = "MovebankLogin"),
  definition = function(study, login, ...) {
    data <- getMovebank("sensor", login, tag_study_id = study, ...)
    studySensors <- unique(data$sensor_type_id)
    data2 <- lapply(studySensors, function(y, login, study) {
      try(getMovebank("study_attribute", login, study_id = study, sensor_type_id = y), silent = T)
    }, login = login, study = study)
    data2 <- data2[(lapply(data2, class)) != "try-error"]
    return(as.data.frame(do.call(rbind, data2)))
  }
)

### all or a certain ID
setGeneric("getMovebankID", function(study, login) standardGeneric("getMovebankID"))
setMethod(
  f = "getMovebankID",
  signature = c(study = "character", login = "missing"),
  definition = function(study, login) {
    login <- movebankLogin()
    getMovebankID(study = study, login = login)
  }
)

setMethod(
  f = "getMovebankID",
  signature = c(study = "character", login = "MovebankLogin"),
  definition = function(study = NA, login) {
    data <- getMovebank("study", login, sort = "name", attributes = "id%2Cname%2Ci_am_owner%2Ci_can_see_data%2Cthere_are_data_which_i_cannot_see")
    if (is.na(study)) {
      return(data[, c("id", "name")])
    } else {
      studyNUM <- data[gsub(" ", "", data$name) == gsub(" ", "", study), c("id")] # get rid of all spaces to avoid miss matching between different spaced words
      if (length(studyNUM) > 1) stop(paste("There was more than one study with the name:", study))
      return(studyNUM)
    }
  }
)



### retrieving information of a certain study
setGeneric("getMovebankStudy", function(study, login) standardGeneric("getMovebankStudy"))
setMethod(
  f = "getMovebankStudy",
  signature = c(study = "numeric", login = "MovebankLogin"),
  definition = function(study, login) {
    data <- getMovebank("study", login, id = study)
    return(data)
  }
)
setMethod(
  f = "getMovebankStudy",
  signature = c(study = "character", login = "MovebankLogin"),
  definition = function(study, login) {
    study <- getMovebankID(study, login)
    callGeneric()
  }
)

setMethod(
  f = "getMovebankStudy",
  signature = c(study = "ANY", login = "missing"),
  definition = function(study, login) {
    login <- movebankLogin()
    getMovebankStudy(study = study, login = login)
  }
)


## get all animals with their IDs
setGeneric("getMovebankAnimals", function(study, login) standardGeneric("getMovebankAnimals"))
setMethod(
  f = "getMovebankAnimals",
  c(study = "character", login = "MovebankLogin"),
  definition = function(study, login) {
    study <- getMovebankID(study, login)
    callGeneric()
  }
)
setMethod(
  f = "getMovebankAnimals",
  c(study = "numeric", login = "MovebankLogin"),
  definition = function(study, login) {
    tags <- getMovebank(entity_type = "sensor", login, tag_study_id = study)
    tagNames <- getMovebank(entity_type = "tag", login, study_id = study)[, c("id", "local_identifier")]
    colnames(tagNames) <- c("tag_id", "tag_local_identifier")
    tags <- merge.data.frame(x = tags, y = tagNames, by = "tag_id")
    animalID <- getMovebank("individual", login, study_id = study)
    # animalID <- getMovebank("individual", login, study_id=study, attributes="id%2Clocal_identifier")
    deploymentID <- getMovebank("deployment", login = login, study_id = study, attributes = "individual_id%2Ctag_id%2Cid")
    #  if (nrow(deploymentID)==0) warning("There are no deployment IDs available!")
    # names(deploymentID)  <- c("individual_id", "tag_id", "deployment_id")
    names(deploymentID) <- sub("^id$", "deployment_id", names(deploymentID))
    if (nrow(tags) != 0) {
      tagdep <- merge.data.frame(x = tags, y = deploymentID, by.x = "tag_id", by.y = "tag_id", all = TRUE) # skipping tags that have no deployment
      tagdepid <- merge.data.frame(x = tagdep, y = animalID, by.x = "individual_id", by.y = "id", all.y = TRUE)[, -3] # skipping the column of the movebank internal tag id
      tagdepid$animalName <- tagdepid$local_identifier
      # 	  colnames(tagdepid) <- c("individual_id", "tag_id", "sensor_type_id", "deployment_id", "animalName")
      if (any(duplicated(tagdepid$individual_id) | duplicated(tagdepid$tag_id))) {
        tagdepid$animalName <- paste(tagdepid$animalName, tagdepid$deployment_id, sep = "_")
        # 				  names(tagdepid)  <- c("individual_id", "tag_id", "sensor_type_id", "deployment_id", "animalName_deployment")}
      }
      return(tagdepid)
    } else {
      return(merge.data.frame(x = deploymentID, y = animalID, by.x = "individual_id", by.y = "id", all.y = TRUE))
    }
  }
)

setMethod(
  f = "getMovebankAnimals",
  c(study = "ANY", login = "missing"),
  definition = function(study, login) {
    login <- movebankLogin()
    getMovebankAnimals(study = study, login = login)
  }
)

## getMovebankData
setGeneric("getMovebankData", function(study, animalName, login, ...) standardGeneric("getMovebankData"))
setMethod(
  f = "getMovebankData",
  signature = c(study = "ANY", animalName = "missing", login = "missing"),
  definition = function(study, animalName, login = login, ...) {
    login <- movebankLogin()
    getMovebankData(study = study, login = login, ...)
  }
)

setMethod(
  f = "getMovebankData",
  signature = c(study = "ANY", animalName = "ANY", login = "missing"),
  definition = function(study, animalName, login = login, ...) {
    login <- movebankLogin()
    getMovebankData(study = study, animalName = animalName, login = login, ...)
  }
)

setMethod(
  f = "getMovebankData",
  signature = c(study = "character", animalName = "ANY", login = "MovebankLogin"),
  definition = function(study, animalName, login, ...) {
    study <- getMovebankID(study, login)
    callGeneric()
  }
)

setMethod(
  f = "getMovebankData",
  signature = c(study = "numeric", animalName = "missing", login = "MovebankLogin"),
  definition = function(study, animalName, login, ...) {
    d <- getMovebank("individual", login = login, study_id = study, attributes = c("id"))$id
    getMovebankData(study = study, login = login, ..., animalName = d)
  }
)
setMethod(
  f = "getMovebankData",
  signature = c(study = "numeric", animalName = "character", login = "MovebankLogin"),
  definition = function(study, animalName, login, ...) {
    d <- getMovebank("individual", login = login, study_id = study, attributes = c("id", "local_identifier"))
    animalName <- d[as.character(d$local_identifier) %in% animalName, "id"]
    callGeneric()
  }
)
setMethod(
  f = "getMovebankData",
  signature = c(
    study = "numeric",
    animalName = "numeric",
    login = "MovebankLogin"
  ),
  definition = function(study,
                        animalName,
                        login,
                        removeDuplicatedTimestamps = FALSE,
                        includeExtraSensors = FALSE,
                        deploymentAsIndividuals = FALSE,
                        includeOutliers = FALSE,
                        ...) {
    # get id data
    idData <- do.call("rbind", lapply(split(animalName, ceiling((1:length(animalName)) / 200)), function(x, ...) {
      getMovebank(
        "individual",
        login = login,
        study_id = study,
        id = x,
        ...
      )
    }, ...))
    colnames(idData)[which(names(idData) == "id")] <-
      "individual_id"
    if (deploymentAsIndividuals) {
      dep <- do.call("rbind", lapply(split(animalName, ceiling((1:length(animalName)) / 200)), function(x, ...) {
        getMovebank(
          "deployment",
          login = login,
          study_id = study,
          individual_id = x,
          ...
        )
      }, ...))

      dep <- do.call("rbind", lapply(split(animalName, ceiling((1:length(animalName)) / 200)), function(x, ...) {
        getMovebank(
          "deployment",
          login = login,
          study_id = study,
          individual_id = x,
          attributes = c(
            "individual_id",
            names(which(
              !unlist(lapply(lapply(dep, is.na), all))
            ))
          ),
          ...
        )
      }, ...))
      colnames(dep)[which(names(dep) == "id")] <- "deployment_id"
      if (any(colnames(dep) == "local_identifier")) {
        colnames(dep)[which(names(dep) == "local_identifier")] <-
          "deployment_local_identifier"
      } else {
        dep$deployment_local_identifier <- dep$deployment_id
      }
      idData <- merge.data.frame(idData, dep, by = "individual_id")
    } else {
      if (all(is.na(idData$local_identifier))) {
        if (any(duplicated(idData$local_identifier))) {
          stop("This needs checking")
        }
        idData$local_identifier <- idData$individual_id
      }
    }
    # get sensor data
    sensorTypes <- getMovebank("tag_type", login = login)
    rownames(sensorTypes) <- sensorTypes$id
    locSen <-
      sensorTypes[as.logical(sensorTypes$is_location_sensor), "id"] # reduce track to location only sensors & only the correct animals
    # get all attributes used in study
    attribs <-
      unique(
        c(
          as.character(getMovebankSensorsAttributes(study, login, ...)$short_name),
          "sensor_type_id",
          "visible",
          "deployment_id",
          "event_id",
          "individual_id",
          "tag_id"
        )
      )
    # get all event data
    trackDF <- do.call(
      "rbind", # split and recombine because requested url can get too long
      lapply(split(animalName, ceiling(1:length(animalName) / 200)), function(x, ...) {
        getMovebank(
          "event",
          login = login,
          study_id = study,
          attributes = attribs,
          individual_id = x,
          sensor_type_id = locSen,
          ...
        )
      }, ...)
    )
    if (includeExtraSensors) {
      otherSen <-
        sensorTypes[!as.logical(sensorTypes$is_location_sensor), "id"]
      otherDF <- do.call(
        "rbind", # split and recombine because requested url can get too long
        lapply(split(animalName, ceiling(1:length(animalName) / 200)), function(x, ...) {
          getMovebank(
            "event",
            login = login,
            study_id = study,
            attributes = attribs,
            individual_id = x,
            sensor_type_id = otherSen,
            ...
          )
        }, ...)
      )
      trackDF <- rbind(trackDF, otherDF)
    }

    if (nrow(trackDF) == 0) {
      stop("No records found for this individual/study combination")
    }

    trackDF <-
      merge.data.frame(trackDF, sensorTypes[, c("id", "name")],
        by.x = "sensor_type_id", by.y =
          "id"
      )
    colnames(trackDF)[which(names(trackDF) == "name")] <-
      "sensor_type"

    if (!is.factor(trackDF$sensor_type)) {
      trackDF$sensor_type <- factor(trackDF$sensor_type)
    }
    trackDF$sensor_type <- droplevels(trackDF$sensor_type)
    trackDF <-
      merge.data.frame(trackDF, unique(idData[, c("individual_id", "local_identifier")]),
        by =
          "individual_id"
      )
    trackDF$individual_id <-
      NULL # this is in idData and not needed here

    ## add users provided tag ID
    tagID <- getMovebank("tag", login, study_id = study)[, c("id", "local_identifier")]
    colnames(tagID) <- c("tag_id", "tag_local_identifier")
    trackDF <- merge.data.frame(trackDF, tagID, by = "tag_id", all.x = T)
    # trackDF$tag_id <- NULL ## could be removed as probably no one uses it....

    if (deploymentAsIndividuals) {
      trackDF <-
        merge.data.frame(trackDF, idData[, c("deployment_id", "deployment_local_identifier")],
          by =
            "deployment_id"
        )
      trackDF$deployment_id <-
        NULL # this is in idData and not needed here
    }
    trackDF$timestamp <-
      as.POSIXct(strptime(
        as.character(trackDF$timestamp),
        format = "%Y-%m-%d %H:%M:%OS",
        tz = "UTC"
      ),
      tz = "UTC"
      )
    if (!deploymentAsIndividuals) {
      if (any(tapply(trackDF$sensor_type_id, trackDF$local_identifier, length) !=
        1)) {
        # data comes in ordered by sensor but needs to be ordered by timestamp
        trackDF <-
          trackDF[with(trackDF, order(trackDF$local_identifier, timestamp)), ]
      }
      trackDF$local_identifier <- as.factor(trackDF$local_identifier)
      levels(trackDF$local_identifier) <-
        validNames(levels((trackDF$local_identifier))) # changing names to 'goodNames' skipping spaces
      rownames(idData) <- validNames(idData$local_identifier)
    } else {
      if (any(
        tapply(
          trackDF$sensor_type_id,
          trackDF$deployment_local_identifier,
          length
        ) != 1
      )) {
        # data comes in ordered by sensor but needs to be ordered by timestamp
        trackDF <-
          trackDF[with(
            trackDF,
            order(trackDF$deployment_local_identifier, timestamp)
          ), ]
      }
      trackDF$deployment_local_identifier <-
        as.factor(trackDF$deployment_local_identifier)
      levels(trackDF$deployment_local_identifier) <-
        validNames(levels((
          trackDF$deployment_local_identifier
        ))) # changing names to 'goodNames' skipping spaces
      rownames(idData) <-
        validNames(idData$deployment_local_identifier)
    }

    outliers <- is.na(trackDF$location_long)
    stopifnot("visible" %in% colnames(trackDF))
    if (!includeOutliers) {
      outliers[trackDF$visible == "false"] <- T
    }
    # outliers[trackDF$visible == "false"] <- T
    # if ('algorithm_marked_outlier' %in% names(trackDF))
    #   outliers[trackDF$algorithm_marked_outlier == "true"] <- T
    # if ('manually_marked_outlier' %in% names(trackDF)) {
    #   outliers[trackDF$manually_marked_outlier == "true"] <- T
    #   outliers[trackDF$manually_marked_outlier == "false"] <- F
    # }
    if (all(outliers)) {
      stop("There not observed records for this study/individual")
    }

    spdf <-
      SpatialPointsDataFrame(
        trackDF[!outliers, c("location_long", "location_lat")],
        data = trackDF[!outliers, ],
        proj4string = CRS("+proj=longlat +datum=WGS84"),
        match.ID = T
      )

    if (!deploymentAsIndividuals) {
      idCol <- "local_identifier"
    } else {
      idCol <- "deployment_local_identifier"
    }
    id <-
      paste(
        format(trackDF$timestamp, "%Y %m %d %H %M %OS4"),
        trackDF[[idCol]],
        trackDF$sensor_type_id
      )
    if (!is.factor(spdf[[idCol]])) {
      spdf[[idCol]] <- factor(spdf[[idCol]])
    }
    trackId <- droplevels(spdf[[idCol]])
    spdf[[idCol]] <- NULL
    if (anyDuplicated(id)) {
      if (any(s <- id[outliers] %in% id[!outliers])) {
        warning(
          "There are timestamps ",
          sum(s),
          " in the unused data that are also in the real data, those records are omitted"
        )
        outliers[outliers][s] <- F
      }
      if (any(s <- duplicated(id[outliers]))) {
        warning(
          "There are ",
          sum(s),
          " duplicated timestamps in the unused that those will be removed"
        )
        outliers[outliers][s] <- F
      }
    }

    unUsed <-
      new(
        ".unUsedRecordsStack",
        dataUnUsedRecords = trackDF[outliers, ],
        timestampsUnUsedRecords = trackDF$timestamp[outliers],
        sensorUnUsedRecords = trackDF[outliers, "sensor_type"],
        trackIdUnUsedRecords = trackDF[outliers, idCol]
      )

    if (any(!(s <-
      (
        as.character(unUsed@trackIdUnUsedRecords) %in% levels(trackId)
      )))) {
      warning(
        "Omiting individual(s) (n=",
        length(unique(unUsed@trackIdUnUsedRecords[!s])),
        ") that have only unUsedRecords"
      )
      unUsed <- unUsed[s, ]
    }
    unUsed@trackIdUnUsedRecords <-
      factor(unUsed@trackIdUnUsedRecords, levels = levels(trackId))
    if (removeDuplicatedTimestamps) {
      message(
        "removeDupilcatedTimestamps was set to TRUE, this will retain the first of multiple records with the same animal ID and timestamp, and remove any subsequent duplicates"
      )
      dupsDf <-
        (data.frame(
          format(spdf$timestamp, "%Y %m %d %H %M %OS4"),
          spdf$sensor_type_id,
          trackId
        ))
      dups <- duplicated(dupsDf)
      spdf <- spdf[!dups, ]
      trackId <- trackId[!dups]
      warning(
        sum(dups),
        " location(s) is/are removed by removeDuplicatedTimestamps"
      )
    }
    s <- getMovebankStudy(study, login)
    if (is.na(s$license_terms)) {
      s$license_terms <- s$license_type
    } else {
      s$license_terms <- paste0(s$license_type, " - ", s$license_terms)
    }

    res <- new(
      "MoveStack",
      spdf,
      timestamps = spdf$timestamp,
      sensor = spdf$sensor_type,
      unUsed,
      trackId = trackId,
      idData = idData[as.character(unique(trackId)), ],
      study = ifelse(is.na(s$name), character(), as.character(s$name)),
      citation = ifelse(is.na(s$citation), character(), as.character(s$citation)),
      license = ifelse(
        is.na(s$license_terms),
        character(),
        as.character(s$license_terms)
      )
    )
    if (length(n.locs(res)) == 1) {
      res <- as(res, "Move")
    }
    return(res)
  }
)


## getMovebankNonLocationData
setGeneric("getMovebankNonLocationData", function(study, sensorID, animalName, login, ...) standardGeneric("getMovebankNonLocationData"))

setMethod(
  f = "getMovebankNonLocationData",
  signature = c(study = "ANY", sensorID = "missing", animalName = "missing", login = "missing"),
  definition = function(study, sensorID, animalName, login = login, ...) {
    login <- movebankLogin()
    getMovebankNonLocationData(study = study, login = login, ...)
  }
)
setMethod(
  f = "getMovebankNonLocationData",
  signature = c(study = "ANY", sensorID = "ANY", animalName = "ANY", login = "missing"),
  definition = function(study, sensorID, animalName, login = login, ...) {
    login <- movebankLogin()
    getMovebankNonLocationData(study = study, sensorID = sensorID, animalName = animalName, login = login, ...)
  }
)
setMethod(
  f = "getMovebankNonLocationData",
  signature = c(study = "ANY", sensorID = "ANY", animalName = "missing", login = "missing"),
  definition = function(study, sensorID, animalName, login = login, ...) {
    login <- movebankLogin()
    getMovebankNonLocationData(study = study, sensorID = sensorID, login = login, ...)
  }
)

setMethod(
  f = "getMovebankNonLocationData",
  signature = c(study = "character", sensorID = "ANY", animalName = "ANY", login = "MovebankLogin"),
  definition = function(study, sensorID, animalName, login, ...) {
    study <- getMovebankID(study, login)
    callGeneric()
  }
)

setMethod(
  f = "getMovebankNonLocationData",
  signature = c(study = "numeric", sensorID = "missing", animalName = "ANY", login = "MovebankLogin"),
  definition = function(study, sensorID, animalName, login, ...) {
    allsens <- getMovebank("tag_type", login = login)[(c("id", "is_location_sensor"))]
    allNL <- allsens$id[allsens$is_location_sensor == "false"]
    sensStudy <- unique(getMovebankSensors(study = study, login = login)$sensor_type_id)
    sensorID <- sensStudy[sensStudy %in% allNL]
    if (missing(animalName)) {
      getMovebankNonLocationData(study = study, sensorID = sensorID, login = login, ...)
    } else {
      getMovebankNonLocationData(study = study, sensorID = sensorID, login = login, animalName = animalName, ...)
    }
  }
)

setMethod(
  f = "getMovebankNonLocationData",
  signature = c(study = "numeric", sensorID = "character", animalName = "ANY", login = "MovebankLogin"),
  definition = function(study, sensorID, animalName, login, ...) {
    ss <- getMovebank("tag_type", login = login)[c("name", "id")]
    sensorID <- ss[as.character(ss$name) %in% sensorID, "id"]
    callGeneric()
  }
)


setMethod(
  f = "getMovebankNonLocationData",
  signature = c(study = "numeric", sensorID = "numeric", animalName = "missing", login = "MovebankLogin"),
  definition = function(study, sensorID, animalName, login, ...) {
    d <- getMovebank("individual", login = login, study_id = study, attributes = c("id"))$id
    getMovebankNonLocationData(study = study, sensorID = sensorID, login = login, ..., animalName = d)
  }
)

setMethod(
  f = "getMovebankNonLocationData",
  signature = c(study = "numeric", sensorID = "numeric", animalName = "character", login = "MovebankLogin"),
  definition = function(study, sensorID, animalName, login, ...) {
    d <- getMovebank("individual", login = login, study_id = study, attributes = c("id", "local_identifier"))
    animalName <- d[as.character(d$local_identifier) %in% animalName, "id"]
    callGeneric()
  }
)

setMethod(
  f = "getMovebankNonLocationData",
  signature = c(study = "numeric", sensorID = "numeric", animalName = "numeric", login = "MovebankLogin"),
  definition = function(study, sensorID, animalName, login, ...) {
    idData <- do.call("rbind", lapply(split(animalName, ceiling((1:length(animalName)) / 200)), function(x, ...) { # split and recombine because requested url can get too long
      getMovebank("individual", login = login, study_id = study, id = x, ...)
    }, ...))
    colnames(idData)[which(names(idData) == "id")] <- "individual_id"

    if (length(study) > 1) {
      stop("Download only possible for a single study")
    }

    sensorTypes <- getMovebank("tag_type", login = login)
    if (length(sensorID) == 0 | length(sensorID[!sensorID %in% sensorTypes$id]) > 0) {
      stop("Sensor name(s) not valid. See 'getMovebankSensors(login)' for valid sensor names")
    }

    if (any(as.logical(sensorTypes$is_location_sensor[sensorTypes$id %in% sensorID]))) {
      stop("The selected sensor(s): '", paste0(sensorTypes$name[sensorTypes$id %in% sensorID & sensorTypes$is_location_sensor == "true"], collapse = ", "), "' is a/are location sensor(s). Only non location data can be downloaded with this function. Use 'getMovebankData' to download location data.")
    }

    sensorAnim <- getMovebankAnimals(study, login)[c("individual_id", "sensor_type_id")]
    if (length(sensorID[!sensorID %in% unique(sensorAnim$sensor_type_id)]) > 0) {
      NoSens <- as.character(sensorTypes$name[sensorTypes$id %in% sensorID[!sensorID %in% unique(sensorAnim$sensor_type_id)]])
      stop("Sensor(s): '", paste0(NoSens, collapse = ", "), "' is/are not available for this study")
    }

    NoDat <- idData$local_identifier[!unlist(lapply(1:nrow(idData), function(x) {
      is.element(sensorID, sensorAnim$sensor_type_id[sensorAnim$individual_id == idData$individual_id[x]])
    }))]
    if (length(NoDat) > 0) {
      animalName <- animalName[!animalName %in% idData$individual_id[as.character(idData$local_identifier) %in% as.character(NoDat)]]
      if (length(NoDat) <= 90) {
        warning("Individual(s): '", paste0(as.character(NoDat), collapse = ", "), "' do(es) not have data for one or more of the selected sensor(s). Data for this/these individual(s) are not downloaded.")
      } else {
        warning("Individual(s): '", paste0(as.character(NoDat[1:90]), collapse = ", "), "' ... and ", (length(NoDat) - 90), " more (total ", length(NoDat), ") do not have data for one or more of the selected sensor(s). Data for these individuals are not downloaded.")
      }
    }

    NonLocData <- do.call(
      "rbind", # split and recombine because requested url can get too long
      lapply(split(animalName, ceiling(1:length(animalName) / 200)), function(x, ...) {
        getMovebank("event", login = login, study_id = study, sensor_type_id = sensorID, individual_id = x, attributes = "all", ...)
      }, ...)
    )

    if (nrow(NonLocData) == 0) { ## when all individuals have 0 data
      stop("This Individual/All Individuals has/have 0 data points for the selected sensor(s).")
    }

    IndivWithData <- unique(NonLocData$individual_id)
    if (!setequal(animalName, IndivWithData)) { ## when some individuals have 0 data
      indivNoData <- idData$local_identifier[!idData$individual_id %in% IndivWithData]
      if (length(indivNoData) <= 90) {
        warning("Individual(s): '", paste0(as.character(indivNoData), collapse = ", "), "' have 0 data points for one or more of the selected sensor(s).")
      } else {
        warning("Individuals: '", paste0(as.character(indivNoData[1:90]), collapse = ", "), "' ... and ", (length(indivNoData) - 90), " more (total ", length(indivNoData), ") have 0 data points for one or more of the selected sensor(s).")
      }
    }

    NonLocData$timestamp <- as.POSIXct(strptime(as.character(NonLocData$timestamp), format = "%Y-%m-%d %H:%M:%OS", tz = "UTC"), tz = "UTC")
    NonLocData$study_name <- as.character(getMovebankStudy(study, login)$name)
    for (i in unique(NonLocData$sensor_type_id)) { ## add sensor type with name
      NonLocData$sensor_type[NonLocData$sensor_type_id == i] <- as.character(sensorTypes$name[sensorTypes$id == i])
    }
    return(NonLocData)
  }
)


## getMovebankLocationData as table
setGeneric("getMovebankLocationData", function(study, sensorID, animalName, login, ...) standardGeneric("getMovebankLocationData"))

setMethod(
  f = "getMovebankLocationData",
  signature = c(study = "ANY", sensorID = "missing", animalName = "missing", login = "missing"),
  definition = function(study, sensorID, animalName, login = login, ...) {
    login <- movebankLogin()
    getMovebankLocationData(study = study, login = login, ...)
  }
)
setMethod(
  f = "getMovebankLocationData",
  signature = c(study = "ANY", sensorID = "ANY", animalName = "ANY", login = "missing"),
  definition = function(study, sensorID, animalName, login = login, ...) {
    login <- movebankLogin()
    getMovebankLocationData(study = study, sensorID = sensorID, animalName = animalName, login = login, ...)
  }
)
setMethod(
  f = "getMovebankLocationData",
  signature = c(study = "ANY", sensorID = "ANY", animalName = "missing", login = "missing"),
  definition = function(study, sensorID, animalName, login = login, ...) {
    login <- movebankLogin()
    getMovebankLocationData(study = study, sensorID = sensorID, login = login, ...)
  }
)

setMethod(
  f = "getMovebankLocationData",
  signature = c(study = "character", sensorID = "ANY", animalName = "ANY", login = "MovebankLogin"),
  definition = function(study, sensorID, animalName, login, ...) {
    study <- getMovebankID(study, login)
    callGeneric()
  }
)

setMethod(
  f = "getMovebankLocationData",
  signature = c(study = "numeric", sensorID = "missing", animalName = "ANY", login = "MovebankLogin"),
  definition = function(study, sensorID, animalName, login, ...) {
    allsens <- getMovebank("tag_type", login = login)[(c("id", "is_location_sensor"))]
    allNL <- allsens$id[allsens$is_location_sensor == "true"]
    sensStudy <- unique(getMovebankSensors(study = study, login = login)$sensor_type_id)
    sensorID <- sensStudy[sensStudy %in% allNL]
    if (missing(animalName)) {
      getMovebankLocationData(study = study, sensorID = sensorID, login = login, ...)
    } else {
      getMovebankLocationData(study = study, sensorID = sensorID, login = login, animalName = animalName, ...)
    }
  }
)

setMethod(
  f = "getMovebankLocationData",
  signature = c(study = "numeric", sensorID = "character", animalName = "ANY", login = "MovebankLogin"),
  definition = function(study, sensorID, animalName, login, ...) {
    ss <- getMovebank("tag_type", login = login)[c("name", "id")]
    sensorID <- ss[as.character(ss$name) %in% sensorID, "id"]
    callGeneric()
  }
)


setMethod(
  f = "getMovebankLocationData",
  signature = c(study = "numeric", sensorID = "numeric", animalName = "missing", login = "MovebankLogin"),
  definition = function(study, sensorID, animalName, login, ...) {
    d <- getMovebank("individual", login = login, study_id = study, attributes = c("id"))$id
    getMovebankLocationData(study = study, sensorID = sensorID, login = login, ..., animalName = d)
  }
)

setMethod(
  f = "getMovebankLocationData",
  signature = c(study = "numeric", sensorID = "numeric", animalName = "character", login = "MovebankLogin"),
  definition = function(study, sensorID, animalName, login, ...) {
    d <- getMovebank("individual", login = login, study_id = study, attributes = c("id", "local_identifier"))
    animalName <- d[as.character(d$local_identifier) %in% animalName, "id"]
    callGeneric()
  }
)

setMethod(
  f = "getMovebankLocationData",
  signature = c(study = "numeric", sensorID = "numeric", animalName = "numeric", login = "MovebankLogin"),
  definition = function(study, sensorID, animalName, login, includeOutliers = FALSE, underscoreToDots = TRUE, ...) {
    idData <- do.call("rbind", lapply(split(animalName, ceiling((1:length(animalName)) / 200)), function(x, ...) { # split and recombine because requested url can get too long
      getMovebank("individual", login = login, study_id = study, id = x, ...)
    }, ...))
    colnames(idData)[which(names(idData) == "id")] <- "individual_id"

    if (length(study) > 1) {
      stop("Download only possible for a single study")
    }

    sensorTypes <- getMovebank("tag_type", login = login)
    if (length(sensorID) == 0 | length(sensorID[!sensorID %in% sensorTypes$id]) > 0) {
      stop("Sensor name(s) not valid. See 'getMovebankSensors(login)' for valid sensor names")
    }

    if (any(!(as.logical(sensorTypes$is_location_sensor[sensorTypes$id %in% sensorID])))) {
      stop("The selected sensor(s): '", paste0(sensorTypes$name[sensorTypes$id %in% sensorID & sensorTypes$is_location_sensor == "false"], collapse = ", "), "' is a/are non-location sensor(s). Only location data can be downloaded with this function. Use 'getMovebankNonLocationData' to download non-location data.")
    }

    sensorAnim <- getMovebankAnimals(study, login)[c("individual_id", "sensor_type_id")]
    if (length(sensorID[!sensorID %in% unique(sensorAnim$sensor_type_id)]) > 0) {
      NoSens <- as.character(sensorTypes$name[sensorTypes$id %in% sensorID[!sensorID %in% unique(sensorAnim$sensor_type_id)]])
      stop("Sensor(s): '", paste0(NoSens, collapse = ", "), "' is/are not available for this study")
    }

    NoDat <- idData$local_identifier[!unlist(lapply(1:nrow(idData), function(x) {
      is.element(sensorID, sensorAnim$sensor_type_id[sensorAnim$individual_id == idData$individual_id[x]])
    }))]
    if (length(NoDat) > 0) {
      animalName <- animalName[!animalName %in% idData$individual_id[as.character(idData$local_identifier) %in% as.character(NoDat)]]
      if (length(NoDat) <= 90) {
        warning("Individual(s): '", paste0(as.character(NoDat), collapse = ", "), "' do(es) not have data for one or more of the selected sensor(s). Data for this/these individual(s) are not downloaded.")
      } else {
        warning("Individual(s): '", paste0(as.character(NoDat[1:90]), collapse = ", "), "' ... and ", (length(NoDat) - 90), " more (total ", length(NoDat), ") do not have data for one or more of the selected sensor(s). Data for these individuals are not downloaded.")
      }
    }


    LocData <- do.call(
      "rbind", # split and recombine because requested url can get too long
      lapply(split(animalName, ceiling(1:length(animalName) / 200)), function(x, ...) {
        getMovebank("event", login = login, study_id = study, sensor_type_id = sensorID, individual_id = x, attributes = "all", ...)
      }, ...)
    )
    if (nrow(LocData) == 0) { ## when all individuals have 0 data
      stop("This Individual/All Individuals has/have 0 data points for the selected sensor(s).")
    }

    WithData <- unique(LocData$individual_id)
    if (!setequal(animalName, WithData)) { ## when some individuals have 0 data
      withNoData <- idData$local_identifier[!idData$individual_id %in% WithData]
      if (length(withNoData) <= 90) {
        warning("Individual(s): '", paste0(as.character(withNoData), collapse = ", "), "' have 0 data points for one or more of the selected sensor(s).")
      } else {
        warning("Individuals: '", paste0(as.character(withNoData[1:90]), collapse = ", "), "' ... and ", (length(withNoData) - 90), " more (total ", length(withNoData), ") have 0 data points for one or more of the selected sensor(s).")
      }
    }

    attbsFrist <- c("event_id", "visible", "timestamp", "location_long", "location_lat")
    attribsAll <- colnames(LocData)
    attribs <- c(attbsFrist, attribsAll[!attribsAll %in% attbsFrist])
    LocData <- LocData[, attribs]

    LocData$timestamp <- as.POSIXct(strptime(as.character(LocData$timestamp), format = "%Y-%m-%d %H:%M:%OS", tz = "UTC"), tz = "UTC")
    LocData$study_name <- as.character(getMovebankStudy(study, login)$name)
    for (i in unique(LocData$sensor_type_id)) { ## add sensor type with name
      LocData$sensor_type[LocData$sensor_type_id == i] <- as.character(sensorTypes$name[sensorTypes$id == i])
    }

    if (!includeOutliers) { # exclude outliers
      LocDataOutl <- LocData[LocData$visible == "true", ]
      if (underscoreToDots) {
        names(LocDataOutl) <- gsub("_", ".", names(LocDataOutl))
        return(LocDataOutl)
      } else {
        return(LocDataOutl)
      }
    }

    if (includeOutliers) { # include outliers
      if (underscoreToDots) {
        names(LocData) <- gsub("_", ".", names(LocData))
        return(LocData)
      } else {
        return(LocData)
      }
    }
  }
)



## get reference table
setGeneric("getMovebankReferenceTable", function(study, login, allAttributes = FALSE) standardGeneric("getMovebankReferenceTable"))
setMethod(
  f = "getMovebankReferenceTable",
  c(study = "character", login = "MovebankLogin"),
  definition = function(study, login, allAttributes) {
    study <- getMovebankID(study, login)
    callGeneric()
  }
)
setMethod(
  f = "getMovebankReferenceTable",
  c(study = "numeric", login = "MovebankLogin"),
  definition = function(study, login, allAttributes) {
    ## get tags and sensors
    tags <- getMovebank(entity_type = "sensor", login, tag_study_id = study)
    tags$id <- NULL
    ## get tag attributes
    tagNames <- getMovebank(entity_type = "tag", login, study_id = study)
    colnames(tagNames)[colnames(tagNames) %in% colnames(tagNames)[-grep("tag", colnames(tagNames))]] <- paste0("tag_", colnames(tagNames)[-grep("tag", colnames(tagNames))])
    tagAtrb <- merge.data.frame(x = tags, y = tagNames, by = "tag_id", all = T)
    ## get animal attributes
    animalAtrb <- getMovebank("individual", login, study_id = study)
    colnames(animalAtrb) <- paste0("animal_", colnames(animalAtrb))
    ## get link between deployments and tags
    deploymentID <- getMovebank("deployment", login = login, study_id = study, attributes = "individual_id%2Ctag_id%2Cid")
    # names(deploymentID)  <- c("animal_id", "tag_id", "deployment_id")
    names(deploymentID) <- sub("^id$", "deployment_id", names(deploymentID))
    names(deploymentID) <- sub("^individual_id$", "animal_id", names(deploymentID))

    ## get deployment attributes
    deploymentAtrb <- getMovebank("deployment", login = login, study_id = study)
    colnames(deploymentAtrb)[colnames(deploymentAtrb) %in% c("comments", "id", "local_identifier")] <- paste0("deployment_", c("comments", "id", "local_identifier"))
    deploymentAtrbs <- merge.data.frame(x = deploymentAtrb, y = deploymentID, by = "deployment_id", all = T)
    ## create one big table
    tagdep <- merge.data.frame(tagAtrb, deploymentAtrbs, by = "tag_id", all = T)
    animtagdep <- merge.data.frame(animalAtrb, tagdep, by = "animal_id", all = T)
    ## showing only number of events for location sensors
    colnames(animtagdep)[which(names(animtagdep) == "animal_number_of_events")] <- "number_of_location_events"
    Sens <- getMovebankSensors(login = login)[c("id", "is_location_sensor")]
    nonLocSens <- Sens$id[Sens$is_location_sensor == "false"]
    animtagdep$number_of_location_events[animtagdep$sensor_type_id %in% nonLocSens] <- NA

    if (nrow(animtagdep) == 0) {
      stop("Reference data are not available: maybe you have no permission to download this dataset, or maybe password is invalid.")
    }

    if (study == "character") {
      animtagdep$study_id <- getMovebankID(study, login)
    } else {
      animtagdep$study_id <- study
    }

    RefData <- animtagdep[, c("animal_local_identifier", "tag_local_identifier", "sensor_type_id", names(animtagdep)[!names(animtagdep) %in% c("animal_local_identifier", "tag_local_identifier", "sensor_type_id")])]
    if (allAttributes) {
      return(RefData)
    } else {
      RefDataRed <- RefData[, colSums(is.na(RefData)) != nrow(RefData)]
      return(RefDataRed)
    }
  }
)

setMethod(
  f = "getMovebankReferenceTable",
  c(study = "ANY", login = "missing"),
  definition = function(study, login, allAttributes) {
    login <- movebankLogin()
    getMovebankReferenceTable(study = study, login = login, allAttributes = FALSE)
  }
)
