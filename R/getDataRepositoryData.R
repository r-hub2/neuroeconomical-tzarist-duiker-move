# require(move)
if (F) {
  require(move)
  require(xml2)

  require(httr)
  x <- "10.5441/001/1.3gc013f3"
  x <- "doi:10.5441/001/1.2k536j54" # has extra diag
  x <- "doi:10.5441/001/1.tc76g560" # has argos and csv & dups
  x <- "doi:10.5441/001/1.f3550b4f"
  x <- "doi:10.5441/001/1.ct8sk835"
  x <- "doi:10.5441/001/1.rt00m81v"
  x <- "doi:10.5441/001/1.kn0816jn" # baboons need unziping
  x <- "doi:10.5441/001/1.s528h83q"
  x <- "https://www.datarepository.movebank.org/handle/10255/move.523"
  x <- "doi:10.5441/001/1.245kb7r6"
  x <- "doi:10.5441/001/1.12hv60k6"
  x <- "https://www.datarepository.movebank.org/handle/10255/move.612"
  (x <- movebankDois[19])
  x <- "10.5441/001/1.35fs26kq"
  x <- "10.5441/001/1.32m2335q"
  x <- "10.5441/001/1.mf903197"
  x <- "10.5441/001/1.ct8sk835"
  x <- "10.5441/001/1.tc76g560"
  x <- "10.5441/001/1.245kb7r6"
  x <- "10.5441/001/1.5k6b1364"
  x <- "10.5441/001/1.k1fm26kh"
  x <- "10.5441/001/1.32m2335q"
  getDataRepositoryData(x)
  x <- "10.5441/001/1.4rr97k10"
  x <- "10.5441/001/1.1rc3hj8d"
  x <- "10.5441/001/1.q206rm6b"
  x <- "10.5441/001/1.k8s2g5v7"
}
# does not work :  "doi:10.5441/001/1.pv048q7v" Since there are multiple deployments
# no loc data "doi:10.5441/001/1.874gb577"
metaDataFromDoi <- function(x) {
  x <- sub("^10.5441/001/", "doi:10.5441/001/", as.character(x))
  res1 <- HEAD(sub("^doi:", "dx.doi.org/", as.character(x)))
  u <- res1$url
  message(u)
  GET(sub("handle", "metadata/handle", paste0(u, "/mets.xml")))
}

setGeneric("getDataRepositoryData", function(x, ...) {
  standardGeneric("getDataRepositoryData")
})
setMethod(
  f = "getDataRepositoryData",
  signature = c(x = "character"),
  definition =
    function(x, ...) {
      .Defunct(msg="As the movebank data repository changed this function not working. This functionality won't be restored. Similar functionality might be provided in `move2`, please make an issue there with your requirements (https://gitlab.com/bartk/move2/-/issues).")
      stopifnot(length(x) == 1)
      metaData <- metaDataFromDoi(x)
      replace <- as.character(xml_contents(
        xml_find_all(
          content(metaData),
          "//dim:field[@element='relation' and @qualifier='isreplacedby']"
        )
      ))

      if (length(replace) != 0) {
        warning("Data is replaced now returning: ", replace)
        return(getDataRepositoryData(replace))
      }
      replace <- as.character(xml_contents(
        xml_find_all(
          content(metaData),
          "//dim:field[@element='relation' and @qualifier='ispartof']"
        )
      ))
      if (length(replace) != 0) {
        message("Data is part of other dataset now returning: ", replace)
        return(getDataRepositoryData(replace))
      }
      cont <- as.character(xml_contents(
        xml_find_all(
          content(metaData),
          "//dim:field[@element='relation' and @qualifier='iscontinuedby']"
        )
      ))
      if (length(cont) != 0) {
        message("Note: This dataset is continued by DOI: ", cont, " You might want to consider using this")
      }
      der <- as.character(xml_contents(
        xml_find_all(
          content(metaData),
          "//dim:field[@element='relation' and @qualifier='isderivedfrom']"
        )
      ))
      if (length(der) != 0) {
        message("Note: This dataset is derived from or overlapping with DOI: ", der)
      }
      subfiles <-
        xml_contents(
          xml_find_all(
            content(metaData),
            "//dim:field[@element='relation' and @qualifier='haspart']"
          )
        )
      cit <- paste(collapse = "\\n", as.character(xml_contents(
        xml_find_all(
          content(metaData),
          "//dim:field[@element='identifier' and @qualifier='citation']"
        )
      )))
      fileMetaData <- lapply(subfiles, metaDataFromDoi)
      rights <-
        unique(unlist(lapply(
          lapply(
            lapply(
              lapply(fileMetaData, content),
              xml_find_all,
              "//dim:field[@element='rights']"
            ),
            xml_contents
          ), as.character
        )))
      fileList <-
        unlist(
          lapply(
            lapply(fileMetaData, content),
            xml_find_all,
            "//mets:fileGrp[@USE='CONTENT']/mets:file"
          ),
          recursive = F
        )
      fileList <-
        fileList[!duplicated(lapply(fileList, xml_attr, "CHECKSUM"))]
      fileLoc <- lapply(fileList, xml_find_all, ".//mets:FLocat")
      fileName <- unlist(lapply(fileLoc, xml_attr, "title"))
      fileClass <- unlist(lapply(fileLoc, xml_attr, "label"))
      fileUrl <- unlist(lapply(fileLoc, xml_attr, "href"))
      fileUrl <-
        unlist(lapply(fileUrl, function(url, x) {
          modify_url(url, path = x)
        }, url = parse_url(metaData$url)))
      fileMime <- unlist(lapply(fileList, xml_attr, "MIMETYPE"))
      guessedCSV <-
        fileMime == "application/octet-stream" &
          fileClass == "dataset-file" &
          !(
            fileName %in% c(
              "Re-colonization by common eiders (Somateria mollissima) in the Aleutian Archipelago (data from Petersen et al. 2015)-bird-measurements.csv",
              "Distribution of spectacled eiders in Russia and Alaska 1993-1996 (data from Petersen et al. 1999)-sensor-messages.csv",
              "Petersen_etal_2003_Wildfowl_LocationData.csv",
              "mangabey-plants.csv",
              "Post-fledging movements of common mergansers (Mergus merganser) in Alaska (data from Pearce and Petersen 2009)-sensor-messages.csv",
              "mangabey-notes.txt",
              "rivers.zip",
              "pictures.zip",
              "study site habitat data.csv"
            )
          ) &
          !grepl("\\.dia$", fileName) &
          !grepl("\\.ds$", fileName) &
          !grepl("^mangabey\\-.*\\-data\\-original.csv$", fileName) # all files that should not be considered
      if (sum(!guessedCSV) != 1) {
        stopifnot(sum(!guessedCSV) > 1 | all(grepl("^Oilbird", fileName)))
        s <- fileName[!guessedCSV]
        s <- s[s != "README.txt"]
        message("Non csv file(s) found (not included in data): ", paste0('"', s, '"', collapse = " "))
      }
      stopifnot(sum(
        refDataSel <-
          grepl("reference.data\\.csv$|metadata\\.csv$", fileName[guessedCSV]) | grepl("-reference-data V2\\.csv$", fileName[guessedCSV])
      ) == 1)
      data <- lapply(fileUrl[guessedCSV], function(x) {
        if (grepl("\\.csv\\.zip", x)) {
          # extract csv from zip file (atleast for baboon dataset
          GET(x, write_disk(tf <- tempfile(fileext = ".csv.zip")))
          files <- as.character(unzip(tf, list = T)$Name)
          stopifnot(sum(s <- dirname(files) == "." & grepl("\\.csv$", files)) == 1)
          read.csv(unz(tf, files[s]))
        } else {
          read.csv(textConnection(content(GET(x), as = "text", encoding = "ISO-8859-1"))) # can't use read_csv unless specifying colums problem with individ id that have mix of number and char
        }
      })
      data <- lapply(data, function(df) {
        if ("timestamp" %in% colnames(df)) {
          df$timestamp <-
            as.POSIXct(strptime(
              as.character(df$timestamp),
              format = "%Y-%m-%d %H:%M:%OS",
              tz = "UTC"
            ),
            tz = "UTC"
            )
        }
        return(df)
      })
      refData <- data[refDataSel][[1]]
      if (any(duplicated(refData$"animal.id"))) {
        stop("Currently multiple deployments are not implemented")
      }
      stopifnot(!any(is.na(refData$animal.id)))
      locData <- data[!refDataSel]
      # combine all loc data in one df
      uniqCols <- unique(unlist(lapply(locData, colnames)))
      naFilledCols <- mapply(
        function(x, i) {
          x[, i] <-
            NA
          return(x)
        },
        i = lapply(lapply(
          lapply(lapply(locData, colnames), "%in%", x = uniqCols), "!"
        ), function(x, i) {
          x[i]
        }, x = uniqCols),
        x = locData,
        SIMPLIFY = F
      )
      # can happen when zip files are loaded that posixct and factors are combined
      toChar <-
        names(which(apply(do.call(cbind, lapply(
          lapply(lapply(
            lapply(naFilledCols, lapply, class), lapply, head, 1
          ), function(x, i) {
            x[i]
          }, i = uniqCols), unlist
        )), 1, function(x) {
          all(c("factor", "integer") %in% x) |
            all(c("factor", "POSIXct") %in% x)
        })))
      naFilledCols <-
        lapply(naFilledCols, function(x, i) {
          for (j in i) {
            x[, j] <- as.character(x[, j])
          }
          return(x)
        }, i = toChar)
      allLocData <-
        do.call("rbind", naFilledCols)
      rm(naFilledCols)
      if (!all(c("location.lat", "location.long") %in% colnames(allLocData))) {
        stop("No location data present")
      }
      if (!("sensor.type" %in% colnames(allLocData))) {
        allLocData$sensor.type <- "unknown"
      }

      if (any(s <- ("" == (allLocData$individual.local.identifier)))) {
        warning(sum(s), " Records were omited because there is no individual local identifier")
        allLocData <- allLocData[!s, ]
      } # Orinoco dataset
      allLocData <-
        allLocData[order(
          as.character(allLocData$individual.local.identifier),
          allLocData$timestamp
        ), ]
      # make idData
      uniqs <-
        lapply(lapply(lapply(
          lapply(
            lapply(
              allLocData,
              split,
              allLocData$`individual.local.identifier`
            ),
            lapply,
            duplicated
          ), lapply, tail, -1
        ), unlist), all)
      toMove <- names(uniqs)[unlist(uniqs)]
      toMove <-
        toMove[!(toMove %in% c("sensor.type", "individual.local.identifier", "visible", "location.lat", "location.long"))]
      idData <-
        merge(refData, allLocData[!duplicated(allLocData$`individual.local.identifier`), c("individual.local.identifier", toMove),
          drop =
            F
        ], by.x = "animal.id", by.y = "individual.local.identifier")
      idData <- idData[order(as.character(idData$animal.id)), ]
      rownames(idData) <- validNames(idData$animal.id) # first order like allLocData

      # need to do valid names after ordering to stay the same as main data
      dups <- getDuplicatedTimestamps(allLocData, onlyVisible = T)
      if (!is.null(dups)) {
        toOmit <- unlist(lapply(dups, tail, -1))
        message(
          "Duplicated locations found on ",
          length(dups),
          " occassions, those are omited, consider using a more informed approach then using the first one."
        )
        outliers <- 1:nrow(allLocData) %in% toOmit
      } else {
        outliers <- F
      }
      outliers <-
        is.na(allLocData$location.lat) |
          is.na(allLocData$location.long) | outliers
      if ("visible" %in% colnames(allLocData)) {
        outliers <- allLocData$visible == "false" | outliers
      }
      ids <- lapply(split(allLocData$individual.local.identifier, outliers), unique)
      outlierIds <- ids[["TRUE"]][!(ids[["TRUE"]] %in% ids[["FALSE"]])]
      if (length(outlierIds) != 0) {
        warning(length(outlierIds), " individual omitted since there is only outlier data")
        outliers <- outliers[!(allLocData$individual.local.identifier %in% outlierIds)]
        allLocData <- allLocData[!(allLocData$individual.local.identifier %in% outlierIds), ]
        idData <- idData[!(idData$animal.id %in% outlierIds), ]
      }
      idData$animal.id <- NULL

      individual.local.identifier <-
        factor(as.character(allLocData$individual.local.identifier))
      levels(individual.local.identifier) <-
        validNames(levels(individual.local.identifier))
      sensor.type <- factor(allLocData$sensor.type)
      timestamp <- allLocData$timestamp
      crds <- allLocData[, c("location.long", "location.lat")]
      allLocData[, c(
        toMove,
        "sensor.type",
        "timestamp",
        "location.long",
        "location.lat",
        "individual.local.identifier"
      )] <- NULL

      unUsed <-
        new(
          ".unUsedRecordsStack",
          trackIdUnUsedRecords = individual.local.identifier[outliers],
          sensorUnUsedRecords = sensor.type[outliers],
          dataUnUsedRecords = allLocData[outliers, ],
          timestampsUnUsedRecords = timestamp[outliers]
        )
      spdf <-
        SpatialPointsDataFrame(crds[!outliers, ],
          allLocData[!outliers, ],
          proj4string = CRS("+proj=longlat +datum=WGS84")
        )
      m <- new(
        "MoveStack",
        spdf,
        license = rights,
        unUsed,
        timestamps = timestamp[!outliers],
        trackId = individual.local.identifier[!outliers],
        idData = idData,
        sensor = sensor.type[!outliers]
      )
      citations(m) <- cit
      return(m)
    }
)
