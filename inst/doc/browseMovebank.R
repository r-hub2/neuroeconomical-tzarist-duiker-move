## ----echo=FALSE, include=FALSE------------------------------------------------
knitr::opts_chunk$set(collapse = TRUE)

## ----eval=F-------------------------------------------------------------------
#  library("move")
#  loginStored <- movebankLogin(username="user", password="password")

## ----eval=F-------------------------------------------------------------------
#  searchMovebankStudies(x="oose", login=loginStored)

## ----eval=F-------------------------------------------------------------------
#  getMovebankID("Ocelots on Barro Colorado Island, Panama",login=loginStored)

## ----eval=F-------------------------------------------------------------------
#  getMovebankStudy(study="Ocelots on Barro Colorado Island, Panama",login=loginStored)

## ----eval=F-------------------------------------------------------------------
#  getMovebankSensors(study="Ocelots on Barro Colorado Island, Panama",login=loginStored)

## ----eval=F-------------------------------------------------------------------
#  getMovebankSensors(login=loginStored)

## ----eval=F-------------------------------------------------------------------
#  getMovebankSensorsAttributes(study="Ocelots on Barro Colorado Island, Panama",login=loginStored)

## ----eval=F-------------------------------------------------------------------
#  getMovebankAnimals(study="Ocelots on Barro Colorado Island, Panama",login=loginStored)

## ----eval=F-------------------------------------------------------------------
#  getMovebankReferenceTable(study="Ocelots on Barro Colorado Island, Panama",login=loginStored)

## ----eval=F-------------------------------------------------------------------
#  bci_ocelot <- getMovebankData(study="Ocelots on Barro Colorado Island, Panama", login=loginStored)

## ----eval=F-------------------------------------------------------------------
#  # for one individual
#  bobby <- getMovebankData(study="Ocelots on Barro Colorado Island, Panama", animalName="Bobby", login=loginStored)

## ----eval=F-------------------------------------------------------------------
#  # for several individuals
#  ocelot2ind <- getMovebankData(study="Ocelots on Barro Colorado Island, Panama", animalName=c("Bobby","Darlen"), login=loginStored)

## ----eval=F-------------------------------------------------------------------
#  # download all data between "2003-03-22 17:44:00.000" and "2003-04-22 17:44:00.000"
#  bci_ocelot_range1 <- getMovebankData(study="Ocelots on Barro Colorado Island, Panama", login=loginStored,
#                                       timestamp_start="20030322174400000",
#                                       timestamp_end="20030422174400000")
#  
#  # alternative:
#  t <- strptime("20030322174400",format="%Y%m%d%H%M%S", tz='UTC')
#  bci_ocelot_ranget <- getMovebankData(study="Ocelots on Barro Colorado Island, Panama", login=loginStored,
#                                       timestamp_start=t,
#                                       timestamp_end=t+as.difftime(31,units='days'))

## ----eval=F-------------------------------------------------------------------
#  # download all data before "2003-07-24 20:00:00.000"
#  bci_ocelot_range2 <- getMovebankData(study="Ocelots on Barro Colorado Island, Panama", login=loginStored,
#                                       timestamp_end="20030724200000000")
#  

## ----eval=F-------------------------------------------------------------------
#  # download all data after "2003-07-01 20:00:00.000" only for "Bobby"
#  bobby_range <- getMovebankData(study="Ocelots on Barro Colorado Island, Panama", login=loginStored, animalName="Bobby",
#                                 timestamp_start="20030701200000000")

## ----eval=F-------------------------------------------------------------------
#  bci_ocelot <- getMovebankData(study="Ocelots on Barro Colorado Island, Panama", login=loginStored,
#                                removeDuplicatedTimestamps=TRUE)

## ----eval=F-------------------------------------------------------------------
#  ## fist get the animal names of the study
#  animalDF <- getMovebankAnimals(study="Ocelots on Barro Colorado Island, Panama",login=loginStored)
#  animalNames <- unique(animalDF$local_identifier[animalDF$number_of_events>0]) ## to make sure only to include the animals that actually have locations
#  
#  ## if one is sure that all individuals in the study have locations, this is a shorter way to go
#  # animalNames <- unique(getMovebankAnimals(study="Ocelots on Barro Colorado Island, Panama",login=loginStored)$local_identifier)
#  
#  ## OPTION 1: create a loop to download each individual and afterwards create a MoveStack (if study is very large, maybe option 2 is better)
#  animalList <- lapply(animalNames, function(x){
#    print(paste0(x," (",match(x,animalNames), " of ", length(animalNames),")"))
#    getMovebankData(study="Ocelots on Barro Colorado Island, Panama", animalName=x, login=loginStored, removeDuplicatedTimestamps=T)
#  })
#  ocelotsMS <- moveStack(animalList, forceTz="UTC")
#  
#  ## OPTION 2: if the study is very large, loading and handling the large moveStack might be very time consuming and somewhat frustrating. Therefore it might be a good idea to save each individual separately as e.g. a .RData file, and do subsequent analysis always looping through all the single individual files
#  animalList <- lapply(animalNames, function(x){
#    print(paste0(x," (",match(x,animalNames), " of ", length(animalNames),")"))
#    ocelot <- getMovebankData(study="Ocelots on Barro Colorado Island, Panama", animalName=x, login=loginStored, removeDuplicatedTimestamps=T)
#    save(file=paste0("/path/to/my/folder/OcelotsIndv/",x,".RData"), ocelot)
#  })

## ----eval=F-------------------------------------------------------------------
#  getMovebankLocationData(study=74496970 , sensorID="GPS",
#                             animalName="DER AR439", login=loginStored)

## ----eval=F-------------------------------------------------------------------
#  # get acceleration data for all individuals of the study between the "2013-08-15 15:00:00.000" and "2013-08-15 15:01:00.000"
#  getMovebankLocationData(study=74496970 , sensorID=653, login=loginStored,
#                             timestamp_start="20130815150000000",
#                             timestamp_end="20130815150100000")

## ----eval=F-------------------------------------------------------------------
#  ## fist get the animal names of the study
#  animalNames <- unique(getMovebankAnimals(study=74496970,login=loginStored)$local_identifier)
#  
#  ## OPTION 1: create a loop to download each individual and afterwards rbind into one large data.frame (if study is very large, maybe option 2 is better). Use the "TryCatch" function in case there are individuals with no data.
#  animalList <- lapply(animalNames, function(x){
#    print(paste0(x," (",match(x,animalNames), " of ", length(animalNames),")"))
#    tryCatch(getMovebankLocationData(study=74496970, animalName=x, sensorID="GPS", login=loginStored), error=function(e) NULL)
#  })
#  storksDF <- do.call("rbind", animalList)
#  
#  ## OPTION 2: if the study is very large, loading and handling the large data.frame might be very time consuming and somewhat frustrating. Therefore it might be a good idea to save each individual separately as e.g. a .RData file, and do subsequent analysis always looping through all the single individual files. Use the "TryCatch" function in case there are individuals with no data.
#  animalList <- lapply(animalNames, function(x){
#    print(paste0(x," (",match(x,animalNames), " of ", length(animalNames),")"))
#    tryCatch({storkDF <- getMovebankLocationData(study=74496970, animalName=x, sensorID="GPS", login=loginStored)
#   save(file=paste0("/path/to/my/folder/StorkIndv/",x,".RData"), storkDF)
#   }, error=function(e) NULL)
#  })

## ----eval=F-------------------------------------------------------------------
#  getMovebankNonLocationData(study=74496970 , sensorID="Acceleration",
#                             animalName="DER AR439", login=loginStored)

## ----eval=F-------------------------------------------------------------------
#  # get acceleration data for all individuals of the study between the "2013-08-15 15:00:00.000" and "2013-08-15 15:01:00.000"
#  getMovebankNonLocationData(study=74496970 , sensorID=2365683, login=loginStored,
#                             timestamp_start="20130815150000000",
#                             timestamp_end="20130815150100000")

## ----eval=F-------------------------------------------------------------------
#  mymove <- getMovebankData(study=74496970, login=loginStored,
#                            animalName="DER AR439",includeExtraSensors=TRUE)

## ----eval=F-------------------------------------------------------------------
#  ## to get a data.frame containing the data for the non-location sensors use the "unUsedRecords" function
#  nonlocation <- as.data.frame(unUsedRecords(mymove))

## ----eval=F-------------------------------------------------------------------
#  ## fist get the animal names of the study
#  animalNames <- unique(getMovebankAnimals(study=74496970,login=loginStored)$local_identifier)
#  
#  ## OPTION 1: create a loop to download each individual and afterwards rbind into one large data.frame (if study is very large, maybe option 2 is better). Use the "TryCatch" function in case there are individuals with no data.
#  animalList <- lapply(animalNames, function(x){
#    print(paste0(x," (",match(x,animalNames), " of ", length(animalNames),")"))
#    tryCatch(getMovebankNonLocationData(study=74496970, animalName=x, sensorID=2365683, login=loginStored), error=function(e) NULL)
#  })
#  storksACC <- do.call("rbind", animalList)
#  
#  ## OPTION 2: if the study is very large, loading and handling the large data.frame might be very time consuming and somewhat frustrating. Therefore it might be a good idea to save each individual separately as e.g. a .RData file, and do subsequent analysis always looping through all the single individual files. Use the "TryCatch" function in case there are individuals with no data.
#  animalList <- lapply(animalNames, function(x){
#    print(paste0(x," (",match(x,animalNames), " of ", length(animalNames),")"))
#    tryCatch({storkACC <- getMovebankNonLocationData(study=74496970, animalName=x, sensorID=2365683, login=loginStored)
#   save(file=paste0("/path/to/my/folder/StorkIndv/",x,".RData"), storkACC)
#   }, error=function(e) NULL)
#  })

## ----eval=F-------------------------------------------------------------------
#  getDataRepositoryData("doi:10.5441/001/1.2k536j54")

