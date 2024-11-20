
setAs(".MoveTrack", "data.frame", function(from) {
  return(data.frame(
    data.frame(from),
    sensor = from@sensor, timestamps = from@timestamps
  ))
})
setAs(".MoveTrackSingle", "data.frame", function(from) {
  return(data.frame(as(as(from, ".MoveTrack"), "data.frame"), from@idData[rep(1, n.locs(from)), ]))
}) # rep the idDate because it causes trouble with a difftime object
setAs(".MoveTrackSingleBurst", "data.frame", function(from) {
  t <- burstId(from)
  t[n.locs(from)] <- NA
  return(data.frame(as(as(from, ".MoveTrack"), "data.frame"), burstId = t))
})
setAs(".MoveTrackStack", "data.frame", function(from) {
  return(data.frame(as(as(from, ".MoveTrack"), "data.frame"), trackId = from@trackId, from@idData[as.character(from@trackId), ]))
})
setAs("dBMvarianceStack", "data.frame", function(from) {
  return(data.frame(as(as(from, ".MoveTrackStack"), "data.frame"), as(as(from, "dBMvarianceTmp"), "data.frame")))
})
setAs("dBMvariance", "data.frame", function(from) {
  return(data.frame(as(as(from, ".MoveTrack"), "data.frame"), as(as(from, "dBMvarianceTmp"), "data.frame")))
})
setAs("dBMvarianceTmp", "data.frame", function(from) {
  data.frame(window.size = from@window.size, margin = from@margin, means = from@means, in.windows = from@in.windows, interest = from@interest)
})


setAs(".unUsedRecordsStack", "data.frame", function(from) {
  return(data.frame(
    as(as(from, ".unUsedRecords"), "data.frame"),
    trackId = from@trackIdUnUsedRecords
  ))
})
setAs(".unUsedRecords", "data.frame", function(from) {
  return(
    data.frame(
      from@dataUnUsedRecords,
      sensor = from@sensorUnUsedRecords, timestamps = from@timestampsUnUsedRecords
    )
  )
})
setMethod(
  "as.data.frame", ".unUsedRecords",
  function(x) {
    as(x, "data.frame")
  }
)
setMethod(
  "as.data.frame", ".unUsedRecordsStack",
  function(x) {
    as(x, "data.frame")
  }
)
setMethod(
  "as.data.frame", "dBMvariance",
  function(x) {
    as(x, "data.frame")
  }
)
setMethod(
  "as.data.frame", "dBMvarianceStack",
  function(x) {
    as(x, "data.frame")
  }
)
setMethod(
  "as.data.frame", "Move",
  function(x) {
    as(x, "data.frame")
  }
)
setMethod(
  "as.data.frame", "MoveStack",
  function(x) {
    as(x, "data.frame")
  }
)
setMethod(
  "as.data.frame", "MoveBurst",
  function(x) {
    as(x, "data.frame")
  }
)
