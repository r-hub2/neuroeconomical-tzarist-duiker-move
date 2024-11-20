setGeneric("licenseTerms", function(obj) standardGeneric("licenseTerms"))
setMethod("licenseTerms", ".MoveGeneral", function(obj) {
  return(obj@license)
})

setGeneric("licenseTerms<-", function(obj, value) standardGeneric("licenseTerms<-"))
setReplaceMethod("licenseTerms", ".MoveGeneral", function(obj, value) {
  if (length(value) != 1) {
    value <- as.character(value[1])
    warning("There was more than one license entered. Only using first element")
  }
  slot(obj, "license", check = T) <- value
  obj
})
