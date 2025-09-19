###############################
# Utility functions 
# Mathieu Fortin - March 2020
###############################


removeAllExcept <- function(except="", includeFunctions = F) {
  allObjects <- ls(envir = globalenv())
  if (includeFunctions) {
    objectsToBeConsidered <- allObjects    
  } else {
    objectsToBeConsidered <- c()
    for (obj in allObjects) {
      if (!is.function(get(obj, envir = globalenv()))) {
        objectsToBeConsidered <- c(objectsToBeConsidered, obj)
      }
    }
  }
  rm(list = objectsToBeConsidered[which(!objectsToBeConsidered %in% except)], envir = globalenv())
}

replaceFieldName <- function(dataFrame, index, newName) {
  fieldNames <- colnames(dataFrame)
  fieldNames[index] <- newName
  colnames(dataFrame) <- fieldNames
  return(dataFrame)
}


setLastValueOf <- function(dataSet, fieldName, newFieldName, plotIndexField, measureIndexField) {
  tmp <- extractLastValue(dataSet, fieldName, newFieldName, plotIndexField, measureIndexField)
  indexPlots <<- merge(indexPlots, tmp[,c(plotIndexField, newFieldName)], all.x = T, by=c(plotIndexField))
}

extractLastValue <- function(dataSet, fieldName, newFieldName, plotIndexField, measureIndexField) {
  vector <- dataSet[, fieldName]
#  data <- dataSet[!is.na(vector), c(fieldName, plotIndexField, measureIndexField)]  ### former version that did not account for field set to ""
  data <- dataSet[which(!is.na(vector) & vector != ""), c(fieldName, plotIndexField, measureIndexField)]   
  lastMeasure <- aggregate(formula(paste(measureIndexField, "~", plotIndexField)), data = data, FUN="max")
  data <- merge(data, lastMeasure, by=c(plotIndexField, measureIndexField))
  data <- replaceFieldName(data, length(data[1,]), newFieldName)
  return(data)
}

createDummyNonMissingValues <- function(dataSet, vectorFieldNames) {
  for (index in 1:length(vectorFieldNames)) {
    value_i <- as.numeric(!is.na(dataSet[,vectorFieldNames[index]]))    
    if (index == 1) {
      value <- value_i
    } else {
      value <- value * value_i
    }
  }
  dataSet$nonMissingValues <- as.logical(value)
  return(dataSet)
}

compareTwoDataFrame <- function(dataFrameNew, dataFrameRef) {
  index <- which(!colnames(dataFrameNew) %in% colnames(dataFrameRef))
  if (length(index) > 0) {
    newFields <- colnames(dataFrameNew)[index]
    print(paste("Warning: the new file contains additional fields : ", paste(newFields, collapse = ",")))
  }
  index <- which(!colnames(dataFrameRef) %in% colnames(dataFrameNew)) 
  if (length(index) > 0) {
    print(index)
    stop(paste("Some fields of the previous file are missing in the new one:", colnames(dataFrameRef)[index]))
  } else {
    for (field in colnames(dataFrameRef)) {
      vec1 <- dataFrameNew[,field]
      vec2 <- dataFrameRef[,field]
      index <- which(is.na(vec1) != is.na(vec2))
      index <- c(index, which(vec1 != vec2))
      if (length(index) > 0) {
        print(field)
        print(index)
        stop("Differences detected in this field")
      }
    }
  }
  return("Ok! No difference")
}

removeTheseFields <- function(dataSet, fieldsToBeRemoved) {
  currentFields <- colnames(dataSet)
  fieldsToKeep <- currentFields[which(!currentFields %in% fieldsToBeRemoved)]
  output <- dataSet[, fieldsToKeep]
  return(output)
}


