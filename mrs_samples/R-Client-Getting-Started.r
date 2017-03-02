# Getting started with R Client
# illustrates using rx functions with .xdf files

# Example from:
# https://msdn.microsoft.com/en-us/microsoft-r/r-client-get-started

# see website for full R Client installation instructions
# and code documentation

github <- "https://raw.githubusercontent.com/brohrer-ms/RTVS-docs/master/examples/MRS_and_Machine_Learning/Datasets/"
inputFileFlightURL <- paste0(github, "Flight_Delays_Sample.csv")
inputFileWeatherURL <- paste0(github, "Weather_Sample.csv")

td <- tempdir()
outFileFlight <- paste0(td, "/flight.xdf")
outFileWeather <- paste0(td, "/weather.xdf")
outFileOrigin <- paste0(td, "/originData.xdf")
outFileDest <- paste0(td, "/destData.xdf")
outFileFinal <- paste0(td, "/finalData.xdf")

flight_mrs <- rxImport(
  inData = inputFileFlightURL, outFile = outFileFlight,
  missingValueString = "M", stringsAsFactors = FALSE,
  # Remove columns that are possible target leakers from the flight data.
  varsToDrop = c("DepDelay", "DepDel15", "ArrDelay", "Cancelled", "Year"),
  # Define "Carrier" as categorical.
  colInfo = list(Carrier = list(type = "factor")),
  # Round down scheduled departure time to full hour.
  transforms = list(CRSDepTime = floor(CRSDepTime/100)),  
  overwrite = TRUE
)

head(flight_mrs)
rxSummary(~., data = flight_mrs, blocksPerRead = 2)

xform <- function(dataList) {
  # Create a function to normalize some numerical features.
  featureNames <- c(
    "Visibility", 
    "DryBulbCelsius", 
    "DewPointCelsius", 
    "RelativeHumidity", 
    "WindSpeed", 
    "Altimeter"
  )
  dataList[featureNames] <- lapply(dataList[featureNames], scale)
  return(dataList)
}

weather_mrs <- rxImport(
  inData = inputFileWeatherURL, outFile = outFileWeather,
  missingValueString = "M", stringsAsFactors = FALSE,
  # Eliminate some features due to redundance.
  varsToDrop = c("Year", "Timezone", 
                 "DryBulbFarenheit", "DewPointFarenheit"),
  # Create a new column "DestAirportID" in weather data.
  transforms = list(DestAirportID = AirportID),
  # Apply the normalization function.
  transformFunc = xform,  
  transformVars = c(
    "Visibility", 
    "DryBulbCelsius", 
    "DewPointCelsius", 
    "RelativeHumidity", 
    "WindSpeed", 
    "Altimeter"
  ),
  overwrite = TRUE
)

newVarInfo <- list(
  AdjustedMonth = list(newName = "Month"),
  AdjustedDay = list(newName = "DayofMonth"),
  AirportID = list(newName = "OriginAirportID"),
  AdjustedHour = list(newName = "CRSDepTime")
)
rxSetVarInfo(varInfo = newVarInfo, data = weather_mrs)
rxGetInfo(flight_mrs, getVarInfo = TRUE)

originData_mrs <- rxMerge(
  inData1 = flight_mrs, inData2 = weather_mrs, outFile = outFileOrigin,
  type = "inner", autoSort = TRUE, 
  matchVars = c("Month", "DayofMonth", "OriginAirportID", "CRSDepTime"),
  varsToDrop2 = "DestAirportID",
  overwrite = TRUE
)

rxGetInfo(originData_mrs, getVarInfo = TRUE)

destData_mrs <- rxMerge(
  inData1 = originData_mrs, inData2 = weather_mrs, outFile = outFileDest,
  type = "inner", autoSort = TRUE, 
  matchVars = c("Month", "DayofMonth", "DestAirportID", "CRSDepTime"),
  varsToDrop2 = c("OriginAirportID"),
  duplicateVarExt = c("Origin", "Destination"),
  overwrite = TRUE
)

rxFactors(inData = destData_mrs, outFile = outFileFinal, sortLevels = TRUE,
          factorInfo = c("OriginAirportID", "DestAirportID"),
          overwrite = TRUE)

rxSplit(inData = outFileFinal,
        outFilesBase = paste0(td, "/modelData"),
        outFileSuffixes = c("Train", "Test"),
        splitByFactor = "splitVar",
        overwrite = TRUE,
        transforms = list(
          splitVar = factor(sample(c("Train", "Test"),
                                   size = .rxNumRows,
                                   replace = TRUE,
                                   prob = c(.80, .20)),
                            levels = c("Train", "Test"))),
        rngSeed = 17,
        consoleOutput = TRUE)

train <- RxXdfData(paste0(td, "/modelData.splitVar.Train.xdf"))
test <- RxXdfData(paste0(td, "/modelData.splitVar.Test.xdf"))


# Build the formula.
modelFormula <- formula(train, depVars = "ArrDel15",
                        varsToDrop = c("RowNum", "splitVar"))

# Fit a Logistic Regression model.
logitModel_mrs <- rxLogit(modelFormula, data = train)

# Review the model results.
summary(logitModel_mrs)

# Predict the probability on the test dataset.
myPredictions <- rxPredict(logitModel_mrs, data = test,
          type = "response",
          predVarNames = "ArrDel15_Pred_Logit",
          overwrite = TRUE)

rxGetInfo(tmp, getVarInfo = TRUE)

# Calculate Area Under the Curve (AUC).
paste0("AUC of Logistic Regression Model:",
       rxAuc(rxRoc("ArrDel15", "ArrDel15_Pred_Logit", test)))

# Plot the ROC curve.
rxRocCurve("ArrDel15", "ArrDel15_Pred_Logit", data = test,
           title = "ROC curve - Logistic regression")

# Build a decision tree model.
dTree1_mrs <- rxDTree(modelFormula, data = test, reportProgress = 1)

# Find the Best Value of cp for Pruning rxDTree Object.
treeCp_mrs <- rxDTreeBestCp(dTree1_mrs)

# Prune a decision tree created by rxDTree and return the smaller tree.
dTree2_mrs <- prune.rxDTree(dTree1_mrs, cp = treeCp_mrs)

# Predict the probability on the test dataset.
rxPredict(dTree2_mrs, data = test,
          predVarNames = "ArrDel15_Pred_Tree",
          overwrite = TRUE)

# Calculate Area Under the Curve (AUC).
paste0("AUC of Decision Tree Model:",
       rxAuc(rxRoc(" ArrDel15 ", " ArrDel15_Pred_Tree ", test)))

# Plot the ROC curve.
rxRocCurve("ArrDel15",
           predVarNames = c("ArrDel15_Pred_Tree", "ArrDel15_Pred_Logit"),
           data = test,
           title = "ROC curve - Logistic regression")

# write results to local CSV
outCSV <- RxTextData(file = "C:/model_results.csv")
rxDataStep(inData  = myPredictions,
           outFile = outCSV,
           overwrite = TRUE)
