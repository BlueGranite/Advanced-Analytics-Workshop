
# reference <https://msdn.microsoft.com/en-us/microsoft-r/scaler-spark-getting-started>

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# define compute context settings
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# if logged directly into edge node (i.e. RStudio Server on edge node)
myHadoopCluster <- RxSpark() 

# if using remote client (i.e. R Client)
mySshUsername <- "user1"
#public facing cluster IP address
mySshHostname <- "12.345.678.90"
mySshSwitches <- "-i /home/yourName/user1.pem" #See NOTE below
myShareDir <- paste("/var/RevoShare", mySshUsername, sep ="/")
myHdfsShareDir <- paste("/user/RevoShare",mySshUsername, sep="/")

myHadoopCluster <- RxSpark(
  hdfsShareDir = myHdfsShareDir,
  shareDir     = myShareDir,
  sshUsername  = mySshUsername,
  sshHostname  = mySshHostname,
  sshSwitches  = mySshSwitches)

# set compute context
rxSetComputeContext("RxSpark")
rxSetComputeContext(myHadoopCluster)

# other compute contexts
rxSetComputeContext("localpar")

rxGetComputeContext()

# ~~~~~~~~~~~
# data access
# ~~~~~~~~~~~

# Example 1
myHdfsFileSystem <- RxFileSystem(fileSystem = "hdfs")

# Example 2
myHdfsFileSystem <- RxFileSystem(fileSystem = "hdfs", 
                                 hostName = "myHost", 
                                 port = 8020)

rxSetFileSystem(fileSystem = myHdfsFileSystem)

POC_data_root <- "/share" # HDFS location of the example data
rxHadoopListFiles(POC_data_root)

# from kbmMain erg_risk46 ~ erg_risk03 + age_gender + county_name

# create rx Hive data source
trainHive <- RxHiveData(query = "select erg_risk46, erg_risk03, age_gender, county_name from sample_table")

# initialize xdf data source. On HDFS, make sure that the file parameter is a path, not a filename
trainXdf <- RxXdfData(file = "/my_HDFS_data_path",
                      fileSystem = myHdfsFileSystem)

# write xdf file to HDFS
rxImport(inData = trainHive, outFile = trainXdf) 

# ~~~~~~~~~~~
# train model
# ~~~~~~~~~~~

form <- erg_risk46 ~ erg_risk03 + age_gender + county_name

trainData <- trainHive
#trainData <- trainXdf

runtime <- system.time(
  mrsModel <- rxBTrees(
    form,
    data = trainData,
    nTree = 10,
    maxDepth = 5,
    learningRate = 0.001,
    minSplit = 10
  )
)

# ~~~~~~~~~~
## reference
# ~~~~~~~~~~

# myShareDir = paste("/var/RevoShare", Sys.info()[["user"]],
#                    sep="/" )
# myHdfsShareDir = paste("/user/RevoShare", Sys.info()[["user"]],
#                        sep="/" )
