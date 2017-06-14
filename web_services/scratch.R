

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
myHdfsFileSystem1 <- RxFileSystem(fileSystem = "hdfs")
rxSetFileSystem(fileSystem = myHdfsFileSystem1 )

# Example 2
myHdfsFileSystem2 <- RxFileSystem(fileSystem = "hdfs", hostName = "myHost", port = 8020)
rxSetFileSystem(fileSystem = myHdfsFileSystem2 )

POC_data_root <- "/share" # HDFS location of the example data
rxHadoopListFiles(POC_data_root)

trainHive <- RxHiveData(query = "select * from sample_table")
