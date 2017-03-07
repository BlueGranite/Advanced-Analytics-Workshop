# Fuzzy clustering example
# BlueGranite, Inc. 2016

# ensure package 'pacman' for package management is installed
if (!require("pacman")) install.packages("pacman")
library(pacman)

# add any required packages to the p_load() function parameters 
pacman::p_load(dplyr,  # data wrangling
               fclust, # fuzzy clustering
               plyr,   # data wrangling
               ggrepel, # for cleaner annotations in plots
               plotly, # for interactive plots
               factoextra, # PCA plots
               gridExtra, 
               update=FALSE)
pacman::p_loaded() # check which packages are loaded

# load and scale data
data("iris")

# scaled data frame of Supply Chain values
dSCM <- data.frame(cbind(scale(iris[1:4]), (iris[,"Species"]))) 

dSCM <- dplyr::select(dSCM, c(volume = Sepal.Length, # sales volume
                              holding.cost = Sepal.Width,
                              stockout.cost = Petal.Length,
                              shelf.life = Petal.Width,
                              original.group = V5)) 
dSCM$ID = seq.int(nrow(dSCM))
rm(iris) 

# calc principal components
myPCA    <- prcomp(dSCM[,1:4])
dSCM$PC1 <- myPCA$x[,1]
dSCM$PC2 <- myPCA$x[,2]

# original values prior to clustering
# create plot object using 'factoextra' package
p0 <- fviz_pca_biplot(myPCA, label = "var") +
  labs(x = "PC1", y = "PC2") +
  ggtitle("Original values prior to clustering, in principal component space") +
  theme_bw()

# plot
p0

# function to get the convex hull of each unique point set 
#   to draw polygon around clusters
findHull  <- function(df) df[chull(df$PC1, df$PC2),]

# fuzzy k-means (FKM) clustering (using 'fclust' package)
set.seed(93)
fitFKM = FKM(dSCM[,1:4], k=3) # fit clusters
dSCM$FKM.cluster  <- fitFKM$clus[,1] # cluster index
dSCM$FKM.clus.mem <- fitFKM$clus[,2] # cluster membership degree

# get the convex hull of each unique point set for FKM clusters
hulls.FKM <- ddply(dSCM, "FKM.cluster", findHull)

# create plot object
p1 <- ggplot(dSCM, aes(x     = PC1,
                       y     = PC2,
                       color = factor(FKM.cluster),
                       fill  = factor(FKM.cluster),
                       size  = FKM.clus.mem)) +
  geom_point           (alpha = 0.6) + 
  scale_colour_manual  (name = "Cluster", values = c("red", "green", "blue")) +
  scale_fill_manual    (name = "Cluster", values = c("red", "green", "blue")) +
  geom_polygon         (data = hulls.FKM , alpha = 0.1, lwd = 0.0001) + 
  scale_size_continuous(name = "Membership degree", range = c(0,4)) + 
  # geom_text(aes(y = PC2 + 0.04, 
  #               x = PC1 + 0.04, 
  #               label=ID), 
  #           size = 3, 
  #           vjust = 0, 
  #           hjust = 0) + 
  theme_bw() +
  theme(legend.position = "right") +
  ggtitle("K-means clusters in principal component space")

# plot
p1

# calc primary and seconday cluster assignment and membership degrees
#####################################################################
# cluster membership degrees
clusMem             <- data.frame(round(fitFKM$U, 3)) 


colnames(clusMem)   <- paste("mem",
                                colnames(clusMem),
                                sep = "_")

# primary cluster index
clusMem$primClus    <- fitFKM$clus[,1] 

# primary cluster membership degree
clusMem$primClusMem <- round(fitFKM$clus[,2],3) 

# secondary cluster index
clusMem$secClus    <- apply(clusMem, 
                              1,
                              function(x) 
                                which(rank(x, ties.method = "first")==2))

# secondary cluster membership degree
clusMem$secClusMem <- apply(clusMem, 
                              1,
                              function(x) 
                                x[which(rank(x, ties.method = "first")==2)])   

# add calculated cols of results to table
dSCM           <- cbind(dSCM, clusMem)

# # calc sum of membership across clusters
dSCM$tot       <- rowSums(round(fitFKM$U, 3))

# probability of being in outlier cluster
dSCM$probOut   <- round(1 - dSCM$tot, 3)

# probability of being in a fuzzy cluster
dSCM$probFuzzy <- round(1-clusMem$primClusMem, 3)

# initialize final cluster column with original primary
#   cluster index
dSCM$finalClus <- dSCM$primClus

# calculate gray area membership (i.e. "fuzzy cluster")
#######################################################
# set fuzzy threshhold. Any item that 
#  has a probability of being in a fuzzy cluster (probFuzzy)
#  greater than this value will be included
fuzThresh <- 0.40

# row indices of items that have a probability of being in the fuzzy cluster
# greater than set fuzzy threshold
fuzInd <- which(dSCM$probFuzzy > fuzThresh)

# for this example, we are excluding cluster 2 from fuzziness consideration
includedClus <- which(dSCM$primClus != 2)

# which rows are in the fuzzy cluster
fuzRows <- intersect(fuzInd, includedClus)

# assign these rows to the fuzzy cluster
dSCM$finalClus[fuzRows] <- "fuzzy"

# get convex hull for final clusters 
hulls.Fuzzy <- ddply(dSCM, "finalClus", findHull)

# create plot object
p2 <- ggplot(dSCM, aes(x     = PC1,
                       y     = PC2,
                       color = factor(finalClus),
                       fill  = factor(finalClus),
                       size  = FKM.clus.mem)) +
  geom_point(alpha = 0.6) +
  scale_colour_manual(name = "Cluster", 
                      values = c("red", "green", "blue", "black")) +
  scale_fill_manual  (name = "Cluster", 
                      values = c("red", "green", "blue", "black")) +
  geom_polygon(data = hulls.Fuzzy , alpha = 0.1, lwd = 0.0001) +
  scale_size_continuous(name = "Membership degree", range = c(0,4)) +
  # geom_text(aes(y = PC2 + 0.04, 
  #               x = PC1 + 0.04, 
  #               label=ID), 
  #           size = 3, 
  #           vjust = 0, 
  #           hjust = 0) + 
  theme_bw() +
  theme(legend.position = "right") +
  ggtitle("K-means clusters in principal component space, with fuzzy cluster")

# plot
p2

# plot all in one view
grid.arrange(p1, p2, ncol = 1)

# plot which labels the rows in the fuzzy cluster
#################################################
# create new column with values for fuzzy cluster
# otherwise, NA
dSCM$fuzClusID <- NA
dSCM$fuzClusID[fuzRows] <- dSCM$ID[fuzRows]

# create plot object
p3 <- ggplot(dSCM, aes(x     = PC1,
                       y     = PC2,
                       color = factor(finalClus),
                       fill  = factor(finalClus),
                       size  = FKM.clus.mem)) +
  geom_point(alpha = 0.6) +
  scale_colour_manual(name = "Cluster", 
                      values = c("red", "green", "blue", "black")) +
  scale_fill_manual  (name = "Cluster", 
                      values = c("red", "green", "blue", "black")) +
  geom_polygon(data = hulls.Fuzzy , alpha = 0.1, lwd = 0.0001) +
  scale_size_continuous(name = "Membership degree", range = c(0,4)) +
  # geom_text(aes(y = PC2 + 0.04,
  #               x = PC1 + 0.04,
  #               label = fuzClusID),
  #           size = 3,
  #           vjust = 0,
  #           hjust = 0) +
  theme_bw() +
  theme(legend.position = "right") +
  ggtitle("K-means with fuzzy cluster and row IDs")

# use ggrepel for cleaner labeling
p3 <- p3 + geom_text_repel(aes(y = PC2 ,
                               x = PC1 ,
                               label = fuzClusID),
                               size = 3,
                               point.padding = unit(1e-01, "lines"))

# plot
p3

# Interactive plot using plotly 
# https://plot.ly/r/text-and-annotations/
dSCM %>% 
  plot_ly(x = ~PC1, 
          y = ~PC2, 
          mode = "markers", 
          color = ~finalClus, 
          size = ~probFuzzy, 
          hoverinfo = "text",
          type = "scatter",
          text = paste("ID = ", 
                       dSCM$ID, 
                       "Fuzzy Cluster Prob = ", 
                       dSCM$probFuzzy)) %>% 
  layout(title ="K-means with fuzzy cluster")


# summary table to inspect fuzzy cluster probability
dSCM.fuz <- dSCM[fuzRows, c("ID", 
                            "mem_Clus.1",
                            "mem_Clus.2",
                            "mem_Clus.3", 
                            "probFuzzy")]

head(dSCM.fuz)
# end

###########################################################
# extra content; plot of original groups from Iris data set;
#   regular k-means from stats package

# get the convex hull of each unique point set to draw polygon
hulls.original <- ddply(dSCM, "original.group", findHull)

# plots of original product groups
##################################
# create plot object
p0 <- ggplot(dSCM, aes(x = PC1
                 , y =  PC2
                 , color = factor(original.group)
                 , fill  = factor(original.group))) +
  geom_point() +
  scale_colour_manual(values = c("red", "green", "blue")) +
  scale_fill_manual(values = c("red", "green", "blue")) +
  geom_polygon(data = hulls.original, alpha = 0.1) +
  geom_text(aes(y = PC2 + 0.04,
                x = PC1 + 0.04,
                label = ID),
            size = 3,
            vjust = 0,
            hjust = 0) +
  theme_bw() +
  theme(legend.position = "right") +
  ggtitle("Plot of original product groups in principal component space")

# plot
p0

# plot all in one view
grid.arrange(p0, p1, p2, p3, ncol = 2)
