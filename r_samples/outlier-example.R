
# detecting outliers example from 
# http://www.rdatamining.com/examples/outlier-detection

library(DMwR)
# remove "Species", which is a categorical column
iris2 <- iris[,1:4]
outlier.scores <- lofactor(iris2, k=5)
plot(density(outlier.scores))

# pick top 5 as outliers
outliers <- order(outlier.scores, decreasing=T)[1:5]
# who are outliers
print(outliers)

n <- nrow(iris2)
labels <- 1:n
labels[-outliers] <- "."
biplot(prcomp(iris2), cex=.8, xlabs=labels)

pch <- rep(".", n)
pch[outliers] <- "+"
col <- rep("black", n)
col[outliers] <- "red"
pairs(iris2, pch=pch, col=col)

library(Rlof)
outlier.scores <- lof(iris2, k=5)
# try with different number of neighbors (k = 5,6,7,8,9 and 10)
outlier.scores <- lof(iris2, k=c(5:10))
head(outlier.scores)
