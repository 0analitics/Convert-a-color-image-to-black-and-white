
# Projeckt 

if (!require(jpeg)) install.packages("jpeg")
library(jpeg)

### defining functions

plotJPEG <- function(img) {
  
  res <- dim(img)[2:1]
  dev.new()
  plot(1, 1, 
       xlim = c(1,res[1]), ylim = c(1,res[2]),
       asp = 1, type = "n", xaxs = "i", yaxs = "i", 
       xaxt = "n", yaxt = "n", xlab = "", ylab = "", bty = "n")
  rasterImage(img, 1, 1, res[1], res[2])
  
}

convertToGray <- function(img, method = "weighted") {
  
  for (i in 1:dim(img)[1]) {
    for (j in 1:dim(img)[2]) {
      if (method == "average") {
        img[i,j,] <- (img[i,j,1]+img[i,j,2]+img[i,j,3])/3
      } else {
        img[i,j,] <- 0.299*img[i,j,1]+0.587*img[i,j,2]+0.114*img[i,j,3]
      }
    }
  }
  
  img
}

binarize <- function(img, threshold = "average") {
  
  if (threshold == "average") {
    threshold <- mean(img)
  } else if (threshold == "median") {
    threshold <- median(img)
  }
  
  for (i in 1:dim(img)[1]) {
    for (j in 1:dim(img)[2]) {
      img[i,j,] <- ifelse(img[i,j,1]<threshold, 0, 1)
    }
  }
  
  img
}

### example

# loading the image
i1 <- readJPEG(source = "./cat1.jpeg")

# display the loaded image
plotJPEG(img = i1)

# default method using the weighted method
plotJPEG(convertToGray(i1))

# simple average method
plotJPEG(convertToGray(i1, method = "average"))

# binarization for the default method using the mean value
plotJPEG(binarize(img = convertToGray(img = i1)))

# binarization for the median method
plotJPEG(binarize(img = convertToGray(img = i1), threshold = "median"))

# binarization for an arbitrary value method
plotJPEG(binarize(img = convertToGray(img = i1), threshold = .35))
