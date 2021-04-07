library(lattice)
library(viridisLite)
library(leaflet)


# Function to get correlations
createCorrelation <- function(Dat){
  CorM = diag(1,4,4)
  for(i in 1:(nrow(CorM)-1)){
    r1 <- data.frame(as.numeric(Tnew[Dat==i,4]))
    j=i+1
    while(j <= nrow(CorM)){
      r2 <- data.frame(as.numeric(Tnew[Dat==j,4]))
      co1 <- var(r1,r2)/sqrt(var(r1)*var(r2)) #correlation
      CorM[i,j] = co1**(j-1) # the power increases with the distance.
      CorM[j,i] = co1**(j-1)
      j=j+1
    }

  }
  return(CorM)
}

#prepare data to heatmap
prepareHeatmap <- function(Dat){
  y <- 1:max(finalTable$Row)
  x <- 1:max(finalTable$Col)
  z <- matrix("NA", nrow = length(x), ncol = length(y))
  for (i in 1:nrow(Tnew)) {
    ref_col = as.numeric(finalTable$Row[i])
    ref_row = as.numeric(finalTable$Col[i])
    z[ref_row,ref_col] <- as.numeric(Dat[i])
  }
  return(z)
}
