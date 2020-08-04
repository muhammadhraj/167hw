##Assignment 4

## 1
plots <- function(dataset){
  if(ncol(dataset)>5){
    return("Too many variables.")
  }else{
    pdf("Plots.pdf")
    colors <- c("red", "blue", "green", "yellow", "magenta")
    par(mfrow=c(ncol(dataset), 2))
    for(variable in 1:ncol(dataset)){
      hist(dataset[,variable], col=colors[variable], xlab=NULL, main=colnames(dataset[variable]))
      boxplot(dataset[,variable], col=colors[variable], main=colnames(dataset[variable]))
    }
    dev.off()
  }
}

## 2
variance <- function(matrix){
  colMeans <- apply(matrix, 2, sum)/length(matrix[,1])
  swept <- sweep(matrix, 2, colMeans, "-")^2
  variance <- apply(swept, 2, sum)/(length(matrix[,1])-1)
  return(variance)
}

## 3
matrixProduct <- function(matrix, vector){
  if(ncol(matrix)!=length(vector)){
    return("Check input. Number of matrix columns and length of vector must be equal.")
  }else{
    return(as.matrix(apply(sweep(matrix, 2, vector, "*"), 1, sum)))
  }
}