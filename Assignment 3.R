## Assignment 3

## 1
## for
product1 <- function(vector){
  if(class(vector)!="numeric"){
    return("Input must be numeric.")
  }
  product <- vector[1]
  if(length(vector)>1){
    for(i in 2:length(vector)){
      product <- product*vector[i]
    }
  }
  return(product)
}

## while
product2 <- function(vector){
  if(class(vector)!="numeric"){
    return("Input must be numeric.")
  }
  product <- vector[1]
  if(length(vector)>1){
    i=2
    while(i<=length(vector)){
      product <- product*vector[i]
      i=i+1
    }
  }
  return(product)
}

## repeat
product3 <- function(vector){
  if(class(vector)!="numeric"){
    return("Input must be numeric.")
  }
  product <- vector[1]
  if(length(vector)>1){
    i=2
    repeat{
      product <- product*vector[i]
      i=i+1
      if(i>length(vector)){
        break
      }
    }
  }
  return(product)
}


## 2
## Helper function
mySum <- function(input){
  runningSum=0
  for(i in 1:length(input)){
    runningSum=runningSum+input[i]
  }
  return(runningSum)
}

## for
matrixProduct1 <- function(matrix1, matrix2){
  if(ncol(matrix1)!=nrow(matrix2)){
    return("Matrix multiplication not possible.")
  }else{
    product <- matrix(nrow=nrow(matrix1), ncol=ncol(matrix2))
    for(i in 1:nrow(matrix1)){
      for(j in 1:ncol(matrix2)){
        product[i, j] <- mySum(matrix1[i,]*matrix2[,j])
      }
    }
    return(product)
  }
}

## while
matrixProduct2 <- function(matrix1, matrix2){
  if(ncol(matrix1)!=nrow(matrix2)){
    return("Matrix multiplication not possible.")
  }else{
    product <- matrix(nrow=nrow(matrix1), ncol=ncol(matrix2))
    i=1
    while(i<=nrow(matrix1)){
      j=1
      while(j<=ncol(matrix2)){
        product[i,j] <- mySum(matrix1[i,]*matrix2[,j])
        j=j+1
      }
      i=i+1
    }
    return(product)
  }
}

## repeat
matrixProduct3 <- function(matrix1, matrix2){
  if(ncol(matrix1)!=nrow(matrix2)){
    return("Matrix multiplication not possible.")
  }else{
    product <- matrix(nrow=nrow(matrix1), ncol=ncol(matrix2))
    i=1
    repeat{
      j=1
      repeat{
        product[i,j] <- mySum(matrix1[i,]*matrix2[,j])
        j=j+1
        if(j>ncol(matrix2)){
          break
        }
      }
      i=i+1
      if(i>nrow(matrix1)){
        break
      }
    }
    return(product)
  }
}


## 3
## for
findPosition1 <- function(number, vector){
  if(class(number)!="numeric" | class(vector)!="numeric"){
    return("Input must be numeric.")
  }
  for(i in vector){
    if(number==vector[i]){
      return(position <- i)
    }
  }
  return("Not found.")
}

## while
findPosition2 <- function(number, vector){
  if(class(number)!="numeric" | class(vector)!="numeric"){
    return("Input must be numeric.")
  }
  i=1
  while(i<=length(vector)){
    if(number==vector[i]){
      return(position <- i)
    }
    i=i+1
  }
  return("Not found.")
}

## repeat
findPosition3 <- function(number, vector){
  if(class(number)!="numeric" | class(vector)!="numeric"){
    return("Input must be numeric.")
  }
  i=1
  repeat{
    if(number==vector[i]){
      return(position <- i)
    }
    i=i+1
    if(i>length(vector)){
      return("Not found.")
    }
  }
}


## 4
## for
nNaturalNumbers1 <- function(x){
  if(class(x)!="numeric"){
    return("Input must be numeric.")
  }
  if(x>=2){
    natural <- numeric(0)
    sum=0
    for(i in 1:x){
      natural <- c(natural, i)
      sum=sum+i
      if(sum>x){
        return(natural)
      }
    }
  }else{
    if(x==1){
      return(1)
    }else{
      return("Not possible for integers less than 1.")
    }
  }
}

## while
nNaturalNumbers2 <- function(x){
  if(class(x)!="numeric"){
    return("Input must be numeric.")
  }
  if(x>2){
    natural <- numeric(0)
    sum=0
    i=1
    while(sum<x & i<=x){
      natural <- c(natural, i)
      sum=sum+i
      if(sum>x){
        return(natural)
      }
      i=i+1
    }
  }else{
    if(x==1){
      return(1)
    }else{
      return("Not possible for integers less than 1.")
    }
  }
}

## repeat
nNaturalNumbers3 <- function(x){
  if(class(x)!="numeric"){
    return("Input must be numeric.")
  }
  if(x>2){
    natural <- numeric(0)
    sum=0
    i=1
    repeat{
      natural <- c(natural, i)
      sum=sum+i
      i=i+1
      if(i>x){
        break
      }
      if(sum>x){
        return(natural)
      }
    }
  }else{
    if(x==1){
      return(1)
    }else{
      return("Not possible for integers less than 1.")
    }
  }
}


## 5
## for
isPrime1 <- function(x){
  if(class(x)!="numeric"){
    return("Input must be numeric.")
  }
  if(x>0){
    isPrime <- TRUE
    for(i in 1:x){
      if(x%%i==0 & i!=1 & i!=x){
        isPrime <- FALSE
      }
    }
    return(isPrime)
  }else{
    return(FALSE)
  }
}

## while
isPrime2 <- function(x){
  if(class(x)!="numeric"){
    return("Input must be numeric.")
  }
  if(x>0){
    isPrime <- TRUE
    i=1
    while(i<=x){
      if(x%%i==0 & i!=1 & i!=x){
        isPrime <- FALSE
      }
      i=i+1
    }
    return(isPrime)
  }else{
    return(FALSE)
  }
}

## repeat
isPrime3 <- function(x){
  if(class(x)!="numeric"){
    return("Input must be numeric.")
  }
  if(x>0){
    isPrime <- TRUE
    i=1
    repeat{
      if(x%%i==0 & i!=1 & i!=x){
        isPrime <- FALSE
      }
      i=i+1
      if(i>x){
        break
      }
    }
    return(isPrime)
  }else{
    return(FALSE)
  }
}