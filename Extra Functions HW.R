##Extra Functions Homework
##1
myMax <- function(x, y){
  if(x>y){
    return(x)
  }else{
    return(y)
  }
}

##2
result <- function(score){
  if(score>100 || score<0){
    print("Invalid score")
  }else{
    if(score>50){
      print("You passed")
    }else{
      print("Try again")
    }
  }
}

##3
taxesOwed <- function(income){
  if(income>80000){
    taxesOwed=income*.3
  }else{
    taxesOwed=income*.2
  }
  return(taxesOwed)
}

