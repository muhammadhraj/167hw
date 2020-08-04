## Assignment 6
## 1
mySD <- function(vector){
  mean <- sum(vector)/length(vector)
  sd <- sqrt(sum((vector-mean)^2)/(length(vector)-1))
  return(sd)
}

myMean <- function(vector){
  sum=0
  for(i in 1:length(vector)){
    sum=sum+vector[i]
  }
  return(sum/length(vector))
}

corrMatrix <- function(matrix){
  corr <- matrix(nrow=ncol(matrix), ncol=ncol(matrix), dimnames=list(colnames(matrix), colnames(matrix)))
  for(i in 1:ncol(matrix)){
    for(j in 1:ncol(matrix)){
      corr[i,j] = (sum((matrix[,i]-myMean(matrix[,i]))*(matrix[,j]-myMean(matrix[,j])))/(nrow(matrix)-1))/((mySD(matrix[,i]))*(mySD(matrix[,j])))
    }
  }
  return(corr)
}

## 2a
t.test(sleep$extra[which(sleep$group==1)], sleep$extra[which(sleep$group==2)], paired=TRUE)
## With p-value .0028, we reject the null hypothesis and conclude that the difference in means between the two groups is
## not equal to 0. 

## 2b

## Method 1:
var.test(sleep$extra[which(sleep$group==2)], sleep$extra[which(sleep$group==1)])
## With p-value of .7427, we fail to reject null hypothesis and conclude that
## ratio of variances is equal to 1 (equal variance)

## OR
## Method 2:
ia=sleep$extra[which(sleep$group==1)]
ib=sleep$extra[which(sleep$group==2)]
sa2=var(ia)
sb2=var(ib)
sa2>sb2
F=sb2/sa2
Falpha=qf(0.95, (length(ia)-1), (length(ib)-1))

## Since F-statistic 1.2526 is less than F-alpha, we fail to reject null
## hypothesis and conclude that ratio of variances is equal to 1 and that both
## groups have equal variance.

## 3
## First, load the titanic2.rdata file into your working directory.
Titanic2[2,4]="No"
Titanic2[4,4]="No"
Titanic2[7,5]=17
titanic2table <- table(Titanic2$Class, Titanic2$Survived)
## Not considering Crew as a class
titanic2table <- titanic2table[-4,]
titanic2table[1,1]=sum(Titanic2$Freq[which(Titanic2$Class=="1st" & Titanic2$Survived=="No" & Titanic2$Sex=="Male" & Titanic2$Age=="Adult")])
titanic2table[1,2]=sum(Titanic2$Freq[which(Titanic2$Class=="1st" & Titanic2$Survived=="Yes" & Titanic2$Sex=="Male" & Titanic2$Age=="Adult")])
titanic2table[2,1]=sum(Titanic2$Freq[which(Titanic2$Class=="2nd" & Titanic2$Survived=="No" & Titanic2$Sex=="Male" & Titanic2$Age=="Adult")])
titanic2table[2,2]=sum(Titanic2$Freq[which(Titanic2$Class=="2nd" & Titanic2$Survived=="Yes" & Titanic2$Sex=="Male" & Titanic2$Age=="Adult")])
titanic2table[3,1]=sum(Titanic2$Freq[which(Titanic2$Class=="3rd" & Titanic2$Survived=="No" & Titanic2$Sex=="Male" & Titanic2$Age=="Adult")])
titanic2table[3,2]=sum(Titanic2$Freq[which(Titanic2$Class=="3rd" & Titanic2$Survived=="Yes" & Titanic2$Sex=="Male" & Titanic2$Age=="Adult")])
chisq.test(titanic2table)
## With p-value almost 0, we reject the null hypothesis and conclude that Class and Survived are not independent for adult male passengers.

