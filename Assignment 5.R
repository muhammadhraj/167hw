## Assignment 5

## 1
## Although the observed quantiles from the mystery distribution 
## do form a perfectly straight line, the quantiles do not agree
## with the expected quantiles from a normal distribution.
## Therefore, the mystery distribution is not a normal distribution.


## 2
tPlot <- function(vector, degrees){
  scaled <- scale(vector)
  points <- seq(from=0, to=1, by=.01)
  quantiles <- quantile(scaled, points)
  plot(qt(points, degrees), quantiles, xlab="Theoretical t-Distribution Quantiles",
       ylab="Sample Quantiles", main="Student's t Quantile Test")
  abline(0, 1, col="red")
}


## 3
## After downloading the "hondas.xlsx" file, save it as a
## tab-delimited .txt file in your working directory.
hondas <- read.table("hondas.txt", header=TRUE)
p <- ggplot(hondas, aes(x=Year, y=price, color=Miles))
p+geom_point()+facet_wrap(.~hondas$type)

## Looking at the graph, we can see that as year increases, so
## does the price of the car which makes sense because newer cars
## are usually more expensive. We can also see that as year 
## increases, the mileage on the car goes down (gets darker)
## which makes sense since newer cars have less time on the road
## than older cars. This also shows that on average, a car with 
## less miles on it has a higher value than one with more miles.
## Also, EX Civics look to be a little more expensive than the 
## LX's which is reasonable since EX's come with more features.


## 4
## After downloading the chocolatechips.r data file, put it in
## your working directory.
x <- c(0, table(chocolatechips))
f <- seq(0, 9)
d1 <- dpois(f[1:9], mean(chocolatechips))
d2 <- 1-sum(d1)
chisq.test(x, p=c(d1, d2))

## With p-value .9463, we fail to reject the null hypothesis and
## conclude that chocolate chips are distributed randomly among
## the cookies. The number of chocolate chips in a cookie do not
## follow a poisson distribution.