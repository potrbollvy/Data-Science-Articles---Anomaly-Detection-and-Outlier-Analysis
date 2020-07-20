pmf_maxent <- function(x,lambda=4/5) (1-4/5)*(4/5)^x
sum(pmf_maxent(0:100))  # check if it's a distribution
mp <- barplot(pmf_maxent(0:15), ylim=c(0,.25), xlab="waiting minutes")
axis(1,at=mp,labels=paste(0:15))