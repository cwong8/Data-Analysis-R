#Problem 1
democratic.approval = pres$approval[pres$party == 1]
republican.approval = pres$approval[pres$party == 2]

pvalCalc = function(teststat, teststat.obs){
  num.GreaterThan = length(which(teststat >= teststat.obs)) 
  pval = num.GreaterThan / length(teststat)
  return(pval)
}

permTestSum = function(R, x, y){ 
  data = c(x, y)
  teststat.obs = sum(x)
  teststat = sapply(1:R, function(i){
    Sum = sum(sample(data, size = length(x), replace = FALSE))
    return(Sum)
  })
  pval = pvalCalc(teststat, teststat.obs)
  return(pval)
}

permTestSum(10000 ,democratic.approval, republican.approval)
wilcox.test(democratic.approval, republican.approval, alternative = "greater")

#Problem 2
#test relationship between quarters and approval rating
quarter1.approval = pres$approval[pres$quarter == 1]
quarter2.approval = pres$approval[pres$quarter == 2]
quarter3.approval = pres$approval[pres$quarter == 3]
quarter4.approval = pres$approval[pres$quarter == 4]

x = c(quarter1.approval, quarter2.approval, quarter3.approval, quarter4.approval)
grps = rep(1:4, each = 30)
k = 4

kruskal.test(pres$quarter, pres$approval)

##
###### Utility functions for Tukey HSD Permutation test for Multiple Comparisons
##
"getmaxTij" <- function(x, grps, MSE)
{
  # estimate the maximum of Tij (pairwise mean diff) of a given data x
  trtmeans = getmeans(x,grps)
  nn = table(factor(grps))
  k = length(trtmeans)
  Tijs = matrix(NA,k,k)
  for (i in 2:k) {
    for (j in 1:(i-1)){
      Tijs[i,j] = abs(trtmeans[i] - trtmeans[j])/sqrt(MSE/2 * (1/nn[i] + 1/nn[j]))
    }}
  return(max(Tijs,na.rm=T))
}

"perm.approx.maxTij" <- function(x,grps,MSE,R)
{
  ### obtain the null permutation distribution of maxTij
  results = rep(NA,R)
  for (i in 1:R) results[i] = getmaxTij(x[sample(1:(length(x)),length(x))],grps,MSE)
  return(results)
}

##
###### Tukey HSD Permutation test for Multiple Comparisons
##
"Tukey.HSD" <- function(x, grps, k, alpha=0.05, R=1000)
{
  #Tukey's HSD
  #summary(aov(x ~ factor(grps)))
  nn = table(factor(grps))
  trtmeans = getmeans(x,grps)
  
  (MSE = summary(aov(x ~ factor(grps)))[[1]][2,3])
  ### observed Tij
  Tijs = matrix(NA,k,k)
  for (i in 2:k){
    for (j in 1:(i-1)){
      Tijs[i,j] = abs(trtmeans[i] - trtmeans[j])/sqrt(MSE/2 * (1/nn[i] + 1/nn[j]))
    }}
  
  ### observed maxTij
  #getmaxTij(x,grps,MSE)
  
  ### permutation maxTij
  perm.maxTij = perm.approx.maxTij(x,grps,MSE,R)
  
  pvalsTij = matrix(NA,k,k)
  for (i in 2:k){
    for (j in 1:(i-1)){
      pvalsTij[i,j] = mean(perm.maxTij >= Tijs[i,j])
    }}
  
  ### compare the pairwise pvalue with alpha
  sig = (pvalsTij <= alpha)
  
  out = list(sig=sig, pvalsTij= pvalsTij)
  return(out)
}

Tukey.HSD(x, grps, k, alpha = 0.05, R = 10000)


#Problem 3
x = pres$gdp
y = pres$approval

r.obs = cor(pres$gdp, pres$approval)
n = length(pres$gdp)
Z.value = r.obs*sqrt(n-1)
p.value = pnorm(Z.value, lower.tail = FALSE)


lo = loess(y~x, span = 0.75)
plot(y~x, main="span=0.75", xlab = "GDP", ylab = "Approval rating")
newx = seq(min(x), max(x), length=50)
pred = predict(lo, data.frame(x = newx))
lines(pred~newx, col=2, lwd=1.5)