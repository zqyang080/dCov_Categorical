dcov_screening = function(Y, X, var.names, min.count = 20){

library(segmented)

dcov = numeric()
var.lst.upd = character()

for(i in 1:ncol(dat)){
  
tab = table(Y, as.factor(dat[,i]))
n = sum(tab)
if(n>min.count){
var.lst.upd = c(var.lst.upd, var.lst[i]) 

obs.dc = 0
n = sum(tab)
for(k in 1:nrow(tab)){
  for(j in 1:ncol(tab)){
    obs.dc = obs.dc + (tab[k, j]/n - sum(tab[k, ])*sum(tab[, j])/n^2)^2
  }
}
dcov = c(dcov, n*obs.dc)
}
}

dcov.sort = sort(dcov)
dcov.temp = data.frame(var.lst.upd, dcov)

df.dcov = data.frame(x = 1:length(dcov), y = dcov.sort)
fit.dcov <- lm(y ~ x, data=df.dcov)
segmented.fit.dcov <- segmented(fit.dcov, seg.Z = ~x, npsi=1)
cutpoint.dcov = dcov.sort[round(summary(segmented.fit.dcov)$psi[2])]
identified.var.list = dcov.temp[dcov>cutpoint.dcov, 1]

plot(c(1:length(dcov)), dcov.sort, type = "l", ylab = "dcov", xlab = "dcov.rank", main = "Distance covariance screening")
abline(h=cutpoint.dcov, col = "red", lty = 2)
text(100, cutpoint.dcov, "cut-point", pos = 3, col="red")

return(list(cutpoint.dcov, identified.var.list))

}

