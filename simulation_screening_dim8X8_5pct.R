library(energy)
library(e1071)
library(USP)
library(segmented)

n = 25
p = 8

n.true = 500
n.false = 9500
n.total = n.true + n.false

ts.chisq = numeric(n.total)
ts.dc = numeric(n.total)

# true part 

for(sim in 1:n.true){
  
  pimat1=rep(1/108, p*p)
  chang1=sample(c(1:(p*p)), 10, rep=F)
  pimat1[chang1]=1/20
  pimat=matrix(pimat1/sum(pimat1), p, p, byrow=T)
  sdata1=rmultinom(1, n, pimat1)
  datmat1=matrix(sdata1, p, p, byrow=T)
  
  obs.dc = 0
  obs.chi = 0
  
  for(i in 1:p){
    for(j in 1:p){
      obs.dc = obs.dc + (datmat1[i, j]/n - sum(datmat1[i, ])*sum(datmat1[, j])/n^2)^2
      obs.chi = obs.chi + (datmat1[i, j]/n - sum(datmat1[i, ])*sum(datmat1[, j])/n^2)^2/(sum(datmat1[i, ])*sum(datmat1[, j])/n^2+0.0001)
    }
  }
  
  ts.dc[sim] = obs.dc
  ts.chisq[sim] = obs.chi
  
}

# false part

for(sim in (n.true+1):n.total){
  
  pimat1=rep(1/(p*p), p*p)
  chang1=sample(c(1:(p*p)),p,rep=F)
  pimat1[chang1]=1/(p*p)
  pimat=matrix(pimat1/sum(pimat1), p, p, byrow=T)
  sdata1=rmultinom(1, n, pimat1)
  datmat1=matrix(sdata1, p, p, byrow=T)
  
  obs.dc = 0
  obs.chi = 0
  
  for(i in 1:p){
    for(j in 1:p){
      obs.dc = obs.dc + (datmat1[i, j]/n - sum(datmat1[i, ])*sum(datmat1[, j])/n^2)^2
      obs.chi = obs.chi + (datmat1[i, j]/n - sum(datmat1[i, ])*sum(datmat1[, j])/n^2)^2/(sum(datmat1[i, ])*sum(datmat1[, j])/n^2+0.0001)
    }
  }
  
  ts.dc[sim] = obs.dc
  ts.chisq[sim] = obs.chi
  
}

# using ROC curve
ts.chisq.sort = sort(ts.chisq)
ts.dc.sort = sort(ts.dc)

sens.chi = numeric(n.total)
spec.chi = numeric(n.total)

sens.dc = numeric(n.total)
spec.dc = numeric(n.total)

for(i in 1:n.total){
  
  cut.chi = ts.chisq.sort[i]
  cut.dc = ts.dc.sort[i] 
  sens.chi[i] = sum(ts.chisq[1:n.true]>=cut.chi)/n.true
  sens.dc[i] = sum(ts.dc[1:n.true]>=cut.dc)/n.true
  spec.chi[i] = sum(ts.chisq[(n.true+1):n.total]<=cut.chi)/n.false
  spec.dc[i] = sum(ts.dc[(n.true+1):n.total]<=cut.dc)/n.false
  
}

# plot(1-spec.chi, sens.chi, type = "l")
# lines(1-spec.dc, sens.dc, col = "red")

output = data.frame(n = n, 
                    method = c(rep("Chisq", length(sens.chi)), rep("DCov", length(sens.dc))),
                    sens = c(sens.chi, sens.dc),
                    spec = c(spec.chi, spec.dc))

#write.csv(output, paste0("p", p, "n", n, ".csv"), row.names = F, quote = F)

# using data-driven cutoff
df.dc = data.frame(x = 1:n.total, y = ts.dc.sort)
fit.dc <- lm(y ~ x, data=df.dc)
segmented.fit.dc <- segmented(fit.dc, seg.Z = ~x, npsi=1)
cutpoint.dc = ts.dc.sort[round(summary(segmented.fit.dc)$psi[2])]

df.chisq = data.frame(x = 1:n.total, y = ts.chisq.sort)
fit.chisq <- lm(y ~ x, data=df.chisq)
segmented.fit.chisq <- segmented(fit.chisq, seg.Z = ~x, npsi=1)
cutpoint.chisq = ts.chisq.sort[round(summary(segmented.fit.chisq)$psi[2])]

sens.chi.best = sum(ts.chisq[1:n.true]>=cutpoint.chisq)/n.true
sens.dc.best = sum(ts.dc[1:n.true]>=cutpoint.dc)/n.true
spec.chi.best = sum(ts.chisq[(n.true+1):n.total]<=cutpoint.chisq)/n.false
spec.dc.best = sum(ts.dc[(n.true+1):n.total]<=cutpoint.dc)/n.false

AUC.chi=0
for(i in 1:(n.total-1)){
  trapezoid=(sens.chi[i]+sens.chi[i+1])*(spec.chi[i+1]-spec.chi[i])/2
  AUC.chi=AUC.chi+trapezoid
}

AUC.dc=0
for(i in 1:(n.total-1)){
  trapezoid=(sens.dc[i]+sens.dc[i+1])*(spec.dc[i+1]-spec.dc[i])/2
  AUC.dc=AUC.dc+trapezoid
}

sens.chi.best
sens.dc.best
spec.chi.best
spec.dc.best
AUC.chi
AUC.dc

