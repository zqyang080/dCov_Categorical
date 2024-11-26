library(energy)
library(e1071)
library(USP)
source("dcovU.R")

n = 200
p = 10

n.sim = 10000

p.val = numeric(n.sim)
p.val.wchisq = numeric(n.sim)
p.val.chisq = numeric(n.sim)
ndcov = numeric(n.sim)

pi.mat = matrix(0, p, p)
for(i in 1:p){
  for(j in 1:p){
    pi.mat[i, j] = (2^(-i-j))/(1-2^(-p))/(1-2^(-p))
  }
}

eps = 1/25
cc = matrix(0, p, p)
cc[1,1] = cc[1,1] + eps
cc[2,2] = cc[2,2] + eps
cc[1,2] = cc[1,2] - eps
cc[2,1] = cc[2,1] - eps

c.vec = as.vector(cc)

pi.vec = as.vector(pi.mat) + c.vec

pimat1 = matrix(pi.vec, p, p)

for(sim in 1:length(p.val)){
  
  x.y = data.frame(x = as.factor(rep(1:p, each = p)), y = as.factor(rep(1:p, p)))
  dat.rand = x.y[sample(1:(p*p), n, prob = pimat1/sum(pimat1), replace = T), ]
  ndcov[sim] = n*dcorU(table(dat.rand))
  p.val.chisq[sim] = 1-pchisq(ndcov[sim]+1, 1)
  
  dat = dat.rand
  
  dat.tab = table(dat)
  
  r = rowSums(dat.tab)
  c = colSums(dat.tab)
  
  obs = dcovU(dat.tab)
  
  DC = numeric(10000)
  
  for(k in 1:length(DC)){
    
    tab.perm = r2dtable(1, r = r, c = c)[[1]]
    
    DC[k] = dcovU(tab.perm)
    
  }
  
  p.val[sim] = sum(DC>obs)/10000
  
}

Dx = matrix(NA, n, n)
Dy = matrix(NA, n, n)

for(i in 1:n){
  
  for(j in 1:n){
    
    Dx[i, j] = ifelse(dat$x[i] == dat$x[j], 0, 1)
    Dy[i, j] = ifelse(dat$y[i] == dat$y[j], 0, 1)
    
  }
  
}

I = diag(n)
J = matrix(1, n, n)

H = I - J/n

eig.x = eigen(H%*%Dx%*%H/n)$values
eig.x = ifelse(abs(eig.x)>0.0001, eig.x, 0)
eig.x = eig.x[eig.x!=0]

eig.y = eigen(H%*%Dy%*%H/n)$values
eig.y = ifelse(abs(eig.y)>0.0001, eig.y, 0)
eig.y = eig.y[eig.y!=0]

w = as.vector(eig.x%*%t(eig.y))/sqrt(sum(eig.x^2)*sum(eig.y^2))

chisq.mat = matrix(rchisq(length(w)*10000, df = 1)-1+1/length(w), nrow = 10000, ncol = length(w), byrow = T)
wchisq = chisq.mat%*%w

for(i in 1:n.sim){
  p.val.wchisq[i] = sum(wchisq>ndcov[i])/10000
}

DC.power = sum(p.val<0.05)/length(p.val)
DC.power

Chisq.power = sum(p.val.chisq<0.05)/length(p.val.chisq)
Chisq.power

WChisq.power = sum(p.val.wchisq<0.05)/n.sim
WChisq.power

