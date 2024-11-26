dcov_wchisq_approx = function(dat.tab){
  
library(e1071)
library(dr)
source("dcovU.R")
  
  r = rowSums(dat.tab)
  c = colSums(dat.tab)
  
  n = sum(dat.tab)
  
  obs = dcovU(dat.tab)
  
  Dx = matrix(NA, n, n)
  Dy = matrix(NA, n, n)
  
  x = numeric()
  y = numeric()
  
  for(a in 1:nrow(dat.tab)){
    for(b in 1:ncol(dat.tab)){
      x = c(x, rep(a, dat.tab[a, b]))
      y = c(y, rep(b, dat.tab[a, b]))
    }
  }
  
  dat = data.frame(x, y)
  
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
  
  chisq.mat = matrix(rchisq(length(w)*10000, df = 1)-1, nrow = 10000, ncol = length(w), byrow = T)
  wchisq = chisq.mat%*%w
  
  p.val.approx = dr.pvalue(coef=w, n*dcorU(dat.tab) + sum(w), chi2approx="wood")[[4]]
  
  p.val.monte.carlo = sum(wchisq>n*dcorU(dat.tab))/10000
  
  return(data.frame(p.val.monte.carlo, 
                    p.val.approx))
  
  }
  
  dat.tab = matrix(c(20, 25, 30, 15, 10, 12, 15, 20, 10, 10), 2, 5)
  # dat.tab = matrix(c(18, 12, 6, 3, 36, 36, 9, 9, 21, 45, 9, 9, 9, 36, 3, 6, 6, 21, 3, 3), 4, 5)
  
dcov_wchisq_approx(dat.tab)
  