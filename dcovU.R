dcovU = function(dat){
  
  p = nrow(dat)
  n = sum(dat)
  
  pai = dat/sum(dat)
  pii = rowSums(pai)
  pjj = colSums(pai)
  
  part1 = 0
  part2 = 0
  
  for(i in 1:p){
    for(j in 1:p){
      part1 = part1 + (pai[i,j] - pii[i]*pjj[j])^2
      part2 = part2 + pai[i,j]*pii[i]*pjj[j]
    }
  }
  
 return(n/(n-3)*part1 - 4*n/(n-2)/(n-3)*part2 + n/(n-1)/(n-3)*(sum(pii^2) + sum(pjj^2)) + 
  n*(3*n-2)/(n-1)/(n-2)/(n-3)*sum(pii^2)*sum(pjj^2)-n/(n-1)/(n-3))
  
  }
  

 
dcorU = function(dat){
  
  p = nrow(dat)
  n = sum(dat)
  
  pai = dat/sum(dat)
  pii = rowSums(pai)
  pjj = colSums(pai)
  
  part1 = 0
  part2 = 0
  
  for(i in 1:p){
    for(j in 1:p){
      part1 = part1 + (pai[i,j] - pii[i]*pjj[j])^2
      part2 = part2 + pai[i,j]*pii[i]*pjj[j]
    }
  }
  
  dcov = n/(n-3)*part1 - 4*n/(n-2)/(n-3)*part2 + n/(n-1)/(n-3)*(sum(pii^2) + sum(pjj^2)) + 
           n*(3*n-2)/(n-1)/(n-2)/(n-3)*sum(pii^2)*sum(pjj^2)-n/(n-1)/(n-3)
  
  dvx = n^3/(n-1)/(n-2)/(n-3)*(1-sum(pii^2))^2 - 2*n^2/(n-2)/(n-3)*sum(pii^3) + n*(3*n+2)/(n-2)/(n-3)*sum(pii^2) -
       n*(n+2)/(n-2)/(n-3)
    
  dvy = n^3/(n-1)/(n-2)/(n-3)*(1-sum(pjj^2))^2 - 2*n^2/(n-2)/(n-3)*sum(pjj^3) + n*(3*n+2)/(n-2)/(n-3)*sum(pjj^2) -
    n*(n+2)/(n-2)/(n-3)
    
  return(dcov/(sqrt(dvx*dvy)))
  
}

# dcorU(matrix(c(10, 0, 1, 1, 10, 2, 3, 5, 10), 3, 3))



