library(USP)
source("dcov_wchisq_approx.R")
set.seed(12345)

eye.color = matrix(c(20, 25, 30, 15, 10, 12, 15, 20, 10, 10), 2, 5)
marital.status = matrix(c(18, 12, 6, 3, 36, 36, 9, 9, 21, 45, 9, 9, 9, 36, 3, 6, 6, 21, 3, 3), 4, 5)
  
dcov_wchisq_approx(eye.color)
USP.test(eye.color,B = 9999)$p.value

dcov_wchisq_approx(marital.status)  
USP.test(marital.status,B = 9999)$p.value
